# Create Specifications for Man vs. Zone

bdb_man_zone_specs <- function(df_plays, df_tracking){
  df_specs <- df_tracking %>%
    inner_join(df_plays %>%
                 select(gameId, playId, home_team, away_team, possessionTeam,
                        defendersInTheBox, numberOfPassRushers, coverage),
               by = c('gameId', 'playId')) %>%
    # Calculate QB Coordinates After Pass
    group_by(gameId, playId) %>%
    mutate(qb_end_x = ifelse(length(x[position == 'QB' & event %in% c('pass_forward', 'pass_shovel', 'qb_sack', 'qb_strip_sack', 'run', 'tackle')][1]) == 0, NA, x[position == 'QB' & event %in% c('pass_forward', 'pass_shovel', 'qb_sack', 'qb_strip_sack', 'run', 'tackle')][1]),
           qb_end_y = ifelse(length(x[position == 'QB' & event %in% c('pass_forward', 'pass_shovel', 'qb_sack', 'qb_strip_sack', 'run', 'tackle')][1]) == 0, NA, y[position == 'QB' & event %in% c('pass_forward', 'pass_shovel', 'qb_sack', 'qb_strip_sack', 'run', 'tackle')][1])) %>%
    ungroup() %>%
    # Calculate Each Players End Coordinates
    group_by(gameId, playId, nflId) %>%
    mutate(end_x = ifelse(length(x[event %in% c('pass_forward', 'pass_shovel', 'qb_sack', 'qb_strip_sack', 'run', 'tackle')][1]) == 0, NA, x[event %in% c('pass_forward', 'pass_shovel', 'qb_sack', 'qb_strip_sack', 'run', 'tackle')][1]),
           end_y = ifelse(length(y[event %in% c('pass_forward', 'pass_shovel', 'qb_sack', 'qb_strip_sack', 'run', 'tackle')][1]) == 0, NA, y[event %in% c('pass_forward', 'pass_shovel', 'qb_sack', 'qb_strip_sack', 'run', 'tackle')][1])) %>%
    ungroup() %>%
    # Filter Out Offense and Football - Keep Frame Where Ball Snapped
    dplyr::filter(poss_team == 0 & event == 'ball_snap' & team_name != 'football') %>%
    group_by(gameId, playId, nflId) %>%
    dplyr::filter(row_number() == 1) %>%
    ungroup() %>%
    # Determine 4 Deepest DBs
    group_by(gameId, playId) %>%
    mutate(deepest_DB = ifelse(end_x == max(end_x[position_clustered %in% c('CB', 'S')])[1], 1, 0),
           deepest_DB_2 = ifelse(end_x == Rfast::nth(end_x[position_clustered %in% c('CB', 'S')], 2, descending = T)[1], 1, 0),
           deepest_DB_3 = ifelse(end_x == Rfast::nth(end_x[position_clustered %in% c('CB', 'S')], 3, descending = T)[1], 1, 0),
           deepest_DB_4 = ifelse(end_x == Rfast::nth(end_x[position_clustered %in% c('CB', 'S')], 4, descending = T)[1], 1, 0)) %>%
    ungroup() %>%
    # Determine if Rushed the QB
    mutate(dist_from_qb = sqrt((end_x - qb_end_x) ^ 2 + (end_y - qb_end_y) ^ 2)) %>%
    group_by(gameId, playId) %>%
    mutate(avg_dist_from_qb = (sum(dist_from_qb) - dist_from_qb) / (n() - 1),
           blitz = ifelse(avg_dist_from_qb > 12 & dist_from_qb < 8, 1, 0)) %>% 
    ungroup() %>%
    # Determine Player Coverages
    mutate(player_coverage = case_when(
      coverage == 'Cover 0 Man' ~ 'man',
      coverage == 'Cover 1 Man' & deepest_DB == 0 ~ 'man',
      coverage == 'Cover 1 Man' & deepest_DB == 1 ~ 'deep_zone',
      coverage == 'Cover 2 Man' & deepest_DB == 0 & deepest_DB_2 == 0 ~ 'man',
      coverage == 'Cover 2 Man' & (deepest_DB == 1 | deepest_DB_2 == 1) ~ 'deep_zone',
      coverage == 'Cover 2 Zone' & deepest_DB == 0 & deepest_DB_2 == 0 ~ 'underneath_zone',
      coverage == 'Cover 2 Zone' & (deepest_DB == 1 | deepest_DB_2 == 1) ~ 'underneath_zone',
      coverage == 'Cover 3 Zone' & deepest_DB == 0 & deepest_DB_2 == 0 & deepest_DB_3 == 0 ~ 'underneath_zone',
      coverage == 'Cover 3 Zone' & (deepest_DB == 1 | deepest_DB_2 == 1 | deepest_DB_3 == 1) ~ 'deep_zone',
      coverage == 'Cover 4 Zone' & deepest_DB == 0 & deepest_DB_2 == 0 & deepest_DB_3 == 0 & deepest_DB_4 == 0 ~ 'underneath_zone',
      coverage == 'Cover 4 Zone' & (deepest_DB == 1 | deepest_DB_2 == 1 | deepest_DB_3 == 1 | deepest_DB_4) ~ 'deep_zone',
      coverage == 'Cover 6 Zone' & deepest_DB == 0 & deepest_DB_2 == 0 & deepest_DB_3 == 0 ~ 'underneath_zone',
      coverage == 'Cover 6 Zone' & (deepest_DB == 1 | deepest_DB_2 == 1 | deepest_DB_3 == 1) ~ 'deep_zone',
      blitz == 1 ~ 'blitz',
      TRUE ~ ''),
      player_coverage = ifelse(player_coverage == '', NA, player_coverage)) %>%
    select(playId, gameId, nflId, player_coverage)
  
  return(df_specs)
}
