# Calculate Defensive Formation Probabilities

bdb_defensive_formation_probs <- function(df_plays, df_tracking, df_preds){
  df_coverages_probability <- df_tracking %>%
    inner_join(df_plays %>%
                 select(gameId, playId, home_team, away_team, possessionTeam,
                        defendersInTheBox, numberOfPassRushers, coverage),
               by = c('gameId', 'playId')) %>%
    left_join(df_preds, by = c('gameId', 'playId', 'nflId')) %>%
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
    # Determine Defensive Formations
    mutate(
      cover_0_man_prob = case_when(
        blitz != 1 ~ prob_man,
        TRUE ~ -1
      ),
      cover_1_man_prob = case_when(
        blitz != 1 & deepest_DB == 0 ~ prob_man,
        blitz != 1 & deepest_DB == 1 ~ prob_deep_zone,
        TRUE ~ -1
      ),
      cover_2_man_prob = case_when(
        blitz != 1 & deepest_DB == 0 & deepest_DB_2 == 0 ~ prob_man,
        blitz != 1 & (deepest_DB == 1 | deepest_DB_2 == 1) ~ prob_deep_zone,
        TRUE ~ -1
      ),
      cover_2_zone_prob = case_when(
        blitz != 1 & deepest_DB == 0 & deepest_DB_2 == 0 ~ prob_underneath_zone,
        blitz != 1 & (deepest_DB == 1 | deepest_DB_2 == 1) ~ prob_deep_zone,
        TRUE ~ -1
      ),
      cover_3_zone_prob = case_when(
        blitz != 1 & deepest_DB == 0 & deepest_DB_2 == 0 & deepest_DB_3 == 0 ~ prob_underneath_zone,
        blitz != 1 & (deepest_DB == 1 | deepest_DB_2 == 1 | deepest_DB_3 == 1) & position_clustered == 'CB' ~ prob_deep_zone * (2 / 3),
        blitz != 1 & (deepest_DB == 1 | deepest_DB_2 == 1 | deepest_DB_3 == 1) & position_clustered == 'S' ~ prob_deep_zone * (1 / 3),
        TRUE ~ -1
      ),
      cover_4_zone_prob = case_when(
        blitz != 1 & deepest_DB == 0 & deepest_DB_2 == 0 & deepest_DB_3 == 0 & deepest_DB_4 == 0 ~ prob_underneath_zone,
        blitz != 1 & (deepest_DB == 1 | deepest_DB_2 == 1 | deepest_DB_3 == 1 | deepest_DB_4 == 1) ~ prob_deep_zone,
        TRUE ~ -1
      ),
      cover_6_zone_prob = case_when(
        blitz != 1 & deepest_DB == 0 & deepest_DB_2 == 0 & deepest_DB_3 ~ prob_underneath_zone,
        blitz != 1 & (deepest_DB == 1 | deepest_DB_2 == 1 | deepest_DB_3 == 1) & position_clustered == 'CB' ~ prob_deep_zone * (1 / 3),
        blitz != 1 & (deepest_DB == 1 | deepest_DB_2 == 1 | deepest_DB_3 == 1) & position_clustered == 'S' ~ prob_deep_zone * (2 / 3),
        TRUE ~ -1
      )) %>%
    mutate_all(list(~na_if(., -1))) %>%
    group_by(gameId, playId) %>%
    summarise(cover_0_man_prob = mean(cover_0_man_prob, na.rm = T),
              cover_1_man_prob = mean(cover_1_man_prob, na.rm = T),
              cover_2_man_prob = mean(cover_2_man_prob, na.rm = T),
              cover_2_zone_prob = mean(cover_2_zone_prob, na.rm = T),
              cover_3_zone_prob = rescale(mean(cover_3_zone_prob, na.rm = T), from = c(0, 2 / 3)),
              cover_4_zone_prob = mean(cover_4_zone_prob, na.rm = T),
              cover_6_zone_prob = rescale(mean(cover_6_zone_prob, na.rm = T), from = c(0, 2 / 3)),
              defensive_formation = case_when(
                max(cover_0_man_prob, cover_1_man_prob, cover_2_man_prob, cover_2_zone_prob, cover_3_zone_prob, cover_4_zone_prob, cover_6_zone_prob) == cover_0_man_prob ~ 'Cover 0 Man',
                max(cover_0_man_prob, cover_1_man_prob, cover_2_man_prob, cover_2_zone_prob, cover_3_zone_prob, cover_4_zone_prob, cover_6_zone_prob) == cover_1_man_prob ~ 'Cover 1 Man',
                max(cover_0_man_prob, cover_1_man_prob, cover_2_man_prob, cover_2_zone_prob, cover_3_zone_prob, cover_4_zone_prob, cover_6_zone_prob) == cover_2_man_prob ~ 'Cover 2 Man',
                max(cover_0_man_prob, cover_1_man_prob, cover_2_man_prob, cover_2_zone_prob, cover_3_zone_prob, cover_4_zone_prob, cover_6_zone_prob) == cover_2_zone_prob ~ 'Cover 2 Zone',
                max(cover_0_man_prob, cover_1_man_prob, cover_2_man_prob, cover_2_zone_prob, cover_3_zone_prob, cover_4_zone_prob, cover_6_zone_prob) == cover_3_zone_prob ~ 'Cover 3 Zone',
                max(cover_0_man_prob, cover_1_man_prob, cover_2_man_prob, cover_2_zone_prob, cover_3_zone_prob, cover_4_zone_prob, cover_6_zone_prob) == cover_4_zone_prob ~ 'Cover 4 Zone',
                max(cover_0_man_prob, cover_1_man_prob, cover_2_man_prob, cover_2_zone_prob, cover_3_zone_prob, cover_4_zone_prob, cover_6_zone_prob) == cover_6_zone_prob ~ 'Cover 6 Zone',
                TRUE ~ ''
              )) %>%
    select(playId, gameId, cover_0_man_prob, cover_1_man_prob, cover_2_man_prob, cover_2_zone_prob,
           cover_3_zone_prob, cover_4_zone_prob, cover_6_zone_prob, defensive_formation)
  
  df_coverages_probability[, -c(1:2, 10)] <- t(apply(df_coverages_probability[, -c(1:2, 10)], 1, function(x) x / sum(x, na.rm = TRUE)))
  
  return(df_coverages_probability)
}