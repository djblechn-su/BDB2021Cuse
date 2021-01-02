# Read Tracking Data

bdb_read_tracking <- function(weeks){
  # blank list to store tracking data
  df_tracking <- {}
  
  # iterating through all weeks
  for(w in weeks){
    
    # read weekly data into list
    # standardizing tracking data so its always going right, add directional movement data - eliminate post play frames
    # filter out plays - inside 12, less than 15 frames or over 55 frames, not exactly 1 qb, any special teams players
    df_tracking[[w]] <- fread(paste0("../input/nfl-big-data-bowl-2021/week", w, ".csv")) %>%
      group_by(gameId, playId) %>%
      dplyr::filter(n_distinct(team) == 3) %>%
      left_join(read.csv('../input/nfl-big-data-bowl-2021/players.csv'), 'nflId') %>%
      mutate(displayName = ifelse(is.na(displayName.x), displayName.y, displayName.x),
             position = ifelse(is.na(position.x), position.y, position.x),
             position = ifelse(position.x != position.y, position.y, position)) %>%
      select(-c(displayName.x, displayName.y, position.x, position.y)) %>%
      mutate(x = ifelse(playDirection == "left", 120 - x, x),
             y = ifelse(playDirection == "left", 160/3 - y, y),
             dir = case_when(
               playDirection == "left" & dir <= 180 ~ 180 + dir,
               playDirection == "left" & dir > 180 ~ dir - 180,
               TRUE ~ dir
             )) %>%
      inner_join(read_csv("../input/nfl-big-data-bowl-2021/games.csv", col_types = cols()) %>%
                   select(gameId, homeTeamAbbr, visitorTeamAbbr),
                 by = c('gameId' = 'gameId')) %>%
      mutate(dir_rad = dir * pi / 180,
             v_x = sin(dir_rad) * s,
             v_y = cos(dir_rad) * s,
             v_theta = atan(v_y / v_x),
             v_theta = ifelse(is.nan(v_theta), 0, v_theta),
             team_name = case_when(team == "home" ~ homeTeamAbbr, team == "away" ~ visitorTeamAbbr, TRUE ~ team),
             poss_team = ifelse(route != '' | position == 'QB', 1, NA),
             los_x = ifelse(team_name == 'football' & frameId == frameId[event == 'ball_snap'][1], x, NA),
             los_y = ifelse(team_name == 'football' & frameId == frameId[event == 'ball_snap'][1], y, NA)) %>%
      group_by(gameId, playId, team_name) %>%
      fill(poss_team, .direction = 'downup') %>%
      mutate(poss_team = ifelse(is.na(poss_team) & team_name != 'football', 0, 1),
             left_side_offensive_player_snap = ifelse(frameId == frameId[event == 'ball_snap'][1] & y >= 29.96667 & poss_team == 1, 1, 0),
             right_side_offensive_player_snap = ifelse(frameId == frameId[event == 'ball_snap'][1] & y <= 23.36667 & poss_team == 1, 1, 0),
             left_side_defensive_player_snap = ifelse(frameId == frameId[event == 'ball_snap'][1] & y >= 29.96667 & poss_team == 0, 1, 0),
             right_side_defensive_player_snap = ifelse(frameId == frameId[event == 'ball_snap'][1] & y <= 23.36667 & poss_team == 0, 1, 0)) %>%
      ungroup() %>%
      group_by(gameId, playId) %>%
      fill(c(los_x, los_y), .direction = 'downup') %>%
      mutate(total_left_side_offensive_players = sum(left_side_offensive_player_snap, na.rm = T),
             total_right_side_offensive_players = sum(right_side_offensive_player_snap, na.rm = T),
             total_left_side_defensive_players = sum(left_side_defensive_player_snap, na.rm = T),
             total_right_side_defensive_players = sum(right_side_defensive_player_snap, na.rm = T),
             left_hash = 29.96667,
             right_hash = 23.36667,
             dist_to_left_hash = left_hash - los_y,
             dist_to_right_hash = los_y - right_hash,
             boundary = ifelse(dist_to_left_hash < dist_to_right_hash, 'Left', 'Right'),
             strength = ifelse(total_right_side_offensive_players > total_left_side_offensive_players, 'Right', 'Left'),
             strength = ifelse(total_right_side_offensive_players == total_left_side_offensive_players, 'Balanced', strength),
             field_strength = ifelse((boundary == 'Left' & strength == 'Left') | (boundary == 'Right' & strength == 'Right'), 'Boundary', 'Field'),
             field_strength = ifelse(total_right_side_offensive_players == total_left_side_offensive_players, 'Balanced', field_strength),
             offensive_players_to_boundary = ifelse(boundary == 'Left', total_left_side_offensive_players, total_right_side_offensive_players),
             offensive_players_to_field = ifelse(boundary == 'Left', total_right_side_offensive_players, total_left_side_offensive_players),
             defensive_players_to_boundary = ifelse(boundary == 'Left', total_left_side_defensive_players, total_right_side_defensive_players),
             defensive_players_to_field = ifelse(boundary == 'Left', total_right_side_defensive_players, total_left_side_defensive_players),
             qbs = sum(position == 'QB' & frameId == 1, na.rm = T),
             sts = sum(position %in% c('LS', 'P'), na.rm = T)) %>%
      ungroup() %>%
      group_by(gameId, playId, nflId) %>%
      mutate(snap = ifelse(frameId == frameId[event == 'ball_snap'][1], 1, 0),
             throw = ifelse(frameId == frameId[event %in% c('pass_forward', 'pass_shovel', 'qb_sack', 'qb_strip_sack', 'run', 'tackle')][1], 1, 0),
             pre_play = ifelse(frameId < frameId[snap == 1][1], 1, 0),
             post_play = ifelse(frameId > frameId[throw == 1][1], 1, 0),
             in_play = ifelse(pre_play == 0 & post_play == 0, 1, 0),
             pre_play_motion = ifelse(event == 'man_in_motion' & pre_play == 1, 1, 0),
             number_frames_in_play = sum(in_play),
             inside_12 = ifelse(los_x[1] > 98, 1, 0)) %>%
      ungroup() %>%
      dplyr::filter(post_play == 0, inside_12 == 0, number_frames_in_play > 15,
             number_frames_in_play < 55, qbs == 1, sts == 0)
    df_tracking[[w]]$week <- w
    
    # see progress
    print(paste('Read Week', w))
  }
  
  # bind rows of dfs to one dataframe containing all weeks - eliminate plays with only one team
  df_tracking <- do.call('bind_rows', df_tracking)
  
  return(df_tracking)
}
