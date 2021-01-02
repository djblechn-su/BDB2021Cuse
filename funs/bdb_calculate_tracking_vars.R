# Calculate Tracking Variables

bdb_calculate_tracking_vars <- function(df_tracking){
  df_tracking_list <- df_tracking %>%
    group_split(week) # split data by week to save memory
  
  print('Starting Loop')
  
  df_tracking_vars <- {}
  for(i in 1:length(df_tracking_list)){
    start_time <- Sys.time()
    
    df_tracking_vars[[i]] <- df_tracking_list[[i]] %>%
      select(playId, gameId, frameId, nflId, x, y, position,
             position_clustered, prob_nb, prob_cb, prob_s, prob_lb,
             poss_team, team_name, snap, throw, v_x, v_y, v_theta, dir, o, s, dis, pre_play,
             los_x, los_y, boundary, strength, field_strength,
             offensive_players_to_boundary, offensive_players_to_field,
             defensive_players_to_boundary, defensive_players_to_field,
             pre_play_motion, in_play) %>%
      inner_join(df_plays %>%
                   select(playId, gameId, offenseFormation, personnelO, defendersInTheBox,
                          numberOfPassRushers, personnelD, coverage), by = c('playId', 'gameId')) %>% # join with play data
      inner_join(df_tracking_list[[i]] %>%
                   select(playId, gameId, frameId, nflId, x, y, poss_team, team_name,
                          position_clustered, v_theta), by = c('playId', 'gameId', 'frameId')) %>% # join with tracking data to see interactions between players
      rename_at(vars(contains('.x')), function(x) gsub('\\.x', '1', x)) %>% # rename vars in df_tracking 1 to player 1
      rename_at(vars(contains('.y')), function(x) gsub('\\.y', '2', x)) %>% # rename vars in df_tracking 2 to player 2
      filter(team_name1 != 'football' & team_name2 != 'football') %>% # filter out football
      group_by(gameId, playId, nflId1, nflId2) %>%
      mutate(play_throw = sum(throw)) %>% 
      filter(play_throw == 1) %>% # filter out plays where a throw was not made
      mutate(snap_x = x1[snap == 1], # x coord of player 1 at snap
             snap_y = y1[snap == 1], # y coord of player 1 at snap
             throw_x = x1[throw == 1], # x coord of player 1 at throw
             throw_y = y1[throw == 1], # x coord of player 1 at throw
             var_x = var(x1), # variance of x coord
             var_y = var(y1), # variance of y coord
             var_v_x = var(v_x), # variance in velocity in x-direction
             var_v_y = var(v_y), # variance in velocity in y-direction
             avg_v_x = mean(v_x, na.rm = TRUE), # avg of velocity in x-dir
             avg_v_y = mean(v_y, na.rm = TRUE), # avg of velocity in y-dir
             var_v_theta = var(v_theta1), # variance in directional velo
             avg_v_theta = mean(v_theta1, na.rm = TRUE), # avg directional velo
             var_dir = var(dir), # variance of player direction
             avg_dir = mean(dir, na.rm = TRUE), # avg player direction
             var_s = var(s), # variance in speed
             avg_s = mean(s, na.rm = TRUE), # avg speed
             sum_dis = sum(dis), # distance travelled
             pre_play_dis = sum(dis[pre_play == 1]), # pre-play distance travelled
             snap_dis_from_los = snap_x - los_x, # distance from LOS at the snap
             snap_dis_from_boundary = ifelse(boundary == 'Right', snap_y, 53.3 - snap_y), # snap distance from the short side of the field
             snap_dis_from_field = ifelse(boundary == 'Right', 53.3 - snap_y, snap_y), # snap distance from the long side of the field
             throw_dis_from_los = throw_x - los_x, # distance from LOS at the throw
             throw_dis_from_boundary = ifelse(boundary == 'Right', throw_y, 53.3 - throw_y), # snap distance from the short side of the field
             throw_dis_from_field = ifelse(boundary == 'Right', 53.3 - throw_y, throw_y), # snap distance from the long side of the field
             sep = sqrt((x1 - x2) ^ 2 + (y1 - y2) ^ 2), # separation between player 1 and player 2
             avg_sep = mean(sep[in_play == 1], na.rm = TRUE), # average separation between player 1 and player 2 on the play
             avg_pre_play_sep = ifelse(sum(pre_play_motion) > 0, mean(sep[pre_play_motion == 1], na.rm = T), mean(sep[pre_play == 1], na.rm = T)), # average pre-play separation between player 1 and player 2
             snap_sep = sep[snap == 1], # separation at the snap
             throw_sep = sep[throw == 1], # separation at the throw
             same_team = ifelse(poss_team1 == poss_team2, 1, 0), # are players 1 and 2 on the same team
             same_player = ifelse(nflId1 == nflId2, 1, 0)) %>% # are players 1 and 2 the same player
      ungroup() %>%
      mutate(avg_pre_play_sep = ifelse(is.na(avg_pre_play_sep), mean(avg_pre_play_sep, na.rm = T), avg_pre_play_sep)) %>% # if no pre-play, take average separation
      group_by(gameId, playId, nflId1, frameId) %>%
      mutate(closest_offensive_player_id = nflId2[sep == min(sep[same_player == 0 & poss_team2 == 1], na.rm = T)][1], # player ID of closest offensive player at frame
             closest_offensive_player_sep = min(sep[same_player == 0 & poss_team2 == 1], na.rm = T), # separation of closest offensive player at frame
             closest_offensive_player_dir_diff = v_theta1 - v_theta2[nflId2 == closest_offensive_player_id], # difference in direction of closest offensive player at frame
             closest_offensive_player_pre_play_sep = min(avg_pre_play_sep[same_player == 0 & poss_team2 == 1], na.rm = T), # pre-play separation of closest offensive player at frame
             closest_offensive_player_snap_sep = min(snap_sep[same_player == 0 & poss_team2 == 1], na.rm = T), # snap separation of closest offensive player at frame
             closest_offensive_player_throw_sep = min(throw_sep[same_player == 0 & poss_team2 == 1], na.rm = T), # throw separation of closest offensive player at frame
             closest_defensive_player_id = nflId2[sep == min(sep[same_player == 0 & poss_team2 == 0], na.rm = T)][1], # same but defensive player
             closest_defensive_player_sep = min(sep[same_player == 0 & poss_team2 == 0], na.rm = T),
             closest_defensive_player_dir_diff = v_theta1 - v_theta2[nflId2 == closest_defensive_player_id],
             closest_defensive_player_pre_play_sep = min(avg_pre_play_sep[same_player == 0 & poss_team2 == 0], na.rm = T),
             closest_defensive_player_snap_sep = min(snap_sep[same_player == 0 & poss_team2 == 0], na.rm = T),
             closest_defensive_player_throw_sep = min(throw_sep[same_player == 0 & poss_team2 == 0], na.rm = T)) %>%
      filter(poss_team1 == 0 & !is.na(closest_offensive_player_sep)) %>% # filter out offensive players
      group_by(gameId, playId, nflId1) %>%
      summarise(offensive_players_to_boundary = mean(offensive_players_to_boundary),
                offensive_players_to_field = mean(offensive_players_to_field),
                defensive_players_to_boundary = mean(defensive_players_to_boundary),
                defensive_players_to_field = mean(defensive_players_to_field),
                snap_dis_from_los = mean(snap_dis_from_los),
                snap_dis_from_boundary = mean(snap_dis_from_boundary),
                snap_dis_from_field = mean(snap_dis_from_field),
                throw_dis_from_los = mean(throw_dis_from_los),
                throw_dis_from_boundary = mean(throw_dis_from_boundary),
                throw_dis_from_field = mean(throw_dis_from_field),
                var_x = mean(var_x),
                var_y = mean(var_y),
                var_v_x = mean(var_v_x),
                var_v_y = mean(var_v_y),
                avg_v_x = mean(avg_v_x),
                avg_v_y = mean(avg_v_y),
                var_v_theta = mean(var_v_theta),
                avg_v_theta = mean(avg_v_theta),
                var_dir = mean(var_dir),
                avg_dir = mean(avg_dir),
                var_s = mean(var_s),
                avg_s = mean(avg_s),
                sum_dis = mean(sum_dis), # take mean of all vars before this one to have it at the player level
                var_closest_offensive_player_sep = var(closest_offensive_player_sep), # variance in separation of closest offensive player
                var_closest_defensive_player_sep = var(closest_defensive_player_sep), # variance in separation of closest defensive player
                avg_closest_offensive_player_sep = mean(closest_offensive_player_sep), # avg separation of closest offensive player
                avg_closest_defensive_player_sep = mean(closest_defensive_player_sep), # avg separation of closest defensive player
                var_closest_offensive_player_dir = var(closest_offensive_player_dir_diff), # same with directional difference
                var_closest_defensive_player_dir = var(closest_defensive_player_dir_diff),
                avg_closest_offensive_player_dir = mean(closest_offensive_player_dir_diff),
                avg_closest_defensive_player_dir = mean(closest_defensive_player_dir_diff),
                closest_offensive_player_pre_play_sep = mean(closest_offensive_player_pre_play_sep), # same but pre-play separation
                closest_defensive_player_pre_play_sep = mean(closest_defensive_player_pre_play_sep),
                closest_offensive_player_snap_sep = mean(closest_offensive_player_snap_sep), # same but snap separation
                closest_defensive_player_snap_sep = mean(closest_defensive_player_snap_sep),
                closest_offensive_player_throw_sep = mean(closest_offensive_player_throw_sep), # same but throw separation
                closest_defensive_player_throw_sep = mean(closest_defensive_player_throw_sep),
                closest_offensive_player_changes = sum(closest_offensive_player_id != lag(closest_offensive_player_id) & !is.na(lag(closest_offensive_player_id))), # number of times the closest offensive player changes during the play
                closest_defensive_player_changes = sum(closest_defensive_player_id != lag(closest_defensive_player_id) & !is.na(lag(closest_defensive_player_id))), # number of times the closest defensive player changes during the play
                pre_play_motion = pre_play_motion[1],
                position = position[1],
                position_clustered = position_clustered1[1],
                prob_lb = prob_lb[1],
                prob_nb = prob_nb[1],
                prob_cb = prob_cb[1],
                prob_s = prob_s[1],
                offensive_formation = offenseFormation[1],
                defensive_formation = coverage[1],
                offensive_personnel = personnelO[1],
                defensive_personnel = personnelD[1],
                defenders_in_box = defendersInTheBox[1],
                number_pass_rushers = numberOfPassRushers[1],
                field_strength = field_strength[1]) %>% # bring all other vars down
      ungroup() %>%
      dplyr::rename(nflId = nflId1) %>%
      mutate(offensive_formation = ifelse(is.na(offensive_formation), 'UNKNOWN', offensive_formation),
             defenders_in_box = ifelse(is.na(defenders_in_box), mean(defenders_in_box, na.rm = T), defenders_in_box),
             number_pass_rushers = ifelse(is.na(number_pass_rushers), mean(number_pass_rushers, na.rm = T), number_pass_rushers),
             closest_offensive_player_pre_play_sep = ifelse(is.infinite(closest_offensive_player_pre_play_sep), mean(is.finite(closest_offensive_player_pre_play_sep), na.rm = T), closest_offensive_player_pre_play_sep),
             closest_defensive_player_pre_play_sep = ifelse(is.infinite(closest_defensive_player_pre_play_sep), mean(is.finite(closest_defensive_player_pre_play_sep), na.rm = T), closest_defensive_player_pre_play_sep)) # take average if NA or infinite
    
    end_time <- Sys.time()
    print(paste('Took', round(end_time - start_time, 2), 'minutes for week', i))
  }
  
  df_tracking_vars <- do.call('rbind', df_tracking_vars)
  
  return(df_tracking_vars)
}
