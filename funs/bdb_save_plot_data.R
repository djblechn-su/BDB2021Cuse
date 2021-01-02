# Save Plot Data

bdb_save_data_data <- function(example_play_gameId, example_play_playId, example_play_week, df_tracking, df_plays, df_tracking_vars, df_preds, df_foae, mod_fo){
  # Example Play
  example_play <- df_tracking %>% filter(gameId == example_play_gameId & playId == example_play_playId)
    
  example_play_full <- fread(paste0("../input/nfl-big-data-bowl-2021/week", example_play_week, ".csv")) %>%
    filter(gameId == example_play_gameId & playId == example_play_playId) %>%
    inner_join(read_csv("../input/nfl-big-data-bowl-2021/games.csv", col_types = cols()) %>%
                 select(gameId, homeTeamAbbr, visitorTeamAbbr),
               by = c('gameId' = 'gameId')) %>%
    mutate(x = ifelse(playDirection == "left", 120 - x, x),
           y = ifelse(playDirection == "left", 160/3 - y, y),
           dir = case_when(
             playDirection == "left" & dir <= 180 ~ 180 + dir,
             playDirection == "left" & dir > 180 ~ dir - 180,
             TRUE ~ dir
           ),
           dir_rad = dir * pi / 180,
           v_x = sin(dir_rad) * s,
           v_y = cos(dir_rad) * s,
           v_theta = atan(v_y / v_x),
           v_theta = ifelse(is.nan(v_theta), 0, v_theta),
           team_name = case_when(team == "home" ~ homeTeamAbbr, team == "away" ~ visitorTeamAbbr, TRUE ~ team),
           poss_team = ifelse(route != '' | position == 'QB', 1, NA),
           los_x = ifelse(team_name == 'football' & frameId == frameId[event == 'ball_snap'][1], x, NA),
           los_y = ifelse(team_name == 'football' & frameId == frameId[event == 'ball_snap'][1], y, NA))
  
  example_play <- example_play %>% 
    bind_rows(example_play_full %>% filter(!(frameId %in% example_play$frameId))) %>%
    left_join(df_plays %>% select(gameId, playId, yardsToGo, playDescription)) %>%
    left_join(nflfastR::teams_colors_logos %>%
                select(team_abbr,
                       home_team_color1 = team_color,
                       home_team_color2 = team_color2,
                       home_team_color3 = team_color3,
                       home_team_color4 = team_color4),
              by = c('homeTeamAbbr' = 'team_abbr')) %>%
    left_join(nflfastR::teams_colors_logos %>%
                select(team_abbr,
                       away_team_color1 = team_color,
                       away_team_color2 = team_color2,
                       away_team_color3 = team_color3,
                       away_team_color4 = team_color4),
              by = c('visitorTeamAbbr' = 'team_abbr')) %>%
    left_join(example_play %>%
                dplyr::filter(event == "ball_snap", poss_team == 0, !is.na(nflId)) %>%
                arrange(gameId, playId, y) %>%
                group_by(gameId, playId) %>%
                mutate(def_left_to_right = 1:n()) %>%
                arrange(gameId, playId, desc(y)) %>%
                mutate(def_right_to_left = 1:n()) %>%
                full_join(example_play %>% dplyr::filter(event == "ball_snap",
                                                         poss_team == 1,
                                                         !is.na(nflId)),
                          by = c("gameId", "playId", "frameId")) %>%
                rename_at(vars(contains('.x')), function(x) gsub('\\.x', '_def', x)) %>%
                rename_at(vars(contains('.y')), function(x) gsub('\\.y', '_off', x)) %>%
                left_join(example_play %>% 
                            dplyr::filter(displayName == "Football") %>%
                            select(frameId, gameId, playId, x, y) %>%
                            dplyr::rename(x_ball = x, y_ball = y)) %>%
                select(playId, gameId, nflId_def, x_def, y_def, x_off, y_off, x_ball, y_ball, los_x_def,
                       def_left_to_right, def_right_to_left, left_hash = left_hash_def, right_hash = right_hash_def,
                       dist_to_left_hash = dist_to_left_hash_def, dist_to_right_hash = dist_to_right_hash_def,
                       strength = strength_def, displayName = displayName_def, position = position_def) %>%  
                mutate(dist = sqrt((x_def - x_off) ^ 2 + (y_def - y_off) ^ 2),
                       x_def = x_def - los_x_def,
                       x_off = los_x_def - x_off,
                       ball_horz_dist_def = abs(y_def - y_ball),
                       ball_horz_dist_off = abs(y_off - y_ball)) %>%
                arrange(gameId, playId, nflId_def, dist) %>%
                group_by(gameId, playId, nflId_def) %>%
                mutate(assignment = 1:n()) %>%
                arrange(playId, gameId, y_def) %>%
                ungroup() %>%
                inner_join(df_plays %>% select(playId, gameId, ydstogo)) %>%
                dplyr::filter(assignment == 1, !is.na(nflId_def)) %>%
                group_by(gameId, playId) %>%
                mutate(min_y_def = min(y_def, na.rm = T),
                       max_y_def = max(y_def, na.rm = T)) %>%
                ungroup() %>%
                mutate(OutsideReceiver = ifelse(y_def == max_y_def | y_def == min_y_def, "OutsideCorner", "NotOutsideCorner"),
                       Safety = ifelse(x_def >= 11 , "FreeSafety", "NonFreeSafety"),
                       Safety = ifelse(Safety == "FreeSafety" & OutsideReceiver != "OutsideCorner", "FreeSafety", "NonFreeSafety"),
                       Safety = ifelse(Safety == "NonFreeSafety" & OutsideReceiver == "OutsideCorner", "OutsideCorner", Safety),
                       Nickelback = ifelse(((y_def - y_off <= 1.5) & (y_def - y_off >= -1.5)) & !between(y_def, right_hash, left_hash) & OutsideReceiver != "OutsideCorner" & Safety != "FreeSafety", "Nickelback", "NonNickelback")) %>%
                group_by(playId, gameId) %>%
                mutate(HighSafeties = ifelse(Safety == 'FreeSafety' & Nickelback != 'Nickelback', 1, 0)) %>%
                ungroup() %>%
                mutate(Safety = ifelse(Safety == "FreeSafety" & (((y_def >= right_hash & strength == "Right") | (y_def <= left_hash & strength == "Left")) |  between(y_def, right_hash, left_hash)) , "FreeSafety", Safety),
                       Safety = ifelse(Safety == "FreeSafety" & (((y_def <= right_hash & strength == "Right") | (y_def >= left_hash & strength == "Left")) ) , "StrongSafety", Safety),
                       Safety = ifelse((x_def > 9 & x_def < 11), "StrongSafety", Safety),
                       Safety = ifelse((x_def > 11) & between(y_def, right_hash, left_hash), "FreeSafety", Safety),
                       Safety = ifelse(OutsideReceiver == "OutsideCorner", "OutsideCorner", Safety),
                       Safety = ifelse(Nickelback == "Nickelback", "Nickelback", Safety),
                       Safety = ifelse(Safety == "NonFreeSafety" & between(y_def, right_hash, left_hash), "ILB", Safety),
                       Safety = ifelse(Safety == "NonFreeSafety" & !between(y_def, right_hash, left_hash), "OLB", Safety),
                       FinalPlayPosition = case_when(
                         Safety == 'FreeSafety' ~ 'FS',
                         Safety == 'StrongSafety' ~ 'SS',
                         Safety == 'OutsideCorner' ~ 'CB',
                         Safety == 'Nickelback' ~ 'NB',
                         # Safety %in% c('ILB', 'OLB') ~ 'LB',
                         TRUE ~ Safety)) %>%
                select(nflId = nflId_def, rules_pos = FinalPlayPosition)) %>%
    mutate(rules_pos = ifelse(is.na(rules_pos), position, rules_pos))
  
  
  # Example Play Field Control
  xmin <- 0
  xmax <- 160/3
  
  example_play_frames <- example_play %>%
    filter(!is.na(week)) %>%
    group_split(frameId)
  
  control_list <- {}
  
  for(i in 1:length(example_play_frames)){
    beaten <- example_play_frames[[i]] %>%
      arrange(nflId) %>%
      dplyr::rename(x_pos = y, y_pos = x) %>%
      select(gameId, playId, nflId, displayName, jerseyNumber, team, x_pos, y_pos, s, dir)
    
    find_angle <- function(x0, y0, x1, y1, dir){
      init_angle <- dir * pi / 180
      diff_x <- x1 - x0; diff_y <- y1 - y0
      atan2(cos(init_angle) * diff_y - diff_x * sin(init_angle),
            diff_x * cos(init_angle) + diff_y * sin(init_angle))
    }
    
    ymin <- 30
    ymax <- 90
    
    discretized_field <- expand.grid(x1 = seq(xmin, xmax, by = 0.5), y1 = seq(ymin, ymax, by = 0.5))
    
    player_expanded <- expand.grid.df(beaten %>% dplyr::filter(!(displayName %in% c("football", "Football"))), discretized_field)
    
    player_expanded <- player_expanded %>%
      mutate(distance = sqrt((x1 - x_pos) ^ 2 + (y1 - y_pos) ^ 2),
             angle = find_angle(x_pos, y_pos, x1, y1, dir)) %>%
      dplyr::rename(speed = s)
    
    times <- mod_fo %>%
      predict(player_expanded %>%
                select(distance, speed, angle) %>%
                as.matrix())
    
    player_expanded$time <- times[,1]
    
    control_list[[i]] <- player_expanded %>%
      group_by(x1, y1) %>%
      arrange(time) %>%
      filter(row_number() == 1) %>%
      mutate(frameId = i)
    
    print(i)
  }
  
  example_play_control <- do.call('rbind', control_list)
  
  # Plot 1.2 Data
  data_1_2 <- df_positions %>%
    inner_join(df_tracking_vars %>% select(nflId, position_clustered), by = 'nflId') %>%
    group_by(position_clustered) %>%
    summarise_if(is.numeric, mean) %>%
    select(-nflId) %>%
    gather(var, value, -position_clustered) %>%
    mutate(var = case_when(var == 'cb_pct' ~ 'CB',
                           var == 'fs_pct' ~ 'FS',
                           var == 'ilb_pct' ~ 'ILB',
                           var == 'nb_pct' ~ 'NB',
                           var == 'olb_pct' ~ 'OLB',
                           var == 'ss_pct' ~ 'SS',
                           TRUE ~ ''))
  
  # Plot 3.1 Data
  data_3_1 <- data.frame(var = row.names(varImp(mod_coverage, scale = F)[[1]]), value = varImp(mod_coverage, scale = F)[[1]][, 1]) %>%
    remove_rownames() %>%
    mutate(var = gsub('?([A-Z0-9]).*', '', var)) %>%
    group_by(var) %>%
    summarise(value = mean(value)) %>%
    arrange(desc(value)) %>%
    top_n(15) %>%
    mutate(var = str_to_title(gsub('_', ' ', var)),
           var = case_when(var == 'Avg S' ~ 'Avg Speed',
                           var == 'Avg V X' ~ 'Avg Velocity X',
                           var == 'Prob Cb' ~ 'Prob CB',
                           var == 'Prob Lb' ~ 'Prob LB',
                           TRUE ~ var))
  
  # Plot 3.2 Data
  data_3_2 <- df_preds %>%
    filter(actual != '') %>%
    group_by(actual, pred) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    mutate(actual = str_to_title(gsub('_', ' ', actual)),
           pred = str_to_title(gsub('_', ' ', pred)))
  
  # Plot 5.1 Data
  data_5_1 <- df_foae %>%
    group_by(nflId, displayName, team_name, position_clustered, position) %>%
    summarise(avg_efo = mean(foae, na.rm = T),
              avg_foae = mean(control_pct - foae, na.rm = T),
              snaps = n()) %>%
    filter(snaps > 275 & !(position == 'CB' & position_clustered == 'S')) %>%
    arrange(desc(avg_foae)) %>%
    group_by(position_clustered) %>%
    slice(1:5) %>%
    ungroup() %>%
    mutate(avg_efo = scales::label_percent(accuracy = 0.0001)(avg_efo),
           avg_foae = scales::label_percent(accuracy = 0.0001)(avg_foae)) %>%
    select('Player' = displayName, 'Clustered Position' = position_clustered,
           'Team' = team_name, 'Avg. EFO' = avg_efo, 'Avg. FOAE' = avg_foae, 'Snaps' = snaps)
  
  # Plot 5.2 Data
  data_5_2 <- df_foae %>%
    group_by(nflId, displayName, team_name, position_clustered, position) %>%
    summarise(avg_foae = mean(control_pct - foae, na.rm = T),
              snaps = n()) %>%
    filter(snaps > 275 & !(position == 'CB' & position_clustered == 'S')) %>%
    arrange(desc(avg_foae)) %>%
    ungroup() %>%
    slice(1:25)
  
  # Plot 6.1 Data
  data_6_1 <- df_foae %>%
    group_by(nflId, displayName, team_name, position_clustered, position, coverage) %>%
    summarise(avg_efo = mean(foae, na.rm = T),
              avg_foae = mean(control_pct - foae, na.rm = T),
              snaps = n()) %>%
    filter(snaps > 100 & !(position == 'CB' & position_clustered == 'S')) %>%
    arrange(desc(avg_foae)) %>%
    group_by(position_clustered, coverage) %>%
    slice(1:2) %>%
    ungroup() %>%
    mutate(coverage = str_to_title(gsub('_', ' ', coverage))) %>%
    select(displayName, position_clustered, coverage, team_name, avg_efo, avg_foae, snaps) %>%
    ungroup() %>%
    mutate(avg_efo = scales::label_percent(accuracy = 0.0001)(avg_efo),
           avg_foae = scales::label_percent(accuracy = 0.0001)(avg_foae)) %>%
    select('Player' = displayName, 'Clustered Position' = position_clustered, 'Coverage' = coverage,
           'Team' = team_name, 'Avg. EFO' = avg_efo, 'Avg. FOAE' = avg_foae, 'Snaps' = snaps)
  
  # Plot 6.2
  data_6_2 <- df_foae %>%
    group_by(nflId, displayName, team_name, position_clustered, position, coverage) %>%
    summarise(avg_foae = mean(control_pct - foae, na.rm = T),
              snaps = n()) %>%
    filter(snaps > 100 & !(position == 'CB' & position_clustered == 'S')) %>%
    arrange(desc(avg_foae)) %>%
    ungroup() %>%
    mutate(coverage = str_to_title(gsub('_', ' ', coverage)),
           player_coverage = paste(displayName, '-', coverage)) %>%
    slice(1:25)
  
  # Plot 7
  data_7 <- df_foae %>%
    left_join(df_plays %>%
                select(gameId, playId, epa)) %>%
    mutate(foae = control_pct - foae,
           defensive_formation_simple = ifelse(grepl('Man', defensive_formation), 'Man', 'Zone')) %>%
    group_by(team_name, gameId, playId, defensive_formation_simple) %>%
    summarise(foae = sum(foae, na.rm = T),
              epa = mean(epa, na.rm = T)) %>%
    group_by(team_name, defensive_formation_simple) %>%
    summarise(foae = mean(foae, na.rm = T),
              epa_allowed = mean(epa, na.rm = T)) %>%
    inner_join(nflfastR::teams_colors_logos %>%
                 filter(team_abbr != 'LV') %>%
                 select(team_abbr, team_logo_espn),
               by = c('team_name' = 'team_abbr'))
  
  # Save Data
  saveRDS(example_play, './plot_data/example_play.RDS')
  saveRDS(example_play_control, './plot_data/example_play_control.RDS')
  saveRDS(data_1_2, './plot_data/data_1_2.RDS')
  saveRDS(data_3_1, './plot_data/data_3_1.RDS')
  saveRDS(data_3_2, './plot_data/data_3_2.RDS')
  saveRDS(data_5_1, './plot_data/data_5_1.RDS')
  saveRDS(data_5_2, './plot_data/data_5_2.RDS')
  saveRDS(data_6_1, './plot_data/data_6_1.RDS')
  saveRDS(data_6_2, './plot_data/data_6_2.RDS')
  saveRDS(data_7, './plot_data/data_7.RDS')
  
  print('Files Saved')
}