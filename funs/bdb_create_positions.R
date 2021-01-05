# Create Position Dataframe

bdb_create_positions <- function(df_plays, df_tracking){
  df_positions <- df_tracking %>%
    dplyr::filter(event == "ball_snap", poss_team == 0, !is.na(nflId)) %>% # filter out ball snap
    arrange(gameId, playId, y) %>%
    group_by(gameId, playId) %>%
    mutate(def_left_to_right = 1:n()) %>% # order defensive players left to right
    arrange(gameId, playId, desc(y)) %>%
    mutate(def_right_to_left = 1:n()) %>% # order defensive players right to left
    full_join(df_tracking %>% dplyr::filter(event == "ball_snap",
                                     poss_team == 1,
                                     !is.na(nflId)),
              by = c("gameId", "playId", "frameId")) %>% # join offensive players
    rename_at(vars(contains('.x')), function(x) gsub('\\.x', '_def', x)) %>% # rename defense
    rename_at(vars(contains('.y')), function(x) gsub('\\.y', '_off', x)) %>% # rename offense
    left_join(df_tracking %>% 
                dplyr::filter(displayName == "Football") %>%
                select(frameId, gameId, playId, x, y) %>%
                dplyr::rename(x_ball = x, y_ball = y)) %>% # join all players
    select(playId, gameId, nflId_def, x_def, y_def, x_off, y_off, x_ball, y_ball, los_x_def,
           def_left_to_right, def_right_to_left, left_hash = left_hash_def, right_hash = right_hash_def,
           dist_to_left_hash = dist_to_left_hash_def, dist_to_right_hash = dist_to_right_hash_def,
           strength = strength_def, displayName = displayName_def, position = position_def) %>%  # select relevant vars
    mutate(dist = sqrt((x_def - x_off) ^ 2 + (y_def - y_off) ^ 2),
           x_def = x_def - los_x_def,
           x_off = los_x_def - x_off,
           ball_horz_dist_def = abs(y_def - y_ball),
           ball_horz_dist_off = abs(y_off - y_ball)) %>% # calculate distance between players, los, and ball
    arrange(gameId, playId, nflId_def, dist) %>%
    group_by(gameId, playId, nflId_def) %>%
    mutate(assignment = 1:n()) %>% # number distance from players
    arrange(playId, gameId, y_def) %>%
    ungroup() %>%
    inner_join(df_plays %>% select(playId, gameId, ydstogo)) %>% # join yards to go
    dplyr::filter(assignment == 1, !is.na(nflId_def)) %>%
    group_by(gameId, playId) %>%
    mutate(min_y_def = min(y_def, na.rm = T),
           max_y_def = max(y_def, na.rm = T)) %>% # find min y and max y
    ungroup() %>%
    mutate(OutsideReceiver = ifelse(y_def == max_y_def | y_def == min_y_def, "OutsideCorner", "NotOutsideCorner"), # if widest defender on left and right side, then outside corner
           Safety = ifelse(x_def >= 11 , "FreeSafety", "NonFreeSafety"), # if 11 yards or deeper from los, then free safety
           Safety = ifelse(Safety == "FreeSafety" & OutsideReceiver != "OutsideCorner", "FreeSafety", "NonFreeSafety"),
           Safety = ifelse(Safety == "NonFreeSafety" & OutsideReceiver == "OutsideCorner", "OutsideCorner", Safety),
           Nickelback = ifelse(((y_def - y_off <= 1.5) & (y_def - y_off >= -1.5)) & !between(y_def, right_hash, left_hash) & OutsideReceiver != "OutsideCorner" & Safety != "FreeSafety", "Nickelback", "NonNickelback")) %>% # if y-distance between defender and offensive player is less than or equal to 1.5 yards and not between hash marks, then nickelback
    group_by(playId, gameId) %>%
    mutate(HighSafeties = ifelse(Safety == 'FreeSafety' & Nickelback != 'Nickelback', 1, 0)) %>% # if no nickelbacks and a safety, high safeties
    ungroup() %>%
    mutate(Safety = ifelse(Safety == "FreeSafety" & (((y_def >= right_hash & strength == "Right") | (y_def <= left_hash & strength == "Left")) |  between(y_def, right_hash, left_hash)) , "FreeSafety", Safety),
           Safety = ifelse(Safety == "FreeSafety" & (((y_def <= right_hash & strength == "Right") | (y_def >= left_hash & strength == "Left")) ) , "StrongSafety", Safety),
           Safety = ifelse((x_def > 9 & x_def < 11), "StrongSafety", Safety), # ss - if between 9 and 11 yards
           Safety = ifelse((x_def > 11) & between(y_def, right_hash, left_hash), "FreeSafety", Safety), # fs - if further than 11 yards away
           Safety = ifelse(OutsideReceiver == "OutsideCorner", "OutsideCorner", Safety), # cb - if outside corner
           Safety = ifelse(Nickelback == "Nickelback", "Nickelback", Safety), # nb - if nickelback
           Safety = ifelse(Safety == "NonFreeSafety" & between(y_def, right_hash, left_hash), "ILB", Safety), # ilb - if not safety and between hashes
           Safety = ifelse(Safety == "NonFreeSafety" & !between(y_def, right_hash, left_hash), "OLB", Safety), # olb - if not safety and outside hashes
           FinalPlayPosition = Safety) %>%
    group_by(nflId = nflId_def, displayName, position) %>%
    summarise(cb_pct = sum(FinalPlayPosition == 'OutsideCorner', na.rm = T) / n(),
              nb_pct = sum(FinalPlayPosition == 'Nickelback', na.rm = T) / n(),
              fs_pct = sum(FinalPlayPosition == 'FreeSafety', na.rm = T) / n(),
              ss_pct = sum(FinalPlayPosition == 'StrongSafety', na.rm = T) / n(),
              olb_pct = sum(FinalPlayPosition == 'OLB', na.rm = T) / n(),
              ilb_pct = sum(FinalPlayPosition == 'ILB', na.rm = T) / n()) # calculate percentage of time lined up at each position
  
  return(df_positions)
}
