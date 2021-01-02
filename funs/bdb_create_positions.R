# Create Position Dataframe

bdb_create_positions <- function(df_plays, df_tracking){
  df_positions <- df_tracking %>%
    dplyr::filter(event == "ball_snap", poss_team == 0, !is.na(nflId)) %>%
    arrange(gameId, playId, y) %>%
    group_by(gameId, playId) %>%
    mutate(def_left_to_right = 1:n()) %>%
    arrange(gameId, playId, desc(y)) %>%
    mutate(def_right_to_left = 1:n()) %>%
    full_join(df_tracking %>% dplyr::filter(event == "ball_snap",
                                     poss_team == 1,
                                     !is.na(nflId)),
              by = c("gameId", "playId", "frameId")) %>%
    rename_at(vars(contains('.x')), function(x) gsub('\\.x', '_def', x)) %>%
    rename_at(vars(contains('.y')), function(x) gsub('\\.y', '_off', x)) %>%
    left_join(df_tracking %>% 
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
           FinalPlayPosition = Safety) %>%
    group_by(nflId = nflId_def, displayName, position) %>%
    summarise(cb_pct = sum(FinalPlayPosition == 'OutsideCorner', na.rm = T) / n(),
              nb_pct = sum(FinalPlayPosition == 'Nickelback', na.rm = T) / n(),
              fs_pct = sum(FinalPlayPosition == 'FreeSafety', na.rm = T) / n(),
              ss_pct = sum(FinalPlayPosition == 'StrongSafety', na.rm = T) / n(),
              olb_pct = sum(FinalPlayPosition == 'OLB', na.rm = T) / n(),
              ilb_pct = sum(FinalPlayPosition == 'ILB', na.rm = T) / n())
  
  return(df_positions)
}
