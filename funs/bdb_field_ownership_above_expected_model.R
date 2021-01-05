# Field Ownership Above Expected

bdb_field_ownership_above_expected_model <- function(df_control, df_tracking, df_plays, df_preds, df_coverages_probability){
  df_fo_merged <- df_control %>%
    left_join(df_tracking %>%
                mutate(opp_team_name = ifelse(team_name == homeTeamAbbr, visitorTeamAbbr, homeTeamAbbr)) %>%
                select(gameId, playId, nflId, displayName, yardline_snap = los_x, position, position_clustered,
                       prob_nb, prob_cb, prob_s, prob_lb, poss_team, team_name, opp_team_name) %>% 
                distinct(gameId, playId, nflId, .keep_all = T)) %>% # join tracking vars
    left_join(df_plays %>%
                select(gameId, playId, down, yardsToGo, offensive_formation = offenseFormation, def_wp)) %>% # join formation and defensive wp
    left_join(df_preds %>%
                mutate(coverage = ifelse(is.na(actual), as.character(pred), as.character(actual))) %>%
                select(gameId, playId, nflId, prob_deep_zone, prob_man, prob_underneath_zone, coverage)) %>% # join coverages
    left_join(df_coverages_probability) %>%
    dplyr::filter(poss_team == 0) %>%
    na.omit()
  
  print('Running Model')
  
  fo_above_expected_model <- ranger(control_pct ~ opp_team_name + position_clustered +
                                      prob_cb + prob_nb + prob_s + prob_lb +
                                      down + yardsToGo + yardline_snap + def_wp +
                                      offensive_formation + defensive_formation +
                                      cover_0_man_prob + cover_1_man_prob +
                                      cover_2_man_prob + cover_2_zone_prob +
                                      cover_3_zone_prob + cover_4_zone_prob +
                                      cover_6_zone_prob + coverage +
                                      prob_deep_zone + prob_underneath_zone,
                                    data = df_fo_merged,
                                    importance = 'impurity') # run random forest model
    
  return(fo_above_expected_model)
}
