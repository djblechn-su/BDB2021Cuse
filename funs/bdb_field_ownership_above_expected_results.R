# Field Ownership Above Expected Results

bdb_fo_above_expected_results <- function(mod_fo_above_expected, df_control, df_tracking, df_plays, df_preds, df_coverages_probability){
  df_foae <- df_control %>%
    left_join(df_tracking %>%
                mutate(opp_team_name = ifelse(team_name == homeTeamAbbr, visitorTeamAbbr, homeTeamAbbr)) %>%
                select(gameId, playId, nflId, displayName, yardline_snap = los_x, position, position_clustered,
                       prob_nb, prob_cb, prob_s, prob_lb, poss_team, team_name, opp_team_name) %>%
                distinct(gameId, playId, nflId, .keep_all = T)) %>%
    left_join(df_plays %>%
                select(gameId, playId, down, yardsToGo, offensive_formation = offenseFormation, def_wp)) %>%
    left_join(df_preds %>%
                mutate(coverage = ifelse(is.na(actual), as.character(pred), as.character(actual))) %>%
                select(gameId, playId, nflId, prob_deep_zone, prob_man, prob_underneath_zone, coverage)) %>%
    left_join(df_coverages_probability) %>%
    dplyr::filter(poss_team == 0) %>%
    na.omit() %>%
    ungroup() %>% # get all variables for model
    mutate(foae = predict(mod_fo_above_expected, data = .)[['predictions']]) # get model predictions
  
  return(df_foae)  
}
