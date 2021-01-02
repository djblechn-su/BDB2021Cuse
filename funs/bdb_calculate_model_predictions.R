# Calculate Model Predictions

bdb_calculate_model_predictions <- function(coverage_model_nnet, df_train, df_test){
  df_preds <- as.data.frame(predict(coverage_model_nnet, rbind(df_train, df_test), type = "prob")) %>%
    dplyr::rename(prob_deep_zone = deep_zone, prob_man = man, prob_underneath_zone = underneath_zone) %>%
    mutate(gameId = rbind(df_train, df_test)[['gameId']],
           playId = rbind(df_train, df_test)[['playId']],
           nflId = rbind(df_train, df_test)[['nflId']],
           pred = predict(coverage_model_nnet, rbind(df_train, df_test), type = "raw"),
           actual = rbind(df_train, df_test)[['player_coverage']],
           correct = ifelse(as.character(pred) == as.character(actual), 1, 0)) %>%
    relocate(c('gameId', 'playId', 'nflId'))
  
  return(df_preds)
}
