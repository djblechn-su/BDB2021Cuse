# Evaluate Model Predictions

bdb_evaluate_model_predictions <- function(coverage_model_nnet, df_train){
  cm <- table(as.data.frame(predict(coverage_model_nnet, df_train, type = "prob")) %>%
                dplyr::rename(prob_deep_zone = deep_zone, prob_man = man, prob_underneath_zone = underneath_zone) %>%
                mutate(pred = predict(coverage_model_nnet, df_train, type = "raw"),
                       actual = df_train$player_coverage,
                       correct = ifelse(pred == actual, 1, 0)) %>%
                select(actual, pred))
  confusionMatrix(cm)
  
  return(cm)
}
