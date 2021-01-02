# Run Position Model

bdb_run_position_model <- function(df_positions){
  position_model_mclust <- Mclust(df_positions[, c('cb_pct', 'nb_pct', 'fs_pct', 'ss_pct', 'olb_pct', 'ilb_pct')], G = 1:11)
  df_positions$position_clustered <- position_model_mclust$classification
  summary(position_model_mclust)
  
  return(position_model_mclust)
}