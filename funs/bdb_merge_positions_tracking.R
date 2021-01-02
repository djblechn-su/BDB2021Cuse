# Merge Positions and Tracking Data

bdb_merge_positions_tracking <- function(df_tracking, df_positions, position_model_mclust){
  df_tracking <- df_tracking %>%
    left_join(cbind(df_positions$nflId, position_model_mclust$z, position_model_mclust$classification) %>%
                as.data.table() %>%
                dplyr::rename(nflId = V1, prob_cb = V2, prob_lb = V3, prob_s = V4, prob_nb = V5, position_clustered = V6) %>%
                mutate(position_clustered = case_when(
                  position_clustered == 1 ~ 'CB',
                  position_clustered == 2 ~ 'LB',
                  position_clustered == 3 ~ 'S',
                  position_clustered == 4 ~ 'NB',
                  TRUE ~ ''
                )), by = 'nflId')
  
  return(df_tracking)
}
