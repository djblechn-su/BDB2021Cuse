# Merge Tracking and Specs

bdb_merge_tracking_specs <- function(df_tracking_vars, df_specs){
  df_tracking_model <- df_tracking_vars %>%
    full_join(df_specs, by = c('playId', 'gameId', 'nflId')) %>%
    dplyr::filter(is.na(player_coverage) | player_coverage %in% c('underneath_zone', 'deep_zone', 'man') & defensive_formation != 'Prevent Zone')
  
  return(df_tracking_model)
}
