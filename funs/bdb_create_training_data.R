# Create Training Set

bdb_create_training_data <- function(df_tracking_model){
  model_vars <- c('offensive_players_to_boundary', 'defensive_players_to_boundary',
                  'offensive_players_to_field', 'defensive_players_to_field',
                  'snap_dis_from_los', 'snap_dis_from_boundary', 'snap_dis_from_field',
                  'throw_dis_from_los', 'throw_dis_from_boundary', 'throw_dis_from_field',
                  'var_x', 'var_y', 'var_v_x', 'var_v_y', 'avg_v_x', 'avg_v_y', 'var_v_theta',
                  'avg_v_theta', 'var_dir', 'avg_dir', 'var_s', 'avg_s', 'sum_dis',
                  'var_closest_offensive_player_sep', 'var_closest_defensive_player_sep',
                  'avg_closest_offensive_player_sep', 'avg_closest_defensive_player_sep',
                  'var_closest_offensive_player_dir', 'var_closest_defensive_player_dir',
                  'avg_closest_offensive_player_dir', 'avg_closest_defensive_player_dir',
                  'closest_offensive_player_pre_play_sep', 'closest_defensive_player_pre_play_sep',
                  'closest_offensive_player_snap_sep', 'closest_defensive_player_snap_sep',
                  'closest_offensive_player_throw_sep', 'closest_defensive_player_throw_sep',
                  'closest_offensive_player_changes', 'closest_defensive_player_changes',
                  'pre_play_motion', 'position', 'position_clustered', 'prob_lb', 'prob_nb', 'prob_cb', 'prob_s',
                  'offensive_formation', 'offensive_personnel', 'defensive_personnel',
                  'defenders_in_box', 'number_pass_rushers', 'field_strength') # model vars
  
  df_train <- df_tracking_model %>%
    select(c('player_coverage', all_of(model_vars), 'gameId', 'playId', 'nflId')) %>%
    dplyr::filter(player_coverage != '') %>% # filter out plays where coverage isn't given
    mutate_if(is.character, as.factor) # character to factor
  
  return(df_train)
}
