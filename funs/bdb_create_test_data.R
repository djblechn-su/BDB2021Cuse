# Create Test Set

bdb_create_test_data <- function(df_tracking_model){
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
                  'defenders_in_box', 'number_pass_rushers', 'field_strength')
  
  df_test <- df_tracking_model %>%
    select(c('player_coverage', all_of(model_vars), 'gameId', 'playId', 'nflId')) %>%
    dplyr::filter(is.na(player_coverage)) %>%
    mutate(position = case_when(position %in% c('WR', 'TE') ~ 'FS', position == 'DT' ~ 'NT', TRUE ~ position),
           offensive_formation = ifelse(offensive_formation == 'JUMBO', 'UNKNOWN', offensive_formation),
           offensive_personnel = case_when(offensive_personnel == '1 RB, 2 TE, 1 WR,1 DL' ~ '2 RB, 2 TE, 1 WR',
                                           offensive_personnel == '6 OL, 2 RB, 1 TE, 1 WR' ~ '6 OL, 1 RB, 2 TE, 1 WR',
                                           offensive_personnel == '' ~ '1 RB, 1 TE, 3 WR',
                                           offensive_personnel == '3 RB, 2 TE, 0 WR' ~ '3 RB, 1 TE, 1 WR',
                                           offensive_personnel == '1 RB, 0 TE, 3 WR,1 DL' ~ '2 RB, 0 TE, 3 WR',
                                           offensive_personnel == '0 RB, 0 TE, 5 WR' ~ '0 RB, 1 TE, 4 WR',
                                           offensive_personnel == '2 RB, 3 TE, 0 WR' ~ '2 RB, 2 TE, 1 WR',
                                           offensive_personnel == '6 OL, 1 RB, 0 TE, 3 WR' ~ '1 RB, 1 TE, 3 WR',
                                           offensive_personnel == '0 RB, 3 TE, 2 WR' ~ '0 RB, 2 TE, 3 WR',
                                           offensive_personnel == '3 RB, 0 TE, 2 WR' ~ '2 RB, 1 TE, 2 WR',
                                           offensive_personnel == '1 RB, 2 TE, 1 WR,1 DB' ~ '1 RB, 2 TE, 2 WR',
                                           offensive_personnel == '1 RB, 0 TE, 3 WR,1 DB' ~ '1 RB, 0 TE, 4 WR',
                                           offensive_personnel == '1 RB, 1 TE, 2 WR,1 DB' ~ '1 RB, 1 TE, 3 WR',
                                           offensive_personnel == '1 RB, 4 TE, 0 WR' ~ '1 RB, 3 TE, 1 WR',
                                           offensive_personnel == '6 OL, 2 RB, 2 TE, 0 WR' ~ '6 OL, 2 RB, 0 TE, 2 WR',
                                           offensive_personnel == '6 OL, 2 RB, 1 TE, 0 WR,1 DL' ~ '6 OL, 2 RB, 0 TE, 2 WR',
                                           TRUE ~ offensive_personnel),
           defensive_personnel = case_when(defensive_personnel == '2 DL, 2 LB, 7 DB' ~ '2 DL, 3 LB, 6 DB',
                                           defensive_personnel == '1 DL, 4 LB, 5 DB' ~ '1 DL, 5 LB, 5 DB',
                                           defensive_personnel == '3 DL, 5 LB, 3 DB' ~ '3 DL, 4 LB, 4 DB',
                                           defensive_personnel == '3 DL, 2 LB, 5 DB, 1 WR' ~ '3 DL, 2 LB, 6 DB',
                                           defensive_personnel == '3 DL, 2 LB, 5 DB' ~ '3 DL, 3 LB, 5 DB',
                                           defensive_personnel == '' ~ '3 DL, 3 LB, 5 DB',
                                           defensive_personnel == '5 DL, 4 LB, 2 DB' ~ '4 DL, 4 LB, 3 DB',
                                           defensive_personnel == '0 DL, 4 LB, 7 DB' ~ '0 DL, 5 LB, 6 DB',
                                           defensive_personnel == '6 DL, 1 LB, 4 DB' ~ '5 DL, 2 LB, 4 DB',
                                           defensive_personnel == '2 DL, 2 LB, 6 DB, 1 WR' ~ '2 DL, 3 LB, 6 DB',
                                           defensive_personnel == '2 DL, 3 LB, 5 DB' ~ '2 DL, 3 LB, 6 DB',
                                           defensive_personnel == '4 DL, 0 LB, 7 DB' ~ '3 DL, 1 LB, 7 DB',
                                           defensive_personnel == '3 DL, 1 LB, 6 DB, 1 WR' ~ '3 DL, 1 LB, 7 DB',
                                           defensive_personnel == '3 DL, 0 LB, 8 DB' ~ '3 DL, 1 LB, 7 DB',
                                           defensive_personnel == '2 DL, 4 LB, 4 DB' ~ '2 DL, 4 LB, 5 DB',
                                           defensive_personnel == '2 DL, 3 LB, 5 DB, 1 WR' ~ '2 DL, 3 LB, 6 DB',
                                           defensive_personnel == '3 DL, 1 LB, 6 DB, 1 TE' ~ '3 DL, 1 LB, 7 DB',
                                           defensive_personnel == '5 DL, 3 LB, 3 DB' ~ '5 DL, 2 LB, 4 DB',
                                           defensive_personnel == '4 DL, 2 LB, 4 DB' ~ '4 DL, 2 LB, 5 DB',
                                           defensive_personnel == '7 DL, 3 LB, 1 DB' ~ '5 DL, 2 LB, 4 DB',
                                           defensive_personnel == '1 DL, 2 LB, 7 DB' ~ '1 DL, 3 LB, 7 DB',
                                           TRUE ~ defensive_personnel)) %>%
    mutate_if(is.character, as.factor) %>%
    dplyr::filter(complete.cases(.[,-1]))
  
  return(df_test)
}
