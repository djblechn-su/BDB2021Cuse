# Read Plays Data

bdb_read_plays <- function(){
  df_plays <- read_csv("../input/nfl-big-data-bowl-2021/plays.csv", col_types = cols()) %>%
    left_join(read_csv("../input/nfl-big-data-bowl-2021-bonus/coverages_week1.csv", col_types = cols()), by = c('gameId', 'playId')) %>%
    left_join(read_csv("../input/nfl-big-data-bowl-2021-bonus/targetedReceiver.csv", col_types = cols()), by = c('gameId', 'playId')) %>%
    left_join(readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2018.rds')) %>%
                dplyr::filter(season_type == 'REG') %>%
                mutate(old_game_id = as.numeric(old_game_id)) %>%
                select(-c(down, epa)),
              by = c('gameId' = 'old_game_id', 'playId' = 'play_id'))
  
  return(df_plays)
}
