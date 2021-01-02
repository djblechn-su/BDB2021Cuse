# Read Plays Data - Read in Kaggle Data for BDB Competition (Only works if data is downloaded separately)

bdb_read_plays <- function(){
  df_plays <- read_csv("../input/nfl-big-data-bowl-2021/plays.csv", col_types = cols()) %>% # read plays data
    left_join(read_csv("../input/nfl-big-data-bowl-2021-bonus/coverages_week1.csv", col_types = cols()), by = c('gameId', 'playId')) %>% # read bonus Telemtry coverage labels for week 1
    left_join(read_csv("../input/nfl-big-data-bowl-2021-bonus/targetedReceiver.csv", col_types = cols()), by = c('gameId', 'playId')) %>% # read bonus targeted receiver data
    left_join(readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2018.rds')) %>%
                dplyr::filter(season_type == 'REG') %>%
                mutate(old_game_id = as.numeric(old_game_id)) %>%
                select(-c(down, epa)),
              by = c('gameId' = 'old_game_id', 'playId' = 'play_id')) # read in nflfastR data for 2018 regular season
  
  return(df_plays)
}
