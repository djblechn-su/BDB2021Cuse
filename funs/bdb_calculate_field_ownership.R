# Field Ownership - Calculate Field Ownership (Source: https://github.com/burrisk/Big-Data-Bowl)

bdb_calculate_field_ownership <- function(model, data){
  control_list <- {}
  
  # filter frame at throw
  play <- data %>%
    group_by(gameId, playId) %>%
    dplyr::filter(frameId == max(frameId[!(displayName %in% c('football', 'Football'))])) %>%
    select(gameId, playId, nflId, displayName, jerseyNumber, team, x, y, s, dir) %>%
    ungroup() %>%
    group_split(gameId, playId)
  
  # field boundaries
  xmin <- 0
  xmax <- 160/3
  
  for(i in 1:length(play)){
    # subset data
    beaten <- play[[i]] %>%
      arrange(nflId) %>%
      dplyr::rename(x_pos = y, y_pos = x) %>%
      select(gameId, playId, nflId, displayName, jerseyNumber, team, x_pos, y_pos, s, dir)
    
    # calculate angle
    find_angle <- function(x0, y0, x1, y1, dir){
      init_angle <- dir * pi / 180
      diff_x <- x1 - x0; diff_y <- y1 - y0
      atan2(cos(init_angle) * diff_y - diff_x * sin(init_angle),
            diff_x * cos(init_angle) + diff_y * sin(init_angle))
    }
    
    # add 5 yards beyond deepest defender and 2.5 yards behind QB (or deepest player behind the LOS)
    ymin <- max(round(min(play[[i]]$x, na.rm = TRUE) - 2.5, -1), 0)
    ymax <- min(round(max(play[[i]]$x, na.rm = TRUE) + 5, -1), 120)
    
    # expand field by half-yard segments in the x and y directions (0.25 square yards)
    discretized_field <- expand.grid(x1 = seq(xmin, xmax, by = 0.5), y1 = seq(ymin, ymax, by = 0.5))
    
    # filter out ball
    player_expanded <- expand.grid.df(beaten %>% dplyr::filter(!(displayName %in% c("football", "Football"))), discretized_field)
    
    # calculate distance, angle, rename speed
    player_expanded <- player_expanded %>%
      mutate(distance = sqrt((x1 - x_pos) ^ 2 + (y1 - y_pos) ^ 2),
             angle = find_angle(x_pos, y_pos, x1, y1, dir)) %>%
      dplyr::rename(speed = s)
    
    # determine field ownership by quarter square yard
    times <- model %>%
      predict(player_expanded %>%
                select(distance, speed, angle) %>%
                as.matrix())
    
    player_expanded$time <- times[,1]
    
    # aggregate field ownership by player in terms of percentage of field and total field (yards)
    control_list[[i]] <- player_expanded %>%
      group_by(x1, y1) %>%
      arrange(time) %>%
      dplyr::filter(row_number() == 1) %>%
      group_by(gameId, playId, nflId, displayName) %>%
      summarise(control_pct = n() / nrow(discretized_field),
                control_dist = n() * 0.25) %>%
      full_join(play[[i]] %>%
                  select(gameId, playId, nflId, displayName) %>% 
                  dplyr::filter(!(displayName %in% c('football', 'Football')))) %>%
      mutate(control_pct = ifelse(is.na(control_pct), 0, control_pct),
             control_dist = ifelse(is.na(control_dist), 0, control_dist))
    print(i)
  }
  
  control <- do.call('rbind', control_list)
  
  return(control)
}
