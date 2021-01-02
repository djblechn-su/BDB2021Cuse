# Field Ownership - Plot Ownership

bdb_plot_ownership <- function(play){
  beaten <- play %>%
    arrange(nflId) %>%
    dplyr::rename(x_pos = y, y_pos = x) %>%
    select(nflId, displayName, jerseyNumber, team, x_pos, y_pos, s, dir)
  
  find_angle <- function(x0, y0, x1, y1, dir){
    init_angle <- dir * pi / 180
    diff_x <- x1 - x0; diff_y <- y1 - y0
    atan2(cos(init_angle) * diff_y - diff_x * sin(init_angle),
          diff_x * cos(init_angle) + diff_y * sin(init_angle))
  }
  
  xmin <- 0
  xmax <- 160/3
  hash.right <- 38.35
  hash.left <- 12
  hash.width <- 3.3
  
  ## Specific boundaries for a given play
  ymin <- max(round(min(play$x, na.rm = TRUE) - 2.5, -1), 0)
  ymax <- min(round(max(play$x, na.rm = TRUE) + 5, -1), 120)
  
  discretized_field <- expand.grid(x1 = seq(xmin, xmax, by = 0.5), y1 = seq(ymin, ymax, by = 0.5))
  player_expanded <- expand.grid.df(beaten %>%
                                      filter(displayName != "football"), discretized_field)
  player_expanded <- player_expanded %>%
    mutate(distance = sqrt((x1 - x_pos)^2 + (y1 - y_pos) ^ 2),
           angle = find_angle(x_pos, y_pos, x1, y1, dir)) %>%
    dplyr::rename(speed = s)
  
  times <- model %>%
    predict(player_expanded[, c("distance", "speed", "angle")] %>% as.matrix())
  
  player_expanded$time <- times[,1]
  
  control <- player_expanded %>%
    group_by(x1, y1) %>%
    arrange(time) %>%
    filter(row_number() == 1)
  
  plot_beaten <- ggplot() +
    geom_raster(data = control, aes(x = xmax - x1, y = y1, fill = team), alpha = 0.5) +
    geom_point(data = beaten, aes(x = (xmax-x_pos), y = y_pos,
                                  colour = team, group = nflId, pch = team, size = team)) +
    geom_text(data = beaten, aes(x = (xmax-x_pos), y = y_pos, label = jerseyNumber), colour = "white",
              vjust = 0.36, size = 7) +
    scale_size_manual(values = c(12, 8, 12), guide = FALSE) +
    scale_shape_manual(values = c(19, 16, 19), guide = FALSE) +
    scale_colour_manual(values = c("black", "#654321", "darkorange"), guide = FALSE) +
    scale_fill_manual(values = c("grey", "orange"), guide = FALSE) +
    annotate("segment", x = xmin,
             y = seq(max(10, ymin), min(ymax, 110), by = 5),
             xend =  xmax,
             yend = seq(max(10, ymin), min(ymax, 110), by = 5)) +
    annotate("text", x = rep(hash.left, 11), y = seq(10, 110, by = 10),
             label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"),
             angle = 270, size = 8) +
    annotate("text", x = rep((xmax - hash.left), 11), y = seq(10, 110, by = 10),
             label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "),
             angle = 90, size = 8) +
    annotate("segment", x = c(xmin, xmin, xmax, xmax),
             y = c(ymin, ymax, ymax, ymin),
             xend = c(xmin, xmax, xmax, xmin),
             yend = c(ymax, ymax, ymin, ymin), colour = "black") +
    ylim(ymin, ymax) +
    coord_fixed() +
    theme_nothing()
  
  return(plot_beaten)
}