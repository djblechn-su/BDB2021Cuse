# Animate/Plot Play - Based on work from https://www.kaggle.com/adamsonty/nfl-big-data-bowl-a-basic-field-control-model

bdb_animate_plot_field <- function(example_play, example_play_control, method = 'animate'){
  
  # function to plot a general football field
  plot_field <- function(field_color = "#009A17", # #ffffff
                         line_color = "#ffffff", # #212529
                         number_color = "#ffffff") { # #adb5bd
    
    field_height <- 160/3
    field_width <- 120
    
    field <- ggplot() +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 13, hjust = 0.5),
        plot.subtitle = element_text(hjust = 1),
        legend.position = "bottom",
        legend.title.align = 1,
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        panel.background = element_rect(fill = alpha(field_color, 0.5), color = "white"),
        panel.border = element_blank(),
        aspect.ratio = field_height/field_width
      ) +
      # major lines
      annotate(
        "segment",
        x = c(0, 0, 0,field_width, seq(10, 110, by=5)),
        xend = c(field_width,field_width, 0, field_width, seq(10, 110, by=5)),
        y = c(0, field_height, 0, 0, rep(0, 21)),
        yend = c(0, field_height, field_height, field_height, rep(field_height, 21)),
        colour = line_color
      ) +
      # hashmarks
      annotate(
        "segment",
        x = rep(seq(10, 110, by = 1), 4),
        xend = rep(seq(10, 110, by = 1), 4),
        y = c(rep(0, 101), rep(field_height - 1, 101), rep(160/6 + 18.5/6, 101), rep(160/6 - 18.5/6, 101)),
        yend = c(rep(1, 101), rep(field_height, 101), rep(160/6 + 18.5/6 + 1, 101), rep(160/6 - 18.5/6 - 1, 101)),
        colour = line_color
      ) +
      # yard numbers
      annotate(
        "text",
        x = seq(20, 100, by = 10),
        y = rep(12, 9),
        label = c(seq(10, 50, by = 10), rev(seq(10, 40, by = 10))),
        size = 10,
        colour = number_color,
      ) +
      # yard numbers upside down
      annotate(
        "text",
        x = seq(20, 100, by = 10),
        y = rep(field_height - 12, 9),
        label = c(seq(10, 50, by = 10), rev(seq(10, 40, by = 10))),
        angle = 180,
        size = 10,
        colour = number_color, 
      )
    
    return(field)
  }
  
  # extract los, first down line, and team colors (changed JAX black to white for readability)
  line_of_scrimmage = example_play$los_x
  to_go_line = line_of_scrimmage + unique(example_play$yardsToGo)
  df_colors = data.frame(home_1 = unique(example_play$home_team_color2)[1],
                         home_2 = '#ffffff',
                         away_1 = unique(example_play$away_team_color1)[1],
                         away_2 = unique(example_play$away_team_color2)[1])
  
  # allows for snapshot at the snap, total play animation, and field control animation
  if(method == 'animate'){
    play_frames <- plot_field() + 
      # line of scrimmage
      annotate(
        "segment",
        x = line_of_scrimmage, xend = line_of_scrimmage, y = 0, yend = 160/3,
        colour = "#0d41e1", size = 1.5
      ) +
      # 1st down marker
      annotate(
        "segment",
        x = to_go_line, xend = to_go_line, y = 0, yend = 160/3,
        colour = "#f9c80e", size = 1.5
      ) +
      # away team velocities
      geom_segment(
        data = example_play %>% dplyr::filter(team_name == unique(example_play$visitorTeamAbbr)),
        mapping = aes(x = x, y = y, xend = x + v_x, yend = y + v_y),
        colour = df_colors$away_1, size = 1, arrow = arrow(length = unit(0.01, "npc"))
      ) + 
      # home team velocities
      geom_segment(
        data = example_play %>% dplyr::filter(team_name == unique(example_play$homeTeamAbbr)),
        mapping = aes(x = x, y = y, xend = x + v_x, yend = y + v_y),
        colour = df_colors$home_2, size = 1, arrow = arrow(length = unit(0.01, "npc"))
      ) +
      # away team locs and jersey numbers
      geom_point(
        data = example_play %>% dplyr::filter(team_name == unique(example_play$visitorTeamAbbr)),
        mapping = aes(x = x, y = y),
        fill = "#f8f9fa", colour = df_colors$away_2,
        shape = 21, alpha = 1, size = 8, stroke = 1.5
      ) +
      geom_text(
        data = example_play %>% dplyr::filter(team_name == unique(example_play$visitorTeamAbbr)),
        mapping = aes(x = x, y = y, label = jerseyNumber),
        colour = df_colors$away_1, size = 4.5
      ) +
      # home team locs and jersey numbers
      geom_point(
        data = example_play %>% dplyr::filter(team_name == unique(example_play$homeTeamAbbr)),
        mapping = aes(x = x, y = y),
        fill = df_colors$home_1, colour = df_colors$home_2,
        shape = 21, alpha = 1, size = 8, stroke = 1.5
      ) +
      geom_text(
        data = example_play %>% dplyr::filter(team_name == unique(example_play$homeTeamAbbr)),
        mapping = aes(x = x, y = y, label = jerseyNumber),
        colour = df_colors$home_2, size = 4.5, 
      ) +
      # ball
      geom_point(
        data = example_play %>% dplyr::filter(team_name == "football"),
        mapping = aes(x = x, y = y),
        fill = "#935e38", colour = "#d9d9d9",
        shape = 21, alpha = 1, size = 4, stroke = 1
      ) +
      # title 
      labs(title = 'Figure 2 - Play Animation: PIT @ JAX, November 18, 2018\n',
           subtitle = '(1:16 - Q2) PIT 37 - B.Roethlisberger pass deep right intended for A.Brown \nINTERCEPTED by B.Church [Y.Ngakoue] at JAX 43.\n B.Church to PIT 46 for 11 yards (J.Conner).') +
      theme(plot.title = element_text(face = "bold"),
            plot.subtitle = element_text(hjust = 0.5)) +
      # cut field
      xlim(c(30, 80)) +
      # animation stuff
      transition_time(frameId) +
      ease_aes('linear') + 
      NULL
    
    play_length <- length(unique(example_play$frameId))
    play_anim <- animate(
      play_frames,
      fps = 10, 
      nframe = play_length,
      width = 750,
      height = 450,
      end_pause = 0
    )
  } else if(method == 'frame'){
    example_play <- example_play %>% filter(snap == 1)
    play_anim <- plot_field() + 
      # line of scrimmage
      annotate(
        "segment",
        x = line_of_scrimmage, xend = line_of_scrimmage, y = 0, yend = 160/3,
        colour = "#0d41e1", size = 1.5
      ) +
      # 1st down marker
      annotate(
        "segment",
        x = to_go_line, xend = to_go_line, y = 0, yend = 160/3,
        colour = "#f9c80e", size = 1.5
      ) +
      # away team locs and jersey numbers
      geom_point(
        data = example_play %>% dplyr::filter(team_name == unique(example_play$visitorTeamAbbr)),
        mapping = aes(x = x, y = y),
        fill = "#f8f9fa", colour = df_colors$away_2,
        shape = 21, alpha = 1, size = 8, stroke = 1.5
      ) +
      geom_text(
        data = example_play %>% dplyr::filter(team_name == unique(example_play$visitorTeamAbbr)),
        mapping = aes(x = x, y = y, label = rules_pos),
        colour = df_colors$away_1, size = 4
      ) +
      geom_text_repel(
        data = example_play %>% dplyr::filter(team_name == unique(example_play$visitorTeamAbbr)),
        mapping = aes(x = x, y = y, label = displayName),
        colour = df_colors$away_1, size = 4, min.segment.length = 999,
        point.padding = unit(0.5, 'lines'), nudge_x = -2
      ) +
      # home team locs and jersey numbers
      geom_point(
        data = example_play %>% dplyr::filter(team_name == unique(example_play$homeTeamAbbr)),
        mapping = aes(x = x, y = y),
        fill = df_colors$home_1, colour = df_colors$home_2,
        shape = 21, alpha = 1, size = 8, stroke = 1.5
      ) +
      # rules-based position and player name
      geom_text(
        data = example_play %>% dplyr::filter(team_name == unique(example_play$homeTeamAbbr)),
        mapping = aes(x = x, y = y, label = rules_pos),
        colour = df_colors$home_2, size = 4, 
      ) +
      geom_text_repel(
        data = example_play %>% dplyr::filter(team_name == unique(example_play$homeTeamAbbr)),
        mapping = aes(x = x, y = y, label = displayName),
        colour = df_colors$away_1, size = 4, min.segment.length = 999,
        point.padding = unit(0.5, 'lines'), nudge_x = 2
      ) +
      # ball
      geom_point(
        data = example_play %>% dplyr::filter(team_name == "football"),
        mapping = aes(x = x, y = y),
        fill = "#935e38", colour = "#d9d9d9",
        shape = 21, alpha = 1, size = 4, stroke = 1
      ) +
      # cut field
      xlim(c(30, 70)) +
      # title 
      labs(title = 'Pre-Snap Alignment: PIT @ JAX, November 18, 2018\n',
           subtitle = '(1:16 - Q2) PIT 37 - B.Roethlisberger pass deep right intended for A.Brown \nINTERCEPTED by B.Church [Y.Ngakoue] at JAX 43.\n B.Church to PIT 46 for 11 yards (J.Conner).') +
      theme(plot.title = element_text(face = "bold"),
            plot.subtitle = element_text(hjust = 0.5))
  } else if(method == 'field control'){
    control <- example_play_control
    example_play <- example_play %>% filter(frameId %in% unique(control$frameId))
    
    play_frames <- plot_field() + 
      # field control
      geom_raster(
        data = control, 
        mapping = aes(x = y1, y = x1, fill = team), alpha = 0.5, interpolate = T
      ) +
      scale_fill_manual(
        values = c(df_colors$away_2, df_colors$home_1), name = "Team Field Control",
        labels = c(unique(example_play$visitorTeamAbbr), unique(example_play$homeTeamAbbr))
      ) +
      # line of scrimmage
      annotate(
        "segment",
        x = line_of_scrimmage, xend = line_of_scrimmage, y = 0, yend = 160/3,
        colour = "#0d41e1", size = 1.5
      ) +
      # 1st down marker
      annotate(
        "segment",
        x = to_go_line, xend = to_go_line, y = 0, yend = 160/3,
        colour = "#f9c80e", size = 1.5
      ) +
      # away team velocities
      geom_segment(
        data = example_play %>% dplyr::filter(team_name == unique(example_play$visitorTeamAbbr)),
        mapping = aes(x = x, y = y, xend = x + v_x, yend = y + v_y),
        colour = df_colors$away_1, size = 1, arrow = arrow(length = unit(0.01, "npc"))
      ) + 
      # home team velocities
      geom_segment(
        data = example_play %>% dplyr::filter(team_name == unique(example_play$homeTeamAbbr)),
        mapping = aes(x = x, y = y, xend = x + v_x, yend = y + v_y),
        colour = df_colors$home_2, size = 1, arrow = arrow(length = unit(0.01, "npc"))
      ) +
      # away team locs and jersey numbers
      geom_point(
        data = example_play %>% dplyr::filter(team_name == unique(example_play$visitorTeamAbbr)),
        mapping = aes(x = x, y = y),
        fill = "#f8f9fa", colour = df_colors$away_2,
        shape = 21, alpha = 1, size = 8, stroke = 1.5
      ) +
      geom_text(
        data = example_play %>% dplyr::filter(team_name == unique(example_play$visitorTeamAbbr)),
        mapping = aes(x = x, y = y, label = jerseyNumber),
        colour = df_colors$away_1, size = 4.5
      ) +
      # home team locs and jersey numbers
      geom_point(
        data = example_play %>% dplyr::filter(team_name == unique(example_play$homeTeamAbbr)),
        mapping = aes(x = x, y = y),
        fill = df_colors$home_1, colour = df_colors$home_2,
        shape = 21, alpha = 1, size = 8, stroke = 1.5
      ) +
      geom_text(
        data = example_play %>% dplyr::filter(team_name == unique(example_play$homeTeamAbbr)),
        mapping = aes(x = x, y = y, label = jerseyNumber),
        colour = df_colors$home_2, size = 4.5, 
      ) +
      # ball
      geom_point(
        data = example_play %>% dplyr::filter(team_name == "football"),
        mapping = aes(x = x, y = y),
        fill = "#935e38", colour = "#d9d9d9",
        shape = 21, alpha = 1, size = 4, stroke = 1
      ) +
      # title 
      labs(title = 'Figure 4 - Field Ownership Animation: PIT @ JAX, November 18, 2018 (Animation Ends at Throw)\n',
           subtitle = '(1:16 - Q2) PIT 37 - B.Roethlisberger pass deep right intended for A.Brown \nINTERCEPTED by B.Church [Y.Ngakoue] at JAX 43.\n B.Church to PIT 46 for 11 yards (J.Conner).') +
      theme(plot.title = element_text(face = "bold"),
            plot.subtitle = element_text(hjust = 0.5)) +
      # cut field
      xlim(c(30, 80)) +
      # animation stuff
      transition_time(frameId) +
      ease_aes('linear') + 
      NULL
    
    play_length <- length(unique(example_play$frameId))
    play_anim <- animate(
      play_frames,
      fps = 10, 
      nframe = play_length,
      width = 750,
      height = 450,
      end_pause = 0
    )
  }
  
  return(play_anim)
}

