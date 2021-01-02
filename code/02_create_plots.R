# bdb 2021 cuse

# libraries
library(tidyverse)
library(reshape)
library(caret)
library(ggplot2)
library(wesanderson)
library(ggpubr)
library(gganimate)
library(ggimage)
library(ggrepel)
library(nflfastR)

# read data and field animation function
source('./funs/bdb_animate_plot_field.R')
for(i in 1:length(list.files(path = './plot_data/'))){
  assign(gsub('.RDS', '', list.files(path = './plot_data/')[i]), readRDS(paste0('./plot_data/', list.files(path = './plot_data/')[i])))
}


# example play
# https://www.youtube.com/watch?v=FueeYXD8yFY&feature=onebox (3:56)

# plot 1.1 - position assignments
plot_1_1 <- bdb_animate_plot_field(example_play, example_play_control, 'frame')

# plot 1.2 - cluster breakdown
plot_1_2 <- data_1_2 %>%
  ggplot(aes(x = position_clustered, y = value, fill = var)) +
  coord_flip() +
  geom_bar(stat = 'identity') +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = wes_palette('IsleofDogs1'),
                    name = 'Rule-Based\nPosition') +
  labs(x = 'Clustered Position',
       y = 'Percentage of Snaps') + 
  theme_bw()

# plot 1 - combined
figure_1 <- ggarrange(plot_1_1, plot_1_2, labels = c('1A', '1B'), ncol = 2, nrow = 1) %>%
  annotate_figure(top = text_grob(expression(atop(bold('Clustered Positions'),
                                                  '\nPositional Lineup for an Example Play (1A) and Percentage of Snaps Lined Up at Rule-Based Positions (1B)')),
                                  size = 12, lineheight = 1.25),
                  fig.lab = 'Figure 1', fig.lab.face = 'bold')

figure_1

# plot 2 - defensive coverage animation
figure_2 <- bdb_animate_plot_field(example_play, example_play_control, 'animate')

figure_2

# plot 3.1 - neural network importance
plot_3_1 <- data_3_1 %>%
  ggplot(aes(x = reorder(var, value), y = value)) +
  coord_flip() +
  geom_linerange(aes(ymin = 0, ymax = value), size = 1.5, color = 'grey') +
  geom_point(size = 3, color = 'black') +
  scale_x_discrete(expand = c(0.05, 0.05)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) + 
  labs(x = 'Variable',
       y = 'Variable Importance') +
  theme_bw()

# plot 3.2 - confusion matrix
plot_3_2 <- data_3_2 %>%
  ggplot(aes(x = actual, y = pred)) +
  geom_tile(aes(fill = n), colour = 'white') +
  geom_text(aes(label = n), vjust = 1, color = 'black') +
  scale_fill_gradientn(colors = wes_palette('Zissou1', 3, 'continuous')) +
  labs(x = 'Predicted Coverage',
       y = 'Actual Coverage') +
  theme_bw()

# plot 3 - combined
figure_3 <- ggarrange(plot_3_1, plot_3_2, labels = c('3A', '3B'), ncol = 2, nrow = 1) %>%
  annotate_figure(top = text_grob(expression(atop(bold('Individual Coverage Model'),
                                                  '\nTop 15 Most Important Variables (3A) and Confusion Matrix (3B)')),
                                  size = 12, lineheight = 1.25),
                  fig.lab = 'Figure 3', fig.lab.face = 'bold')

figure_3

# plot 4 - field ownership plot/animation
figure_4 <- bdb_animate_plot_field(example_play, example_play_control, 'field control')

figure_4

# plot 5.1 - avg. field ownership by player - table - top n by clustered position
plot_5_1 <- data_5_1 %>%
  ggtexttable(rows = NULL, theme = ttheme("blank")) %>%
  tab_add_hline(at.row = 1:2, row.side = "top", linewidth = 2) %>%
  tab_add_hline(at.row = 7, row.side = "top", linewidth = 1, linetype = 2) %>%
  tab_add_hline(at.row = 12, row.side = "top", linewidth = 1, linetype = 2) %>%
  tab_add_hline(at.row = 17, row.side = "top", linewidth = 1, linetype = 2) %>%
  tab_add_footnote(text = '*Blitzes not included in snap counts', size = 10, face = "italic")

# plot 5.2 - avg. field ownership by player - graph - top 25 players
plot_5_2 <- data_5_2 %>%
  ggplot(aes(x = reorder(displayName, avg_foae), y = avg_foae, fill = position_clustered)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  scale_y_continuous(labels = scales::percent,
                     expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(name = 'Clustered Position',
                    values = wes_palette('Moonrise2')) +
  labs(x = 'Player',
       y = 'Avg. FOAE') +
  theme_bw()

figure_5 <- ggarrange(plot_5_1, plot_5_2, labels = c('5A', '5B'), hjust = -0.5, vjust = 1, ncol = 2, nrow = 1) %>%
  annotate_figure(top = text_grob(expression(atop(bold('Average Field Ownership Above Expected'),
                                                  '\nTop 5 Players by Clustered Position (5A) and Top 25 Players Overall (5B) - Minimum 275 Snaps')),
                                  size = 12, lineheight = 1.25),
                  fig.lab = 'Figure 5', fig.lab.face = 'bold')

figure_5

# plot 6.1 - avg. player field ownership - by individual defensive coverage (man vs zone)
plot_6_1 <- data_6_1 %>%
  ggtexttable(rows = NULL, theme = ttheme("blank")) %>%
  tab_add_hline(at.row = 1:2, row.side = "top", linewidth = 2) %>%
  tab_add_hline(at.row = 4, row.side = "top", linewidth = 1, linetype = 2) %>%
  tab_add_hline(at.row = 6, row.side = "top", linewidth = 1, linetype = 2) %>%
  tab_add_hline(at.row = 8, row.side = "top", linewidth = 1, linetype = 1) %>%
  tab_add_hline(at.row = 10, row.side = "top", linewidth = 1, linetype = 2) %>%
  tab_add_hline(at.row = 12, row.side = "top", linewidth = 1, linetype = 1) %>%
  tab_add_hline(at.row = 14, row.side = "top", linewidth = 1, linetype = 2) %>%
  tab_add_hline(at.row = 16, row.side = "top", linewidth = 1, linetype = 1) %>%
  tab_add_hline(at.row = 18, row.side = "top", linewidth = 1, linetype = 2) %>%
  tab_add_hline(at.row = 20, row.side = "top", linewidth = 1, linetype = 2) %>%
  tab_add_footnote(text = '*Blitzes not included in snap counts', size = 10, face = "italic")

# plot 6.2 - avg. field ownership by player/coverage - graph - top 25 players
plot_6_2 <- data_6_2 %>%
  ggplot(aes(x = reorder(player_coverage, avg_foae), y = avg_foae, fill = position_clustered)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  scale_y_continuous(labels = scales::percent,
                     expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(name = 'Clustered Position',
                    values = wes_palette('Moonrise2')) +
  labs(x = 'Player - Coverage',
       y = 'Avg. FOAE') +
  theme_bw()

figure_6 <- ggarrange(plot_6_1, plot_6_2, labels = c('6A', '6B'), hjust = -0.5, vjust = 1, ncol = 2, nrow = 1, widths = c(5, 4)) %>%
  annotate_figure(top = text_grob(expression(atop(bold('Average Field Ownership Above Expected'),
                                                  '\nTop 2 Players by Clustered Position and Coverage (6A) and Top 25 Players Overall (6B) - Minimum 100 Snaps')),
                                  size = 12, lineheight = 1.25),
                  fig.lab = 'Figure 6', fig.lab.face = 'bold')

figure_6

# plot 7 - avg. team field ownership vs. defensive epa per play (team) - scatter - by team defensive coverage (man vs zone)
plot_7 <- data_7 %>%
  ggplot(aes(x = foae, y = epa_allowed, label = team_name)) +
  facet_wrap(~defensive_formation_simple) + #, scales = 'free_y') +
  geom_abline(intercept = -0.4, slope = -15, color = 'grey') +
  geom_abline(intercept = -0.3, slope = -15, color = 'grey') +
  geom_abline(intercept = -0.2, slope = -15, color = 'grey') +
  geom_abline(intercept = -0.1, slope = -15, color = 'grey') +
  geom_abline(intercept = 0, slope = -15, color = 'grey') +
  geom_abline(intercept = 0.1, slope = -15, color = 'grey') +
  geom_abline(intercept = 0.2, slope = -15, color = 'grey') +
  geom_abline(intercept = 0.3, slope = -15, color = 'grey') +
  geom_hline(yintercept = 0.0372, color = 'red', linetype = 'dashed') +
  geom_vline(xintercept = -0.0046, color = 'red', linetype = 'dashed') +
  geom_image(aes(image = team_logo_espn), size = 0.09) +
  scale_x_continuous(labels = scales::percent, breaks = c(-0.015, -0.01, -0.005, 0, 0.005)) +
  scale_y_continuous(trans = 'reverse') +
  labs(x = 'Field Ownership Above Expected/Play',
       y = 'Defensive EPA/Play') +
  theme_bw()

figure_7 <- ggarrange(plot_7, ncol = 1, nrow = 1) %>%
  annotate_figure(top = text_grob(expression(atop(bold('Team Defensive EPA vs. Field Ownership by Defensive Formation'),
                                                  '\nAggregate Field Ownership per Play by Team and Defensive Formation')),
                                  size = 12, lineheight = 1.25),
                  fig.lab = 'Figure 7', fig.lab.face = 'bold')

figure_7

# Save PDFs
ggsave('./plot_images/figure_1.pdf', figure_1, width = 12, height = 5, units = 'in')
ggsave('./plot_images/figure_3.pdf', figure_3, width = 12, height = 5, units = 'in')
ggsave('./plot_images/figure_5.pdf', figure_5, width = 13, height = 7, units = 'in')
ggsave('./plot_images/figure_6.pdf', figure_6, width = 13, height = 7, units = 'in')
ggsave('./plot_images/figure_7.pdf', figure_7, width = 8, height = 5, units = 'in')

# Save Images
ggsave('./plot_images/figure_1.png', figure_1, width = 12, height = 5, units = 'in')
ggsave('./plot_images/figure_3.png', figure_3, width = 12, height = 5, units = 'in')
ggsave('./plot_images/figure_5.png', figure_5, width = 13, height = 7, units = 'in')
ggsave('./plot_images/figure_6.png', figure_6, width = 13, height = 7, units = 'in')
ggsave('./plot_images/figure_7.png', figure_7, width = 8, height = 5, units = 'in')

# Save GIFS
anim_save('./plot_images/figure_2.gif', figure_2)
anim_save('./plot_images/figure_4.gif', figure_4)
