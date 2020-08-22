# Downloading Data to use in my Shiny App and Report/Article
library(tidyverse)
library(baseballr)

# Need to find Mike Trout's MLBAM ID
playerid_lookup("Trout") %>% filter(first_name == "Mike")  # Player ID is 545361

# Now I can download his pitch-by-pitch data since 2017
trout_2017 <- scrape_statcast_savant_batter(start_date = "2017-04-01", end_date = "2017-10-30", batterid = 545361)
trout_2018 <- scrape_statcast_savant_batter(start_date = "2018-04-01", end_date = "2018-10-30", batterid = 545361)
trout_2019 <- scrape_statcast_savant_batter(start_date = "2019-04-01", end_date = "2019-10-30", batterid = 545361)
trout_2020 <- scrape_statcast_savant_batter(start_date = "2020-07-01", end_date = "2020-08-19", batterid = 545361)

trout_data <- trout_2017 %>%
  full_join(trout_2018) %>%
  full_join(trout_2019) %>%
  full_join(trout_2020)


# Peak at data to see the different variables we have to work with
glimpse(trout_data)

# RDS to transfer the data to my Shiny app
write_rds(trout_data, path = "pitching_to_Trout/trout_data")

# WORD
# Now I need to learn how to plot a pitch in a strikezone
#
# Important variables:
#
# sz_top    - top of the strike zone (coordinate)
# sz_bot - bottom of the strike zone (coordinate)
# plate_x   - x-coordinate of ball when it crosses home plate
# plate_z   - height of the ball when it crosses home plate
# 
# the height of the "strike zone" can vary pitch to pitch, but the width cannot
# Okay, let's do this



trout_data %>%
  filter(pitch_name %in% c("2-Seam Fastball", "4-Seam Fastball", "Changeup", "Curveball", "Cutter", "Sinker", "Slider")) %>%
  ggplot(aes(x = plate_x, y = plate_z)) +
  geom_segment(aes(x = -0.333, y = mean(sz_top), xend = -0.333, yend = mean(sz_bot)), color = "gray") +
  geom_segment(aes(x = 0.333, y = mean(sz_top), xend = 0.333, yend = mean(sz_bot)), color = "gray") +
  geom_segment(aes(x = -1, y = ((mean(sz_top) - mean(sz_bot))/3) + mean(sz_bot),
                   xend = 1, yend = ((mean(sz_top) - mean(sz_bot))/3) + mean(sz_bot)), color = "gray") +
  geom_segment(aes(x = -1, y = mean(sz_top) - ((mean(sz_top) - mean(sz_bot))/3),
                   xend = 1, yend = mean(sz_top) - ((mean(sz_top) - mean(sz_bot))/3)), color = "gray") +
  geom_segment(aes(x = -1, y = mean(sz_top), xend = 1, yend = mean(sz_top))) +
  geom_segment(aes(x = -1, y = mean(sz_bot), xend = 1, yend = mean(sz_bot))) +
  geom_segment(aes(x = -1, y = mean(sz_top), xend = -1, yend = mean(sz_bot))) +
  geom_segment(aes(x = 1, y = mean(sz_top), xend = 1, yend = mean(sz_bot))) +
  geom_segment(aes(x = 1, y = mean(sz_top), xend = 1, yend = mean(sz_bot))) +
  geom_point(aes(color = pitch_name), size = 1.5) +
  ylim(1, 4) +
  xlim(-2, 2) +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank())


# OKAY
# Now I want to plot a side-view that shows the launch angle and launch speed of hit balls
#
# Only variables I need are:
# Launch Speed - defines the length of the line
# Launch Direction - defines the direction of the line
# All lines will start at 0,0
# Then an h-line will represent the ground

test <- trout_data %>%
  filter(type == "X") %>%
  head(n = 20L)

test %>%
  ggplot() +
  geom_segment(aes(x = 0, y = 0, xend = launch_speed, yend = sin(launch_angle*pi/180), color = bb_type)) +
  geom_hline(yintercept = -0.25) +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank())
