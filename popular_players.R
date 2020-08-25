# Preparing data for popular players table
library(baseballr)
library(tidyverse)

# Very inefficient but I am in a hurry... will functionalize later
devers <- playerid_lookup("Devers") %>%
  mutate(MLBAM_ID = as.integer(mlbam_id),
         First_Season = as.integer(mlb_played_first),
         First = first_name,
         Last = last_name) %>%
  filter(First == "Rafael") %>%
  filter(!is.na(First_Season)) %>%
  select(First, Last, MLBAM_ID, First_Season)

trout <- playerid_lookup("Trout") %>%
  mutate(MLBAM_ID = as.integer(mlbam_id),
         First_Season = as.integer(mlb_played_first),
         First = first_name,
         Last = last_name) %>%
  filter(First == "Mike") %>%
  filter(!is.na(First_Season)) %>%
  select(First, Last, MLBAM_ID, First_Season)

popular <- rbind(devers, trout)

judge <- playerid_lookup("Judge") %>%
  mutate(MLBAM_ID = as.integer(mlbam_id),
         First_Season = as.integer(mlb_played_first),
         First = first_name,
         Last = last_name) %>%
  filter(First == "Aaron") %>%
  filter(!is.na(First_Season)) %>%
  select(First, Last, MLBAM_ID, First_Season)

popular <- rbind(popular, judge)

tatis <- playerid_lookup("Tatis") %>%
  mutate(MLBAM_ID = as.integer(mlbam_id),
         First_Season = as.integer(mlb_played_first),
         First = first_name,
         Last = last_name) %>%
  filter(First == "Fernando") %>%
  filter(First_Season == 2019) %>%
  select(First, Last, MLBAM_ID, First_Season)

popular <- rbind(popular, tatis)

mookie <- playerid_lookup("Betts") %>%
  mutate(MLBAM_ID = as.integer(mlbam_id),
         First_Season = as.integer(mlb_played_first),
         First = first_name,
         Last = last_name) %>%
  filter(First == "Mookie") %>%
  filter(!is.na(First_Season)) %>%
  select(First, Last, MLBAM_ID, First_Season)

popular <- rbind(popular, mookie)

harper <- playerid_lookup("Harper") %>%
  mutate(MLBAM_ID = as.integer(mlbam_id),
         First_Season = as.integer(mlb_played_first),
         First = first_name,
         Last = last_name) %>%
  filter(First == "Bryce") %>%
  filter(!is.na(First_Season)) %>%
  select(First, Last, MLBAM_ID, First_Season)

popular <- rbind(popular, harper)

yelich <- playerid_lookup("Yelich") %>%
  mutate(MLBAM_ID = as.integer(mlbam_id),
         First_Season = as.integer(mlb_played_first),
         First = first_name,
         Last = last_name) %>%
  filter(First == "Christian") %>%
  filter(!is.na(First_Season)) %>%
  select(First, Last, MLBAM_ID, First_Season)

popular <- rbind(popular, yelich)

write_rds(popular, path="MLB-hitting-and-pitching/hitting_and_pitching_app/popular")


