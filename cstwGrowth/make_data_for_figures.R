#
# Makes data for figures describing the wealth distribution on several countries over time
# Combines data from WID.world, OECD, OECD/Eurostat
#
# The resulting dataset si of the form:
# iso | year | source | top wealth shares | middle 60 share | gini | mean-to-median |
#     | top20 / bottom20  | (top5 - median)/median | (median - bottom 20)/median

library(plyr)
library(tidyverse)
library(magrittr)
library(haven)
library(readxl)
library(scales)
library(wid)
library(glue)
library(gpinter)

setwd("/Users/andreea/Documents/phd/2ndyrpaper/HARK/cstwGrowth/Empirical")

# ---------------------- #
# Import WID.world data
# ---------------------- #

world_dist <- read_dta("./wid_wealth/gpinterized.dta") %>% 
  mutate(year = as.integer(year)) %>%
  filter(iso != "WO" & iso != "CN") %>%
  filter(year >= 1984 & year <= 2013)

# Add Spain
wealth_spain <- read_csv("./wid_wealth/wealth-spain.csv") %>%
  filter(year >= 1984 & year <= 2013) %>% 
  mutate(p = 1e5*p) %>%
  select(-pop)
world_dist <- bind_rows(world_dist, wealth_spain)

# Construct wealth shares of top 1%, 5%, 10%, and bottom 60%
top1_share <- world_dist %>%
  group_by(iso, year) %>%
  filter(p < 99000) %>%
  summarise(top1_share = 1-sum(sinc)) %>%
  mutate(source = "WID.world")
top5_share <- world_dist %>%
  group_by(iso, year) %>%
  filter(p < 95000) %>%
  summarise(top5_share = 1-sum(sinc)) %>%
  mutate(source = "WID.world")
top10_share <- world_dist %>%
  group_by(iso, year) %>%
  filter(p < 90000) %>%
  summarise(top10_share = 1-sum(sinc)) %>%
  mutate(source = "WID.world")
bot60_share <- world_dist %>%
  group_by(iso, year) %>%
  filter(p < 60000) %>%
  summarise(bot60_share = sum(sinc)) %>%
  mutate(source = "WID.world")

# Construct mean-to-median ratio
mean_to_median <- world_dist %>%
  group_by(iso, year) %>%
  filter(p == 50000) %>%
  mutate(mean_to_median = totinc/tinc) %>%
  select(iso, year, mean_to_median) %>%
  mutate(source = "WID.world")

# Construct ratio of top quintile to bottom quintile
top20_share <- world_dist %>%
  group_by(iso, year) %>%
  filter(p < 80000) %>%
  summarise(top20_share = (1-sum(sinc)))
bot20_share <- world_dist %>%
  group_by(iso, year) %>%
  filter(p < 20000) %>%
  summarise(bot20_share = sum(sinc))
top20_to_bot20 <- left_join(top20_share, bot20_share) %>%
  mutate(top20_to_bot20 = top20_share/bot20_share) %>%
  select(-top20_share, -bot20_share) %>%
  mutate(source = "WID.world")

# Construct wealth share of middle 40%
mid40_share <- left_join(bot60_share, bot20_share) %>%
  mutate(mid40_share = bot60_share-bot20_share) %>%
  select(-bot60_share, -bot20_share) %>%
  mutate(source = "WID.world")

# Construct ratio of average wealth of top 5% to median: (top5_avg-median)/median
top5_avg <- world_dist %>%
  group_by(iso, year) %>%
  filter(p < 95000) %>%
  summarise(top5_avg = (1-sum(sinc))*totinc[1]/0.05)
median <- world_dist %>%
  group_by(iso, year) %>%
  filter(p == 50000) %>%
  rename(median = tinc) %>%
  select(iso, year, median)
top5_to_median <- left_join(top5_avg, median) %>%
  mutate(top5_to_median = (top5_avg-median)/median) %>%
  select(-top5_avg, -median) %>%
  mutate(source = "WID.world")

# Construct ratio of average wealth of bottom 20% to median: (bot_60_avg-median)/median
bot20_avg <- world_dist %>%
  group_by(iso, year) %>%
  filter(p < 20000) %>%
  summarise(bot20_avg = sum(sinc)*totinc[1]/0.2)
bot20_to_median <- left_join(bot20_avg, median) %>%
  mutate(bot20_to_median = (median-bot20_avg)/median) %>%
  select(-bot20_avg, -median) %>%
  mutate(source = "WID.world")

# Combine data from WID.world
wealth_inequality <- join_all(list(top1_share, top5_share, top10_share), by=c('iso','year'), type='left')



























