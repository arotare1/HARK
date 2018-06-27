# =============================================================================================== #
# Make figures of top wealth shares over time
#
# Saves output in ../../../output/Figures_data/ and ../../../tex/
# =============================================================================================== #

library(plyr)
library(tidyverse)
library(magrittr)
library(haven)
library(readxl)
library(scales)
library(wid)
library(glue)
library(gpinter)

setwd('/Users/andreea/Documents/phd/2ndyrpaper/output/Figures_data')

# ---------------------- #
# Import WID.world data
# ---------------------- #

# Keep just wealth shares of bottom 1,2,3...,99 percent
world_dist <- read_dta("/Users/andreea/Documents/phd/2ndyrpaper/HARK/cstwGrowth/Empirical/wid_wealth/gpinterized.dta") %>% 
  filter(p > 0 & p <= 99000 & iso != "WO" & iso != "CN") %>%
  mutate(year = as.integer(year), p = p/1000) %>%
  rename(mean = totinc) %>%
  group_by(iso, year) %>%
  mutate(median = tinc[which(p==50)]) %>%
  select(iso, year, p, botsh, mean, median) %>%
  arrange(iso, year, p)

# Load wealth data from Spain
wealth_spain_raw <- read_csv("/Users/andreea/Documents/phd/2ndyrpaper/HARK/cstwGrowth/Empirical/wid_wealth/wealth-spain.csv")
wealth_spain <- wealth_spain_raw %>%
  filter(p <= 0.99) %>%
  rename(mean = totinc) %>%
  group_by(year) %>%
  mutate(median = tinc[which(p==0.5)],
         p = as.integer(lead(p)*100)) %>%
  select(iso, year, p, sinc, mean, median) %>%
  arrange(year)
wealth_spain <- wealth_spain[complete.cases(wealth_spain),]

# Transform bracket shares into shares of bottom percentile
wealth_spain %<>% group_by(year) %>%
  mutate(sinc = cumsum(sinc)) %>%
  rename(botsh = sinc)

# Merge Spain with other countries
world_dist <- bind_rows(world_dist, wealth_spain)

# Keep just wealth shares of the top 1% and 10%
fig_data <- world_dist %>% filter(p==99 | p==90) %>%
  mutate(topsh = 1-botsh,
         p = if_else(p==99, "(a) Wealth share of the top 1%", "(b) Wealth share of the top 10%")) %>%
  select(iso, year, p, topsh)


fig <- ggplot(fig_data, aes(x = year, y = topsh, color = iso)) +
  geom_point() +
  geom_line() +
  labs(x = "", y = "", color = "") +
  facet_wrap(~ p, scales = "fixed") +
  theme_light() +
  scale_color_hue(labels = c("Spain", "France", "United Kingdom", "United States")) +
  theme(axis.title.y = element_text(size = 9),
        strip.text.x = element_text(size = 10, color = "black"),
        strip.background = element_rect(colour="white", fill="white"),
        legend.position = "bottom",
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(0,0,0,0))
ggsave("historical.pdf", plot = fig, device = "pdf", width = 8, height = 4)
ggsave("../../tex/historical.pdf", plot = fig, device = "pdf", width = 8, height = 4)


