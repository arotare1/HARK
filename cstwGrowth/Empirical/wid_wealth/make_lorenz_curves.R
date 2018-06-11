# ================================================================================================ #
# Makes Lorenz curves from WID.world to be used in quantitative exercise
# Uses data and code from the methodological appendix of the World Inequality Report 2018 (WIR)
# Assumes the files gpinterized.dta and wealth-spain.csv have been downloaded from the appendix
# Link to WIR: http://wir2018.wid.world/
# Link to methodological appendix: http://wir2018.wid.world/methodology.html
# Link to computer codes: http://wid.world/static/computer-codes.zip
# Countries: CN, ES, FR, US, GB
# ================================================================================================ #

library(plyr)
library(tidyverse)
library(magrittr)
library(haven)
library(readxl)
library(scales)
library(wid)
library(glue)
library(gpinter)

setwd('/Users/andreea/Documents/phd/2ndyrpaper/HARK/cstwGrowth/Empirical/wid_wealth')

# ---------------------- #
# Import WID.world data
# ---------------------- #

# Keep just wealth shares of bottom 1,2,3...,99 percent
world_dist <- read_dta("gpinterized.dta") %>% 
  filter(p > 0 & p <= 99000 & iso != "WO") %>%
  mutate(year = as.integer(year), p = p/1000) %>%
  select(iso, year, p, botsh) %>%
  arrange(iso, year, p)

# Load wealth data from Spain
wealth_spain <- read_csv("wealth-spain.csv") %>%
  filter(p > 0 & p <= 0.99) %>% 
  mutate(p = as.integer(p*100),
         sinc = cumsum(sinc)) %>% # transform bracket share into share of bottom x percent
  rename(botsh = sinc) %>% # rename it like in world_dist
  select(iso, year, p, botsh)

# Fill in missing percentiles for Spain
missing_spain <- ldply(min(wealth_spain$year):max(wealth_spain$year), function(year) {
  tibble(iso = "ES", 
         year = year,
         p = c(1:9, 11:19, 21:29, 31:39, 41:49, 51:59, 61:69, 71:74, 76:79, 81:89, 91:94, 96:98), 
         botsh = NA)
  })

wealth_spain <- bind_rows(wealth_spain, missing_spain) %>%
  arrange(iso, year, p) %>%
  mutate(botsh = if_else(p < 10, 0, botsh))

wealth_spain %<>% fill(botsh)

# Merge Spain with other countries
world_dist <- bind_rows(world_dist, wealth_spain)


# ----------------------------------------------------------------------------------------- #
# Write function that takes a country and a year as input and creates a .csv file with the 
# corresponding wealth shares
# ----------------------------------------------------------------------------------------- #

get_lorenz_curve <- function(data, ISO = "US", YEAR = 2014) {
  output <- data %>% filter(iso == ISO & year == YEAR) %>% select(p, botsh)
  name <- paste(c("wealthData_", ISO, "_", as.character(YEAR), ".csv"), collapse = "")
  write_csv(output, name)
}

get_lorenz_curve(world_dist, ISO = "FR", YEAR = 2014)










