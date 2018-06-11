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
# corresponding wealth shares and the average growth rate from the previous LAG years
# ----------------------------------------------------------------------------------------- #

# Import real GDP data from the World Bank
# URL: https://data.worldbank.org/indicator/NY.GDP.MKTP.KD
gdp_raw <- read.csv('../worldbank_gdp/API_NY.GDP.MKTP.KD_DS2_en_csv_v2.csv')

get_lorenz_curve <- function(ISO = "US", YEAR = 1988, LAG = 25) {
  # Get average real GDP growth from previous LAG years
  if(ISO=="CN") CountryCode <- "CHN"
  if(ISO=="ES") CountryCode <- "ESP"
  if(ISO=="FR") CountryCode <- "FRA"
  if(ISO=="GB") CountryCode <- "GBR"
  if(ISO=="US") CountryCode <- "USA"
  first <- as.character(YEAR - LAG)
  last <- as.character(YEAR)
  gdp_first <- gdp_raw[gdp_raw$CountryCode==CountryCode, grep(first, colnames(gdp_raw))]
  gdp_last <- gdp_raw[gdp_raw$CountryCode==CountryCode, grep(last, colnames(gdp_raw))]
  growth_factor <- (gdp_last/gdp_first)^(1/LAG)
  
  output <- world_dist %>% filter(iso == ISO & year == YEAR) %>% 
    select(p, botsh) %>%
    mutate(growth_factor = growth_factor)
  name <- paste(c("../../wealthData_", ISO, "_", as.character(YEAR), ".csv"), collapse = "")
  write_csv(output, name)
}

get_lorenz_curve(ISO = "CN", YEAR = 1988)
get_lorenz_curve(ISO = "ES", YEAR = 1988)
get_lorenz_curve(ISO = "FR", YEAR = 1988)
get_lorenz_curve(ISO = "GB", YEAR = 1988)
get_lorenz_curve(ISO = "US", YEAR = 1988)









