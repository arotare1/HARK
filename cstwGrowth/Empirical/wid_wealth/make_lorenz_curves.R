# ================================================================================================ #
# Makes Lorenz curves from WID.world to be used in quantitative exercise
# Uses data and code from the methodological appendix of the World Inequality Report 2018 (WIR)
# Assumes the files gpinterized.dta and wealth-spain.csv have been downloaded from the appendix
# Link to WIR: http://wir2018.wid.world/
# Link to methodological appendix: http://wir2018.wid.world/methodology.html
# Link to computer codes: http://wid.world/static/computer-codes.zip
# Countries: ES, FR, US, GB
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
wealth_spain_raw <- read_csv("wealth-spain.csv")
wealth_spain <- ldply(min(wealth_spain_raw$year):max(wealth_spain_raw$year), function(YEAR) {
  output <- wealth_spain_raw %>%
    filter(p <= 0.99 & year==YEAR) %>% 
    mutate(p = as.integer(lead(p)*100)) %>%
    select(iso, year, p, sinc)
  output <- output[complete.cases(output),]
})
  
# Transform bracket shares into shares of bottom percentile
wealth_spain <- ldply(min(wealth_spain$year):max(wealth_spain$year), function(YEAR) {
  wealth_spain %>% filter(year == YEAR) %>%
    mutate(sinc = cumsum(sinc)) %>%
    rename(botsh = sinc)
  })

# Fill in missing percentiles for Spain
missing_spain <- ldply(min(wealth_spain$year):max(wealth_spain$year), function(year) {
  tibble(iso = "ES", 
         year = year,
         p = c(1:9, 11:19, 21:29, 31:39, 41:49, 51:59, 61:69, 71:74, 76:79, 81:89, 91:94, 96:98), 
         botsh = NA)
  })

wealth_spain <- bind_rows(wealth_spain, missing_spain) %>%
  arrange(iso, year, p)

#wealth_spain %<>% mutate(botsh = if_else(p < 10, 0, botsh)) %>% fill(botsh)

# Merge Spain with other countries
world_dist <- bind_rows(world_dist, wealth_spain)


# ---------------------------------------------------------------------------------------------- #
# Write function that takes a country and a year as input and creates a .csv file with the 
# following structure:
# percentile | bottom wealth share | growth past 25 yrs (2 cols) | growth future 25 yrs (2 cols)
# ---------------------------------------------------------------------------------------------- #

# Import real GDP data from the World Bank
# URL: https://data.worldbank.org/indicator/NY.GDP.MKTP.KD
gdp_raw <- read.csv('../worldbank_gdp/API_NY.GDP.MKTP.KD_DS2_en_csv_v2.csv')

# Import real GDP growth data from the World Bank
# URL: https://data.worldbank.org/indicator/NY.GDP.MKTP.KD.ZG
gdp_growth <- read.csv('../worldbank_growth/API_NY.GDP.MKTP.KD.ZG_DS2_en_csv_v2.csv')

get_lorenz_curve <- function(ISO = "US", YEAR = 1988, LAG = 25) {
  # Get average real GDP growth from past and future LAG years. Use two measures for consistency
  if(ISO=="CN") CountryCode <- "CHN"
  if(ISO=="ES") CountryCode <- "ESP"
  if(ISO=="FR") CountryCode <- "FRA"
  if(ISO=="GB") CountryCode <- "GBR"
  if(ISO=="US") CountryCode <- "USA"
  
  before <- as.character(YEAR - LAG)
  before_plus_one <- as.character(YEAR - LAG + 1)
  now <- as.character(YEAR)
  now_plus_one <- as.character(YEAR + 1)
  after <- as.character(YEAR + LAG)

  gdp_before <- gdp_raw[gdp_raw$CountryCode==CountryCode, grep(before, colnames(gdp_raw))]
  gdp_now <- gdp_raw[gdp_raw$CountryCode==CountryCode, grep(now, colnames(gdp_raw))]
  gdp_after <- gdp_raw[gdp_raw$CountryCode==CountryCode, grep(after, colnames(gdp_raw))]
  growth_before_1 <- (gdp_now/gdp_before)^(1/LAG)
  growth_after_1 <- (gdp_after/gdp_now)^(1/LAG)
  
  range_before <- grep(before_plus_one, colnames(gdp_growth)) : grep(now, colnames(gdp_growth))
  range_after <- grep(now_plus_one, colnames(gdp_growth)) : grep(after, colnames(gdp_growth))
  rates_before <- as.numeric(gdp_growth[gdp_growth$CountryCode==CountryCode, range_before])
  rates_after <- as.numeric(gdp_growth[gdp_growth$CountryCode==CountryCode, range_after])
                  
  growth_before_2 <- mean(rates_before)/100 + 1
  growth_after_2 <- mean(rates_after)/100 + 1
  
  # Get Lorenz curves from YEAR and YEAR+LAG
  lorenz_now <- world_dist %>% 
    filter(iso == ISO & year == YEAR) %>% 
    select(p, botsh) %>%
    rename(botsh_now = botsh)
  lorenz_after <- world_dist %>% 
    filter(iso == ISO & year == YEAR+LAG) %>% 
    select(p, botsh) %>%
    rename(botsh_after = botsh)
  
  output <- inner_join(lorenz_now, lorenz_after) %>%
    mutate(now = YEAR,
           before = YEAR-LAG,
           after = YEAR+LAG,
           growth_before_1 = growth_before_1,
           growth_before_2 = growth_before_2,
           growth_after_1 = growth_after_1,
           growth_after_2 = growth_after_2)
  
  name <- paste(c('/Users/andreea/Documents/phd/2ndyrpaper/outputFiles/wealthDataByCountry/wealthData_', 
                  ISO, '_', as.character(YEAR), ".csv"), collapse = '')
  write_csv(output, name)
}

get_lorenz_curve(ISO = "ES", YEAR = 1988)
get_lorenz_curve(ISO = "FR", YEAR = 1988)
get_lorenz_curve(ISO = "GB", YEAR = 1988)
get_lorenz_curve(ISO = "US", YEAR = 1988)









