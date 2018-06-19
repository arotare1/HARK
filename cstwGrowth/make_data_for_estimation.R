# ================================================================================================ #
# Makes Lorenz curves for different countries to be used in quantitative exercise (WID.world)
# Appends growth rates over the period before and after a reference year (worldbank)
# Appends aggregate wealth-to-income ratios in reference year (WID.world)
#
# Lorenz curves are from the methodological appendix of the World Inequality Report 2018 (WIR)
# Assumes the files gpinterized.dta and wealth-spain.csv have been downloaded from the appendix
# Link to WIR: http://wir2018.wid.world/
# Link to methodological appendix: http://wir2018.wid.world/methodology.html
# Link to computer codes: http://wid.world/static/computer-codes.zip
# Countries: ES, FR, GB, US
# 
# Growth rates are from the World Bank
# The first measure computes the annualized growth rate of real gdp from start to end period
# Link: https://data.worldbank.org/indicator/NY.GDP.MKTP.KD
# The second measure averages over annual real gdp growth rates over relevant period
# Link: https://data.worldbank.org/indicator/NY.GDP.MKTP.KD.ZG

# Wealth-to-income ratios are from WID.world
# Variable code: wwealp_999_i
# Variable name: Net Private Wealth to Net National Income Ratio
# Link: http://wid.world/data/
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
  filter(p > 0 & p <= 99000 & iso != "WO" & iso != "CN") %>%
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


# ----------------------------------------------------------------------------------------------- #
# Write function that takes a country and two years as input and creates a .csv file with the 
# following structure:
# prc | bottom share now | bottom share after | K/Y now | K/Y after | growth before | growth after
# ----------------------------------------------------------------------------------------------- #

# Import real GDP data from the World Bank
gdp_wb <- read.csv('../worldbank_gdp/API_NY.GDP.MKTP.KD_DS2_en_csv_v2.csv')
gdp_growth_wb <- read.csv('../worldbank_growth/API_NY.GDP.MKTP.KD.ZG_DS2_en_csv_v2.csv')

# Import real GDP data from Penn World Table
gdp_penn <- read_excel('../penn_table_gdp/FebPwtExport6192018.xlsx', sheet='Data')


# Import wealth-to-income ratio from WID.world

# KY_now_ES <- 4.23159027099609   # 1988
# KY_now_FR <- 3.22252440452576
# KY_now_GB <- 3.71645903587341
# KY_now_US <- 3.64715075492859
# KY_after_ES <- 6.56139802932739 # 2013
# KY_after_FR <- 5.86394023895264
# KY_after_GB <- 5.63525152206421
# KY_after_US <- 4.6410756111145

KY_now_ES <- 3.68297553062439 # 1984
KY_now_FR <- 3.25881457328796 # 1984
KY_now_GB <- 2.76541519165039 # 1982
KY_now_US <- 3.30949544906616 # 1984
KY_after_ES <- 6.56139802932739 # 2013
KY_after_FR <- 5.80645942687988 # 2014
KY_after_GB <- 5.57926177978516 # 2012
KY_after_US <- 4.91764497756958 # 2014

get_lorenz_curve <- function(ISO = "US", YEAR1 = 1984, YEAR2 = 2014) {
  # Get average real GDP growth between YEAR1 and YEAR2
  if(ISO=="ES") {
    CountryCode <- "ESP"
    KY_now <- KY_now_ES
    KY_after <- KY_after_ES
  }
  if(ISO=="FR") {
    CountryCode <- "FRA"
    KY_now <- KY_now_FR
    KY_after <- KY_after_FR
  }
  if(ISO=="GB") {
    CountryCode <- "GBR"
    KY_now <- KY_now_GB
    KY_after <- KY_after_GB
  }
  if(ISO=="US") {
    CountryCode <- "USA"
    KY_now <- KY_now_US
    KY_after <- KY_after_US
  }
  
  LAG = YEAR2 - YEAR1
  
  gdp_before_penn <- (gdp_penn %>% filter(RegionCode==CountryCode & YearCode==YEAR1-LAG))$AggValue
  gdp_before_penn <- as.numeric(gdp_before_penn)
  
  gdp_now_penn <- (gdp_penn %>% filter(RegionCode==CountryCode & YearCode==YEAR1))$AggValue
  gdp_now_penn <- as.numeric(gdp_now_penn)
  
  gdp_after_penn <- (gdp_penn %>% filter(RegionCode==CountryCode & YearCode==YEAR2))$AggValue
  gdp_after_penn <- as.numeric(gdp_after_penn)
  
  growth_before_penn <- (gdp_now_penn/gdp_before_penn)^(1/LAG)
  growth_after_penn <- (gdp_after_penn/gdp_now_penn)^(1/LAG)
  
  if(YEAR1 - LAG < 1960) { # We don't have World Bank Data on GDP
    growth_before_wb1 <- NA
    growth_before_wb2 <- NA
    growth_after_wb1 <- NA
    growth_after_wb2 <- NA
  } else {
    before <- as.character(YEAR1 - LAG)
    before_plus_one <- as.character(YEAR1 - LAG + 1)
    now <- as.character(YEAR1)
    now_plus_one <- as.character(YEAR1 + 1)
    after <- as.character(YEAR2)
    
    gdp_before_wb <- gdp_wb[gdp_wb$CountryCode==CountryCode, grep(before, colnames(gdp_wb))]
    gdp_now_wb <- gdp_wb[gdp_wb$CountryCode==CountryCode, grep(now, colnames(gdp_wb))]
    gdp_after_wb <- gdp_wb[gdp_wb$CountryCode==CountryCode, grep(after, colnames(gdp_wb))]
    growth_before_wb1 <- (gdp_now_wb/gdp_before_wb)^(1/LAG)
    growth_after_wb1 <- (gdp_after_wb/gdp_now_wb)^(1/LAG)
    
    range_before <- grep(before_plus_one, colnames(gdp_growth_wb)) : grep(now, colnames(gdp_growth_wb))
    range_after <- grep(now_plus_one, colnames(gdp_growth_wb)) : grep(after, colnames(gdp_growth_wb))
    rates_before <- as.numeric(gdp_growth_wb[gdp_growth_wb$CountryCode==CountryCode, range_before])
    rates_after <- as.numeric(gdp_growth_wb[gdp_growth_wb$CountryCode==CountryCode, range_after])
    
    growth_before_wb2 <- mean(rates_before)/100 + 1
    growth_after_wb2 <- mean(rates_after)/100 + 1
  }
  
  # Get Lorenz curves from YEAR1 and YEAR2
  lorenz_now <- world_dist %>% 
    filter(iso == ISO & year == YEAR1) %>% 
    select(p, botsh) %>%
    rename(botsh_now = botsh)
  lorenz_after <- world_dist %>% 
    filter(iso == ISO & year == YEAR2) %>% 
    select(p, botsh) %>%
    rename(botsh_after = botsh)
  
  output <- inner_join(lorenz_now, lorenz_after) %>%
    mutate(KY_now = KY_now,
           KY_after = KY_after,
           growth_before_penn = growth_before_penn,
           growth_after_penn = growth_after_penn,
           growth_before_wb1 = growth_before_wb1,
           growth_before_wb2 = growth_before_wb2,
           growth_after_wb1 = growth_after_wb1,
           growth_after_wb2 = growth_after_wb2,
           now = YEAR1,
           before = YEAR1-LAG,
           after = YEAR2)
  
  name <- paste(c('/Users/andreea/Documents/phd/2ndyrpaper/output/countryWealth/wealthData_', 
                  ISO, '_', as.character(YEAR1), '_', as.character(YEAR2), '.csv'),
                collapse = '')
  write_csv(output, name)
}

# get_lorenz_curve(ISO = "ES", YEAR1 = 1988, YEAR2 = 2013)
# get_lorenz_curve(ISO = "FR", YEAR1 = 1988, YEAR2 = 2013)
# get_lorenz_curve(ISO = "GB", YEAR1 = 1988, YEAR2 = 2013)
# get_lorenz_curve(ISO = "US", YEAR1 = 1988, YEAR2 = 2013)

get_lorenz_curve(ISO = "ES", YEAR1 = 1984, YEAR2 = 2013)
get_lorenz_curve(ISO = "FR", YEAR1 = 1984, YEAR2 = 2014)
get_lorenz_curve(ISO = "GB", YEAR1 = 1982, YEAR2 = 2012)
get_lorenz_curve(ISO = "US", YEAR1 = 1984, YEAR2 = 2014)










