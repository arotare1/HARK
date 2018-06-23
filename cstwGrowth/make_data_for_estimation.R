# ================================================================================================ #
# Makes Lorenz curves for different countries to be used in quantitative exercise (WID.world)
# Appends growth rates over the period before and after a reference year (World Bank)
# Appends aggregate wealth-to-income ratios in reference year (WID.world)
#
# Lorenz curves are from the methodological appendix of the World Inequality Report 2018 (WIR)
# Assumes the files gpinterized.dta and wealth-spain.csv have been downloaded from the appendix
# Link to WIR: http://wir2018.wid.world/
# Link to methodological appendix: http://wir2018.wid.world/methodology.html
# Link to computer codes: http://wid.world/static/computer-codes.zip
# Countries: ES, FR, GB, US
# 
# Growth rates are from the World Bank and Penn World Table (use two sources for consistency)
# Link (WB): https://data.worldbank.org/indicator/NY.GDP.MKTP.KD
# Link (PWT) (indicator RGDPNA): http://febpwt.webhosting.rug.nl/Dmn/AggregateXs/PivotShow 
#
# Wealth-to-income ratios are from WID.world
# Variable code: wwealp_999_i
# Variable name: Net Private Wealth to Net National Income Ratio
# Link: http://wid.world/data/
#
# Saves output in ../../output/CountryWealth/
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
# Write function that takes a country and a time lag as input and creates a .csv file with 
# the wealth Lorenz curves for the latest year in the data and for lag years before
# The .csv file has the following structure:
# prc | bottom share now | bottom share after | K/Y now | K/Y after | growth before | growth after
# ----------------------------------------------------------------------------------------------- #

# Import real GDP data from the World Bank
gdp_wb <- read.csv('../worldbank_gdp/API_NY.GDP.MKTP.KD_DS2_en_csv_v2.csv')

# Import real GDP data from Penn World Table
gdp_penn <- read_excel('../penn_table_gdp/FebPwtExport6192018.xlsx', sheet='Data')


# Import wealth-to-income ratio from WID.world:
# For end period use year of latest available wealth data
KY_after_ES <- 6.56139802932739 # 2013
KY_after_FR <- 5.80645942687988 # 2014
KY_after_GB <- 5.57926177978516 # 2012
KY_after_US <- 4.91764497756958 # 2014

# For starting period use one of the following:
# 20 years before
KY_now_ES_20 <- 4.44789695739746 # 1993
KY_now_FR_20 <- 3.2654709815979  # 1994
KY_now_GB_20 <- 3.80056047439575 # 1992
KY_now_US_20 <- 3.76956367492676 # 1994

# 25 years before
KY_now_ES_25 <- 4.23159027099609 # 1988
KY_now_FR_25 <- 3.31351017951965 # 1989
KY_now_GB_25 <- 3.49701714515686 # 1987
KY_now_US_25 <- 3.75719499588013 # 1989

# 30 years before
KY_now_ES_30 <- 3.68297553062439 # 1984 (can't use 1983 since 1984 is the first year of data)
KY_now_FR_30 <- 3.25881457328796 # 1984
KY_now_GB_30 <- 2.76541519165039 # 1982
KY_now_US_30 <- 3.30949544906616 # 1984

# Define function generating lorenz curves
get_lorenz_curve <- function(ISO = "US", LAG=20) {
  # Currently works only for LAG = 20, 25, 30
  if(ISO=="ES") {
    CountryCode <- "ESP"
    YEAR2 <- 2013 # last year of data
    KY_now <- ifelse(LAG==20, KY_now_ES_20,
                     ifelse(LAG==25, KY_now_ES_25, KY_now_ES_30))
    KY_after <- KY_after_ES
  }
  if(ISO=="FR") {
    CountryCode <- "FRA"
    YEAR2 <- 2014 # last year of data
    KY_now <- ifelse(LAG==20, KY_now_FR_20,
                     ifelse(LAG==25, KY_now_FR_25, KY_now_FR_30))
    KY_after <- KY_after_FR
  }
  if(ISO=="GB") {
    CountryCode <- "GBR"
    YEAR2 <- 2012 # last year of data
    KY_now <- ifelse(LAG==20, KY_now_GB_20,
                     ifelse(LAG==25, KY_now_GB_25, KY_now_GB_30))
    KY_after <- KY_after_GB
  }
  if(ISO=="US") {
    CountryCode <- "USA"
    YEAR2 <- 2014 # last year of data
    KY_now <- ifelse(LAG==20, KY_now_US_20,
                     ifelse(LAG==25, KY_now_US_25, KY_now_US_30))
    KY_after <- KY_after_US
  }
  
  YEAR1 <- YEAR2 - LAG
  
  gdp_before_penn <- (gdp_penn %>% filter(RegionCode==CountryCode & YearCode==YEAR1-LAG))$AggValue
  gdp_before_penn <- as.numeric(gdp_before_penn)
  
  gdp_now_penn <- (gdp_penn %>% filter(RegionCode==CountryCode & YearCode==YEAR1))$AggValue
  gdp_now_penn <- as.numeric(gdp_now_penn)
  
  gdp_after_penn <- (gdp_penn %>% filter(RegionCode==CountryCode & YearCode==YEAR2))$AggValue
  gdp_after_penn <- as.numeric(gdp_after_penn)
  
  growth_before_penn <- (gdp_now_penn/gdp_before_penn)^(1/LAG)
  growth_after_penn <- (gdp_after_penn/gdp_now_penn)^(1/LAG)
  
  if(YEAR1 - LAG < 1960) { # We don't have World Bank Data on GDP
    growth_before_wb <- NA
    growth_after_wb <- NA
  } else {
    before = as.character(YEAR1 - LAG)
    now = as.character(YEAR1)
    after = as.character(YEAR2)
    
    gdp_before_wb <- gdp_wb[gdp_wb$CountryCode==CountryCode, grep(before, colnames(gdp_wb))]
    gdp_now_wb <- gdp_wb[gdp_wb$CountryCode==CountryCode, grep(now, colnames(gdp_wb))]
    gdp_after_wb <- gdp_wb[gdp_wb$CountryCode==CountryCode, grep(after, colnames(gdp_wb))]
    growth_before_wb <- (gdp_now_wb/gdp_before_wb)^(1/LAG)
    growth_after_wb <- (gdp_after_wb/gdp_now_wb)^(1/LAG)
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
  
  output <- inner_join(lorenz_now, lorenz_after, by = "p") %>%
    mutate(KY_now = KY_now,
           KY_after = KY_after,
           growth_before_penn = growth_before_penn,
           growth_after_penn = growth_after_penn,
           growth_before_wb = growth_before_wb,
           growth_after_wb = growth_after_wb,
           now = YEAR1,
           before = YEAR1-LAG,
           after = YEAR2)
  
  name <- paste(c('/Users/andreea/Documents/phd/2ndyrpaper/output/CountryWealth/WealthData_', 
                  ISO, '_', as.character(LAG), '.csv'), collapse = '')
  write_csv(output, name)
}

get_lorenz_curve(ISO = "ES", LAG = 20)
get_lorenz_curve(ISO = "FR", LAG = 20)
get_lorenz_curve(ISO = "GB", LAG = 20)
get_lorenz_curve(ISO = "US", LAG = 20)

get_lorenz_curve(ISO = "ES", LAG = 25)
get_lorenz_curve(ISO = "FR", LAG = 25)
get_lorenz_curve(ISO = "GB", LAG = 25)
get_lorenz_curve(ISO = "US", LAG = 25)

get_lorenz_curve(ISO = "ES", LAG = 29)
get_lorenz_curve(ISO = "FR", LAG = 30)
get_lorenz_curve(ISO = "GB", LAG = 30)
get_lorenz_curve(ISO = "US", LAG = 30)
