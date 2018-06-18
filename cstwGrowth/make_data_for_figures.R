# ================================================================================================ #
# Makes data for figures describing the wealth distribution of several countries over time
# Combines data from WID.world, OECD/Eurostat, OECD, Credit Suisse on the wealth distribution
# with real GDP growth data from the World Bank
#
# WID.world
# ----------
# Data comes from the methodological appendix of the World Inequality Report 2018 (WIR)
# Assumes the files gpinterized.dta and wealth-spain.csv have been downloaded from the appendix
# Link to WIR: http://wir2018.wid.world/
# Link to methodological appendix: http://wir2018.wid.world/methodology.html
# Link to computer codes: http://wid.world/static/computer-codes.zip
#
# OECD/Eurostat
# ---------
# Wealth gini data comes from an experimental data set on Income, Consumption and Wealth (ICW)
# Link to data description: http://ec.europa.eu/eurostat/web/experimental-statistics/income-consumption-and-wealth
# Link to article discussing results: http://ec.europa.eu/eurostat/statistics-explained/index.php/Interaction_of_household_income,_consumption_and_wealth_-_statistics_on_main_results
# Link to data: http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=icw_sr_05&lang=en
#
# OECD
# -----
# Data on mean-to-median and wealth shares is from the Wealth Distribution Database (WDD)
# Link to data: https://stats.oecd.org/Index.aspx?DataSetCode=WEALTH
# Data on all other indicators is from tables in the 2015 report "In It Together: Why Less Inequality Benefits All"
# Link to report: https://www.oecd-ilibrary.org/employment/in-it-together-why-less-inequality-benefits-all_9789264235120-en
# Link to table 6.3.: http://dx.doi.org/10.1787/888933209076
#
# World Bank
# -----------
# Growth rates are from the World Bank
# The first measure computes the annualized growth rate of real gdp from start to end period
# Link: https://data.worldbank.org/indicator/NY.GDP.MKTP.KD
# The second measure averages over annual real gdp growth rates over relevant period
# Link: https://data.worldbank.org/indicator/NY.GDP.MKTP.KD.ZG
#
# The resulting dataset has the following columns:
# "country"         "iso"             "year"            "mean"            "median"         
# "mean_to_median"  "top1_share"      "top1_to_median"  "top5_share"      "top5_to_median" 
# "top10_share"     "top10_to_median" "bot20_share"     "bot20_to_median" "bot60_share"    
# "top20_to_bot20"  "mid60_share"     "gini"            "source"          "gdp_now"        
# "gdp_last25"      "gdp_growth"
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

setwd("/Users/andreea/Documents/phd/2ndyrpaper/HARK/cstwGrowth/Empirical")

# Make country-iso crosswalk
country_iso <- tibble(country = c("Australia","Austria","Belgium","Canada","Chile",
                                  "Cyprus","Denmark","Estonia","Finland","France","Germany",
                                  "Greece","Hungary","Ireland","Italy","Japan","Korea","Latvia",
                                  "Luxembourg","Malta","Netherlands","New Zealand","Norway",
                                  "Poland","Portugal","Slovak Republic","Slovakia","Slovenia",
                                  "Spain","United Kingdom","United  Kingdom","United States"),
                      iso = c("AU", "AT", "BE", "CA", "CL", "CY", "DK", "EE", "FI", "FR", "DE",
                              "GR", "HU", "IE", "IT", "JP", "KR", "LV", "LU", "MT", "NL", "NZ",
                              "NO", "PL", "PT", "SK", "SK", "SI", "ES", "GB","GB", "US"))

# ---------------------- #
# Import WID.world data
# ---------------------- #

wid <- read_dta("./wid_wealth/gpinterized.dta") %>% 
  mutate(year = as.integer(year)) %>%
  filter(iso != "WO" & iso != "CN") %>%
  filter(year >= 1984 & year <= 2013)

# Add Spain
wid_spain <- read_csv("./wid_wealth/wealth-spain.csv") %>%
  filter(year >= 1984 & year <= 2013) %>% 
  mutate(p = 1e5*p) %>%
  select(-pop)
wid <- bind_rows(wid, wid_spain)
wid <- arrange(wid, iso, year)

# Construct mean, median, and mean-to-median
wid_mean_to_median <- wid %>%
  group_by(iso, year) %>%
  filter(p == 50000) %>%
  mutate(mean = totinc, median = tinc, mean_to_median = totinc/tinc) %>%
  select(iso, year, mean, median, mean_to_median)

# Create inital dataset and append new measures as we create them
wealth_inequality <- right_join(country_iso, wid_mean_to_median, by = "iso")

# Construct wealth shares of top 1%, 5%, 10%, bottom 20% and corresponding percent
# deviation from median. The latter measure top- and bottom inequality and are computed as:
# (avg wealth of top x% - median)/median and (median - avg wealth of bottom x%)/median
wid_top1_share <- wid %>%
  group_by(iso, year) %>%
  filter(p < 99000) %>%
  summarise(top1_share = 1-sum(sinc))
wealth_inequality <- left_join(wealth_inequality, wid_top1_share, by=c("iso", "year"))
wealth_inequality %<>% mutate(top1_to_median = ((top1_share*mean/0.01)-median)/median)
   
wid_top5_share <- wid %>%
  group_by(iso, year) %>%
  filter(p < 95000) %>%
  summarise(top5_share = 1-sum(sinc))
wealth_inequality <- left_join(wealth_inequality, wid_top5_share, by=c("iso", "year"))
wealth_inequality %<>% mutate(top5_to_median = ((top5_share*mean/0.05)-median)/median)

wid_top10_share <- wid %>%
  group_by(iso, year) %>%
  filter(p < 90000) %>%
  summarise(top10_share = 1-sum(sinc))
wealth_inequality <- left_join(wealth_inequality, wid_top10_share, by=c("iso", "year"))
wealth_inequality %<>% mutate(top10_to_median = ((top10_share*mean/0.1)-median)/median)

wid_bot20_share <- wid %>%
  group_by(iso, year) %>%
  filter(p < 20000) %>%
  summarise(bot20_share = sum(sinc))
wealth_inequality <- left_join(wealth_inequality, wid_bot20_share, by=c("iso", "year"))
wealth_inequality %<>% mutate(bot20_to_median = (median-(bot20_share*mean/0.2))/median)

# Construct wealth share of bottom 60%
wid_bot60_share <- wid %>%
  group_by(iso, year) %>%
  filter(p < 60000) %>%
  summarise(bot60_share = sum(sinc))
wealth_inequality <- left_join(wealth_inequality, wid_bot60_share, by=c("iso", "year"))

# Construct ratio of top quintile to bottom quintile
wid_top20_share <- wid %>%
  group_by(iso, year) %>%
  filter(p < 80000) %>%
  summarise(top20_share = (1-sum(sinc)))
wealth_inequality <- left_join(wealth_inequality, wid_top20_share, by=c("iso", "year")) %>%
  mutate(top20_to_bot20 = top20_share/bot20_share) %>%
  select(-top20_share)

# Construct wealth share of middle 60%
wid_mid60_share <- left_join(wid_top20_share, wid_bot20_share, by = c("iso", "year")) %>%
  mutate(mid60_share = 1-top20_share-bot20_share) %>%
  select(-top20_share, -bot20_share)
wealth_inequality <- left_join(wealth_inequality, wid_mid60_share, by = c("iso", "year"))

# Add Gini column with NAs
wealth_inequality$gini <- NA

# Add column with source
wealth_inequality$source <- "WID.world"


# -------------------------- #
# Import OECD/Eurostat data
# -------------------------- #

eurostat <- read.csv("./oecd_wealth/oecd_eurostat_icw_sr_05/icw_sr_05_1_Data.csv")

eurostat_gini <- eurostat %>%
  filter(STK_FLOW=="Net wealth" & Value!=":") %>% 
  rename(country = GEO, year = TIME, gini = Value) %>%
  mutate(source = "OECD/Eurostat ICW") %>%
  select(country, year, gini, source)

eurostat_gini <- left_join(eurostat_gini, country_iso, by="country")

# Join with big dataset
wealth_inequality <- bind_rows(wealth_inequality, eurostat_gini)


# -------------------------- #
# Import OECD data from WDD
# -------------------------- #

oecd_wdd <- read.csv("./oecd_wealth/oecd_wdd/WEALTH_17062018190946358.csv")

# Construct mean-to-median ratio. For GB only household level data is available, so use that
# instead of individual level data that was used for the others
oecd_mean <- oecd_wdd %>%
  filter(Variable=="Mean net wealth per household (current prices)") %>%
  group_by(Country) %>%
  rename(country = Country) %>%
  summarise(year = max(TIME),
            mean = Value[which.max(TIME)])
oecd_median <- oecd_wdd %>%
  filter(Variable=="Median net wealth per household (current prices)") %>%
  group_by(Country) %>%
  rename(country = Country) %>%
  summarise(year = max(TIME),
            median = Value[which.max(TIME)])

# For Canada, add 2012 because this is the year with latest wealth share data
oecd_CA_mean <- oecd_wdd %>%
  filter(Country=="Canada" & TIME==2012 & 
           Variable=="Mean net wealth per household (current prices)") %>%
  rename(country = Country, year = TIME) %>%
  mutate(mean = Value) %>%
  select(country, year, mean)
oecd_mean <- bind_rows(oecd_mean, oecd_CA_mean)

oecd_CA_median <- oecd_wdd %>%
  filter(Country=="Canada" & TIME==2012 & 
           Variable=="Median net wealth per household (current prices)") %>%
  rename(country = Country, year = TIME) %>%
  mutate(median = Value) %>%
  select(country, year, median)
oecd_median <- bind_rows(oecd_median, oecd_CA_median)

oecd_mean_to_median <- left_join(oecd_mean, oecd_median, by=c("country", "year")) %>%
  mutate(mean_to_median = mean/median)

# Create inital dataset and append new measures as we create them
oecd_inequality <- right_join(country_iso, oecd_mean_to_median, by="country") %>%
  arrange(iso, year)

# Construct wealth shares of top 1%, 5%, 10%, and corresponding percent deviation from median.
# The latter measure top inequality and are computed as:
# (avg wealth of top x% - median)/median
oecd_top1_share <- oecd_wdd %>%
  filter(VAR=="ST1") %>%
  group_by(Country) %>%
  rename(country = Country) %>%
  summarise(year = max(TIME),
            top1_share = Value[which.max(TIME)]/100)
oecd_inequality <- left_join(oecd_inequality, oecd_top1_share, by=c("country", "year"))
oecd_inequality %<>% mutate(top1_to_median = ((top1_share*mean/0.01)-median)/median)

oecd_top5_share <- oecd_wdd %>%
  filter(VAR=="ST5") %>%
  group_by(Country) %>%
  rename(country = Country) %>%
  summarise(year = max(TIME),
            top5_share = Value[which.max(TIME)]/100)
oecd_inequality <- left_join(oecd_inequality, oecd_top5_share, by=c("country", "year"))
oecd_inequality %<>% mutate(top5_to_median = ((top5_share*mean/0.05)-median)/median)

oecd_top10_share <- oecd_wdd %>%
  filter(VAR=="ST10") %>%
  group_by(Country) %>%
  rename(country = Country) %>%
  summarise(year = max(TIME),
            top10_share = Value[which.max(TIME)]/100)
oecd_inequality <- left_join(oecd_inequality, oecd_top10_share, by=c("country", "year"))
oecd_inequality %<>% mutate(top10_to_median = ((top10_share*mean/0.1)-median)/median)

# Construct wealth share of bottom 60%
oecd_bot60_share <- oecd_wdd %>%
  filter(VAR=="SB60") %>%
  group_by(Country) %>%
  rename(country = Country) %>%
  summarise(year = max(TIME),
            bot60_share = Value[which.max(TIME)])
oecd_inequality <- left_join(oecd_inequality, oecd_bot60_share, by=c("country", "year"))

# Add column with source
oecd_inequality$source <- "OECD WDD"

# Join with big dataset
wealth_inequality <- bind_rows(wealth_inequality, oecd_inequality)


# --------------------------------------------------- #
# Import OECD data from 2015 report "In it Together"
# --------------------------------------------------- #

oecd_report <- read_excel("./oecd_wealth/oecd2015_table6.3.xls",
                          sheet="Tab 6.3 EN", range="A10:K28")

# Construct ratio of top quintile to bottom quintile
oecd_top20_to_bot20 <- oecd_report %>%
  rename(country = X__1,
         top20 = `Top quintile`,
         bot20 = `Bottom\n quintile`) %>%
  mutate(top20_to_bot20 = top20/bot20,
         year = 2010) %>%
  select(country, year, top20_to_bot20)

# Create inital dataset and append new measures as we create them
oecd_report_inequality <- right_join(country_iso, oecd_top20_to_bot20, by="country") %>%
  arrange(iso, year)

# Construct wealth share of middle 60%
oecd_mid60_share <- oecd_report %>%
  rename(country = X__1,
         mid60_avg = `Average of the\n three middle quintiles`) %>%
  mutate(mid60_share = mid60_avg*0.6/Mean,
         year = 2010) %>%
  select(country, year, mid60_share)
oecd_report_inequality <- left_join(oecd_report_inequality, oecd_mid60_share, 
                                    by=c("country", "year"))
  
# Construct ratio of average wealth of top 5% to median: (top5_avg-median)/median
oecd_top5_to_median <- oecd_report %>%
  rename(country = X__1,
         top5_median = `Ratio\n(wealth of top 5% - median wealth)/\nmedian`) %>%
  filter(country != "Korea") %>%
  mutate(year = 2010,
         top5_to_median = as.numeric(top5_median)) %>%
  select(country, year, top5_to_median)
oecd_report_inequality <- left_join(oecd_report_inequality, oecd_top5_to_median, 
                                    by=c("country", "year"))

# Construct ratio of average wealth of bottom 20% to median: (bot_60_avg-median)/median
oecd_bot20_to_median <- oecd_report %>%
  rename(country = X__1,
         bot20_to_median = `Ratio\n(median wealth - bottom quintile)/\nmedian`) %>%
  mutate(year = 2010) %>%
  select(country, year, bot20_to_median)
oecd_report_inequality <- left_join(oecd_report_inequality, oecd_bot20_to_median, 
                                    by=c("country", "year"))

# Add column with source
oecd_report_inequality$source <- "OECD In it Together"

# Join with big dataset
wealth_inequality <- bind_rows(wealth_inequality, oecd_report_inequality)


# ----------------------------------------- #
# Import real GDP data from the World Bank
# ----------------------------------------- #

gdp_raw <- read.csv('./worldbank_gdp/API_NY.GDP.MKTP.KD_DS2_en_csv_v2.csv')
#gdp_growth <- read.csv('./worldbank_growth/API_NY.GDP.MKTP.KD.ZG_DS2_en_csv_v2.csv')

# Before joining with GDP data make a list of variables to keep
keep <- c(colnames(wealth_inequality), "gdp_now", "gdp_last25", "gdp_growth")

# Join inequality with GDP data
wealth_inequality <- left_join(wealth_inequality, gdp_raw, by=c("country" = "CountryName"))

# Add growth variable (from 25 years before observation year)
wealth_inequality <- adply(wealth_inequality, 1, function(row){
  if(row$year < 1985) {
    output <- data_frame(gdp_now=NA, gdp_last25=NA, gdp_growth=NA)
  }
  else {
    gdp_now <- as.numeric(row[grep(as.character(row$year), colnames(row))]) # get current GDP
    gdp_last25 <- as.numeric(row[grep(as.character(row$year - 25), colnames(row))]) # get past GDP
    gdp_growth <- (gdp_now/gdp_last25)^(1/25) # get annualized growth factor
    output <- data_frame(gdp_now=gdp_now, gdp_last25=gdp_last25, gdp_growth=gdp_growth)
  }
  return(output)
})

# Delete GDP data
wealth_inequality <- wealth_inequality[, colnames(wealth_inequality) %in% keep]

# Save data as .csv
write_csv(wealth_inequality, 
          "/Users/andreea/Documents/phd/2ndyrpaper/output/countryWealth/wealthData_combined.csv")






