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
# --------------
# Wealth gini data comes from an experimental data set on Income, Consumption and Wealth (ICW)
# Link to data description: http://ec.europa.eu/eurostat/web/experimental-statistics/income-consumption-and-wealth
# Link to article discussing results: http://ec.europa.eu/eurostat/statistics-explained/index.php/Interaction_of_household_income,_consumption_and_wealth_-_statistics_on_main_results
# Link to data: http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=icw_sr_05&lang=en
#
# OECD
# -----
# Data on mean-to-median and most wealth shares is from the Wealth Distribution Database (WDD)
# Link to data: https://stats.oecd.org/Index.aspx?DataSetCode=WEALTH
# Data on bottom 40% share is from the following report
# Balestra, C. and R. Tonkin (2018), “Inequalities in household wealth across OECD countries: Evidence from the OECD Wealth Distribution Database”, OECD Statistics Working Papers, 2018/01, OECD Publishing, Paris
# Also import data from table 6.3. in the 2015 report "In It Together: Why Less Inequality Benefits All"
# Link to report: https://www.oecd-ilibrary.org/employment/in-it-together-why-less-inequality-benefits-all_9789264235120-en
# Link to table 6.3.: http://dx.doi.org/10.1787/888933209076
#
# World Bank
# -----------
# Growth is computed as the annualized growth of real GDP from start to end period
# Link: https://data.worldbank.org/indicator/NY.GDP.MKTP.KD
#
# The resulting dataset has the following columns:
# "country"         "iso"             "year"            "gini"            "mean"           
# "median"          "mean_to_median"  "top1_share"      "top5_share"      "top10_share"    
# "bot20_share"     "bot40_share"     "bot60_share"     "mid60_share"     "bot90_share"    
# "bot95_share"     "bot99_share"     "top1_to_median"  "top5_to_median"  "top10_to_median"
# "bot20_to_median" "bot40_to_median" "bot90_to_median" "bot95_to_median" "bot99_to_median"
# "gdp_now"         "gdp_last20"      "gdp_last25"      "gdp_last30"      "gdp_last35"
# "gdp_last40"      "growth_last20"   "growth_last25"   "growth_last30"   "growth_last35"
# "growth_last40"   "source"          "quality"
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

setwd("/Users/andreea/Documents/phd/2ndyrpaper/HARK/cstwGrowth/Empirical")

# Make country-iso crosswalk
country_iso <- tibble(country = c("Australia","Austria","Belgium","Canada","Chile",
                                  "Cyprus","Denmark","Estonia","Finland","France","Germany",
                                  "Greece","Hungary","Ireland","Italy","Japan","Korea","Latvia",
                                  "Luxembourg","Malta","Netherlands","New Zealand","Norway",
                                  "Poland","Portugal","Slovak Republic","Slovakia","Slovenia",
                                  "Spain","United Kingdom","United  Kingdom","United States",
                                  "Brazil","China","Colombia","Czech Republic","India","Indonesia",
                                  "Israel","Mexico","Russian Federation","Singapore","South Africa",
                                  "Sweden","Switzerland","Taiwan","Thailand"),
                      iso = c("AU", "AT", "BE", "CA", "CL", "CY", "DK", "EE", "FI", "FR", "DE",
                              "GR", "HU", "IE", "IT", "JP", "KR", "LV", "LU", "MT", "NL", "NZ",
                              "NO", "PL", "PT", "SK", "SK", "SI", "ES", "GB", "GB", "US", "BR",
                              "CN", "CO", "CZ", "IN", "ID", "IL", "MX", "RU", "SG", "ZA", "SE",
                              "CH", "TW", "TH"))

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

# Construct wealth shares of top 1%, 5%, 10%, bottom 20% and corresponding ratio to median.
wid_top1_share <- wid %>%
  group_by(iso, year) %>%
  filter(p < 99000) %>%
  summarise(top1_share = 1-sum(sinc))
wealth_inequality <- left_join(wealth_inequality, wid_top1_share, by=c("iso", "year"))
   
wid_top5_share <- wid %>%
  group_by(iso, year) %>%
  filter(p < 95000) %>%
  summarise(top5_share = 1-sum(sinc))
wealth_inequality <- left_join(wealth_inequality, wid_top5_share, by=c("iso", "year"))

wid_top10_share <- wid %>%
  group_by(iso, year) %>%
  filter(p < 90000) %>%
  summarise(top10_share = 1-sum(sinc))
wealth_inequality <- left_join(wealth_inequality, wid_top10_share, by=c("iso", "year"))

wid_bot20_share <- wid %>%
  group_by(iso, year) %>%
  filter(p < 20000) %>%
  summarise(bot20_share = sum(sinc))
wealth_inequality <- left_join(wealth_inequality, wid_bot20_share, by=c("iso", "year"))

# Construct wealth share of bottom 60%
wid_bot60_share <- wid %>%
  group_by(iso, year) %>%
  filter(p < 60000) %>%
  summarise(bot60_share = sum(sinc))
wealth_inequality <- left_join(wealth_inequality, wid_bot60_share, by=c("iso", "year"))

# Construct wealth share of bottom 40%
wid_bot40_share <- wid %>%
  group_by(iso, year) %>%
  filter(p < 40000) %>%
  summarise(bot40_share = sum(sinc))
wealth_inequality <- left_join(wealth_inequality, wid_bot40_share, by=c("iso", "year"))

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

# Add column with data quality
wealth_inequality$quality <- "Good"


# -------------------------- #
# Import OECD/Eurostat data
# -------------------------- #

eurostat <- read.csv("./oecd_wealth/oecd_eurostat_icw_sr_05/icw_sr_05_1_Data.csv")

eurostat_gini <- eurostat %>%
  filter(STK_FLOW=="Net wealth" & Value!=":") %>% 
  rename(country = GEO, year = TIME) %>%
  mutate(gini = as.numeric(Value)/100,
         source = "OECD/Eurostat ICW",
         quality = "Satisfactory") %>%
  select(country, year, gini, source, quality)

eurostat_gini <- left_join(eurostat_gini, country_iso, by="country")

# Join with big dataset
wealth_inequality <- bind_rows(wealth_inequality, eurostat_gini)


# -------------------------- #
# Import OECD data from WDD
# -------------------------- #

oecd_wdd <- read.csv("./oecd_wealth/oecd_wdd/WEALTH_17062018190946358.csv")

# Construct mean-to-median ratio. For countries other than Canada take the latest year
oecd_mean <- oecd_wdd %>%
  filter(Variable=="Mean net wealth per household (current prices)" & Country!="Canada") %>%
  group_by(Country) %>%
  rename(country = Country) %>%
  summarise(year = max(TIME),
            mean = Value[which.max(TIME)])
oecd_median <- oecd_wdd %>%
  filter(Variable=="Median net wealth per household (current prices)" & Country!="Canada") %>%
  group_by(Country) %>%
  rename(country = Country) %>%
  summarise(year = max(TIME),
            median = Value[which.max(TIME)])

# For Canada, use 2012 because this is the year with latest wealth share data
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

oecd_top5_share <- oecd_wdd %>%
  filter(VAR=="ST5") %>%
  group_by(Country) %>%
  rename(country = Country) %>%
  summarise(year = max(TIME),
            top5_share = Value[which.max(TIME)]/100)
oecd_inequality <- left_join(oecd_inequality, oecd_top5_share, by=c("country", "year"))

oecd_top10_share <- oecd_wdd %>%
  filter(VAR=="ST10") %>%
  group_by(Country) %>%
  rename(country = Country) %>%
  summarise(year = max(TIME),
            top10_share = Value[which.max(TIME)]/100)
oecd_inequality <- left_join(oecd_inequality, oecd_top10_share, by=c("country", "year"))

# Construct wealth share of bottom 60%
oecd_bot60_share <- oecd_wdd %>%
  filter(VAR=="SB60") %>%
  group_by(Country) %>%
  rename(country = Country) %>%
  summarise(year = max(TIME),
            bot60_share = Value[which.max(TIME)]/100)
oecd_inequality <- left_join(oecd_inequality, oecd_bot60_share, by=c("country", "year"))

# Construct wealth share of bottom 40%. This is not included in the database. I import it manually
# from Balestra and Tonkin (2018) Table 2.1
oecd_bot40_share <- tibble(country = c("Australia","Austria","Belgium","Canada","Chile","Denmark",
                                        "Estonia","Finland","France","Germany","Greece","Hungary",
                                        "Ireland","Italy","Japan","Korea","Latvia","Luxembourg",
                                        "Netherlands","New Zealand","Norway","Poland","Portugal",
                                        "Slovak Republic","Slovenia","Spain","United Kingdom",
                                        "United States"),
                            bot40_share = c(4.9, 1.0, 5.7, 3.4, 0.0, -8.6, 3.8, 2.2, 2.7, 0.5,
                                            5.3, 5.0, -2.1, 4.5, 5.3, 6.0, 0.0, 3.9, -6.9, 3.1, 
                                            -3.0, 6.2, 3.2, 10.6, 5.6, 6.9, 3.4, -0.1))
oecd_inequality <- left_join(oecd_inequality, oecd_bot40_share, by=c("country")) %>%
  mutate(bot40_share = bot40_share/100)

# Add column with source
oecd_inequality$source <- "OECD WDD"

# Add column with data quality
oecd_inequality$quality <- "Good"

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

# Construct mean to median ratio
oecd_report_mean_to_median <- oecd_report %>%
  rename(country = X__1) %>%
  mutate(mean_to_median = Mean/Median,
         year = 2010) %>%
  select(country, year, mean_to_median)
oecd_report_inequality <- left_join(oecd_report_inequality, oecd_report_mean_to_median,
                                    by=c("country", "year"))

# Construct ratio of average wealth of top 5% to median: (top5_avg-median)/median
oecd_report_top5_to_median <- oecd_report %>%
  rename(country = X__1,
         top5_median = `Ratio\n(wealth of top 5% - median wealth)/\nmedian`) %>%
  filter(country != "Korea") %>%
  mutate(year = 2010,
         top5_to_median = as.numeric(top5_median)) %>%
  select(country, year, top5_to_median)
oecd_report_inequality <- left_join(oecd_report_inequality, oecd_report_top5_to_median, 
                                    by=c("country", "year"))

# Construct ratio of average wealth of bottom 20% to median: (bot_60_avg-median)/median
oecd_report_bot20_to_median <- oecd_report %>%
  rename(country = X__1,
         bot20_to_median = `Ratio\n(median wealth - bottom quintile)/\nmedian`) %>%
  mutate(year = 2010) %>%
  select(country, year, bot20_to_median)
oecd_report_inequality <- left_join(oecd_report_inequality, oecd_report_bot20_to_median, 
                                    by=c("country", "year"))

# Construct wealth share of middle 60%
oecd_mid60_share <- oecd_report %>%
  rename(country = X__1,
         mid60_avg = `Average of the\n three middle quintiles`) %>%
  mutate(mid60_share = mid60_avg*0.6/Mean,
         year = 2010) %>%
  select(country, year, mid60_share)
oecd_report_inequality <- left_join(oecd_report_inequality, oecd_mid60_share, 
                                    by=c("country", "year"))
  
# Add column with source
oecd_report_inequality$source <- "OECD In it Together"

# Add column with data quality
oecd_report_inequality$quality <- "Satisfactory"

# Join with big dataset
wealth_inequality <- bind_rows(wealth_inequality, oecd_report_inequality)


# ------------------------------------------------------------------ #
# Import wealth data from Credit Suisse Global Wealth Databook 2017
# ------------------------------------------------------------------ #

cs_wealth1 <- read.csv("./credit_suisse_wealth/tabula-databook_table_6_1.csv", header=FALSE)
cs_wealth2 <- read.csv("./credit_suisse_wealth/tabula-databook_table_6_5.csv", header=FALSE)
cs_wealth3 <- read.csv("./credit_suisse_wealth/tabula-databook_table_3_1.csv", header=FALSE)
cs_wealth3 <- cs_wealth3[1:171,]

cs_wealth1 <- cs_wealth1 %>% 
  rename(country = V1, mean = V5, median = V6, quality = V11) %>%
  mutate(country = as.character(country),
         mean  = as.numeric(gsub(",", "", mean)),
         median = as.numeric(gsub(",", "", median)),
         mean_to_median = mean/median,
         source = "Credit Suisse",
         year = 2016) %>%
  select(country, year, mean, median, mean_to_median, quality, source)

cs_wealth2 <- cs_wealth2 %>%
  rename(country = V1) %>%
  mutate(country = as.character(country),
         top1_share = V13/100,
         top5_share = V12/100,
         top10_share = V11/100,
         bot20_share = (V2+V3)/100,
         bot40_share = (V2+V3+V4+V5)/100,
         bot60_share = (V2+V3+V4+V5+V6+V7)/100,
         top20_share = (100-V2-V3-V4-V5-V6-V7-V8-V9)/100,
         mid60_share = 1-top20_share-bot20_share,
         top20_to_bot20 = top20_share/bot20_share) %>%
  select(country, top1_share, top5_share, top10_share, bot20_share, bot40_share, bot60_share,
         mid60_share, top20_to_bot20)

cs_wealth3 <- cs_wealth3 %>%
  rename(country = V1, gini = V10) %>%
  mutate(country = as.character(country),
         gini = gini/100) %>%
  select(country, gini)

cs_wealth <- left_join(cs_wealth1, cs_wealth2, by="country")
cs_wealth <- left_join(cs_wealth, cs_wealth3, by="country")
cs_wealth <- right_join(country_iso, cs_wealth, by="country")

# Join with big dataset
wealth_inequality <- bind_rows(wealth_inequality, cs_wealth)


# ----------------------------------------- #
# Import real GDP data from the World Bank
# ----------------------------------------- #

gdp_raw <- read.csv('./worldbank_gdp/API_NY.GDP.MKTP.KD_DS2_en_csv_v2.csv')

# Join inequality with GDP data
wealth_inequality <- left_join(wealth_inequality, gdp_raw, by=c("country" = "CountryName"))

# Add growth variable (from 25 years before observation year)
wealth_inequality <- adply(wealth_inequality, 1, function(row){
  gdp_now <- as.numeric(row[grep(as.character(row$year), colnames(row))]) # get current GDP
  
  gdp_last20 <- as.numeric(row[grep(as.character(row$year - 20), colnames(row))]) # get past GDP
  gdp_last20 <- ifelse(length(gdp_last20) > 0, gdp_last20, NA)
  gdp_last25 <- as.numeric(row[grep(as.character(row$year - 25), colnames(row))]) # get past GDP
  gdp_last25 <- ifelse(length(gdp_last25) > 0, gdp_last25, NA)
  gdp_last30 <- as.numeric(row[grep(as.character(row$year - 30), colnames(row))]) # get past GDP
  gdp_last30 <- ifelse(length(gdp_last30) > 0, gdp_last30, NA)
  gdp_last35 <- as.numeric(row[grep(as.character(row$year - 35), colnames(row))]) # get past GDP
  gdp_last35 <- ifelse(length(gdp_last35) > 0, gdp_last35, NA)
  gdp_last40 <- as.numeric(row[grep(as.character(row$year - 35), colnames(row))]) # get past GDP
  gdp_last40 <- ifelse(length(gdp_last40) > 0, gdp_last40, NA)
  
  growth_last20 <- (gdp_now/gdp_last20)^(1/20) # get annual growth factor
  growth_last25 <- (gdp_now/gdp_last25)^(1/25)
  growth_last30 <- (gdp_now/gdp_last30)^(1/30)
  growth_last35 <- (gdp_now/gdp_last35)^(1/35)
  growth_last40 <- (gdp_now/gdp_last40)^(1/40)
  output <- data_frame(gdp_now = gdp_now,
                       gdp_last20 = gdp_last20,
                       gdp_last25 = gdp_last25,
                       gdp_last30 = gdp_last30,
                       gdp_last35 = gdp_last35,
                       gdp_last40 = gdp_last40,
                       growth_last20 = growth_last20,
                       growth_last25 = growth_last25,
                       growth_last30 = growth_last30,
                       growth_last35 = growth_last35,
                       growth_last40 = growth_last40)
  
  return(output)
})

# Compute some extra stats
wealth_inequality %<>% mutate(bot99_share = 1-top1_share,
                              bot95_share = 1-top5_share,
                              bot90_share = 1-top10_share,
                              top1_to_median = (top1_share*mean/0.01-median)/median,
                              top5_to_median = if_else(is.na(top5_to_median),
                                                       (top5_share*mean/0.05-median)/median,
                                                       top5_to_median),
                              top10_to_median = (top10_share*mean/0.1-median)/median,
                              bot20_to_median = if_else(is.na(bot20_to_median),
                                                              (median-bot20_share*mean/0.2)/median,
                                                              bot20_to_median),
                              bot40_to_median = (median-bot40_share*mean/0.4)/median,
                              bot99_to_median = (bot99_share*mean/0.99-median)/median,
                              bot95_to_median = (bot95_share*mean/0.95-median)/median,
                              bot90_to_median = (bot90_share*mean/0.9-median)/median)

# Select which variables to keep
wealth_inequality %<>% select(country, iso, year, gini, mean, median, mean_to_median,
                              top1_share, top5_share, top10_share,
                              bot20_share, bot40_share, bot60_share, mid60_share,
                              bot90_share, bot95_share, bot99_share,
                              top1_to_median, top5_to_median, top10_to_median,
                              bot20_to_median, bot40_to_median,
                              bot90_to_median, bot95_to_median, bot99_to_median,
                              gdp_now, gdp_last20, gdp_last25, gdp_last30, gdp_last35, gdp_last40,
                              growth_last20, growth_last25, growth_last30, growth_last35,
                              growth_last40, source, quality)

# Save data as .csv
write_csv(wealth_inequality, 
          "/Users/andreea/Documents/phd/2ndyrpaper/output/CountryWealth/WealthData_combined.csv")
