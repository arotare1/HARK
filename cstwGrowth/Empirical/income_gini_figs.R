library(foreign)
library(plyr)
library(ggplot2)
library(ggrepel)

setwd('/Users/andreea/Documents/phd/2ndyrpaper/data')

#----------------------------------------------------------------------------
# Read income group of each country from the World Bank
# https://data.worldbank.org/indicator/SI.POV.GINI?view=chart
#----------------------------------------------------------------------------
inc_group <- read.csv('./worldbank_gini/Metadata_Country_API_SI.POV.GINI_DS2_en_csv_v2.csv')
inc_group <- inc_group[, colnames(inc_group) %in% c('CountryCode', 'IncomeGroup')]


#----------------------------------------------------------------------------
# Read Gini data from the World Bank
# https://data.worldbank.org/indicator/SI.POV.GINI?view=chart
#----------------------------------------------------------------------------
wb_gini_raw <- read.csv('./worldbank_gini/API_SI.POV.GINI_DS2_en_csv_v2.csv')

# For each country, keep just earliest and latest years of gini data
wb_gini <- adply(wb_gini_raw, 1, function(row) {
  index <- which(!is.na(row[grep('X', colnames(row))])) # find years with data
  if (length(index)<2) { # if there is no data for more than two years return NAs
    output <- data.frame(first=NA, last=NA, first_gini=NA, last_gini=NA)
  } else {
    index <- index[c(1, length(index))] # find first and last year with data
    yrs <- data.frame(first=index[1]+1959, last=index[2]+1959)
    vals <- row[index+4]/100 # scale Gini to be between 0 and 1
    colnames(vals) <- c('first_gini', 'last_gini') 
    output <- merge(yrs, vals)
  }
  return(output)
})
wb_gini <- wb_gini[, colnames(wb_gini) %in% c('CountryCode', 'CountryName',
                                              'first', 'last', 'first_gini', 'last_gini')]
wb_gini <- wb_gini[complete.cases(wb_gini),]
wb_gini$d_gini <- wb_gini$last_gini-wb_gini$first_gini

# Keep just countries with observations more than 15 years apart
wb_gini <- wb_gini[which(wb_gini$last-wb_gini$first>=15),]

# Merge with income group data
wb_gini <- merge(inc_group, wb_gini, by='CountryCode')

#----------------------------------------------------------------------------
# Read Gini data from the OECD
# http://stats.oecd.org/Index.aspx?DataSetCode=IDD#
#----------------------------------------------------------------------------
oecd_raw <- read.csv('./oecd_gini/IDD_15042018050521557.csv')
oecd_gini <- oecd_raw[grep('GINI', oecd_raw$MEASURE),] # keep just Gini measures
oecd_gini <- oecd_gini[grep('TOT', oecd_gini$AGE),] # keep just values for total population (above 18)

# Keep just Gini for disposable income and just the columns of interest
oecd_gini <- oecd_gini[oecd_gini$MEASURE=='GINI',
                       colnames(oecd_gini) %in% c('LOCATION', 'Country', 'METHODO', 'TIME', 'Value')]

# Rename counrty code and country name columns for consistency with World Bank data
colnames(oecd_gini)[1] <- 'CountryCode'
colnames(oecd_gini)[2] <- 'CountryName'

# For each country, keep just earliest and latest years of data
oecd_gini <- ddply(oecd_gini, .(CountryCode), summarise,
                   CountryName = CountryName[1],
                   first = min(TIME), last = max(TIME),
                   first_gini = Value[which(TIME==min(TIME))[1]], # use methodology from 2012 if available
                   last_gini = Value[which(TIME==max(TIME))[1]], # use methodology from 2012 if available
                   meth_first = METHODO[which(TIME==min(TIME))[1]], # store methodology
                   meth_last = METHODO[which(TIME==max(TIME))[1]], # store methodology
                   d_gini = last_gini-first_gini)

# Keep just countries with observations more than 15 years apart
oecd_gini <- oecd_gini[which(oecd_gini$last-oecd_gini$first>=15),]

# Merge with income group data
oecd_gini <- merge(inc_group, oecd_gini, by='CountryCode')


# #----------------------------------------------------------------------------
# # Read Gini data from the World Income Inequality Database
# # https://www.wider.unu.edu/database/world-income-inequality-database-wiid34
# #----------------------------------------------------------------------------
wiid_raw <- read.csv('./wiid_gini/WIID3.4_19JAN2017New.csv')

# Keep just OECD data
wiid_gini <- wiid_raw[grep('OECD', wiid_raw$Source),]

# Keep just data on disposable income
wiid_gini <- wiid_gini[wiid_gini$Welfaredefn_new=='Income, disposable',]

# Keep just columns of interest
wiid_gini <- wiid_gini[, colnames(wiid_gini) %in% c('Countrycode3', 'Country', 'Year', 'Incomegroup',
                                                    'Gini')]
# Rename column names for compatibility
colnames(wiid_gini)[1] <- 'CountryName'
colnames(wiid_gini)[2] <- 'CountryCode'
colnames(wiid_gini)[4] <- 'IncomeGroup'


# For each country, keep just earliest and latest years of data
wiid_gini <- ddply(wiid_gini, .(CountryCode), summarise,
                   CountryName = CountryName[1],
                   IncomeGroup = IncomeGroup[1],
                   first = min(Year), last = max(Year),
                   # Chile and South Korea have two observations for 2011, so keep just one:
                   first_gini = Gini[which(Year==min(Year))[length(which(Year==min(Year)))]]/100,
                   last_gini = Gini[which(Year==max(Year))[length(which(Year==max(Year)))]]/100,
                   d_gini = last_gini-first_gini)

# Keep just countries with observations more than 15 years apart
wiid_gini <- wiid_gini[which(wiid_gini$last-wiid_gini$first>=15),]

# The wiid and oecd sources have different starting years for CAN, NLD, USA:
#       oecd    wiid
# CAN   1977    1983
# NLD   1977    1985
# USA   1993    1983

# wiid uses just the 2011 methodology while oecd combines the 2011 and 2012 methodologies
      

#----------------------------------------------------------------------------
# Read real GDP data from the World Bank
# https://data.worldbank.org/indicator/NY.GDP.MKTP.KD
#----------------------------------------------------------------------------
gdp_raw <- read.csv('./worldbank_gdp/API_NY.GDP.MKTP.KD_DS2_en_csv_v2.csv')

# Merge gdp data with World Bank gini data and compute annualized gdp growth between 
# the first and last year of gini data
wb_gdp <- gdp_raw[, c(2, 5:63)] # keep just country code and gdp data
wb_gdp <- merge(wb_gini, wb_gdp, by='CountryCode')
wb_gdp <- adply(wb_gdp, 1, function(row) {
  first_gdp <- as.numeric(row[grep(as.character(row$first), colnames(row))]) # get gdp in first year
  last_gdp <- as.numeric(row[grep(as.character(row$last), colnames(row))]) # get gdp in last year
  gdp_growth <- (last_gdp/first_gdp)^(1/(row$last-row$first))-1 # get annualized growth rate
  output <- data.frame(first_gdp=first_gdp, last_gdp=last_gdp, gdp_growth=gdp_growth)
  return(output)
})

wb <- wb_gdp[, -grep('X', colnames(wb_gdp))] # delete annual gdp observations
wb <- wb[complete.cases(wb),]

# Merge gdp data with OECD gini data and compute annualized gdp growth between 
# the first and last year of gini data
oecd_gdp <- gdp_raw[, c(2, 5:63)] # keep just country code and gdp data
oecd_gdp <- merge(oecd_gini, oecd_gdp, by='CountryCode')
oecd_gdp <- adply(oecd_gdp, 1, function(row) {
  first_gdp <- as.numeric(row[grep(as.character(row$first), colnames(row))]) # get gdp in first year
  last_gdp <- as.numeric(row[grep(as.character(row$last), colnames(row))]) # get gdp in last year
  gdp_growth <- (last_gdp/first_gdp)^(1/(row$last-row$first))-1 # get annualized growth rate
  output <- data.frame(first_gdp=first_gdp, last_gdp=last_gdp, gdp_growth=gdp_growth)
  return(output)
})

oecd <- oecd_gdp[, -grep('X', colnames(oecd_gdp))] # delete annual gdp observations
oecd <- oecd[complete.cases(oecd),]

# Make dummy for whether country is in the group of 16 countries in Carroll's email
g16 <- c('DNK', 'CZE', 'NOR', 'FIN', 'SWE', 'HUN', 'DEU', 'LUX', 'CAN', 'AUS', 'ITA', 'NZL', 'JPN', 
         'GBR', 'ISR', 'USA')
oecd$g16 <- ifelse(oecd$CountryCode %in% g16, 1, 0)


# Merge gdp data with WIID gini data and compute annualized gdp growth between 
# the first and last year of gini data
wiid_gdp <- gdp_raw[, c(2, 5:63)] # keep just country code and gdp data
wiid_gdp <- merge(wiid_gini, wiid_gdp, by='CountryCode')
wiid_gdp <- adply(wiid_gdp, 1, function(row) {
  first_gdp <- as.numeric(row[grep(as.character(row$first), colnames(row))]) # get gdp in first year
  last_gdp <- as.numeric(row[grep(as.character(row$last), colnames(row))]) # get gdp in last year
  gdp_growth <- (last_gdp/first_gdp)^(1/(row$last-row$first))-1 # get annualized growth rate
  output <- data.frame(first_gdp=first_gdp, last_gdp=last_gdp, gdp_growth=gdp_growth)
  return(output)
})

wiid <- wiid_gdp[, -grep('X', colnames(wiid_gdp))] # delete annual gdp observations
wiid <- wiid[complete.cases(wiid),]

# Make dummy for whether country is in the group of 16 countries in Carroll's email
g16 <- c('DNK', 'CZE', 'NOR', 'FIN', 'SWE', 'HUN', 'DEU', 'LUX', 'CAN', 'AUS', 'ITA', 'NZL', 'JPN', 
         'GBR', 'ISR', 'USA')
wiid$g16 <- ifelse(wiid$CountryCode %in% g16, 1, 0)

#----------------------------------------------------------------------------
# Read real GDP growth data from the World Bank
# https://data.worldbank.org/indicator/NY.GDP.MKTP.KD.ZG
#----------------------------------------------------------------------------




# Make figures
ggplot(wb, aes(x=gdp_growth, y=d_gini, shape=IncomeGroup, color=IncomeGroup)) +
  geom_point() +
  geom_label_repel(aes(label=ifelse(IncomeGroup=='High income', as.character(CountryCode), ''))) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)

ggplot(oecd, aes(x=gdp_growth, y=d_gini)) +
  geom_point() +
  geom_label_repel(aes(label=CountryCode)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)

ggplot(oecd[which(oecd$g16==1),], aes(x=gdp_growth, y=d_gini)) +
  geom_point() +
  geom_label_repel(aes(label=CountryCode)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)

ggplot(wiid, aes(x=gdp_growth, y=d_gini)) +
  geom_point() +
  geom_label_repel(aes(label=CountryCode)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)

ggplot(wiid[which(wiid$g16==1),], aes(x=gdp_growth, y=d_gini)) +
  geom_point() +
  geom_label_repel(aes(label=CountryCode)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)

            
