library(foreign)
library(plyr)
library(ggplot2)
library(ggrepel)

setwd('/Users/andreea/Documents/phd/2ndyrpaper/HARK/cstwGrowth/Empirical')

#----------------------------------------------------------------------------
# Import inequality results from the model
# Specification: BetaDistPYnw
#----------------------------------------------------------------------------
model <- read.csv('../ResultsAllGrowthFactors/BetaPointPYnw.csv')
model <- model[, -1]

#----------------------------------------------------------------------------
# Import real GDP data from the World Bank
# https://data.worldbank.org/indicator/NY.GDP.MKTP.KD
#----------------------------------------------------------------------------
gdp_raw <- read.csv('./worldbank_gdp/API_NY.GDP.MKTP.KD_DS2_en_csv_v2.csv')

#----------------------------------------------------------------------------
# Import income group of each country from the World Bank
# https://data.worldbank.org/indicator/NY.GDP.MKTP.KD
#----------------------------------------------------------------------------
inc_group <- read.csv('./worldbank_gdp/Metadata_Country_API_NY.GDP.MKTP.KD_DS2_en_csv_v2.csv')
inc_group <- inc_group[, colnames(inc_group) %in% c('CountryCode', 'IncomeGroup', 'TableName')]

#-----------------------------------------------------------------
# Import OECD Wealth distribution database
# http://stats.oecd.org/viewhtml.aspx?datasetcode=WEALTH&lang=en#
#-----------------------------------------------------------------
oecd_wealth_raw <- read.csv('./oecd_wealth/WEALTH_22042018010501539.csv')

# Keep just mean to median net wealth and wealth shares of top 1%,5%,10%
oecd_wealth <- oecd_wealth_raw[sort(c(grep('Mean to median', oecd_wealth_raw$Variable),
                                      grep('Share of top', oecd_wealth_raw$Variable),
                                      grep('Share of bottom', oecd_wealth_raw$Variable))), ]
# Keep just latest year
oecd_wealth <- ddply(oecd_wealth, .(COUNTRY, VAR), summarise,
                     Country = Country[1],
                     Variable = Variable[1],
                     Year = max(TIME),
                     Value = Value[which(TIME==max(TIME))])

# Merge OECD inequality data with GDP data
oecd_wealth <- merge(oecd_wealth, gdp_raw, by.x='COUNTRY', by.y='CountryCode') 

# For each country, compute the GDP growth over the 30 years prior to the latest inequality data
oecd_wealth <- adply(oecd_wealth, 1, function(row) {
  gdp_last <- as.numeric(row[grep(as.character(row$Year), colnames(row))]) # get GDP in last year
  gdp_last30 <- as.numeric(row[grep(as.character(row$Year - 30), colnames(row))]) # get GDP from 30 yrs before
  gdp_growth <- (gdp_last/gdp_last30)^(1/30)-1 # get annualized growth rate
  output <- data.frame(GDP_last=gdp_last, GDP_last30=gdp_last30, GDP_growth=gdp_growth)
  return(output)
})

oecd_wealth <- oecd_wealth[, -grep('X', colnames(oecd_wealth))] # delete annual GDP observations
oecd_wealth <- oecd_wealth[, !(colnames(oecd_wealth) %in% c('CountryName', 
                                                            'IndicatorName', 
                                                            'IndicatorCode'))] # delete useless info
oecd_wealth <- oecd_wealth[complete.cases(oecd_wealth), ]

# Make dummy for whether country is in the group of 16 countries in Carroll's email
g16 <- c('DNK', 'CZE', 'NOR', 'FIN', 'SWE', 'HUN', 'DEU', 'LUX', 'CAN', 'AUS', 'ITA', 'NZL', 'JPN', 
         'GBR', 'ISR', 'USA')
oecd_wealth$g16 <- ifelse(oecd_wealth$COUNTRY %in% g16, 1, 0)

#--------------------------------------------------------------------------
# Import wealth Gini data from Credit Suisse Global Wealth Databook 2017
# http://publications.credit-suisse.com/tasks/render/file/index.cfm?fileid=FB790DB0-C175-0E07-787A2B8639253D5A
#--------------------------------------------------------------------------
cs_wealth <- read.csv('./credit_suisse_wealth/tabula-databook_table_3_1.csv', header=FALSE)

# Keep just column with Gini wealth coefficient
cs_wealth <- cs_wealth[,c(1,10)]
colnames(cs_wealth) <- c('Country', 'Gini')

# Merge with income group data and only keep high-income countries
cs_wealth <- merge(cs_wealth, inc_group, by.x='Country', by.y='TableName')
cs_wealth <- cs_wealth[cs_wealth$IncomeGroup=='High income', ]

# Keep just countries which according to the Credit Suisse report have direct (not imputed)
# wealth distribution data
cs_wealth <- cs_wealth[cs_wealth$CountryCode %in% c('AUS', 'AUT', 'BEL', 'CAN', 'CHL', 'CHN',
                                                    'CYP', 'FIN', 'FRA', 'DEU', 'GRC', 'IND',
                                                    'IDN', 'ITA', 'JPN', 'KOR', 'LUX', 'MLT',
                                                    'NLD', 'NZL', 'NOR', 'PRT', 'SVK', 'SVN',
                                                    'ESP', 'SWE', 'THA', 'GBR', 'USA', 'URY',
                                                    'IRL'), ]
# Merge with GDP data
cs_wealth <- merge(cs_wealth, gdp_raw, by='CountryCode')

# Compute GDP growth between 2014 (HFCS field work used in the was done in 2014) and 1984
cs_wealth$GDP_14 <- cs_wealth[, grep('2014', colnames(cs_wealth))]
cs_wealth$GDP_84 <- cs_wealth[, grep('1984', colnames(cs_wealth))]
cs_wealth$GDP_84_14 <- (cs_wealth$GDP_14/cs_wealth$GDP_84)^(1/30)-1

# Get rid of useless data
cs_wealth <- cs_wealth[, -grep('X', colnames(cs_wealth))] # delete annual GDP observations
cs_wealth <- cs_wealth[, !(colnames(cs_wealth) %in% c('CountryName', 
                                                            'IndicatorName', 
                                                            'IndicatorCode'))] # delete useless info
cs_wealth <- cs_wealth[complete.cases(cs_wealth), ]
cs_wealth$g16 <- ifelse(cs_wealth$CountryCode %in% g16, 1, 0)

#-------------------------------
# Plot OECD data
#-------------------------------

# Plot mean-to-median ratio of net wealth by GDP growth rate
ggplot(oecd_wealth[oecd_wealth$VAR=='M2MR',], aes(x=GDP_growth, y=Value)) +
  geom_point() +
  geom_label_repel(aes(label=COUNTRY)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE) +
  theme_minimal() +
  labs(x='Annual GDP growth rate', y='Mean-to-median ratio of net wealth', 
       title='Mean-to-median ratio of net wealth by GDP growth rate over 30 years')

# Plot mean-to-median ratio of net wealth by GDP growth rate for a subset of countries
ggplot(oecd_wealth[oecd_wealth$VAR=='M2MR' & oecd_wealth$g16==1,], aes(x=GDP_growth, y=Value)) +
  geom_point() +
  geom_label_repel(aes(label=COUNTRY)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE) + 
  labs(x='Annual GDP growth rate', y='Mean-to-median ratio of net wealth', 
       title='Mean-to-median ratio of net wealth by GDP growth rate over 30 years')

# Plot wealth share of top 1% by GDP growth rate
ggplot(oecd_wealth[oecd_wealth$VAR=='ST1',], aes(x=GDP_growth, y=Value/100)) +
  geom_point() +
  geom_label_repel(aes(label=COUNTRY)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE) + 
  labs(x='Annual GDP growth rate', y='Wealth share', 
       title='Wealth share of the top 1% by GDP growth rate over 30 years')

# Plot wealth share of top 1% by GDP growth rate for a subset of countries
ggplot(oecd_wealth[oecd_wealth$VAR=='ST1' & oecd_wealth$g16==1,], aes(x=GDP_growth, y=Value/100)) +
  geom_point() +
  geom_label_repel(aes(label=COUNTRY)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE) + 
  labs(x='Annual GDP growth rate', y='Wealth share', 
       title='Wealth share of the top 1% by GDP growth rate over 30 years')

# Plot wealth share of top 5% by GDP growth rate
ggplot(oecd_wealth[oecd_wealth$VAR=='ST5',], aes(x=GDP_growth, y=Value/100)) +
  geom_point() +
  geom_label_repel(aes(label=COUNTRY)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE) + 
  labs(x='Annual GDP growth rate', y='Wealth share', 
       title='Wealth share of the top 5% by GDP growth rate over 30 years')

# Plot wealth share of top 5% by GDP growth rate for a subset of countries
ggplot(oecd_wealth[oecd_wealth$VAR=='ST5' & oecd_wealth$g16==1,], aes(x=GDP_growth, y=Value/100)) +
  geom_point() +
  geom_label_repel(aes(label=COUNTRY)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE) + 
  labs(x='Annual GDP growth rate', y='Wealth share', 
       title='Wealth share of the top 5% by GDP growth rate over 30 years')

# Plot wealth share of top 10% by GDP growth rate
ggplot(oecd_wealth[oecd_wealth$VAR=='ST10',], aes(x=GDP_growth, y=Value/100)) +
  geom_point() +
  geom_label_repel(aes(label=COUNTRY)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE) + 
  labs(x='Annual GDP growth rate', y='Wealth share', 
       title='Wealth share of the top 10% by GDP growth rate over 30 years')

# Plot wealth share of top 10% by GDP growth rate for a subset of countries
ggplot(oecd_wealth[oecd_wealth$VAR=='ST10' & oecd_wealth$g16==1,], aes(x=GDP_growth, y=Value/100)) +
  geom_point() +
  geom_label_repel(aes(label=COUNTRY)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE) + 
  labs(x='Annual GDP growth rate', y='Wealth share', 
       title='Wealth share of the top 10% by GDP growth rate over 30 years')


#-------------------------------
# Plot Credit Suisse data
#-------------------------------

# Plot mean-to-median ratio of net wealth by GDP growth rate
ggplot(cs_wealth, aes(x=GDP_84_14, y=Gini/100)) +
  geom_point() +
  geom_text_repel(aes(label=CountryCode)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE) +
  theme_minimal() +
  labs(x='Annual GDP growth rate', y='Gini coefficient of net wealth', 
       title='Gini coefficient of net wealth by GDP growth rate over 30 years') +

# Plot mean-to-median ratio of net wealth by GDP growth rate for a subset of countries
ggplot(cs_wealth[cs_wealth$g16==1,], aes(x=GDP_84_14, y=Gini/100)) +
  geom_point() +
  geom_label_repel(aes(label=CountryCode)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE) +
  theme_minimal() + 
  labs(x='Annual GDP growth rate', y='Gini coefficient of net wealth', 
       title='Gini coefficient of net wealth by GDP growth rate over 30 years')


