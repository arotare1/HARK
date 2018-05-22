library(foreign)
library(plyr)
library(ggplot2)
library(ggrepel)


setwd('/Users/andreea/Documents/phd/2ndyrpaper/HARK/cstwGrowth/Empirical')

#----------------------------------------------------------------------------
# Import inequality results from the model
# Specification: BetaDistPYnw
#----------------------------------------------------------------------------
model_agg <- read.csv('../Results/DistPYagg.csv')
model_agg <- model_agg[, -1]

model_perm <- read.csv('../Results/DistPYperm.csv')
model_perm <- model_perm[, -1]

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

# Make dataset with  country code and growth
oecd_ctrygrowth <- ddply(oecd_wealth, .(COUNTRY), summarise, GDP_growth = GDP_growth[1])

# Compute ratio of bottom 90% to bottom 60%
oecd_bot60 <- oecd_wealth[oecd_wealth$VAR=='SB60', colnames(oecd_wealth) %in% c('COUNTRY', 'Value')]
colnames(oecd_bot60)[2] <- 'SB60'

oecd_bot90 <- oecd_wealth[oecd_wealth$VAR=='ST10', colnames(oecd_wealth) %in% c('COUNTRY', 'Value')]
oecd_bot90$Value <- 100-oecd_bot90$Value
colnames(oecd_bot90)[2] <- 'SB90'

oecd_bot90_60 <- merge(oecd_bot90, oecd_bot60, by='COUNTRY')
oecd_bot90_60$SB90_60 <- oecd_bot90_60$SB90 / oecd_bot90_60$SB60
oecd_bot90_60 <- merge(oecd_bot90_60, oecd_ctrygrowth, by='COUNTRY')

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


#---------------------------------------------------------------
# Import WID data on wealth distributions from 1984-2014
# Countries: China, France, USA
#---------------------------------------------------------------

## Get wealth share of top percentiles
# The variable we want to keep is 'Net personal wealth | equal-split adults | Share | Adults | share of total (ratio)'
# The code of this variable is 'shweal992j'
wid_cn <- read.csv('./wid_wealth/WID_CN_InequalityData.csv', header=FALSE)
wid_cn <- wid_cn[9:NROW(wid_cn), c(3, 4, grep('shweal992j', llply(wid_cn[8,], as.character)))]
colnames(wid_cn) <- c('year', 'percentile', 'wealth_share')
wid_cn$wealth_share <- llply(wid_cn$wealth_share, as.character)
wid_cn$wealth_share <- llply(wid_cn$wealth_share, as.numeric)
wid_cn <- wid_cn[wid_cn$year %in% c(2014, 1984), ]

wid_fr <- read.csv('./wid_wealth/WID_FR_InequalityData.csv', header=FALSE)
wid_fr <- wid_fr[9:NROW(wid_fr), c(3, 4, grep('shweal992j', llply(wid_fr[8,], as.character)))]
colnames(wid_fr) <- c('year', 'percentile', 'wealth_share')
wid_fr$wealth_share <- llply(wid_fr$wealth_share, as.character)
wid_fr$wealth_share <- llply(wid_fr$wealth_share, as.numeric)
wid_fr <- wid_fr[wid_fr$year %in% c(2014, 1984), ]

wid_us <- read.csv('./wid_wealth/WID_US_InequalityData.csv', header=FALSE)
wid_us <- wid_us[9:NROW(wid_us), c(3, 4, grep('shweal992j', llply(wid_us[8,], as.character)))]
colnames(wid_us) <- c('year', 'percentile', 'wealth_share')
wid_us$wealth_share <- llply(wid_us$wealth_share, as.character)
wid_us$wealth_share <- llply(wid_us$wealth_share, as.numeric)
wid_us <- wid_us[wid_us$year %in% c(2014, 1984), ]



#-------------------------------
# Plot OECD data
#-------------------------------

# Plot ratio of wealth shares of bottom 90% to bottom 60% by GDP growth rate
ggplot(oecd_bot90_60, aes(x=GDP_growth, y=SB90_60)) +
  geom_point() +
  geom_label_repel(aes(label=COUNTRY)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE) +
  theme_minimal() +
  labs(x='Annual GDP growth rate', y='Bottom 90% / Bottom 60%', 
       title='Ratio of wealth shares of bottom 90% to bottom 60%')

# Plot mean-to-median ratio of net wealth by GDP growth rate
pdf('m2m_data.pdf')
ggplot(oecd_wealth[oecd_wealth$VAR=='M2MR',], aes(x=GDP_growth, y=Value)) +
  geom_point() +
  geom_label_repel(aes(label=COUNTRY)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE) +
  theme_minimal() +
  labs(x='Annual GDP growth rate', y='Mean-to-median ratio of net worth', 
       title='Mean-to-median ratio of net worth')
dev.off()

tmp <- oecd_wealth[oecd_wealth$VAR=='M2MR',]
pdf('m2m_data_model.pdf')
plot(tmp$GDP_growth, tmp$Value, pch=16,
     xlab='Annual GDP growth rate',
     ylab='Mean-to-median ratio of net worth',
     main='Mean-to-median ratio of net worth')
abline(lm(tmp$Value~tmp$GDP_growth), col='black', lwd=2)
lines(model_agg$Growth-1, model_agg$M2M_Lvl, type='o', pch=16, col='red', lwd=2)
legend('topright', legend=c('data', 'model'), col=c('black', 'red'), pch=16, ncol=2, cex=0.7)
dev.off()

# Plot mean-to-median ratio of net wealth by GDP growth rate for a subset of countries
ggplot(oecd_wealth[oecd_wealth$VAR=='M2MR' & oecd_wealth$g16==1,], aes(x=GDP_growth, y=Value)) +
  geom_point() +
  geom_label_repel(aes(label=COUNTRY)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE) + 
  labs(x='Annual GDP growth rate', y='Mean-to-median ratio of net wealth', 
       title='Mean-to-median ratio of net wealth by GDP growth rate over 30 years')

# Plot wealth share of top 1% by GDP growth rate
pdf('top1_data.pdf')
ggplot(oecd_wealth[oecd_wealth$VAR=='ST1',], aes(x=GDP_growth, y=Value/100)) +
  geom_point() +
  geom_label_repel(aes(label=COUNTRY)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE) + 
  theme_minimal() +
  labs(x='Annual GDP growth rate', y='Wealth share', 
       title='Wealth share of the top 1%')
dev.off()

tmp <- oecd_wealth[oecd_wealth$VAR=='ST1',]
pdf('top1_data_model.pdf')
plot(tmp$GDP_growth, tmp$Value/100, pch=16,
     xlab='Annual GDP growth rate',
     ylab='Wealth share',
     main='Wealth share of the top 1%')
abline(lm(tmp$Value/100~tmp$GDP_growth), col='black', lwd=2)
lines(model_agg$Growth-1, model_agg$Top1_Lvl, type='o', pch=16, col='red', lwd=2)
legend('topright', legend=c('data', 'model'), col=c('black', 'red'), pch=16, ncol=2, cex=0.7)
dev.off()

# Plot wealth share of top 1% by GDP growth rate for a subset of countries
ggplot(oecd_wealth[oecd_wealth$VAR=='ST1' & oecd_wealth$g16==1,], aes(x=GDP_growth, y=Value/100)) +
  geom_point() +
  geom_label_repel(aes(label=COUNTRY)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE) + 
  labs(x='Annual GDP growth rate', y='Wealth share', 
       title='Wealth share of the top 1% by GDP growth rate over 30 years')

# Plot wealth share of top 5% by GDP growth rate
pdf('top5_data.pdf')
ggplot(oecd_wealth[oecd_wealth$VAR=='ST5',], aes(x=GDP_growth, y=Value/100)) +
  geom_point() +
  geom_label_repel(aes(label=COUNTRY)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE) + 
  theme_minimal() +
  labs(x='Annual GDP growth rate', y='Wealth share', 
       title='Wealth share of the top 5%')
dev.off()

tmp <- oecd_wealth[oecd_wealth$VAR=='ST5',]
pdf('top5_data_model.pdf')
plot(tmp$GDP_growth, tmp$Value/100, pch=16,
     xlab='Annual GDP growth rate',
     ylab='Wealth share',
     main='Wealth share of the top 5%')
abline(lm(tmp$Value/100~tmp$GDP_growth), col='black', lwd=2)
lines(model_agg$Growth-1, model_agg$Top5_Lvl, type='o', pch=16, col='red', lwd=2)
legend('topright', legend=c('data', 'model'), col=c('black', 'red'), pch=16, ncol=2, cex=0.7)
dev.off()

# Plot wealth share of top 5% by GDP growth rate for a subset of countries
ggplot(oecd_wealth[oecd_wealth$VAR=='ST5' & oecd_wealth$g16==1,], aes(x=GDP_growth, y=Value/100)) +
  geom_point() +
  geom_label_repel(aes(label=COUNTRY)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE) + 
  theme_minimal() +
  labs(x='Annual GDP growth rate', y='Wealth share', 
       title='Wealth share of the top 5% by GDP growth rate over 30 years')

# Plot wealth share of top 10% by GDP growth rate
pdf('top10_data.pdf')
ggplot(oecd_wealth[oecd_wealth$VAR=='ST10',], aes(x=GDP_growth, y=Value/100)) +
  geom_point() +
  geom_label_repel(aes(label=COUNTRY)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE) + 
  theme_minimal() +
  labs(x='Annual GDP growth rate', y='Wealth share', 
       title='Wealth share of the top 10%')
dev.off()

tmp <- oecd_wealth[oecd_wealth$VAR=='ST10',]
pdf('top10_data_model.pdf')
plot(tmp$GDP_growth, tmp$Value/100, pch=16,
     xlab='Annual GDP growth rate',
     ylab='Wealth share',
     main='Wealth share of the top 10%')
abline(lm(tmp$Value/100~tmp$GDP_growth), col='black', lwd=2)
lines(model_agg$Growth-1, model_agg$Top10_Lvl, type='o', pch=16, col='red', lwd=2)
legend('topright', legend=c('data', 'model'), col=c('black', 'red'), pch=16, ncol=2, cex=0.7)
dev.off()

# Plot wealth share of top 10% by GDP growth rate for a subset of countries
ggplot(oecd_wealth[oecd_wealth$VAR=='ST10' & oecd_wealth$g16==1,], aes(x=GDP_growth, y=Value/100)) +
  geom_point() +
  geom_label_repel(aes(label=COUNTRY)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE) + 
  theme_minimal() +
  labs(x='Annual GDP growth rate', y='Wealth share', 
       title='Wealth share of the top 10% by GDP growth rate over 30 years')

# Plot wealth share of bottom 60% by GDP growth rate
pdf('bot60_data.pdf')
ggplot(oecd_wealth[oecd_wealth$VAR=='SB60',], aes(x=GDP_growth, y=Value/100)) +
  geom_point() +
  geom_label_repel(aes(label=COUNTRY)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE) + 
  theme_minimal() +
  labs(x='Annual GDP growth rate', y='Wealth share', 
       title='Wealth share of the bottom 60%')
dev.off()

tmp <- oecd_wealth[oecd_wealth$VAR=='SB60',]
pdf('bot60_data_model.pdf')
plot(tmp$GDP_growth, tmp$Value/100, pch=16,
     xlab='Annual GDP growth rate',
     ylab='Wealth share',
     main='Wealth share of the bottom 60%')
abline(lm(tmp$Value/100~tmp$GDP_growth), col='black', lwd=2)
lines(model_agg$Growth-1, model_agg$Bottom60_Lvl, type='o', pch=16, col='red', lwd=2)
legend('topright', legend=c('data', 'model'), col=c('black', 'red'), pch=16, ncol=2, cex=0.7)
dev.off()



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
       title='Gini coefficient of net wealth by GDP growth rate over 30 years')

# Plot mean-to-median ratio of net wealth by GDP growth rate for a subset of countries
ggplot(cs_wealth[cs_wealth$g16==1,], aes(x=GDP_84_14, y=Gini/100)) +
  geom_point() +
  geom_label_repel(aes(label=CountryCode)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE) +
  theme_minimal() + 
  labs(x='Annual GDP growth rate', y='Gini coefficient of net wealth', 
       title='Gini coefficient of net wealth by GDP growth rate over 30 years')


#-----------------------------------------
# Plot WID.world gini data
#-----------------------------------------

# us_dgini84_14 <- 0.859044206138 - 0.783020385217
# fr_dgini84_14 <- 0.694574152428 - 0.642112571394
# cn_dgini84_14 <- 0.749428254053 - 0.535917715168
# ru_dgini95_14 <- 0.80940773114 - 0.666334643255
# dginis <- c(us_dgini84_14, fr_dgini84_14, cn_dgini84_14, ru_dgini95_14)
# 
# us_dgrowth <- (gdp_raw$X2014[250]/gdp_raw$X1984[250])^(1/30) - (gdp_raw$X1984[250]/gdp_raw$X1960[250])^(1/24)
# fr_dgrowth <- (gdp_raw$X2014[76]/gdp_raw$X1984[76])^(1/30) - (gdp_raw$X1984[76]/gdp_raw$X1960[76])^(1/24)
# 




