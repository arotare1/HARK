#
# Make figures describing the data:
#   - plot inequality measures against growth over past 25 years
#   - plot evolution of inequality measures over time
#

library(plyr)
library(tidyverse)
library(magrittr)
library(haven)
library(readxl)
library(scales)
library(ggplot2)
library(ggrepel)


setwd("/Users/andreea/Documents/phd/2ndyrpaper/output/Figures")

# Import combined wealth inequality data
wealth <- read.csv("../countryWealth/wealthData_combined.csv")

pdf("x.pdf")
ggplot(wealth %>% filter(source!="WID.world"),# iso!="US", iso!="NL"),
       aes(x=gdp_growth, y=mean_to_median, color=source)) +
  geom_point() +
  geom_label_repel(aes(label=iso)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
dev.off()

ggplot(wealth %>% filter(source!="WID.world"),# iso!="US", iso!="NL"), 
       aes(x=gdp_growth, y=top5_to_median, color=source)) +
  geom_point() +
  geom_label_repel(aes(label=iso)) +
  geom_smooth(method=lm, se=TRUE, fullrange=TRUE)

ggplot(wealth %>% filter(source!="WID.world"), aes(x=gdp_growth, y=bot20_to_median, color=source)) +
  geom_point() +
  geom_label_repel(aes(label=iso)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)

ggplot(wealth %>% filter(source!="WID.world"), aes(x=gdp_growth, y=top5_share, color=source)) +
  geom_point() +
  geom_label_repel(aes(label=iso)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)

ggplot(wealth %>% filter(source!="WID.world"), aes(x=gdp_growth, y=top20_to_bot20, color=source)) +
  geom_point() +
  geom_label_repel(aes(label=iso)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)

ggplot(wealth, aes(x=gdp_growth, y=gini, color=source)) +
  geom_point() +
  geom_label_repel(aes(label=iso)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)


ggplot(mtcars, aes(x=wt, y=mpg, color=cyl, shape=cyl)) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)


cs_wealth <- read.csv('/Users/andreea/Documents/phd/2ndyrpaper/HARK/cstwGrowth/Empirical/credit_suisse_wealth/tabula-databook_table_3_1.csv', header=FALSE)
cs_wealth <- cs_wealth[,c(1,10)]
colnames(cs_wealth) <- c('country', 'gini') 
cs_wealth <- left_join(country_iso, cs_wealth, by="country")
cs_wealth <- left_join(cs_wealth, wealth %>% filter(source=="OECD WDD") %>% select(iso, year, gdp_growth),
                       by="iso")

ggplot(cs_wealth, aes(x=gdp_growth, y=gini)) +
  geom_point() +
  geom_label_repel(aes(label=iso)) +
  geom_smooth(method=lm, se=TRUE, fullrange=TRUE)


