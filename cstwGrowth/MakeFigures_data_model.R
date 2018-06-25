#
# Make figures comparing the model and the data. Model matches the 2014 US wealth distribution
#

library(plyr)
library(tidyverse)
library(magrittr)
library(haven)
library(readxl)
library(scales)
library(ggplot2)
library(ggrepel)
library(reshape2)
library(gridExtra)


setwd("/Users/andreea/Documents/phd/2ndyrpaper/output/Figures_data_model")

# Import combined wealth inequality data
wealth <- read.csv("../CountryWealth/WealthData_combined.csv")

# Import model simulation results
model_high_growth <- read.csv("../VaryGrowth/HighGrowth/low_T_age_actual_KY/Dist.csv")
model_no_growth <- read.csv("../VaryGrowth/NoGrowth/low_T_age_actual_KY/Dist.csv")

model_high_growth$source <- "Model high growth"
model_no_growth$source <- "Model no growth"

# Mean to median ratio
ggplot(wealth %>% filter(source=="OECD WDD" & iso!="CN"),
       aes(x = growth_last20-1, y = mean_to_median, color = source)) +
  geom_point(size = 1) +
  geom_text_repel(aes(label=iso), show.legend = FALSE, size = 2, segment.size = 0.2,
                  segment.alpha = 0.5, point.padding = 0.1) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, size = 0.5)  +
  geom_line(data = model_high_growth %>% filter(annual_growth<=1.05), 
            aes(x = annual_growth-1, y = mean_to_median, color = source)) +
  geom_line(data = model_no_growth %>% filter(annual_growth<=1.05), 
            aes(x = annual_growth-1, y = mean_to_median, color = source)) +
  labs(x = "Growth rate", y = "Mean to median ratio", color = "Source") +
  theme_light()

# Gini coefficient
ggplot(wealth %>% filter(source=="OECD/Eurostat ICW" & iso!="CN"),
       aes(x = growth_last20-1, y = gini, color = source)) +
  geom_point(size = 1) +
  geom_text_repel(aes(label=iso), show.legend = FALSE, size = 2, segment.size = 0.2,
                  segment.alpha = 0.5, point.padding = 0.1) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, size = 0.5)  +
  geom_line(data = model_high_growth %>% filter(annual_growth<=1.05), 
            aes(x = annual_growth-1, y = gini, color = source)) +
  geom_line(data = model_no_growth %>% filter(annual_growth<=1.05), 
            aes(x = annual_growth-1, y = gini, color = source)) +
  labs(x = "Growth rate", y = "Gini coefficient", color = "Source") +
  theme_light()

# Deviations from median
ggplot(wealth %>% filter(source=="OECD WDD" & iso!="CN"),
       aes(x = growth_last20-1, y = top1_to_median, color = source)) +
  geom_point(size = 1) +
  geom_text_repel(aes(label=iso), show.legend = FALSE, size = 2, segment.size = 0.2,
                  segment.alpha = 0.5, point.padding = 0.1) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, size = 0.5)  +
  geom_line(data = model_high_growth %>% filter(annual_growth<=1.05), 
            aes(x = annual_growth-1, y = top1_to_median, color = source)) +
  geom_line(data = model_no_growth %>% filter(annual_growth<=1.05), 
            aes(x = annual_growth-1, y = top1_to_median, color = source)) +
  labs(x = "Growth rate", color = "Source") +
  theme_light()
ggplot(wealth %>% filter(source=="OECD WDD" & iso!="CN"),
       aes(x = growth_last20-1, y = top5_to_median, color = source)) +
  geom_point(size = 1) +
  geom_text_repel(aes(label=iso), show.legend = FALSE, size = 2, segment.size = 0.2,
                  segment.alpha = 0.5, point.padding = 0.1) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, size = 0.5)  +
  geom_line(data = model_high_growth %>% filter(annual_growth<=1.05), 
            aes(x = annual_growth-1, y = top5_to_median, color = source)) +
  geom_line(data = model_no_growth %>% filter(annual_growth<=1.05), 
            aes(x = annual_growth-1, y = top5_to_median, color = source)) +
  labs(x = "Growth rate", color = "Source") +
  theme_light()
ggplot(wealth %>% filter(source=="OECD WDD" & iso!="CN"),
       aes(x = growth_last20-1, y = top10_to_median, color = source)) +
  geom_point(size = 1) +
  geom_text_repel(aes(label=iso), show.legend = FALSE, size = 2, segment.size = 0.2,
                  segment.alpha = 0.5, point.padding = 0.1) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, size = 0.5)  +
  geom_line(data = model_high_growth %>% filter(annual_growth<=1.05), 
            aes(x = annual_growth-1, y = top10_to_median, color = source)) +
  geom_line(data = model_no_growth %>% filter(annual_growth<=1.05), 
            aes(x = annual_growth-1, y = top10_to_median, color = source)) +
  labs(x = "Growth rate", color = "Source") +
  theme_light()
ggplot(wealth %>% filter(source=="OECD WDD" & iso!="CN"),
       aes(x = growth_last20-1, y = bot40_to_median, color = source)) +
  geom_point(size = 1) +
  geom_text_repel(aes(label=iso), show.legend = FALSE, size = 2, segment.size = 0.2,
                  segment.alpha = 0.5, point.padding = 0.1) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, size = 0.5)  +
  geom_line(data = model_high_growth %>% filter(annual_growth<=1.05), 
            aes(x = annual_growth-1, y = bot40_to_median, color = source)) +
  geom_line(data = model_no_growth %>% filter(annual_growth<=1.05), 
            aes(x = annual_growth-1, y = bot40_to_median, color = source)) +
  labs(x = "Growth rate", color = "Source") +
  theme_light()
  




