# =============================================================================================== #
# Make figures comparing data and model
#
# Saves output in ../../output/Figures_data_model/ and ../../tex/
# =============================================================================================== #

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

# Write function that extracts legend
# https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# Import combined wealth inequality data
wealth <- read.csv("../CountryWealth/WealthData_combined.csv")

# Import model simulation results
model_high_growth <- read.csv("../VaryGrowth/HighGrowth/Dist.csv")
model_no_growth <- read.csv("../VaryGrowth/NoGrowth/Dist.csv")
model_high_growth$T_age <- as.factor(model_high_growth$T_age)
model_no_growth$T_age <- as.factor(model_no_growth$T_age)

# ------------------------------------------------------------------------------------ #
# Make figure for high estimation growth (1.015)
# ------------------------------------------------------------------------------------ #

panel_a <- ggplot() +
  geom_line(data = model_high_growth %>% filter(T_age==160 & annual_growth<=1.05),
            aes(x = annual_growth-1, y = mean_to_median, color = T_age)) +
  geom_line(data = model_high_growth %>% filter(T_age==200 & annual_growth<=1.05),
            aes(x = annual_growth-1, y = mean_to_median, color = T_age)) +
  geom_line(data = model_high_growth %>% filter(T_age==400 & annual_growth<=1.05),
            aes(x = annual_growth-1, y = mean_to_median, color = T_age)) +
  geom_point(data = wealth %>% filter(source=="OECD WDD" & iso!="CN"),
             aes(x = growth_last20-1, y = mean_to_median, color = source)) +
  # geom_smooth(data = wealth %>% filter(source=="OECD WDD" & iso!="CN"),
  #             aes(x = growth_last20-1, y = mean_to_median, color = source),
  #             method=lm, se=FALSE, fullrange=TRUE, size = 0.5) +
  geom_vline(xintercept = 0.015, linetype = "dashed", color = "blue") +
  scale_color_brewer(palette = "RdGy",
                     labels = c("64 years","74 years","124 years","OECD WDD")) +
  labs(x = "Growth rate", y = "Inequality", title = "(a) Mean to median ratio",
       color = "Terminal age:") +
  guides(color=guide_legend(override.aes=list(shape=c(NA,NA,NA,16),linetype=c(1,1,1,0)))) +
  theme_light() + 
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.title = element_text(size = 9),
        legend.position = "bottom")

panel_b <- ggplot() +
  geom_line(data = model_high_growth %>% filter(T_age==160 & annual_growth<=1.05),
            aes(x = annual_growth-1, y = top1_to_median, color = T_age)) +
  geom_line(data = model_high_growth %>% filter(T_age==200 & annual_growth<=1.05),
            aes(x = annual_growth-1, y = top1_to_median, color = T_age)) +
  geom_line(data = model_high_growth %>% filter(T_age==400 & annual_growth<=1.05),
            aes(x = annual_growth-1, y = top1_to_median, color = T_age)) +
  geom_point(data = wealth %>% filter(source=="OECD WDD" & iso!="CN"),
             aes(x = growth_last20-1, y = top1_to_median, color = source)) +
  # geom_smooth(data = wealth %>% filter(source=="OECD WDD" & iso!="CN"),
  #             aes(x = growth_last20-1, y = top1_to_median, color = source),
  #             method=lm, se=FALSE, fullrange=TRUE, size = 0.5) +
  geom_vline(xintercept = 0.015, linetype = "dashed", color = "blue") +
  scale_color_brewer(palette = "RdGy") +
  labs(x = "Growth rate", y = "Inequality", title = "(b) Top 1% relative to median") +
  theme_light() + 
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.title = element_text(size = 9))

panel_c <- ggplot() +
  geom_line(data = model_high_growth %>% filter(T_age==160 & annual_growth<=1.05),
            aes(x = annual_growth-1, y = top5_to_median, color = T_age)) +
  geom_line(data = model_high_growth %>% filter(T_age==200 & annual_growth<=1.05),
            aes(x = annual_growth-1, y = top5_to_median, color = T_age)) +
  geom_line(data = model_high_growth %>% filter(T_age==400 & annual_growth<=1.05),
            aes(x = annual_growth-1, y = top5_to_median, color = T_age)) +
  geom_point(data = wealth %>% filter(source=="OECD WDD" & iso!="CN"),
             aes(x = growth_last20-1, y = top5_to_median, color = source)) +
  # geom_smooth(data = wealth %>% filter(source=="OECD WDD" & iso!="CN"),
  #             aes(x = growth_last20-1, y = top5_to_median, color = source),
  #             method=lm, se=FALSE, fullrange=TRUE, size = 0.5) +
  geom_vline(xintercept = 0.015, linetype = "dashed", color = "blue") +
  scale_color_brewer(palette = "RdGy") +
  labs(x = "Growth rate", y = "Inequality", title = "(c) Top 5% relative to median") +
  theme_light() + 
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.title = element_text(size = 9))

panel_d <- ggplot() +
  geom_line(data = model_high_growth %>% filter(T_age==160 & annual_growth<=1.05),
            aes(x = annual_growth-1, y = top10_to_median, color = T_age)) +
  geom_line(data = model_high_growth %>% filter(T_age==200 & annual_growth<=1.05),
            aes(x = annual_growth-1, y = top10_to_median, color = T_age)) +
  geom_line(data = model_high_growth %>% filter(T_age==400 & annual_growth<=1.05),
            aes(x = annual_growth-1, y = top10_to_median, color = T_age)) +
  geom_point(data = wealth %>% filter(source=="OECD WDD" & iso!="CN"),
             aes(x = growth_last20-1, y = top10_to_median, color = source)) +
  # geom_smooth(data = wealth %>% filter(source=="OECD WDD" & iso!="CN"),
  #             aes(x = growth_last20-1, y = top10_to_median, color = source),
  #             method=lm, se=FALSE, fullrange=TRUE, size = 0.5) +
  geom_vline(xintercept = 0.015, linetype = "dashed", color = "blue") +
  scale_color_brewer(palette = "RdGy") +
  labs(x = "Growth rate", y = "Inequality", title = "(d) Top 10% relative to median") +
  theme_light() + 
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.title = element_text(size = 9))

model_vs_data_high_growth <- grid.arrange(arrangeGrob(panel_a + theme(legend.position="none", plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm")),
                                            panel_b + theme(legend.position="none", plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm")),
                                            panel_c + theme(legend.position="none", plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm")),
                                            panel_d + theme(legend.position="none", plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm")),
                                            nrow = 2),
                                          g_legend(panel_a), nrow = 2, heights = c(10,1))
ggsave("model_vs_data_high_growth.pdf", plot = model_vs_data_high_growth, 
       device = "pdf", width = 8, height = 8)
ggsave("../../tex/model_vs_data_high_growth.pdf", plot = model_vs_data_high_growth, 
       device = "pdf", width = 8, height = 8)
  

# ------------------------------------------------------------------------------------ #
# Make figure for no estimation growth (1.0)
# ------------------------------------------------------------------------------------ #

panel_a <- ggplot() +
  geom_line(data = model_no_growth %>% filter(T_age==160 & annual_growth<=1.05),
            aes(x = annual_growth-1, y = mean_to_median, color = T_age)) +
  geom_line(data = model_no_growth %>% filter(T_age==200 & annual_growth<=1.05),
            aes(x = annual_growth-1, y = mean_to_median, color = T_age)) +
  geom_line(data = model_no_growth %>% filter(T_age==400 & annual_growth<=1.05),
            aes(x = annual_growth-1, y = mean_to_median, color = T_age)) +
  geom_point(data = wealth %>% filter(source=="OECD WDD" & iso!="CN"),
             aes(x = growth_last20-1, y = mean_to_median, color = source)) +
  # geom_smooth(data = wealth %>% filter(source=="OECD WDD" & iso!="CN"),
  #             aes(x = growth_last20-1, y = mean_to_median, color = source),
  #             method=lm, se=FALSE, fullrange=TRUE, size = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
  scale_color_brewer(palette = "RdGy",
                     labels = c("64 years","74 years","124 years","OECD WDD")) +
  labs(x = "Growth rate", y = "Inequality", title = "(a) Mean to median ratio",
       color = "Terminal age:") +
  guides(color=guide_legend(override.aes=list(shape=c(NA,NA,NA,16),linetype=c(1,1,1,0)))) +
  theme_light() + 
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.title = element_text(size = 9),
        legend.position = "bottom")

panel_b <- ggplot() +
  geom_line(data = model_no_growth %>% filter(T_age==160 & annual_growth<=1.05),
            aes(x = annual_growth-1, y = top1_to_median, color = T_age)) +
  geom_line(data = model_no_growth %>% filter(T_age==200 & annual_growth<=1.05),
            aes(x = annual_growth-1, y = top1_to_median, color = T_age)) +
  geom_line(data = model_no_growth %>% filter(T_age==400 & annual_growth<=1.05),
            aes(x = annual_growth-1, y = top1_to_median, color = T_age)) +
  geom_point(data = wealth %>% filter(source=="OECD WDD" & iso!="CN"),
             aes(x = growth_last20-1, y = top1_to_median, color = source)) +
  # geom_smooth(data = wealth %>% filter(source=="OECD WDD" & iso!="CN"),
  #             aes(x = growth_last20-1, y = top1_to_median, color = source),
  #             method=lm, se=FALSE, fullrange=TRUE, size = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
  scale_color_brewer(palette = "RdGy") +
  labs(x = "Growth rate", y = "Inequality", title = "(b) Top 1% relative to median") +
  theme_light() + 
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.title = element_text(size = 9))

panel_c <- ggplot() +
  geom_line(data = model_no_growth %>% filter(T_age==160 & annual_growth<=1.05),
            aes(x = annual_growth-1, y = top5_to_median, color = T_age)) +
  geom_line(data = model_no_growth %>% filter(T_age==200 & annual_growth<=1.05),
            aes(x = annual_growth-1, y = top5_to_median, color = T_age)) +
  geom_line(data = model_no_growth %>% filter(T_age==400 & annual_growth<=1.05),
            aes(x = annual_growth-1, y = top5_to_median, color = T_age)) +
  geom_point(data = wealth %>% filter(source=="OECD WDD" & iso!="CN"),
             aes(x = growth_last20-1, y = top5_to_median, color = source)) +
  # geom_smooth(data = wealth %>% filter(source=="OECD WDD" & iso!="CN"),
  #             aes(x = growth_last20-1, y = top5_to_median, color = source),
  #             method=lm, se=FALSE, fullrange=TRUE, size = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
  scale_color_brewer(palette = "RdGy") +
  labs(x = "Growth rate", y = "Inequality", title = "(c) Top 5% relative to median") +
  theme_light() + 
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.title = element_text(size = 9))

panel_d <- ggplot() +
  geom_line(data = model_no_growth %>% filter(T_age==160 & annual_growth<=1.05),
            aes(x = annual_growth-1, y = top10_to_median, color = T_age)) +
  geom_line(data = model_no_growth %>% filter(T_age==200 & annual_growth<=1.05),
            aes(x = annual_growth-1, y = top10_to_median, color = T_age)) +
  geom_line(data = model_no_growth %>% filter(T_age==400 & annual_growth<=1.05),
            aes(x = annual_growth-1, y = top10_to_median, color = T_age)) +
  geom_point(data = wealth %>% filter(source=="OECD WDD" & iso!="CN"),
             aes(x = growth_last20-1, y = top10_to_median, color = source)) +
  # geom_smooth(data = wealth %>% filter(source=="OECD WDD" & iso!="CN"),
  #             aes(x = growth_last20-1, y = top10_to_median, color = source),
  #             method=lm, se=FALSE, fullrange=TRUE, size = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
  scale_color_brewer(palette = "RdGy") +
  labs(x = "Growth rate", y = "Inequality", title = "(d) Top 10% relative to median") +
  theme_light() + 
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.title = element_text(size = 9))

model_vs_data_no_growth <- grid.arrange(arrangeGrob(panel_a + theme(legend.position="none", plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm")),
                                                    panel_b + theme(legend.position="none", plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm")),
                                                    panel_c + theme(legend.position="none", plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm")),
                                                    panel_d + theme(legend.position="none", plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm")),
                                                    nrow = 2),
                                          g_legend(panel_a), nrow = 2, heights = c(10,1))
ggsave("model_vs_data_no_growth.pdf", plot = model_vs_data_no_growth, 
       device = "pdf", width = 8, height = 8)
ggsave("../../tex/model_vs_data_no_growth.pdf", plot = model_vs_data_no_growth, 
       device = "pdf", width = 8, height = 8)

