# =============================================================================================== #
# Make figures comparing wealth inequality for the case when agents update their consumption rule 
# as growth changes to the case when they don't. The difference between these two cases comes from 
# the erosion of the wealth-to-income ratio due to the change in income growth.
#
# Saves output in ../../output/Figures_no_update/ and ../../tex/
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


setwd("/Users/andreea/Documents/phd/2ndyrpaper/output/Figures_no_update")

# Write function that extracts legend
# https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# Import model simulation results
high_growth <- read.csv("../NoUpdate/HighGrowth/low_T_age_actual_KY/Dist.csv")
no_growth <- read.csv("../NoUpdate/NoGrowth/low_T_age_actual_KY/Dist.csv")

high_growth <- melt(high_growth, id.vars = "annual_growth")
no_growth <- melt(no_growth, id.vars = "annual_growth")

#----------------------------------------#
# Plot mean to median ratio
#----------------------------------------#

panel_a <- ggplot(high_growth %>% filter(variable=="Lvl_mean_to_median" |
                                           variable=="Lvl_mean_to_median_no_update"),
                  aes(x = annual_growth-1, y = value, color = variable)) +
  geom_line() +
  geom_point() +
  scale_color_discrete(breaks = c("Lvl_mean_to_median", "Lvl_mean_to_median_no_update"),
                       labels = c("update behavior with growth",
                                  "don't update behavior with growth")) +
  labs(x = "Growth rate", y = "Mean to median ratio", title = "(a) Wealth levels", color = "") +
  theme_light() + 
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.title = element_text(size = 9),
        legend.position = "bottom")

panel_b <- ggplot(high_growth %>% filter(variable=="Nrm_mean_to_median" |
                                           variable=="Nrm_mean_to_median_no_update"),
                  aes(x = annual_growth-1, y = value, color = variable)) +
  geom_line() +
  geom_point() +
  labs(x = "Growth rate", y = "Mean to median ratio", title = "(b) Wealth to income ratios") +
  theme_light() + 
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.title = element_text(size = 9))

m2m_no_update <- grid.arrange(arrangeGrob(panel_a + theme(legend.position="none", plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm")),
                                          panel_b + theme(legend.position="none", plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm")),
                                          nrow = 1),
                              g_legend(panel_a), nrow = 2, heights = c(10,1))
ggsave("m2m_no_update.pdf", plot = m2m_no_update, device = "pdf", width = 8, height = 4)
ggsave("../../tex/m2m_no_update.pdf", plot = m2m_no_update, device = "pdf", width = 8, height = 4)


#----------------------------------------#
# Plot Gini coeffcient
#----------------------------------------#

panel_a <- ggplot(high_growth %>% filter(variable=="Lvl_gini" |
                                           variable=="Lvl_gini_no_update"),
                  aes(x = annual_growth-1, y = value, color = variable)) +
  geom_line() +
  geom_point() +
  scale_color_discrete(breaks = c("Lvl_gini", "Lvl_gini_no_update"),
                       labels = c("update behavior with growth",
                                  "don't update behavior with growth")) +
  labs(x = "Growth rate", y = "Gini coefficient", title = "(a) Wealth levels", color = "") +
  theme_light() + 
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.title = element_text(size = 9),
        legend.position = "bottom")

panel_b <- ggplot(high_growth %>% filter(variable=="Nrm_gini" |
                                           variable=="Nrm_gini_no_update"),
                  aes(x = annual_growth-1, y = value, color = variable)) +
  geom_line() +
  geom_point() +
  scale_color_discrete(breaks = c("Nrm_gini", "Nrm_gini_no_update"),
                       labels = c("update behavior with growth",
                                  "don't update behavior with growth")) +
  labs(x = "Growth rate", y = "Gini coefficient", title = "(b) Wealth to income ratios", color = "") +
  theme_light() + 
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.title = element_text(size = 9))

gini_no_update <- grid.arrange(arrangeGrob(panel_a + theme(legend.position="none", plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm")),
                                          panel_b + theme(legend.position="none", plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm")),
                                          nrow = 1),
                              g_legend(panel_a), nrow = 2, heights = c(10,1))
ggsave("gini_no_update.pdf", plot = gini_no_update, device = "pdf", width = 8, height = 4)
ggsave("../../tex/gini_no_update.pdf", plot = gini_no_update, device = "pdf", width = 8, height = 4)
