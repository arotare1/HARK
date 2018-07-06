# =============================================================================================== #
# Make figures of avg discount factor and age among the high income individuals
#
# Saves output in ../../output/Figures_model/ and ../../tex/
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


setwd("/Users/andreea/Documents/phd/2ndyrpaper/output/Figures_model")

# Write function that extracts legend
# https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# Import model simulation results
model_high_growth <- read.csv("../VaryGrowth/HighGrowth/Dist.csv")
model_no_growth <- read.csv("../VaryGrowth/NoGrowth/Dist.csv")
model_high_growth$T_age <- as.factor(model_high_growth$T_age)
model_no_growth$T_age <- as.factor(model_no_growth$T_age)

model_high_growth %<>% group_by(T_age) %>% filter(T_age!=160 & annual_growth<=1.07)
model_no_growth %<>% group_by(T_age) %>% filter(T_age!=160 & annual_growth<=1.07)


panel_a <- ggplot(data = model_no_growth %>% group_by(T_age),
                  aes(x = annual_growth-1, y = DiscFac_top1_inc, color = T_age)) +
  geom_line() +
  geom_point() +
  labs(x = "Growth rate", y = "Average discount factor",
       title = "(a) No initial growth", color = "Terminal age:") +
  theme_light()  +
  scale_color_hue(labels = c("74 years","124 years")) +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.title = element_text(size = 9),
        legend.position = "bottom")

panel_b <- ggplot(data = model_high_growth %>% group_by(T_age),
                  aes(x = annual_growth-1, y = DiscFac_top1_inc, color = T_age)) +
  geom_line() +
  geom_point() +
  labs(x = "Growth rate", y = "Average discount factor",
       title = "(b) Positive initial growth") +
  theme_light()  +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.title = element_text(size = 9))

discfac_top1_inc <- grid.arrange(arrangeGrob(panel_a + theme(legend.position="none", plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm")),
                                                panel_b + theme(legend.position="none", plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm")),
                                                nrow=1),
                                    g_legend(panel_a), nrow = 2, heights = c(10,1))
ggsave("discfac_top1_inc.pdf", plot = discfac_top1_inc, device = "pdf", width = 8, height = 4)
ggsave("../../tex/discfac_top1_inc.pdf", plot = discfac_top1_inc, device = "pdf", width = 8, height = 4)

panel_a <- ggplot(data = model_no_growth %>% group_by(T_age),
                  aes(x = annual_growth-1, y = Age_top1_inc/4+24, color = T_age)) +
  geom_line() +
  geom_point() +
  labs(x = "Growth rate", y = "Average age (years)",
       title = "(a) No initial growth", color = "Terminal age:") +
  theme_light()  +
  scale_color_hue(labels = c("74 years","124 years")) +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.title = element_text(size = 9),
        legend.position = "bottom")

panel_b <- ggplot(data = model_high_growth %>% group_by(T_age),
                  aes(x = annual_growth-1, y = Age_top1_inc/4+24, color = T_age)) +
  geom_line() +
  geom_point() +
  labs(x = "Growth rate", y = "Average age (years)",
       title = "(b) Positive initial growth") +
  theme_light()  +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.title = element_text(size = 9))

age_top1_inc <- grid.arrange(arrangeGrob(panel_a + theme(legend.position="none", plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm")),
                                            panel_b + theme(legend.position="none", plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm")),
                                            nrow = 1),
                                g_legend(panel_a), nrow = 2, heights = c(10,1))
ggsave("age_top1_inc.pdf", plot = age_top1_inc, device = "pdf", width = 8, height = 4)
ggsave("../../tex/age_top1_inc.pdf", plot = age_top1_inc, device = "pdf", width = 8, height = 4)



panel_a <- ggplot(data = model_no_growth %>% group_by(T_age),
                  aes(x = annual_growth-1, y = Age_top5_inc/4+24, color = T_age)) +
  geom_line() +
  geom_point() +
  labs(x = "Growth rate", y = "Average age (years)",
       title = "(a) No initial growth", color = "Terminal age:") +
  theme_light()  +
  scale_color_hue(labels = c("74 years","124 years")) +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.title = element_text(size = 9),
        legend.position = "bottom")

panel_b <- ggplot(data = model_high_growth %>% group_by(T_age),
                  aes(x = annual_growth-1, y = Age_top5_inc/4+24, color = T_age)) +
  geom_line() +
  geom_point() +
  labs(x = "Growth rate", y = "Average age (years)",
       title = "(b) Positive initial growth") +
  theme_light()  +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.title = element_text(size = 9))

age_top5_inc <- grid.arrange(arrangeGrob(panel_a + theme(legend.position="none", plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm")),
                                            panel_b + theme(legend.position="none", plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm")),
                                            nrow = 1),
                                g_legend(panel_a), nrow = 2, heights = c(10,1))
ggsave("age_top5_inc.pdf", plot = age_top5_inc, device = "pdf", width = 8, height = 4)
ggsave("../../tex/age_top5_inc.pdf", plot = age_top5_inc, device = "pdf", width = 8, height = 4)


panel_a <- ggplot(data = model_no_growth %>% group_by(T_age),
                  aes(x = annual_growth-1, y = DiscFac_top5_inc, color = T_age)) +
  geom_line() +
  geom_point() +
  labs(x = "Growth rate", y = "Average discount factor",
       title = "(a) No initial growth", color = "Terminal age:") +
  theme_light()  +
  scale_color_hue(labels = c("74 years","124 years")) +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.title = element_text(size = 9),
        legend.position = "bottom")

panel_b <- ggplot(data = model_high_growth %>% group_by(T_age),
                  aes(x = annual_growth-1, y = DiscFac_top5_inc, color = T_age)) +
  geom_line() +
  geom_point() +
  labs(x = "Growth rate", y = "Average discount factor",
       title = "(b) Positive initial growth") +
  theme_light()  +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.title = element_text(size = 9))

discfac_top5_inc <- grid.arrange(arrangeGrob(panel_a + theme(legend.position="none", plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm")),
                                                panel_b + theme(legend.position="none", plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm")),
                                                nrow=1),
                                    g_legend(panel_a), nrow = 2, heights = c(10,1))
ggsave("discfac_top5_inc.pdf", plot = discfac_top5_inc, device = "pdf", width = 8, height = 4)
ggsave("../../tex/discfac_top5_inc.pdf", plot = discfac_top5_inc, device = "pdf", width = 8, height = 4)



panel_a <- ggplot(data = model_no_growth %>% group_by(T_age),
                  aes(x = annual_growth-1, y = Age_top10_inc/4+24, color = T_age)) +
  geom_line() +
  geom_point() +
  labs(x = "Growth rate", y = "Average age (years)",
       title = "(a) No initial growth", color = "Terminal age:") +
  theme_light()  +
  scale_color_hue(labels = c("74 years","124 years")) +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.title = element_text(size = 9),
        legend.position = "bottom")

panel_b <- ggplot(data = model_high_growth %>% group_by(T_age),
                  aes(x = annual_growth-1, y = Age_top10_inc/4+24, color = T_age)) +
  geom_line() +
  geom_point() +
  labs(x = "Growth rate", y = "Average age (years)",
       title = "(b) Positive initial growth") +
  theme_light()  +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.title = element_text(size = 9))

age_top10_inc <- grid.arrange(arrangeGrob(panel_a + theme(legend.position="none", plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm")),
                                             panel_b + theme(legend.position="none", plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm")),
                                             nrow = 1),
                                 g_legend(panel_a), nrow = 2, heights = c(10,1))
ggsave("age_top10_inc.pdf", plot = age_top10_inc, device = "pdf", width = 8, height = 4)
ggsave("../../tex/age_top10_inc.pdf", plot = age_top10_inc, device = "pdf", width = 8, height = 4)


panel_a <- ggplot(data = model_no_growth %>% group_by(T_age),
                  aes(x = annual_growth-1, y = DiscFac_top10_inc, color = T_age)) +
  geom_line() +
  geom_point() +
  labs(x = "Growth rate", y = "Average discount factor",
       title = "(a) No initial growth", color = "Terminal age:") +
  theme_light()  +
  scale_color_hue(labels = c("74 years","124 years")) +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.title = element_text(size = 9),
        legend.position = "bottom")

panel_b <- ggplot(data = model_high_growth %>% group_by(T_age),
                  aes(x = annual_growth-1, y = DiscFac_top10_inc, color = T_age)) +
  geom_line() +
  geom_point() +
  labs(x = "Growth rate", y = "Average discount factor",
       title = "(b) Positive initial growth") +
  theme_light()  +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.title = element_text(size = 9))

discfac_top10_inc <- grid.arrange(arrangeGrob(panel_a + theme(legend.position="none", plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm")),
                                                 panel_b + theme(legend.position="none", plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm")),
                                                 nrow=1),
                                     g_legend(panel_a), nrow = 2, heights = c(10,1))
ggsave("discfac_top10_inc.pdf", plot = discfac_top10_inc, device = "pdf", width = 8, height = 4)
ggsave("../../tex/discfac_top10_inc.pdf", plot = discfac_top10_inc, device = "pdf", width = 8, height = 4)


