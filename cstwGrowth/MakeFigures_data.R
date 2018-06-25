# =============================================================================================== #
# Make figures describing the data:
#   - plot inequality measures against growth over past 20-40 years
#
# Saves output in ../../output/Figures_data/ and ../../tex/
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


setwd("/Users/andreea/Documents/phd/2ndyrpaper/output/Figures_data")

# Write function that extracts legend
# https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# Import combined Inequality data
wealth <- read.csv("../CountryWealth/WealthData_combined.csv")

# ------------------------------------------------------------------------------------ #
# Make figure with mean to median ratio (panel_a) and gini coefficient (panel_b)
# ------------------------------------------------------------------------------------ #
panel_a <- ggplot(wealth %>% filter(source!="WID.world" & source!="OECD In it Together" &
                                          (quality=="Good" | quality=="Satisfactory") & iso!="CN"),
                      aes(x = growth_last20-1, y = mean_to_median, color = source)) +
  geom_point(size = 1) +
  geom_text_repel(aes(label=iso), show.legend = FALSE, size = 2, segment.size = 0.2,
                  segment.alpha = 0.5, point.padding = 0.15) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, size = 0.5)  +
  labs(x = "Growth rate", y = "Inequality", title = "(a) Mean to median ratio", color = "") +
  theme_light()  + 
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.title = element_text(size = 9),
        legend.position = "bottom")

panel_b <- ggplot(wealth %>% filter(source!="WID.world" & source!="OECD In it Together" &
                                      (quality=="Good" | quality=="Satisfactory") & iso!="CN"),
                  aes(x = growth_last20-1, y = gini, color = source)) +
  geom_point(size = 1) +
  geom_text_repel(aes(label=iso), show.legend = FALSE, size = 2, segment.size = 0.2,
                  segment.alpha = 0.5, point.padding = 0.15) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, size = 0.5)  +
  labs(x = "Growth rate", y = "Inequality", title = "(b) Gini coefficient") +
  theme_light() + 
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.title = element_text(size = 9))

m2m_gini <- grid.arrange(arrangeGrob(panel_a + theme(legend.position="none", plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm")),
                                     panel_b + theme(legend.position="none", plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm")),
                                     nrow = 1),
                         g_legend(panel_a), nrow = 2, heights = c(10,1))
ggsave("m2m_gini.pdf", plot = m2m_gini, device = "pdf", width = 8, height = 4)
ggsave("../../tex/m2m_gini.pdf", plot = m2m_gini, device = "pdf", width = 8, height = 4)


# ------------------------------------------------------------------------------------ #
# Make figure with average wealth relative to median (panel_a through panel_d)
# ------------------------------------------------------------------------------------ #
panel_a <- ggplot(wealth %>% filter((source=="OECD WDD" | source=="Credit Suisse") &
                                      (quality=="Good" | quality=="Satisfactory") & iso!="CN"),
                  aes(x = growth_last20-1, y = top1_to_median, color = source)) +
  geom_point(size = 1) +
  geom_text_repel(aes(label=iso), show.legend = FALSE, size = 2, segment.size = 0.2,
                  segment.alpha = 0.5, point.padding = 0.15) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, size = 0.5)  +
  labs(x = "Growth rate", y = "Inequality", title = "(a) Top 1% relative to median", color = "") +
  theme_light()  + 
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        legend.position = "bottom")

panel_b <- ggplot(wealth %>% filter((source=="OECD WDD" | source=="Credit Suisse") &
                                      (quality=="Good" | quality=="Satisfactory") & iso!="CN"),
                  aes(x = growth_last20-1, y = top5_to_median, color = source)) +
  geom_point(size = 1) +
  geom_text_repel(aes(label=iso), show.legend = FALSE, size = 2, segment.size = 0.2,
                  segment.alpha = 0.5, point.padding = 0.15) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, size = 0.5)  +
  labs(x = "Growth rate", y = "Inequality", title = "(b) Top 5% relative to median") +
  theme_light() + 
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9))

panel_c <- ggplot(wealth %>% filter((source=="OECD WDD" | source=="Credit Suisse") &
                                      (quality=="Good" | quality=="Satisfactory") & iso!="CN"),
                  aes(x = growth_last20-1, y = top10_to_median, color = source)) +
  geom_point(size = 1) +
  geom_text_repel(aes(label=iso), show.legend = FALSE, size = 2, segment.size = 0.2,
                  segment.alpha = 0.5, point.padding = 0.15) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, size = 0.5)  +
  labs(x = "Growth rate", y = "Inequality", title = "(c) Top 5% relative to median") +
  theme_light() + 
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9))

panel_d <- ggplot(wealth %>% filter((source=="OECD WDD" | source=="Credit Suisse") &
                                      (quality=="Good" | quality=="Satisfactory") & iso!="CN"),
                  aes(x = growth_last20-1, y = bot40_to_median, color = source)) +
  geom_point(size = 1) +
  geom_text_repel(aes(label=iso), show.legend = FALSE, size = 2, segment.size = 0.2,
                  segment.alpha = 0.5, point.padding = 0.15) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, size = 0.5)  +
  labs(x = "Growth rate", y = "Inequality", title = "(d) Bottom 40% relative to median") +
  theme_light() + 
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9))

shares_to_median <- grid.arrange(arrangeGrob(panel_a + theme(legend.position="none", plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm")),
                                             panel_b + theme(legend.position="none", plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm")),
                                             panel_c + theme(legend.position="none", plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm")),
                                             panel_d + theme(legend.position="none", plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm")),
                                             nrow = 2),
                                 g_legend(panel_a), nrow = 2, heights=c(10,1))
ggsave("shares_to_median.pdf", plot = shares_to_median, device = "pdf", width = 8, height = 8)
ggsave("../../tex/shares_to_median.pdf", plot = shares_to_median, device = "pdf", width = 8, height = 8)

