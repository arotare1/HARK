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
library(reshape2)


setwd("/Users/andreea/Documents/phd/2ndyrpaper/output/Figures")

# Import combined wealth inequality data
wealth <- read.csv("../countryWealth/wealthData_combined.csv")


fig1_last20 <- melt(wealth %>% filter(source!="WID.world" & source!="OECD In it Together" &
                                        (quality=="Good" | quality=="Satisfactory") & iso!="CN"),
                    id.vars = c("iso", "year", "growth_last20", "source"),
                    measure.vars = c("top1_to_median", "top5_to_median", "top10_to_median",
                                     "bot40_to_median", "mean_to_median", "gini"))
labels <- c(top1_to_median = "Wealth of the top 1% relative to median",
            top5_to_median = "Wealth of the top 5% relative to median",
            top10_to_median = "Wealth of the top 10% relative to median",
            bot40_to_median = "Wealth of the bottom 40% relative to median",
            mean_to_median = "Mean to median ratio",
            gini = "Gini coefficient")
ggplot(fig1_last20, aes(x = growth_last20, y = value, color = source)) +
  geom_point(size = 1) +
  geom_text_repel(aes(label=iso), show.legend = FALSE, size = 2, segment.size = 0.2,
                  segment.alpha = 0.5, point.padding = 0.15) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, show.legend = FALSE, size = 0.5)  +
  labs(color = "Data source") +
  facet_wrap(~ variable, scales = "free", labeller=labeller(variable = labels)) +
  xlab("Average growth over the last 20 years") +
  ylab("Inequality") +
  theme_light() +
  theme(strip.background = element_rect(fill="white"),
        strip.text = element_text(colour = 'black'))
ggsave("fig1_last20.pdf", device = "pdf", width = 12, height = 8, units = "in")


fig1_last25 <- melt(wealth %>% filter(source!="WID.world" & source!="OECD In it Together" &
                                   (quality=="Good" | quality=="Satisfactory") & iso!="CN"),
               id.vars = c("iso", "year", "growth_last25", "source"),
               measure.vars = c("gini", "top1_share", "top5_share", "top10_share"))
labels <- c(gini = "Gini coefficient",
            top1_share = "Wealth share of the top 1%",
            top5_share = "Wealth share of the top 5%",
            top10_share = "Wealth share of the top 10%")
ggplot(fig1_last25, aes(x = growth_last25, y = value, color = source)) +
  geom_point(size = 1) +
  geom_text_repel(aes(label=iso), show.legend = FALSE, size = 2, segment.size = 0.2,
                  segment.alpha = 0.5, point.padding = 0.15) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, show.legend = FALSE, size = 0.5)  +
  labs(color = "Data source") +
  facet_wrap(~ variable, scales = "free", labeller=labeller(variable = labels)) +
  xlab("Average growth over the last 25 years") +
  ylab("Inequality") +
  theme_light() +
  theme(strip.background = element_rect(fill="white"),
        strip.text = element_text(colour = 'black'))
ggsave("fig1_last25.pdf", device = "pdf", width = 12, height = 8, units = "in")


fig2_last25 <- melt(wealth %>% filter(source!="WID.world" & source!="OECD In it Together" &
                                        (quality=="Good" | quality=="Satisfactory") & iso!="CN"),
                    id.vars = c("iso", "year", "growth_last25", "source"),
                    measure.vars = c("mean_to_median", "bot90_to_median",
                                     "bot95_to_median", "bot99_to_median"))
labels <- c(mean_to_median = "Mean to median ratio",
            bot90_to_median = "Wealth of the bottom 90% relative to median",
            bot95_to_median = "Wealth of the bottom 95% relative to median",
            bot99_to_median = "Wealth of the bottom 99% relative to median")
ggplot(fig2_last25, aes(x = growth_last25, y = value, color = source)) +
  geom_point(size = 1) +
  geom_text_repel(aes(label=iso), show.legend = FALSE, size = 2, segment.size = 0.2,
                  segment.alpha = 0.5, point.padding = 0.15) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, show.legend = FALSE, size = 0.5)  +
  labs(color = "Data source") +
  facet_wrap(~ variable, scales = "free", labeller=labeller(variable = labels)) +
  xlab("Average growth over the last 25 years") +
  ylab("Inequality") +
  theme_light() +
  theme(strip.background = element_rect(fill="white"),
        strip.text = element_text(colour = 'black'))
ggsave("fig2_last25.pdf", device = "pdf", width = 11.5, height = 8, units = "in")


fig1_last30 <- melt(wealth %>% filter(source!="WID.world" & source!="OECD In it Together" &
                                        (quality=="Good" | quality=="Satisfactory") & iso!="CN"),
                    id.vars = c("iso", "year", "growth_last30", "source"),
                    measure.vars = c("gini", "top1_share", "top5_share", "top10_share"))
labels <- c(gini = "Gini coefficient",
            top1_share = "Wealth share of the top 1%",
            top5_share = "Wealth share of the top 5%",
            top10_share = "Wealth share of the top 10%")
ggplot(fig1_last30, aes(x = growth_last30, y = value, color = source)) +
  geom_point(size = 1) +
  geom_text_repel(aes(label=iso), show.legend = FALSE, size = 2, segment.size = 0.2,
                  segment.alpha = 0.5, point.padding = 0.15) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, show.legend = FALSE, size = 0.5)  +
  labs(color = "Data source") +
  facet_wrap(~ variable, scales = "free", labeller=labeller(variable = labels)) +
  xlab("Average growth over the last 30 years") +
  ylab("Inequality") +
  theme_light() +
  theme(strip.background = element_rect(fill="white"),
        strip.text = element_text(colour = 'black'))
ggsave("fig1_last30.pdf", device = "pdf", width = 12, height = 8, units = "in")


fig2_last30 <- melt(wealth %>% filter(source!="WID.world" & source!="OECD In it Together" &
                                        (quality=="Good" | quality=="Satisfactory") & 
                                        iso!="CN"),
                    id.vars = c("iso", "year", "growth_last30", "source"),
                    measure.vars = c("mean_to_median", "bot90_to_median",
                                     "bot95_to_median", "bot99_to_median"))
labels <- c(mean_to_median = "Mean to median ratio",
            bot90_to_median = "Wealth of the bottom 90% relative to median",
            bot95_to_median = "Wealth of the bottom 95% relative to median",
            bot99_to_median = "Wealth of the bottom 99% relative to median")
ggplot(fig2_last30, aes(x = growth_last30, y = value, color = source)) +
  geom_point(size = 1) +
  geom_text_repel(aes(label=iso), show.legend = FALSE, size = 2, segment.size = 0.2,
                  segment.alpha = 0.5, point.padding = 0.15) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, show.legend = FALSE, size = 0.5)  +
  labs(color = "Data source") +
  facet_wrap(~ variable, scales = "free", labeller=labeller(variable = labels)) +
  xlab("Average growth over the last 30 years") +
  ylab("Inequality") +
  theme_light() +
  theme(strip.background = element_rect(fill="white"),
        strip.text = element_text(color = 'black'))
ggsave("fig2_last30.pdf", device = "pdf", width = 11.5, height = 8, units = "in")


fig1_last40 <- melt(wealth %>% filter(source!="WID.world" & source!="OECD In it Together" &
                                        (quality=="Good" | quality=="Satisfactory") & iso!="CN"),
                    id.vars = c("iso", "year", "growth_last40", "source"),
                    measure.vars = c("gini", "top1_share", "top5_share", "top10_share"))
labels <- c(gini = "Gini coefficient",
            top1_share = "Wealth share of the top 1%",
            top5_share = "Wealth share of the top 5%",
            top10_share = "Wealth share of the top 10%")
ggplot(fig1_last40, aes(x = growth_last40, y = value, color = source)) +
  geom_point(size = 1) +
  geom_text_repel(aes(label=iso), show.legend = FALSE, size = 2, segment.size = 0.2,
                  segment.alpha = 0.5, point.padding = 0.15) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, show.legend = FALSE, size = 0.5)  +
  labs(color = "Data source") +
  facet_wrap(~ variable, scales = "free", labeller=labeller(variable = labels)) +
  xlab("Average growth over the last 40 years") +
  ylab("Inequality") +
  theme_light() +
  theme(strip.background = element_rect(fill="white"),
        strip.text = element_text(colour = 'black'))
ggsave("fig1_last40.pdf", device = "pdf", width = 12, height = 8, units = "in")


fig2_last40 <- melt(wealth %>% filter(source!="WID.world" & source!="OECD In it Together" &
                                        (quality=="Good" | quality=="Satisfactory") & 
                                        iso!="CN"),
                    id.vars = c("iso", "year", "growth_last40", "source"),
                    measure.vars = c("mean_to_median", "bot90_to_median",
                                     "bot95_to_median", "bot99_to_median"))
labels <- c(mean_to_median = "Mean to median ratio",
            bot90_to_median = "Wealth of the bottom 90% relative to median",
            bot95_to_median = "Wealth of the bottom 95% relative to median",
            bot99_to_median = "Wealth of the bottom 99% relative to median")
ggplot(fig2_last40, aes(x = growth_last40, y = value, color = source)) +
  geom_point(size = 1) +
  geom_text_repel(aes(label=iso), show.legend = FALSE, size = 2, segment.size = 0.2,
                  segment.alpha = 0.5, point.padding = 0.15) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, show.legend = FALSE, size = 0.5)  +
  labs(color = "Data source") +
  facet_wrap(~ variable, scales = "free", labeller=labeller(variable = labels)) +
  xlab("Average growth over the last 40 years") +
  ylab("Inequality") +
  theme_light() +
  theme(strip.background = element_rect(fill="white"),
        strip.text = element_text(color = 'black'))
ggsave("fig2_last40.pdf", device = "pdf", width = 12, height = 8, units = "in")
