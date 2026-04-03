#Tank data
#Biodiversity facilitation experiment
#HIMB Spring 2026
#Maya Powell

#load libraries
library(ggplot2)
library(here)
library(ggpubr)
library(lubridate)
library(tidyverse)

#read in data

td <- read.csv(here("Data/Tank_Data/tank_data_biofac.csv"))

td <- td %>%
  filter(period == "experiment") %>%
  mutate(date_time = paste(date,time)) %>%
  mutate(date_time = mdy_hm(date_time)) %>%
  mutate(date = mdy(date)) %>%
  mutate(tank = as.factor(tank)) %>% 
  filter(date > ymd("2026-03-25")) %>%
  filter(bad == "no") #%>%
  #drop_na(temp) %>%
  #filter(day_night == "day")

#plot ALL data

temp_pre <- ggplot(td, aes(x = date_time, y = temp, color = treatment, group = tank)) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.5) +
  facet_wrap(day_night~., nrow = 2) +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red", "black"))+
  scale_x_datetime(date_labels = "%m-%d", date_breaks = "1 day")

pH_pre <- ggplot(td, aes(x = date_time, y = pH, color = treatment, group = tank)) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.5) +
  facet_wrap(day_night~., nrow = 2) +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red", "black"))+
  scale_x_datetime(date_labels = "%m-%d", date_breaks = "1 day")

mV_pre <- ggplot(td, aes(x = date_time, y = mV, color = treatment, group = tank)) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.5) +
  facet_wrap(day_night~., nrow = 2) +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red", "black"))+
  scale_x_datetime(date_labels = "%m-%d", date_breaks = "1 day")

DO_pre <- ggplot(td, aes(x = date_time, y = DO, color = treatment, group = tank)) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.5) +
  facet_wrap(day_night~., nrow = 2) +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red", "black"))+
  scale_x_datetime(date_labels = "%m-%d", date_breaks = "1 day")

sal_pre <- ggplot(td, aes(x = date_time, y = sal, color = treatment, group = tank)) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.5) +
  facet_wrap(day_night~., nrow = 2) +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red", "black"))+
  scale_x_datetime(date_labels = "%m-%d", date_breaks = "1 day")

cond_pre <- ggplot(td, aes(x = date_time, y = cond, color = treatment, group = tank)) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.5) +
  facet_wrap(day_night~., nrow = 2) +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red", "black"))+
  scale_x_datetime(date_labels = "%m-%d", date_breaks = "1 day")

tank_plots <- ggarrange(temp_pre, pH_pre, mV_pre, DO_pre, sal_pre, cond_pre, 
                        common.legend = T, nrow = 2, ncol = 3)
tank_plots

ggsave(here("Output/tank_plots.pdf"), tank_plots, h = 15, w = 15)

##Average plots across treatments

se <- function(x) sd(x)/sqrt(length(x))

meand <- td %>%
  mutate(timepoint = mdy_hm(timepoint)) %>%
  drop_na(temp) %>%
  group_by(timepoint, treatment, day_night) %>%
  dplyr::summarize(mean_temp = mean(temp),
                   se_temp = se(temp),
                   mean_pH = mean(pH),
                   se_pH = se(pH),
                   mean_mV = mean(mV),
                   se_mV = se(mV),
                   mean_DO = mean(DO),
                   se_DO = se(DO),
                   mean_sal = mean(sal),
                   se_sal = se(sal),
                   mean_cond = mean(cond),
                   se_cond = se(cond),
                   .groups = "drop")

temp_mean <- ggplot(meand, aes(x = timepoint, y = mean_temp, color = treatment)) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.5) +
  facet_wrap(day_night~., nrow = 2) +
  geom_errorbar(aes(ymin=mean_temp+se_temp, ymax= mean_temp-se_temp), alpha = 0.5)+
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red", "black"))+
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_datetime(date_labels = "%m-%d", date_breaks = "1 day")

pH_mean <- ggplot(meand, aes(x = timepoint, y = mean_pH, color = treatment)) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.5) +
  facet_wrap(day_night~., nrow = 2) +
  geom_errorbar(aes(ymin=mean_pH+se_pH, ymax= mean_pH-se_pH), alpha = 0.5)+
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red", "black"))+
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_datetime(date_labels = "%m-%d", date_breaks = "1 day")
ggsave(here("Output/pH_mean_treatment.pdf"), pH_mean, h = 10, w = 10)

mV_mean <- ggplot(meand, aes(x = timepoint, y = mean_mV, color = treatment)) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.5) +
  facet_wrap(day_night~., nrow = 2) +
  geom_errorbar(aes(ymin=mean_mV+se_mV, ymax= mean_mV-se_mV), alpha = 0.5)+
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red", "black"))+
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_datetime(date_labels = "%m-%d", date_breaks = "1 day")

DO_mean <- ggplot(meand, aes(x = timepoint, y = mean_DO, color = treatment)) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.5) +
  facet_wrap(day_night~., nrow = 2) +
  geom_errorbar(aes(ymin=mean_DO+se_DO, ymax= mean_DO-se_DO), alpha = 0.5)+
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red", "black"))+
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_datetime(date_labels = "%m-%d", date_breaks = "1 day")
ggsave(here("Output/DO_mean_treatment.pdf"), DO_mean, h = 10, w = 10)

sal_mean <- ggplot(meand, aes(x = timepoint, y = mean_sal, color = treatment)) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.5) +
  facet_wrap(day_night~., nrow = 2) +
  geom_errorbar(aes(ymin=mean_sal+se_sal, ymax= mean_sal-se_sal), alpha = 0.5)+
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red", "black"))+
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_datetime(date_labels = "%m-%d", date_breaks = "1 day")

cond_mean <- ggplot(meand, aes(x = timepoint, y = mean_cond, color = treatment)) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.5) +
  facet_wrap(day_night~., nrow = 2) +
  geom_errorbar(aes(ymin=mean_cond+se_cond, ymax= mean_cond-se_cond), alpha = 0.5)+
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red", "black"))+
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_datetime(date_labels = "%m-%d", date_breaks = "1 day")


mean_plots <- ggarrange(temp_mean, pH_mean, mV_mean, DO_mean, sal_mean, cond_mean, 
                       common.legend = T, nrow = 2, ncol = 3)
mean_plots

ggsave(here("Output/mean_treatment_plots.pdf"), mean_plots, h = 15, w = 15)

##Sum plots between day and night
#look at this also adjusted by tanks when smaller volume adjusted (24th??)

day_night_sum <- td %>%
  group_by(day_night, treatment) %>%
  dplyr::summarize(mean_temp = mean(temp),
                   se_temp = se(temp),
                   mean_pH = mean(pH),
                   se_pH = se(pH),
                   mean_mV = mean(mV),
                   se_mV = se(mV),
                   mean_DO = mean(DO),
                   se_DO = se(DO),
                   mean_sal = mean(sal),
                   se_sal = se(sal),
                   mean_cond = mean(cond),
                   se_cond = se(cond),
                   .groups = "drop")

temp_dnsum <- ggplot(day_night_sum, aes(x = treatment, y = mean_temp, color = treatment)) +
  geom_point(alpha = 0.5) +
  #geom_line() +
  facet_wrap(.~day_night, nrow = 1) +
  geom_errorbar(aes(ymin=mean_temp+se_temp, ymax= mean_temp-se_temp), alpha = 0.5)+
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red","black"))+
  theme(axis.text.x = element_text(angle = 90))

pH_dnsum <- ggplot(day_night_sum, aes(x = treatment, y = mean_pH, color = treatment)) +
  geom_point(alpha = 0.5) +
  #geom_line() +
  facet_wrap(.~day_night, nrow = 1) +
  geom_errorbar(aes(ymin=mean_pH+se_pH, ymax= mean_pH-se_pH), alpha = 0.5)+
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red","black"))+
  theme(axis.text.x = element_text(angle = 90))

# mV_dnsum <- ggplot(day_night_sum, aes(x = treatment, y = mean_mV, color = treatment)) +
#   geom_point(alpha = 0.5) +
#   #geom_line() +
#   facet_wrap(.~day_night, nrow = 1) +
#   geom_errorbar(aes(ymin=mean_mV+se_mV, ymax= mean_mV-se_mV), alpha = 0.5)+
#   theme_bw(base_size = 20) +
#   scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red","black"))+
#   theme(axis.text.x = element_text(angle = 90))

DO_dnsum <- ggplot(day_night_sum, aes(x = treatment, y = mean_DO, color = treatment)) +
  geom_point(alpha = 0.5) +
  #geom_line() +
  facet_wrap(.~day_night, nrow = 1) +
  geom_errorbar(aes(ymin=mean_DO+se_DO, ymax= mean_DO-se_DO), alpha = 0.5)+
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red","black"))+
  theme(axis.text.x = element_text(angle = 90))

sal_dnsum <- ggplot(day_night_sum, aes(x = treatment, y = mean_sal, color = treatment)) +
  geom_point(alpha = 0.5) +
  #geom_line() +
  facet_wrap(.~day_night, nrow = 1) +
  geom_errorbar(aes(ymin=mean_sal+se_sal, ymax= mean_sal-se_sal), alpha = 0.5)+
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red","black"))+
  theme(axis.text.x = element_text(angle = 90))

# cond_dnsum <- ggplot(day_night_sum, aes(x = day_night, y = mean_cond, color = treatment)) +
#   geom_point(alpha = 0.5) +
#   #geom_line() +
#   geom_errorbar(aes(ymin=mean_cond+se_cond, ymax= mean_cond-se_cond), alpha = 0.5)+
#   theme_bw(base_size = 20) +
#   scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red","black"))+
#   theme(axis.text.x = element_text(angle = 90))


dnsum_plots <- ggarrange(temp_dnsum, pH_dnsum, DO_dnsum, sal_dnsum, 
                       common.legend = T, nrow = 2, ncol = 2)
dnsum_plots

ggsave(here("Output/mean_treatment_day_night_plots.pdf"), dnsum_plots, h = 10, w = 10)

####Data with sump data subtracted####

sump <- read.csv(here("Data/Tank_Data/sump_data_biofac.csv"))
sump <- sump %>% select(-time, -date)

ts <- left_join(td, sump, by = c("timepoint","sump"))

ts <- ts %>% filter(treatment != "sump") %>%
  mutate(temp_dif = temp - temp_sump,
         pH_dif = pH - pH_sump,
         mV_dif = mV - mV_sump,
         sal_dif = sal - sal_sump,
         cond_dif = cond - cond_sump,
         DO_dif = DO - DO_sump
         )

temp_dif <- ggplot(ts, aes(x = date_time, y = temp_dif, color = treatment, group = tank)) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.5) +
  facet_wrap(day_night~., nrow = 2) +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red", "black"))+
  scale_x_datetime(date_labels = "%m-%d", date_breaks = "1 day")

pH_dif <- ggplot(ts, aes(x = date_time, y = pH_dif, color = treatment, group = tank)) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.5) +
  facet_wrap(day_night~., nrow = 2) +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red", "black"))+
  scale_x_datetime(date_labels = "%m-%d", date_breaks = "1 day")

# mV_dif <- ggplot(ts, aes(x = date_time, y = mV_dif, color = treatment, group = tank)) +
#   geom_point(alpha = 0.5) +
#   geom_line(alpha = 0.5) +
#   facet_wrap(day_night~., nrow = 2) +
#   theme_bw(base_size = 20) +
#   theme(axis.text.x = element_text(angle = 90)) +
#   scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red", "black"))+
#   scale_x_datetime(date_labels = "%m-%d", date_breaks = "1 day")

DO_dif <- ggplot(ts, aes(x = date_time, y = DO_dif, color = treatment, group = tank)) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.5) +
  facet_wrap(day_night~., nrow = 2) +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red", "black"))+
  scale_x_datetime(date_labels = "%m-%d", date_breaks = "1 day")

sal_dif <- ggplot(ts, aes(x = date_time, y = sal_dif, color = treatment, group = tank)) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.5) +
  facet_wrap(day_night~., nrow = 2) +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red", "black"))+
  scale_x_datetime(date_labels = "%m-%d", date_breaks = "1 day")

# cond_dif <- ggplot(ts, aes(x = date_time, y = cond_dif, color = treatment, group = tank)) +
#   geom_point(alpha = 0.5) +
#   geom_line(alpha = 0.5) +
#   facet_wrap(day_night~., nrow = 2) +
#   theme_bw(base_size = 20) +
#   theme(axis.text.x = element_text(angle = 90)) +
#   scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red", "black"))+
#   scale_x_datetime(date_labels = "%m-%d", date_breaks = "1 day")

dif_plots <- ggarrange(temp_dif, pH_dif, DO_dif, sal_dif, 
                       common.legend = T, nrow = 2, ncol = 3)
dif_plots

ggsave(here("Output/dif_sump_treatment_plots.pdf"), dif_plots, h = 10, w = 10)

##Average plots of difference between treatments

mean_dif <- ts %>%
  mutate(timepoint = mdy_hm(timepoint)) %>%
  drop_na(temp_dif) %>%
  group_by(timepoint, treatment, day_night) %>%
  dplyr::summarize(mean_temp_dif = mean(temp_dif),
                   se_temp_dif = se(temp_dif),
                   mean_pH_dif = mean(pH_dif),
                   se_pH_dif = se(pH_dif),
                   mean_mV_dif = mean(mV_dif),
                   se_mV_dif = se(mV_dif),
                   mean_DO_dif = mean(DO_dif),
                   se_DO_dif = se(DO_dif),
                   mean_sal_dif = mean(sal_dif),
                   se_sal_dif = se(sal_dif),
                   mean_cond_dif = mean(cond_dif),
                   se_cond_dif = se(cond_dif),
                   .groups = "drop")

temp_dif_mean <- ggplot(mean_dif, aes(x = timepoint, y = mean_temp_dif, color = treatment)) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.5) +
  facet_wrap(day_night~., nrow = 2) +
  geom_errorbar(aes(ymin=mean_temp_dif+se_temp_dif, ymax= mean_temp_dif-se_temp_dif), alpha = 0.5)+
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red", "black"))+
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_datetime(date_labels = "%m-%d", date_breaks = "1 day")

pH_dif_mean <- ggplot(mean_dif, aes(x = timepoint, y = mean_pH_dif, color = treatment)) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.5) +
  facet_wrap(day_night~., nrow = 2) +
  geom_errorbar(aes(ymin=mean_pH_dif+se_pH_dif, ymax= mean_pH_dif-se_pH_dif), alpha = 0.5)+
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red", "black"))+
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_datetime(date_labels = "%m-%d", date_breaks = "1 day")
ggsave(here("Output/pH_dif_mean_treatment.pdf"), pH_dif_mean, h = 10, w = 10)

mV_dif_mean <- ggplot(mean_dif, aes(x = timepoint, y = mean_mV_dif, color = treatment)) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.5) +
  facet_wrap(day_night~., nrow = 2) +
  geom_errorbar(aes(ymin=mean_mV_dif+se_mV_dif, ymax= mean_mV_dif-se_mV_dif), alpha = 0.5)+
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red", "black"))+
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_datetime(date_labels = "%m-%d", date_breaks = "1 day")

DO_dif_mean <- ggplot(mean_dif, aes(x = timepoint, y = mean_DO_dif, color = treatment)) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.5) +
  facet_wrap(day_night~., nrow = 2) +
  geom_errorbar(aes(ymin=mean_DO_dif+se_DO_dif, ymax= mean_DO_dif-se_DO_dif), alpha = 0.5)+
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red", "black"))+
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_datetime(date_labels = "%m-%d", date_breaks = "1 day")
ggsave(here("Output/DO_dif_mean_treatment.pdf"), DO_dif_mean, h = 10, w = 10)

sal_dif_mean <- ggplot(mean_dif, aes(x = timepoint, y = mean_sal_dif, color = treatment)) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.5) +
  facet_wrap(day_night~., nrow = 2) +
  geom_errorbar(aes(ymin=mean_sal_dif+se_sal_dif, ymax= mean_sal_dif-se_sal_dif), alpha = 0.5)+
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red", "black"))+
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_datetime(date_labels = "%m-%d", date_breaks = "1 day")

cond_dif_mean <- ggplot(mean_dif, aes(x = timepoint, y = mean_cond_dif, color = treatment)) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.5) +
  facet_wrap(day_night~., nrow = 2) +
  geom_errorbar(aes(ymin=mean_cond_dif+se_cond_dif, ymax= mean_cond_dif-se_cond_dif), alpha = 0.5)+
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red", "black"))+
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_datetime(date_labels = "%m-%d", date_breaks = "1 day")


mean_dif_plots <- ggarrange(temp_dif_mean, pH_dif_mean, mV_dif_mean, DO_dif_mean, sal_dif_mean, cond_dif_mean, 
                        common.legend = T, nrow = 2, ncol = 3)
mean_dif_plots

ggsave(here("Output/mean_dif_treatment_plots.pdf"), mean_plots, h = 15, w = 15)

##Sum plots between day and night dif from sump

day_night_sum <- ts %>%
  drop_na(cond_dif) %>%
  group_by(day_night, treatment) %>%
  dplyr::summarize(mean_temp_dif = mean(temp_dif),
                   se_temp_dif = se(temp_dif),
                   mean_pH_dif = mean(pH_dif),
                   se_pH_dif = se(pH_dif),
                   mean_mV_dif = mean(mV_dif),
                   se_mV_dif = se(mV_dif),
                   mean_DO_dif = mean(DO_dif),
                   se_DO_dif = se(DO_dif),
                   mean_sal_dif = mean(sal_dif),
                   se_sal_dif = se(sal_dif),
                   mean_cond_dif = mean(cond_dif),
                   se_cond_dif = se(cond_dif),
                   .groups = "drop")

temp_dif_dnsum <- ggplot(day_night_sum, aes(x = treatment, y = mean_temp_dif, color = treatment)) +
  geom_point() +
  #geom_line() +
  facet_wrap(.~day_night, nrow = 1) +
  geom_errorbar(aes(ymin=mean_temp_dif+se_temp_dif, ymax= mean_temp_dif-se_temp_dif), alpha = 0.5)+
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red","black"))+
  theme(axis.text.x = element_text(angle = 90))

pH_dif_dnsum <- ggplot(day_night_sum, aes(x = treatment, y = mean_pH_dif, color = treatment)) +
  geom_point() +
  #geom_line() +
  facet_wrap(.~day_night, nrow = 1) +
  geom_errorbar(aes(ymin=mean_pH_dif+se_pH_dif, ymax= mean_pH_dif-se_pH_dif), alpha = 0.5)+
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red","black"))+
  theme(axis.text.x = element_text(angle = 90))

# mV_dif_dnsum <- ggplot(day_night_sum, aes(x = day_night, y = mean_mV_dif, color = treatment)) +
#   geom_point(alpha = 0.5) +
#   #geom_line() +
#   geom_errorbar(aes(ymin=mean_mV_dif+se_mV_dif, ymax= mean_mV_dif-se_mV_dif), alpha = 0.5)+
#   theme_bw(base_size = 20) +
#   scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red","black"))+
#   theme(axis.text.x = element_text(angle = 90))

DO_dif_dnsum <- ggplot(day_night_sum, aes(x = treatment, y = mean_DO_dif, color = treatment)) +
  geom_point() +
  #geom_line() +
  stat_summary(geom = "text", fun = max, vjust = -1, size = 8,
               label = c("a", "ab", "b", "b", "a", "ab", "","","","","",""))+
  facet_wrap(.~day_night, nrow = 1) +
  geom_errorbar(aes(ymin=mean_DO_dif+se_DO_dif, ymax= mean_DO_dif-se_DO_dif), alpha = 0.5)+
  theme_bw(base_size = 20) +
  ylim(0.1,0.65)+
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red","black"))+
  theme(axis.text.x = element_text(angle = 90))

sal_dif_dnsum <- ggplot(day_night_sum, aes(x = treatment, y = mean_sal_dif, color = treatment)) +
  geom_point() +
  #geom_line() +
  facet_wrap(.~day_night, nrow = 1) +
  geom_errorbar(aes(ymin=mean_sal_dif+se_sal_dif, ymax= mean_sal_dif-se_sal_dif), alpha = 0.5)+
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red","black"))+
  theme(axis.text.x = element_text(angle = 90))

# cond_dif_dnsum <- ggplot(day_night_sum, aes(x = day_night, y = mean_cond_dif, color = treatment)) +
#   geom_point(alpha = 0.5) +
#   #geom_line() +
#   geom_errorbar(aes(ymin=mean_cond_dif+se_cond_dif, ymax= mean_cond_dif-se_cond_dif), alpha = 0.5)+
#   theme_bw(base_size = 20) +
#   scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red","black"))+
#   theme(axis.text.x = element_text(angle = 90))

dnsum_dif_plots <- ggarrange(temp_dif_dnsum, pH_dif_dnsum, DO_dif_dnsum, sal_dif_dnsum, 
                         common.legend = T, nrow = 2, ncol = 2)
dnsum_dif_plots

ggsave(here("Output/mean_dif_treatment_day_night_plots.pdf"), dnsum_dif_plots, h = 10, w = 10)

####Stats

#All data

day <- td %>% 
  filter(day_night == "day") %>%
  drop_na(cond) %>%
  filter(treatment != "sump")

night <- td %>% 
  filter(day_night == "night") %>%
  drop_na(cond) %>%
  filter(treatment != "sump")

#pH day comparisons
pH_day_mod <- lm(pH~treatment, data = day)
Anova(pH_day_mod)
summary(pH_day_mod)
#check_model(DW.mod.spp)

pH_day_emm <- emmeans::emmeans(pH_day_mod, ~ treatment)
pH_day_pairs <- pairs(pH_day_emm)
pH_day_pairs #NO DIFFERENCES

#pH night comparisons
pH_night_mod <- lm(pH~treatment, data = night)
Anova(pH_night_mod)
summary(pH_night_mod)
#check_model(DW.mod.spp)

pH_night_emm <- emmeans::emmeans(pH_night_mod, ~ treatment)
pH_night_pairs <- pairs(pH_night_emm)
pH_night_pairs #NO DIFFERENCES

#DO day comparisons
#pH day comparisons
DO_day_mod <- lm(DO~treatment, data = day)
Anova(DO_day_mod)
summary(DO_day_mod)
#check_model(DW.mod.spp)

DO_day_emm <- emmeans::emmeans(DO_day_mod, ~ treatment)
DO_day_pairs <- pairs(DO_day_emm)
DO_day_pairs #NO DIFFERENCES

#DO night comparisons
DO_night_mod <- lm(DO~treatment, data = night)
Anova(DO_night_mod)
summary(DO_night_mod)
#check_model(DW.mod.spp)

DO_night_emm <- emmeans::emmeans(DO_night_mod, ~ treatment)
DO_night_pairs <- pairs(DO_night_emm)
DO_night_pairs #NO DIFFERENCES

#Sump difference data
#make dataframes for day and night of data from sump differences
day_dif <- ts %>% 
  filter(day_night == "day") %>%
  drop_na(cond_dif)

night_dif <- ts %>% 
  filter(day_night == "night") %>%
  drop_na(cond_dif)

#pH day comparisons
pH_day_dif_mod <- lm(pH_dif~treatment, data = day_dif)
Anova(pH_day_dif_mod)
summary(pH_day_dif_mod)
#check_model(DW.mod.spp)

pH_day_dif_emm <- emmeans::emmeans(pH_day_dif_mod, ~ treatment)
pH_day_dif_pairs <- pairs(pH_day_dif_emm)
pH_day_dif_pairs #NO DIFFERENCES

#pH night comparisons
pH_night_dif_mod <- lm(pH_dif~treatment, data = night_dif)
Anova(pH_night_dif_mod)
summary(pH_night_dif_mod)
#check_model(DW.mod.spp)

pH_night_dif_emm <- emmeans::emmeans(pH_night_dif_mod, ~ treatment)
pH_night_dif_pairs <- pairs(pH_night_dif_emm)
pH_night_dif_pairs #FAKE B almost sig lower than MONOA and 6SP

#DO day comparisons
#pH day comparisons
DO_day_dif_mod <- lm(DO_dif~treatment, data = day_dif)
Anova(DO_day_dif_mod)
summary(DO_day_dif_mod)
#check_model(DW.mod.spp)

DO_day_dif_emm <- emmeans::emmeans(DO_day_dif_mod, ~ treatment)
DO_day_dif_pairs <- pairs(DO_day_dif_emm)
DO_day_dif_pairs #MONOA and 3SP SIG HIGHER THAN FAKE A and FAKE B, 6SP ALMOST SIG HIGHER THAN FAKE A and FAKE B
#MONO B ALMOST SIG LOWER THAN MONOA
# 3SP - FAKEA    0.15048 0.0476 323   3.162  0.0211
# 3SP - FAKEB    0.15643 0.0465 323   3.367  0.0109
# FAKEA - MONOA -0.17319 0.0478 323  -3.623  0.0045
# FAKEB - MONOA -0.17914 0.0467 323  -3.838  0.0021

#DO night comparisons
DO_night_dif_mod <- lm(DO_dif~treatment, data = night_dif)
Anova(DO_night_dif_mod)
summary(DO_night_dif_mod)
#check_model(DW.mod.spp)

DO_night_dif_emm <- emmeans::emmeans(DO_night_dif_mod, ~ treatment)
DO_night_dif_pairs <- pairs(DO_night_dif_emm)
DO_night_dif_pairs #NO DIFFERENCES

