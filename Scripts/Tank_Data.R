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
  mutate(date_time = paste(date,time)) %>%
  mutate(date_time = mdy_hm(date_time)) %>%
  mutate(tank = as.factor(tank)) %>% 
  filter(bad == "no") %>%
  drop_na(temp)

#plot ALL data

temp_pre <- ggplot(td, aes(x = date_time, y = temp, color = treatment, group = tank, shape = coral_no)) +
  geom_point() +
  #geom_line() +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_color_manual(values = c("steelblue","purple","pink","lightgray","darkgray","lightgreen","red"))+
  scale_x_datetime()

pH_pre <- ggplot(td, aes(x = date_time, y = pH, color = treatment, group = tank, shape = coral_no)) +
  geom_point() +
  #geom_line() +
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","pink","lightgray","darkgray","lightgreen","red"))+
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_datetime()

mV_pre <- ggplot(td, aes(x = date_time, y = mV, color = treatment, group = tank, shape = coral_no)) +
  geom_point() +
  #geom_line() +
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","pink","lightgray","darkgray","lightgreen","red"))+
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_datetime()

DO_pre <- ggplot(td, aes(x = date_time, y = DO, color = treatment, group = tank, shape = coral_no)) +
  geom_point() +
  #geom_line() +
  scale_color_manual(values = c("steelblue","purple","pink","lightgray","darkgray","lightgreen","red"))+
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_datetime()

sal_pre <- ggplot(td, aes(x = date_time, y = sal, color = treatment, group = tank, shape = coral_no)) +
  geom_point() +
  #geom_line() +
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","darkgray","darkgreen","orange","lightgreen","red"))+
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_datetime()

cond_pre <- ggplot(td, aes(x = date_time, y = cond, color = treatment, group = tank, shape = coral_no)) +
  geom_point() +
  #geom_line() +
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","darkgray","darkgreen","orange","lightgreen","red"))+
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_datetime()


tank_plots <- ggarrange(temp_pre, pH_pre, mV_pre, DO_pre, sal_pre, cond_pre, common.legend = T, nrow = 3, ncol = 3)
tank_plots

ggsave(here("Output/tank_plots.pdf"), tank_plots, h = 10, w = 10)

ed <- td %>%
  dplyr::filter(period != "acclimation")

temp <- ggplot(ed, aes(x = date_time, y = temp, color = treatment, group = tank)) +
  geom_point(alpha = 0.5) +
  #geom_line() +
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red"))+
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_datetime()

pH <- ggplot(ed, aes(x = date_time, y = pH, color = treatment, group = tank)) +
  geom_point(alpha = 0.5) +
  #geom_line() +
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red"))+
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_datetime()

mV <- ggplot(ed, aes(x = date_time, y = mV, color = treatment, group = tank)) +
  geom_point(alpha = 0.5) +
  #geom_line() +
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red"))+
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_datetime()

DO <- ggplot(ed, aes(x = date_time, y = DO, color = treatment, group = tank)) +
  geom_point(alpha = 0.5) +
  #geom_line() +
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red"))+
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_datetime()

sal <- ggplot(ed, aes(x = date_time, y = sal, color = treatment, group = tank)) +
  geom_point(alpha = 0.5) +
  #geom_line() +
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red"))+
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_datetime()

cond <- ggplot(ed, aes(x = date_time, y = cond, color = treatment, group = tank)) +
  geom_point(alpha = 0.5) +
  #geom_line() +
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red"))+
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_datetime()

expt_plots <- ggarrange(temp, sal, mV, DO, sal, cond, common.legend = T, nrow = 3, ncol = 3)
expt_plots

ggsave(here("Output/experimental_plots.pdf"), expt_plots, h = 10, w = 10)


##Average plots across treatments

se <- function(x) sd(x)/sqrt(length(x))

sumd <- ed %>%
  mutate(timepoint = mdy_hm(timepoint)) %>%
  drop_na(temp) %>%
  group_by(timepoint, treatment) %>%
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

temp_sum <- ggplot(sumd, aes(x = timepoint, y = mean_temp, color = treatment)) +
  geom_point(alpha = 0.5) +
  #geom_line() +
  geom_errorbar(aes(ymin=mean_temp+se_temp, ymax= mean_temp-se_temp), alpha = 0.5)+
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red"))+
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_datetime()

pH_sum <- ggplot(sumd, aes(x = timepoint, y = mean_pH, color = treatment)) +
  geom_point(alpha = 0.5) +
  #geom_line() +
  geom_errorbar(aes(ymin=mean_pH+se_pH, ymax= mean_pH-se_pH), alpha = 0.5)+
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red"))+
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_datetime()

mV_sum <- ggplot(sumd, aes(x = timepoint, y = mean_mV, color = treatment)) +
  geom_point(alpha = 0.5) +
  #geom_line() +
  geom_errorbar(aes(ymin=mean_mV+se_mV, ymax= mean_mV-se_mV), alpha = 0.5)+
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red"))+
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_datetime()

DO_sum <- ggplot(sumd, aes(x = timepoint, y = mean_DO, color = treatment)) +
  geom_point(alpha = 0.5) +
  #geom_line() +
  geom_errorbar(aes(ymin=mean_DO+se_DO, ymax= mean_DO-se_DO), alpha = 0.5)+
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red"))+
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_datetime()

sal_sum <- ggplot(sumd, aes(x = timepoint, y = mean_sal, color = treatment)) +
  geom_point(alpha = 0.5) +
  #geom_line() +
  geom_errorbar(aes(ymin=mean_sal+se_sal, ymax= mean_sal-se_sal), alpha = 0.5)+
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red"))+
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_datetime()

cond_sum <- ggplot(sumd, aes(x = timepoint, y = mean_cond, color = treatment)) +
  geom_point(alpha = 0.5) +
  #geom_line() +
  geom_errorbar(aes(ymin=mean_cond+se_cond, ymax= mean_cond-se_cond), alpha = 0.5)+
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red"))+
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_datetime()


sum_plots <- ggarrange(temp_sum, pH_sum, mV_sum, DO_sum, sal_sum, cond_sum, 
                       common.legend = T, nrow = 3, ncol = 3)
sum_plots

ggsave(here("Output/mean_treatment_plots.pdf"), sum_plots, h = 10, w = 10)

##Sum plots between day and night
#look at this also adjusted by tanks when smaller volume adjusted (24th??)

day_night_sum <- ed %>%
  drop_na(temp) %>%
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

temp_dnsum <- ggplot(day_night_sum, aes(x = day_night, y = mean_temp, color = treatment)) +
  geom_point(alpha = 0.5) +
  #geom_line() +
  geom_errorbar(aes(ymin=mean_temp+se_temp, ymax= mean_temp-se_temp), alpha = 0.5)+
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red"))+
  theme(axis.text.x = element_text(angle = 90))

pH_dnsum <- ggplot(day_night_sum, aes(x = day_night, y = mean_pH, color = treatment)) +
  geom_point(alpha = 0.5) +
  #geom_line() +
  geom_errorbar(aes(ymin=mean_pH+se_pH, ymax= mean_pH-se_pH), alpha = 0.5)+
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red"))+
  theme(axis.text.x = element_text(angle = 90))

mV_dnsum <- ggplot(day_night_sum, aes(x = day_night, y = mean_mV, color = treatment)) +
  geom_point(alpha = 0.5) +
  #geom_line() +
  geom_errorbar(aes(ymin=mean_mV+se_mV, ymax= mean_mV-se_mV), alpha = 0.5)+
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red"))+
  theme(axis.text.x = element_text(angle = 90))

DO_dnsum <- ggplot(day_night_sum, aes(x = day_night, y = mean_DO, color = treatment)) +
  geom_point(alpha = 0.5) +
  #geom_line() +
  geom_errorbar(aes(ymin=mean_DO+se_DO, ymax= mean_DO-se_DO), alpha = 0.5)+
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red"))+
  theme(axis.text.x = element_text(angle = 90))

sal_dnsum <- ggplot(day_night_sum, aes(x = day_night, y = mean_sal, color = treatment)) +
  geom_point(alpha = 0.5) +
  #geom_line() +
  geom_errorbar(aes(ymin=mean_sal+se_sal, ymax= mean_sal-se_sal), alpha = 0.5)+
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red"))+
  theme(axis.text.x = element_text(angle = 90))

cond_dnsum <- ggplot(day_night_sum, aes(x = day_night, y = mean_cond, color = treatment)) +
  geom_point(alpha = 0.5) +
  #geom_line() +
  geom_errorbar(aes(ymin=mean_cond+se_cond, ymax= mean_cond-se_cond), alpha = 0.5)+
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red"))+
  theme(axis.text.x = element_text(angle = 90))


dnsum_plots <- ggarrange(temp_dnsum, pH_dnsum, mV_dnsum, DO_dnsum, sal_dnsum, cond_dnsum, 
                       common.legend = T, nrow = 3, ncol = 3)
dnsum_plots

ggsave(here("Output/mean_treatment_day_night_plots.pdf"), dnsum_plots, h = 10, w = 10)

##Data with sump data subtracted

sump <- read.csv(here("Data/Tank_Data/sump_data_biofac.csv"))
sump <- sump %>%
  select(-time, -date)

ts <- left_join(ed, sump, by = c("timepoint","sump"))

ts <- ts %>%
  mutate(temp_dif = temp - temp_sump,
         pH_dif = pH - pH_sump,
         mV_dif = mV - mV_sump,
         sal_dif = sal - sal_sump,
         cond_dif = cond - cond_sump,
         DO_dif = DO - DO_sump
         )


temp_dif <- ggplot(ts, aes(x = date_time, y = temp_dif, color = treatment, group = tank)) +
  geom_point(alpha = 0.5) +
  #geom_line() +
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red"))+
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_datetime()

pH_dif <- ggplot(ts, aes(x = date_time, y = pH_dif, color = treatment, group = tank)) +
  geom_point(alpha = 0.5) +
  #geom_line() +
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red"))+
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_datetime()

mV_dif <- ggplot(ts, aes(x = date_time, y = mV_dif, color = treatment, group = tank)) +
  geom_point(alpha = 0.5) +
  #geom_line() +
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red"))+
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_datetime()

DO_dif <- ggplot(ts, aes(x = date_time, y = DO_dif, color = treatment, group = tank)) +
  geom_point(alpha = 0.5) +
  #geom_line() +
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red"))+
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_datetime()

sal_dif <- ggplot(ts, aes(x = date_time, y = sal_dif, color = treatment, group = tank)) +
  geom_point(alpha = 0.5) +
  #geom_line() +
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red"))+
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_datetime()

cond_dif <- ggplot(ts, aes(x = date_time, y = cond_dif, color = treatment, group = tank)) +
  geom_point(alpha = 0.5) +
  #geom_line() +
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red"))+
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_datetime()

dif_plots <- ggarrange(temp_dif, pH_dif, mV_dif, DO_dif, sal_dif, cond_dif, 
                       common.legend = T, nrow = 3, ncol = 3)
dif_plots

ggsave(here("Output/dif_sump_treatment_plots.pdf"), dif_plots, h = 10, w = 10)
