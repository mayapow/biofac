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
library(seacarb)
library(broom)
library(car)
library(dplyr)

#git pull
#git add .
#git commmit -a -m "your message here"
#git push

#read in data

td <- read.csv(here("Data/Tank_Data/tank_data_biofac.csv"))

td <- td %>%
  filter(period == "experiment") %>%
  mutate(date_time = paste(date,time)) %>%
  mutate(date_time = mdy_hm(date_time)) %>%
  mutate(date = mdy(date)) %>%
  mutate(tris_date = mdy(tris_date)) %>%
  mutate(tank = as.factor(tank)) %>% 
  #filter(date > ymd("2026-03-25")) %>%
  filter(bad == "no") #%>%
  #drop_na(temp) %>%
  #filter(day_night == "day")

#read in Tris cal data for pH calibration
pHcalib <-read_csv(here("Data/Tank_Data/Tris_Calibration_HIMB.csv"))
pHcalib <- pHcalib %>% mutate(tris_date = mdy(tris_date))
pHData <- td %>% 
  dplyr::select(date, tris_date, tank, timepoint, time, temp, pH_meas, mV, sal, cond) 
#only selecting needed columns otherwise lots of NAs

#Take the mV calibration files by each date and use them to calculate pH using the seacarb package
pHSlope <- pHcalib %>% 
  nest_by(tris_date) %>%
  mutate(fitpH = list(lm(mVTris~TTris, data = pHcalib))) %>% # linear regression of mV and temp of the tris
  reframe(broom::tidy(fitpH)) %>% # make the output tidy
  dplyr::select(tris_date, term, estimate) %>%
  pivot_wider(names_from = term, values_from = estimate) %>% # put slope and intercept in their own column
  left_join(pHData,., by = "tris_date") %>% # join with the pH sample data
  mutate(mVTris = temp*TTris + `(Intercept)`) %>% # calculate the mV of the tris at temperature in which the pH of samples were measured
  drop_na() %>%
  mutate(pHT = pH(Ex=mV,Etris=mVTris,S=sal,T=temp)) %>% # calculate pH of the samples using the pH seacarb function
  dplyr::select(date, tank, timepoint, pHT) # selects just these columns (whatever columns you want + pH)

#updating pH in the dataset with pHT
tank_data <- left_join(td, pHSlope, by = c("date","tank","timepoint"))

sump_data <- tank_data %>% filter(treatment == "sump") %>%
  dplyr::select(-sump) %>%
  rename(sump = tank)

#plot ALL data

temp_pre <- ggplot(tank_data, aes(x = date_time, y = temp, color = treatment, group = tank)) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.5) +
  facet_wrap(day_night~., nrow = 2) +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red", "black"))+
  scale_x_datetime(date_labels = "%m-%d", date_breaks = "1 day")

pHT_pre <- ggplot(tank_data, aes(x = date_time, y = pHT, color = treatment, group = tank)) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.5) +
  facet_wrap(day_night~., nrow = 2) +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red", "black"))+
  scale_x_datetime(date_labels = "%m-%d", date_breaks = "1 day")

# mV_pre <- ggplot(tank_data, aes(x = date_time, y = mV, color = treatment, group = tank)) +
#   geom_point(alpha = 0.5) +
#   geom_line(alpha = 0.5) +
#   facet_wrap(day_night~., nrow = 2) +
#   theme_bw(base_size = 20) +
#   theme(axis.text.x = element_text(angle = 90)) +
#   scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red", "black"))+
#   scale_x_datetime(date_labels = "%m-%d", date_breaks = "1 day")

DO_pre <- ggplot(tank_data, aes(x = date_time, y = DO, color = treatment, group = tank)) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.5) +
  facet_wrap(day_night~., nrow = 2) +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red", "black"))+
  scale_x_datetime(date_labels = "%m-%d", date_breaks = "1 day")

sal_pre <- ggplot(tank_data, aes(x = date_time, y = sal, color = treatment, group = tank)) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.5) +
  facet_wrap(day_night~., nrow = 2) +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red", "black"))+
  scale_x_datetime(date_labels = "%m-%d", date_breaks = "1 day")

# cond_pre <- ggplot(tank_data, aes(x = date_time, y = cond, color = treatment, group = tank)) +
#   geom_point(alpha = 0.5) +
#   geom_line(alpha = 0.5) +
#   facet_wrap(day_night~., nrow = 2) +
#   theme_bw(base_size = 20) +
#   theme(axis.text.x = element_text(angle = 90)) +
#   scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red", "black"))+
#   scale_x_datetime(date_labels = "%m-%d", date_breaks = "1 day")

tank_plots <- ggarrange(temp_pre, pHT_pre, DO_pre, sal_pre, 
                        common.legend = T, nrow = 2, ncol = 2)
tank_plots

ggsave(here("Output/tank_plots.pdf"), tank_plots, h = 15, w = 15)

##Average plots across treatments

se <- function(x) sd(x,na.rm = T)/sqrt(length(x))

meand <- tank_data %>%
  mutate(timepoint = mdy_hm(timepoint)) %>%
  drop_na(temp) %>%
  group_by(timepoint, treatment, day_night) %>%
  dplyr::summarize(mean_temp = mean(temp),
                   se_temp = se(temp),
                   mean_pHT = mean(pHT),
                   se_pHT = se(pHT),
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

pHT_mean <- ggplot(meand, aes(x = timepoint, y = mean_pHT, color = treatment)) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.5) +
  facet_wrap(day_night~., nrow = 2) +
  geom_errorbar(aes(ymin=mean_pHT+se_pHT, ymax= mean_pHT-se_pHT), alpha = 0.5)+
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red", "black"))+
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_datetime(date_labels = "%m-%d", date_breaks = "1 day")
#ggsave(here("Output/pHT_mean_treatment.pdf"), pHT_mean, h = 10, w = 10)

# mV_mean <- ggplot(meand, aes(x = timepoint, y = mean_mV, color = treatment)) +
#   geom_point(alpha = 0.5) +
#   geom_line(alpha = 0.5) +
#   facet_wrap(day_night~., nrow = 2) +
#   geom_errorbar(aes(ymin=mean_mV+se_mV, ymax= mean_mV-se_mV), alpha = 0.5)+
#   theme_bw(base_size = 20) +
#   scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red", "black"))+
#   theme(axis.text.x = element_text(angle = 90)) +
#   scale_x_datetime(date_labels = "%m-%d", date_breaks = "1 day")

DO_mean <- ggplot(meand, aes(x = timepoint, y = mean_DO, color = treatment)) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.5) +
  facet_wrap(day_night~., nrow = 2) +
  geom_errorbar(aes(ymin=mean_DO+se_DO, ymax= mean_DO-se_DO), alpha = 0.5)+
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red", "black"))+
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_datetime(date_labels = "%m-%d", date_breaks = "1 day")
#ggsave(here("Output/DO_mean_treatment.pdf"), DO_mean, h = 10, w = 10)

sal_mean <- ggplot(meand, aes(x = timepoint, y = mean_sal, color = treatment)) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.5) +
  facet_wrap(day_night~., nrow = 2) +
  geom_errorbar(aes(ymin=mean_sal+se_sal, ymax= mean_sal-se_sal), alpha = 0.5)+
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red", "black"))+
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_datetime(date_labels = "%m-%d", date_breaks = "1 day")

# cond_mean <- ggplot(meand, aes(x = timepoint, y = mean_cond, color = treatment)) +
#   geom_point(alpha = 0.5) +
#   geom_line(alpha = 0.5) +
#   facet_wrap(day_night~., nrow = 2) +
#   geom_errorbar(aes(ymin=mean_cond+se_cond, ymax= mean_cond-se_cond), alpha = 0.5)+
#   theme_bw(base_size = 20) +
#   scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red", "black"))+
#   theme(axis.text.x = element_text(angle = 90)) +
#   scale_x_datetime(date_labels = "%m-%d", date_breaks = "1 day")


mean_plots <- ggarrange(temp_mean, pHT_mean, DO_mean, sal_mean, 
                       common.legend = T, nrow = 2, ncol = 2)
mean_plots

ggsave(here("Output/mean_treatment_plots.pdf"), mean_plots, h = 15, w = 15)

##Sum plots between day and night
#look at this also adjusted by tanks when smaller volume adjusted (24th??)

day_night_sum <- tank_data %>%
  group_by(day_night, treatment) %>%
  dplyr::summarize(mean_temp = mean(temp, na.rm = T),
                   se_temp = se(temp),
                   mean_pHT = mean(pHT, na.rm = T),
                   se_pHT = se(pHT),
                   mean_mV = mean(mV, na.rm = T),
                   se_mV = se(mV),
                   mean_DO = mean(DO, na.rm = T),
                   se_DO = se(DO),
                   mean_sal = mean(sal, na.rm = T),
                   se_sal = se(sal),
                   mean_cond = mean(cond, na.rm = T),
                   se_cond = se(cond),
                   .groups = "drop")

temp_dnsum <- ggplot(day_night_sum, aes(x = treatment, y = mean_temp, color = treatment)) +
  geom_point() +
  #geom_line() +
  facet_wrap(.~day_night, nrow = 1) +
  geom_errorbar(aes(ymin=mean_temp+se_temp, ymax= mean_temp-se_temp), alpha = 0.5)+
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red","black"))+
  theme(axis.text.x = element_text(angle = 90))

pHT_dnsum <- ggplot(day_night_sum, aes(x = treatment, y = mean_pHT, color = treatment)) +
  geom_point() +
  #geom_line() +
  facet_wrap(.~day_night, nrow = 1) +
  geom_errorbar(aes(ymin=mean_pHT+se_pHT, ymax= mean_pHT-se_pHT), alpha = 0.5)+
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
  geom_point() +
  #geom_line() +
  facet_wrap(.~day_night, nrow = 1) +
  geom_errorbar(aes(ymin=mean_DO+se_DO, ymax= mean_DO-se_DO), alpha = 0.5)+
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red","black"))+
  theme(axis.text.x = element_text(angle = 90))

sal_dnsum <- ggplot(day_night_sum, aes(x = treatment, y = mean_sal, color = treatment)) +
  geom_point() +
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


dnsum_plots <- ggarrange(pHT_dnsum, DO_dnsum, temp_dnsum, sal_dnsum, 
                       common.legend = T, nrow = 2, ncol = 2)
dnsum_plots

ggsave(here("Output/mean_treatment_day_night_plots.pdf"), dnsum_plots, h = 10, w = 10)

####Data with sump data subtracted####

sump_data <- tank_data %>% filter(treatment == "sump") %>%
  select(-sump) %>%
  rename(sump = tank, 
         temp_sump = temp, 
         pHT_sump = pHT, 
         mV_sump = mV, 
         sal_sump = sal,
         cond_sump = cond,
         DO_sump = DO) %>%
  select(timepoint, sump, temp_sump, pHT_sump, mV_sump, sal_sump, cond_sump, DO_sump)

ts <- left_join(tank_data, sump_data, by = c("timepoint","sump"))

ts <- ts %>%
  mutate(temp_dif = temp - temp_sump,
         pHT_dif = pHT - pHT_sump,
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

pHT_dif <- ggplot(ts, aes(x = date_time, y = pHT_dif, color = treatment, group = tank)) +
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

dif_plots <- ggarrange(temp_dif, pHT_dif, DO_dif, sal_dif, 
                       common.legend = T, nrow = 2, ncol = 2)
dif_plots

ggsave(here("Output/dif_sump_treatment_plots.pdf"), dif_plots, h = 10, w = 10)

##Average plots of difference between treatments

mean_dif <- ts %>%
  mutate(timepoint = mdy_hm(timepoint)) %>%
  drop_na(temp_dif) %>%
  group_by(timepoint, treatment, day_night) %>%
  dplyr::summarize(mean_temp_dif = mean(temp_dif,na.rm = T),
                   se_temp_dif = se(temp_dif),
                   mean_pHT_dif = mean(pHT_dif,na.rm = T),
                   se_pHT_dif = se(pHT_dif),
                   mean_mV_dif = mean(mV_dif,na.rm = T),
                   se_mV_dif = se(mV_dif),
                   mean_DO_dif = mean(DO_dif,na.rm = T),
                   se_DO_dif = se(DO_dif),
                   mean_sal_dif = mean(sal_dif,na.rm = T),
                   se_sal_dif = se(sal_dif),
                   mean_cond_dif = mean(cond_dif,na.rm = T),
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

pHT_dif_mean <- ggplot(mean_dif, aes(x = timepoint, y = mean_pHT_dif, color = treatment)) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.5) +
  facet_wrap(day_night~., nrow = 2) +
  geom_errorbar(aes(ymin=mean_pHT_dif+se_pHT_dif, ymax= mean_pHT_dif-se_pHT_dif), alpha = 0.5)+
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red", "black"))+
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_datetime(date_labels = "%m-%d", date_breaks = "1 day")
ggsave(here("Output/pHT_dif_mean_treatment.pdf"), pHT_dif_mean, h = 10, w = 10)

# mV_dif_mean <- ggplot(mean_dif, aes(x = timepoint, y = mean_mV_dif, color = treatment)) +
#   geom_point(alpha = 0.5) +
#   geom_line(alpha = 0.5) +
#   facet_wrap(day_night~., nrow = 2) +
#   geom_errorbar(aes(ymin=mean_mV_dif+se_mV_dif, ymax= mean_mV_dif-se_mV_dif), alpha = 0.5)+
#   theme_bw(base_size = 20) +
#   scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red", "black"))+
#   theme(axis.text.x = element_text(angle = 90)) +
#   scale_x_datetime(date_labels = "%m-%d", date_breaks = "1 day")

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

# cond_dif_mean <- ggplot(mean_dif, aes(x = timepoint, y = mean_cond_dif, color = treatment)) +
#   geom_point(alpha = 0.5) +
#   geom_line(alpha = 0.5) +
#   facet_wrap(day_night~., nrow = 2) +
#   geom_errorbar(aes(ymin=mean_cond_dif+se_cond_dif, ymax= mean_cond_dif-se_cond_dif), alpha = 0.5)+
#   theme_bw(base_size = 20) +
#   scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red", "black"))+
#   theme(axis.text.x = element_text(angle = 90)) +
#   scale_x_datetime(date_labels = "%m-%d", date_breaks = "1 day")


mean_dif_plots <- ggarrange(temp_dif_mean, pHT_dif_mean, DO_dif_mean, sal_dif_mean, 
                        common.legend = T, nrow = 2, ncol = 2)
mean_dif_plots

ggsave(here("Output/mean_dif_treatment_plots.pdf"), mean_plots, h = 15, w = 15)

##Sum plots between day and night dif from sump

day_night_sum <- ts %>%
  drop_na(cond_dif) %>%
  group_by(day_night, treatment) %>%
  dplyr::summarize(mean_temp_dif = mean(temp_dif, na.rm = T),
                   se_temp_dif = se(temp_dif),
                   mean_pHT_dif = mean(pHT_dif,na.rm = T),
                   se_pHT_dif = se(pHT_dif),
                   mean_mV_dif = mean(mV_dif,na.rm = T),
                   se_mV_dif = se(mV_dif),
                   mean_DO_dif = mean(DO_dif,na.rm = T),
                   se_DO_dif = se(DO_dif),
                   mean_sal_dif = mean(sal_dif,na.rm = T),
                   se_sal_dif = se(sal_dif),
                   mean_cond_dif = mean(cond_dif,na.rm = T),
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

pHT_dif_dnsum <- ggplot(day_night_sum, aes(x = treatment, y = mean_pHT_dif, color = treatment)) +
  geom_point() +
  #geom_line() +
  facet_wrap(.~day_night, nrow = 1) +
  geom_errorbar(aes(ymin=mean_pHT_dif+se_pHT_dif, ymax= mean_pHT_dif-se_pHT_dif), alpha = 0.5)+
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
  #stat_summary(geom = "text", fun = max, vjust = -1, size = 8,
  #             label = c("a", "ab", "b", "b", "a", "ab", "","","","","",""))+
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

dnsum_dif_plots <- ggarrange(pHT_dif_dnsum, DO_dif_dnsum, temp_dif_dnsum, sal_dif_dnsum, 
                         common.legend = T, nrow = 2, ncol = 2)
dnsum_dif_plots

ggsave(here("Output/mean_dif_treatment_day_night_plots.pdf"), dnsum_dif_plots, h = 10, w = 10)

#differences between day and night in tanks and sump to compare!!

day <- tank_data %>% 
  filter(day_night == "day") %>%
  drop_na(cond)

night <- tank_data %>% 
  filter(day_night == "night") %>%
  filter(date != "2026-03-09") %>% #need to remove the 9th bc sumps were set up this day so official start on the 10th
  drop_na(cond) %>%
  dplyr::select(date, tank, timepoint, time, temp, pH_meas, mV, sal, cond, DO, pHT) %>%
  rename(timepoint_night = timepoint,
         time_night = time,
         temp_night = temp, 
         pHT_night = pHT, 
         pH_meas_night = pH_meas,
         mV_night = mV, 
         sal_night = sal,
         cond_night = cond,
         DO_night = DO)

day_night <- night %>% left_join(day, by = c("tank","date"))

day_night <- day_night %>% 
  mutate(temp_dif = temp - temp_night,
         pHT_dif = pHT - pHT_night,
         sal_dif = sal - sal_night,
         cond_dif = cond - cond_night,
         DO_dif = DO - DO_night
  )

day_night_summary <- day_night %>%
  drop_na(temp) %>%
  group_by(treatment) %>%
  dplyr::summarize(mean_temp_dif = mean(temp_dif, na.rm = T),
                   se_temp_dif = se(temp_dif),
                   mean_pHT_dif = mean(pHT_dif,na.rm = T),
                   se_pHT_dif = se(pHT_dif),
                   mean_DO_dif = mean(DO_dif,na.rm = T),
                   se_DO_dif = se(DO_dif),
                   mean_sal_dif = mean(sal_dif,na.rm = T),
                   se_sal_dif = se(sal_dif),
                   mean_cond_dif = mean(cond_dif,na.rm = T),
                   se_cond_dif = se(cond_dif),
                   .groups = "drop")


temp_dif_day_night <- ggplot(day_night_summary, aes(x = treatment, y = mean_temp_dif, color = treatment)) +
  geom_point() +
  geom_errorbar(aes(ymin=mean_temp_dif+se_temp_dif, ymax= mean_temp_dif-se_temp_dif), alpha = 0.5)+
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red","black"))+
  theme(axis.text.x = element_text(angle = 90))

pHT_dif_day_night <- ggplot(day_night_summary, aes(x = treatment, y = mean_pHT_dif, color = treatment)) +
  geom_point() +
  geom_errorbar(aes(ymin=mean_pHT_dif+se_pHT_dif, ymax= mean_pHT_dif-se_pHT_dif), alpha = 0.5)+
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red","black"))+
  theme(axis.text.x = element_text(angle = 90))

DO_dif_day_night <- ggplot(day_night_summary, aes(x = treatment, y = mean_DO_dif, color = treatment)) +
  geom_point() +
  #stat_summary(geom = "text", fun = max, vjust = -1, size = 8,
  #             label = c("a", "ab", "b", "b", "a", "ab", "","","","","",""))+
  geom_errorbar(aes(ymin=mean_DO_dif+se_DO_dif, ymax= mean_DO_dif-se_DO_dif), alpha = 0.5)+
  theme_bw(base_size = 20) +
  #ylim(0.1,0.65)+
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red","black"))+
  theme(axis.text.x = element_text(angle = 90))

sal_dif_day_night <- ggplot(day_night_summary, aes(x = treatment, y = mean_sal_dif, color = treatment)) +
  geom_point() +
  geom_errorbar(aes(ymin=mean_sal_dif+se_sal_dif, ymax= mean_sal_dif-se_sal_dif), alpha = 0.5)+
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red","black"))+
  theme(axis.text.x = element_text(angle = 90))

day_night_dif_plots <- ggarrange(pHT_dif_day_night, DO_dif_day_night, temp_dif_day_night, sal_dif_day_night, 
                             common.legend = T, nrow = 2, ncol = 2)
day_night_dif_plots

ggsave(here("Output/mean_dif_tank_treatment_day_night_plots.pdf"), day_night_dif_plots, h = 10, w = 10)

####Stats

#All data

day <- tank_data %>% 
  filter(day_night == "day") %>%
  drop_na(cond) %>%
  filter(treatment != "sump")

night <- tank_data %>% 
  filter(day_night == "night") %>%
  drop_na(cond) %>%
  filter(treatment != "sump")

#pHT day comparisons
pHT_day_mod <- lm(pHT~treatment, data = day)
Anova(pHT_day_mod)
summary(pHT_day_mod)
#check_model(DW.mod.spp)

pHT_day_emm <- emmeans::emmeans(pHT_day_mod, ~ treatment)
pHT_day_pairs <- pairs(pHT_day_emm)
pHT_day_pairs #NO DIFFERENCES

#pHT night comparisons
pHT_night_mod <- lm(pHT~treatment, data = night)
Anova(pHT_night_mod)
summary(pHT_night_mod)
#check_model(DW.mod.spp)

pHT_night_emm <- emmeans::emmeans(pHT_night_mod, ~ treatment)
pHT_night_pairs <- pairs(pHT_night_emm)
pHT_night_pairs #NO DIFFERENCES

#DO day comparisons
#pHT day comparisons
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

#pHT day comparisons
pHT_day_dif_mod <- lm(pHT_dif~treatment, data = day_dif)
Anova(pHT_day_dif_mod)
summary(pHT_day_dif_mod)
#check_model(DW.mod.spp)

pHT_day_dif_emm <- emmeans::emmeans(pHT_day_dif_mod, ~ treatment)
pHT_day_dif_pairs <- pairs(pHT_day_dif_emm)
pHT_day_dif_pairs #NO DIFFERENCES

#pHT night comparisons
pHT_night_dif_mod <- lm(pHT_dif~treatment, data = night_dif)
Anova(pHT_night_dif_mod)
summary(pHT_night_dif_mod)
#check_model(DW.mod.spp)

pHT_night_dif_emm <- emmeans::emmeans(pHT_night_dif_mod, ~ treatment)
pHT_night_dif_pairs <- pairs(pHT_night_dif_emm)
pHT_night_dif_pairs #FAKEB AND MONOA 0.0901

#DO day comparisons
#pHT day comparisons
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


##differences between day and night within tanks and sumps
#Dataset = day_night

#pHT day comparisons
pHT_mod <- lm(pHT_dif~treatment, data = day_night)
Anova(pHT_mod)
summary(pHT_mod)
#check_model(DW.mod.spp)

pHT_emm <- emmeans::emmeans(pHT_mod, ~ treatment)
pHT_pairs <- pairs(pHT_emm)
pHT_pairs
#3SP dif than both fakes and sump
#MONOA dif than both fakes and sump
# contrast                    estimate      SE  df t.ratio p.value
# 3SP - 6SP                    0.01173 0.00978 166   1.200  0.8935
# 3SP - FAKEA (PCOM)           0.02888 0.01000 166   2.885  0.0655 .
# 3SP - FAKEB (MCAP)           0.03396 0.00978 166   3.474  0.0114 *
# 3SP - MONOA (PCOM)          -0.00544 0.00989 166  -0.550  0.9980
# 3SP - MONOB (MCAP)           0.00595 0.00967 166   0.615  0.9963
# 3SP - sump                   0.02452 0.00883 166   2.777  0.0866 .
# 6SP - FAKEA (PCOM)           0.01715 0.01010 166   1.696  0.6195
# 6SP - FAKEB (MCAP)           0.02224 0.00988 166   2.251  0.2746
# 6SP - MONOA (PCOM)          -0.01717 0.00999 166  -1.718  0.6047
# 6SP - MONOB (MCAP)          -0.00578 0.00978 166  -0.591  0.9970
# 6SP - sump                   0.01279 0.00894 166   1.430  0.7848
# FAKEA (PCOM) - FAKEB (MCAP)  0.00508 0.01010 166   0.503  0.9988
# FAKEA (PCOM) - MONOA (PCOM) -0.03432 0.01020 166  -3.358  0.0166 *
# FAKEA (PCOM) - MONOB (MCAP) -0.02293 0.01000 166  -2.291  0.2548
# FAKEA (PCOM) - sump         -0.00436 0.00920 166  -0.474  0.9991
# FAKEB (MCAP) - MONOA (PCOM) -0.03941 0.00999 166  -3.944  0.0022 **
# FAKEB (MCAP) - MONOB (MCAP) -0.02802 0.00978 166  -2.866  0.0689 .
# FAKEB (MCAP) - sump         -0.00945 0.00894 166  -1.056  0.9397
# MONOA (PCOM) - MONOB (MCAP)  0.01139 0.00989 166   1.152  0.9108
# MONOA (PCOM) - sump          0.02996 0.00907 166   3.304  0.0196 *
# MONOB (MCAP) - sump          0.01857 0.00883 166   2.103  0.3557

#DO comparisons
DO_mod <- lm(DO_dif~treatment, data = day_night)
Anova(DO_mod)
summary(DO_mod)
#check_model(DW.mod.spp)

DO_emm <- emmeans::emmeans(DO_mod, ~ treatment)
DO_pairs <- pairs(DO_emm)
DO_pairs
#3SP dif than both fakes and sump
#6SP dif than both fakes and sump
#MONOA dif than both fakes and sump
# contrast                    estimate     SE  df t.ratio p.value
# 3SP - 6SP                    0.02435 0.0778 166   0.313  0.9999 
# 3SP - FAKEA (PCOM)           0.28762 0.0796 166   3.612  0.0072 **
# 3SP - FAKEB (MCAP)           0.24087 0.0778 166   3.098  0.0364 *
# 3SP - MONOA (PCOM)           0.02091 0.0786 166   0.266  1.0000
# 3SP - MONOB (MCAP)           0.15792 0.0769 166   2.053  0.3857
# 3SP - sump                   0.33778 0.0702 166   4.810 <0.0001 ***
# 6SP - FAKEA (PCOM)           0.26327 0.0804 166   3.274  0.0216 *
# 6SP - FAKEB (MCAP)           0.21652 0.0786 166   2.756  0.0913 .
# 6SP - MONOA (PCOM)          -0.00344 0.0795 166  -0.043  1.0000
# 6SP - MONOB (MCAP)           0.13357 0.0778 166   1.718  0.6051
# 6SP - sump                   0.31343 0.0711 166   4.407  0.0004 ***
# FAKEA (PCOM) - FAKEB (MCAP) -0.04675 0.0804 166  -0.581  0.9973
# FAKEA (PCOM) - MONOA (PCOM) -0.26671 0.0813 166  -3.281  0.0211 *
# FAKEA (PCOM) - MONOB (MCAP) -0.12970 0.0796 166  -1.629  0.6638
# FAKEA (PCOM) - sump          0.05016 0.0732 166   0.686  0.9932
# FAKEB (MCAP) - MONOA (PCOM) -0.21996 0.0795 166  -2.768  0.0885 .
# FAKEB (MCAP) - MONOB (MCAP) -0.08295 0.0778 166  -1.067  0.9368
# FAKEB (MCAP) - sump          0.09691 0.0711 166   1.362  0.8208
# MONOA (PCOM) - MONOB (MCAP)  0.13701 0.0786 166   1.742  0.5889
# MONOA (PCOM) - sump          0.31687 0.0721 166   4.394  0.0004 ***
# MONOB (MCAP) - sump          0.17986 0.0702 166   2.561  0.1449
