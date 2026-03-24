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
  drop_na(temp)

#plot ALL data

temp <- ggplot(td, aes(x = date_time, y = temp, color = treatment, group = tank, shape = coral_no)) +
  geom_point() +
  geom_line() +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_datetime()

pH <- ggplot(td, aes(x = date_time, y = pH, color = treatment, group = tank, shape = coral_no)) +
  geom_point() +
  geom_line() +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_datetime()

DO <- ggplot(td, aes(x = date_time, y = DO, color = treatment, group = tank, shape = coral_no)) +
  geom_point() +
  geom_line() +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_datetime()

sal <- ggplot(td, aes(x = date_time, y = sal, color = treatment, group = tank, shape = coral_no)) +
  geom_point() +
  geom_line() +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_datetime()
  

tank_plots <- ggarrange(temp, pH, DO, sal, common.legend = T, nrow = 2, ncol = 2)
tank_plots

ggsave(here("Output/tank_plots.pdf"), tank_plots, h = 10, w = 10)

ed <- td %>%
  dplyr::filter(treatment != "acclimation")

temp <- ggplot(ed, aes(x = date_time, y = temp, color = treatment, group = tank)) +
  geom_point() +
  geom_line() +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_datetime()

pH <- ggplot(ed, aes(x = date_time, y = pH, color = treatment, group = tank)) +
  geom_point() +
  geom_line() +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_datetime()

DO <- ggplot(ed, aes(x = date_time, y = DO, color = treatment, group = tank)) +
  geom_point() +
  geom_line() +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_datetime()

sal <- ggplot(ed, aes(x = date_time, y = sal, color = treatment, group = tank, shape = coral_no)) +
  geom_point() +
  geom_line() +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_datetime()

expt_plots <- ggarrange(temp, pH, DO, sal, common.legend = T, nrow = 2, ncol = 2)
expt_plots

ggsave(here("Output/experimental_plots.pdf"), expt_plots, h = 10, w = 10)

se <- function(x) sd(x)/sqrt(length(x))

sumd <- td %>%
  dplyr::filter(treatment != "acclimation") %>% 
  mutate(timepoint = mdy_hm(timepoint)) %>%
  drop_na(temp) %>%
  group_by(timepoint, treatment) %>%
  dplyr::summarize(mean_temp = mean(temp),
                   se_temp = se(temp),
                   mean_pH = mean(pH),
                   se_pH = se(pH),
                   mean_DO = mean(DO),
                   se_DO = se(DO),
                   mean_sal = mean(sal),
                   se_sal = se(sal),
                   .groups = "drop")

temp_sum <- ggplot(sumd, aes(x = timepoint, y = mean_temp, color = treatment)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin=mean_temp+se_temp, ymax= mean_temp-se_temp))+
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_datetime()

pH_sum <- ggplot(sumd, aes(x = timepoint, y = mean_pH, color = treatment)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin=mean_pH+se_pH, ymax= mean_pH-se_pH))+
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_datetime()

DO_sum <- ggplot(sumd, aes(x = timepoint, y = mean_DO, color = treatment)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin=mean_DO+se_DO, ymax= mean_DO-se_DO))+
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_datetime()

sal_sum <- ggplot(sumd, aes(x = timepoint, y = mean_sal, color = treatment)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin=mean_sal+se_sal, ymax= mean_sal-se_sal))+
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_datetime()

sum_plots <- ggarrange(temp_sum, pH_sum, DO_sum, sal_sum, common.legend = T, nrow = 2, ncol = 2)
sum_plots
