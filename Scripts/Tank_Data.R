#Tank data
#Biodiversity facilitation experiment
#HIMB Spring 2026
#Maya Powell

#load libraries
library(ggplot2)
library(here)
library(ggpubr)
library(lubridate)

#read in data

td <- read.csv(here("Data/tank_data_biofac.csv"))

td <- td %>%
  mutate(date_time = paste(date,pH_time)) %>%
  mutate(date_time = mdy_hm(date_time)) %>%
  mutate(tank = as.factor(tank)) #%>% 
  #dplyr::filter(treatment != "acclimation")

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

expt_plots <- ggarrange(temp, pH, DO, common.legend = T, nrow = 2, ncol = 2)
expt_plots

ggsave(here("Output/experimental_plots.pdf"), expt_plots, h = 10, w = 10)

#sunset at 18:38 so during sampling on 3/6
#flow rates all over the place so residence times super unpredictable and weird
#for initial tank data - much better!!
