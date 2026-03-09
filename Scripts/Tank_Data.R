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
  mutate(date_time = paste(date,pH_time))


#plot data

temp <- ggplot(td, aes(x = date_time, y = temp, color = as.factor(tank), shape = coral_no)) +
  geom_point() +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 90))

pH <- ggplot(td, aes(x = date_time, y = pH, color = as.factor(tank), shape = coral_no)) +
  geom_point() +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 90))

DO <- ggplot(td, aes(x = date_time, y = DO, color = as.factor(tank), shape = coral_no)) +
  geom_point() +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 90))

sal <- ggplot(td, aes(x = date_time, y = sal, color = as.factor(tank), shape = coral_no)) +
  geom_point() +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 90))
  

tank_plots <- ggarrange(temp, pH, DO, sal, common.legend = T, nrow = 2, ncol = 2)
tank_plots

ggsave(here("Output/tank_plots.pdf"), tank_plots, h = 10, w = 10)

#sunset at 18:38 so during sampling on 3/6
#flow rates all over the place so residence times super unpredictable and weird
