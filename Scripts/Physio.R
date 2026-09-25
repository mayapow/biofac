#Physio data
#Biodiversity facilitation experiment
#HIMB Spring 2026
#Maya Powell

#load libraries
library(ggplot2)
library(here)
library(ggpubr)
library(lubridate)
library(tidyverse)
library(car)
library(dplyr)

#read in data

bw_initial <- read.csv(here("Data/Physiology/BuoyantWeight_Initial.csv"))
bw_initial <- bw_initial %>% filter(sample_ID != "STD") %>% dplyr::select(sample_ID, weight_g_T0)
bw_final <- read.csv(here("Data/Physiology/BuoyantWeight_Final.csv"))
bw_final <- bw_final %>% filter(sample_ID != "STD") %>% 
  dplyr::select(sample_ID, weight_g_T1, tank, treatment, species)

bw <- bw_initial %>% 
  left_join(bw_final, by = "sample_ID") %>%
  drop_na() %>%
  mutate(weight_dif = weight_g_T1 - weight_g_T0) %>%
  mutate(percent_change = ((weight_g_T1-weight_g_T0)/abs(weight_g_T0))*100) %>%
  filter(species == "PCOM" | species == "MCAP") %>%
  filter(weight_dif > 0)

ggplot(bw, aes(x = treatment, y = weight_dif, color = treatment)) +
  geom_jitter() +
  geom_boxplot() +
  facet_wrap(~species, scales = "free")

ggplot(bw, aes(x = treatment, y = percent_change, color = treatment)) +
  geom_jitter() +
  geom_boxplot() +
  facet_wrap(~species, scales = "free")

ggplot(bw, aes(x = species, y = percent_change, color = species)) +
  geom_point() +
  geom_boxplot() 

ggplot(bw, aes(x = treatment, y = percent_change, color = treatment)) +
  geom_jitter() +
  geom_boxplot() +
  facet_wrap(.~species, scales = "free")
