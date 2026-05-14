##fDOM Processing
##Claire Moreland-Ochoa

library("here")
library("eemR")
library("tidyverse")

# define base directory (only thing to change per project if needed)
base_dir <- "Scripts/aqualog_fdom"

# load functions
source(file.path(base_dir, "aqualog", "process_aqualog_functions.R"))
source(file.path(base_dir, "aqualog", "eem_plot_functions.R"))

base_dir_data <- "Data/aqualog_fdom"


# process normal runs
run1 <- process_aqualog(
  data_directory = file.path(base_dir_data, "run1"),
  run_name = "run1",
  sample_key_file = "SampleDataSheet.txt"
)

run2 <- process_aqualog(
  data_directory = file.path(base_dir_data, "run2"),
  run_name = "run2",
  sample_key_file = "SampleDataSheet.txt"
)

run3 <- process_aqualog(
  data_directory = file.path(base_dir_data, "run3"),
  run_name = "run3",
  sample_key_file = "SampleDataSheet.txt"
)

run4 <- process_aqualog(
  data_directory = file.path(base_dir_data, "run4"),
  run_name = "run4",
  sample_key_file = "SampleDataSheet.txt"
)

run5 <- process_aqualog(
  data_directory = file.path(base_dir_data, "run5"),
  run_name = "run5",
  sample_key_file = "SampleDataSheet.txt"
)

run6 <- process_aqualog(
  data_directory = file.path(base_dir_data, "run6"),
  run_name = "run6",
  sample_key_file = "SampleDataSheet.txt"
)

run7 <- process_aqualog(
  data_directory = file.path(base_dir_data, "run7"),
  run_name = "run7",
  sample_key_file = "SampleDataSheet.txt"
)

run8 <- process_aqualog(
  data_directory = file.path(base_dir_data, "run8"),
  run_name = "run8",
  sample_key_file = "SampleDataSheet.txt"
)

# re-run with custom org sheet if needed to fix blank assignments
# uncomment only if needed

run1 <- process_aqualog(
  data_directory = file.path(base_dir_data, "run1"),
  run_name = "run1",
  sample_key_file = "SampleDataSheet.txt",
  org_file = "processed_data/run1_sample_sheet_clean.csv"
)

run2 <- process_aqualog(
  data_directory = file.path(base_dir_data, "run2"),
  run_name = "run2",
  sample_key_file = "SampleDataSheet.txt",
  org_file = "processed_data/run2_sample_sheet_clean.csv"
)

run3 <- process_aqualog(
  data_directory = file.path(base_dir_data, "run3"),
  run_name = "run3",
  sample_key_file = "SampleDataSheet.txt",
  org_file = "processed_data/run3_sample_sheet_clean.csv"
)

run4 <- process_aqualog(
  data_directory = file.path(base_dir_data, "run4"),
  run_name = "run4",
  sample_key_file = "SampleDataSheet.txt",
  org_file = "processed_data/run4_sample_sheet_clean.csv"
)

run5 <- process_aqualog(
  data_directory = file.path(base_dir_data, "run5"),
  run_name = "run5",
  sample_key_file = "SampleDataSheet.txt",
  org_file = "processed_data/run5_sample_sheet_clean.csv"
)

run8 <- process_aqualog(
  data_directory = file.path(base_dir_data, "run8"),
  run_name = "run8",
  sample_key_file = "SampleDataSheet.txt",
  org_file = "processed_data/run8_sample_sheet_clean.csv"
)

# automatically grab all run folders
run_dirs <- list.dirs(path = base_dir_data, full.names = FALSE, recursive = FALSE)

run_dirs <- run_dirs[grepl("^run", run_dirs)]

# sanity check
print(run_dirs)

# compile all runs
compiled <- compile_runs(
  run_dirs = file.path(base_dir_data, run_dirs),
  out_dir = file.path(base_dir_data, "compiled_runs")
)


# fDOM INDICES – quick reference / what these actually mean


# CORE DOM CHARACTERIZATION
# -------------------------------
# FI (Fluorescence Index)
# - 470 / 520 at Ex = 370
# - Basically tells you where the DOM is coming from
#     ~1.2–1.4 → more terrestrial / plant-derived
#     ~1.4–1.6 → mixed
#     ~1.6–1.9 → more microbial / autochthonous
# - Pretty standard index, used everywhere

# HIX (Humification Index)
# - ratio of higher emission (434–480) to lower emission (300–346) at Ex = 255
# - tells you how "processed" or humified the DOM is
#     ~1–5   → fresh / microbial DOM
#     ~5–10  → somewhat humified
#     >10    → really humified / terrestrial
# - can blow up (huge values or Inf) if the denominator is tiny → just keep that in mind

# BIX (Biological Index)
# - 380 / 430 at Ex = 310
# - indicator of recent biological production
#     ~0.6–0.8 → not much biological signal
#     ~0.8–1.0 → moderate
#     >1.0     → strong microbial contribution


# STRUCTURE / COMPOSITION
# -------------------------------
# Coble peaks = fluorescence intensity at specific Ex/Em combos
# these are more about "what kind" of DOM is there

# CobleA (320 / 450)
# - humic-like, more terrestrial/aromatic DOM

# CobleC (345 / 445)
# - also humic-like, often dominant in natural waters

# CobleB (275 / 305)
# - protein-like (tyrosine)
# - very fresh / labile DOM signal

# Fpeak (240 / 299)
# - general protein-like fluorescence
# - can be biological production or contamination depending on context

# CobleM (310 / 410)
# - microbial humic-like → kind of processed/microbial DOM

# CobleT (275 / 340)
# - protein-like (tryptophan)
# - often tied to microbes, wastewater, labile DOM

# M_to_C (CobleM / CobleC)
# - quick way to look at microbial vs humic balance
#     higher → more microbial
#     lower  → more humic/terrestrial


# GENERAL NOTES
# -------------------------------
# - none of these are absolute → best used for comparisons
# - usually plotted as:
#     x = group / treatment / site
#     y = index value


# plots are of FI, Fpeak, CobleB, CobleA, CobleT, M_to_C
#these inidces were recommended by Zach Quinlan as the ones to focus on 

library(tidyr)

indices <- read.csv(file.path(base_dir_data, "compiled_runs", "all_sample_indices.csv"))

metadata <- read.csv(file.path(base_dir_data, "metadata.csv")) 
data <- left_join(indices, metadata, by = "UniqueID")
data <- data %>%
   mutate(timepoint = as.POSIXct(timepoint, format = "%m/%d/%Y %H:%M"),
          date = as.Date(date, format = "%m/%d/%Y"))

 
# keep the full high-frequency window:
# Apr 6 at 6 AM, 12 PM, 6 PM
# Apr 7 at 12 AM, 6 AM
fullday_data <- data %>%
   filter( timepoint %in% as.POSIXct(c(
       "2026-04-06 06:00","2026-04-06 12:00","2026-04-06 18:00","2026-04-07 00:00","2026-04-07 06:00"))
   ) %>%
   mutate(
     timepoint_f = factor(
       format(timepoint, "%b %d %I%p"),
       levels = c("Apr 06 06AM","Apr 06 12PM","Apr 06 06PM","Apr 07 12AM","Apr 07 06AM"))
   )
 
fullday_long <- fullday_data %>%
   pivot_longer(
     cols = c(FI, Fpeak, CobleB, CobleA, CobleT, M_to_C),
     names_to = "Index",
     values_to = "Value"
   )
 
ggplot(fullday_long, aes(x = timepoint_f, y = Value, fill = treatment)) +
   geom_boxplot(alpha = 0.7,outlier.size = 1) +
   facet_wrap(~Index, scales = "free_y") +
   theme_bw() +
   scale_fill_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red", "black"))+
   labs(
     title = "High-frequency sampling (April 6–7, 2026)",
     x = "Timepoint",
     y = "Value") +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(here("Output", "fullday_fdom.jpg"),
       height = 5, width = 10, units = "in")
 
outliers_fullday_long <- fullday_long %>%
  group_by(Index) %>%
  mutate(
    Q1 = quantile(Value, 0.25, na.rm = TRUE),
    Q3 = quantile(Value, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    lower = Q1 - 1.5 * IQR,
    upper = Q3 + 1.5 * IQR,
    is_outlier = Value < lower | Value > upper
  ) %>%
  ungroup() %>%
  filter(is_outlier) %>%
  mutate(source = "fullday")

outlier_fullday_summary <- outliers_fullday_long %>%
  group_by(UniqueID) %>%
  summarise(
    n_outlier_rows = n(),
    indices = paste(unique(Index), collapse = ", "),
    sources = paste(unique(source), collapse = ", "),
    .groups = "drop"
  ) %>%
  mutate(
    qc_flag = case_when(
      n_outlier_rows >= 4 ~ "Strong QA/QC flag — inspect raw EEM and consider rerun",
      n_outlier_rows == 3 ~ "Inspect raw EEM and compare with related samples",
      n_outlier_rows == 2 ~ "Moderate deviation — likely biological but worth reviewing",
      TRUE ~ "Minor deviation — likely normal variability"
    )
  ) %>%
  arrange(desc(n_outlier_rows))
 
#Weekly data
main_data <- data %>%
   filter(date != as.Date("2026-04-06"),
          date != as.Date("2026-04-07")) 
 
main_long <- main_data %>%
  pivot_longer(cols = c(FI, Fpeak, CobleB, CobleA, CobleT, M_to_C),
    names_to = "Index",
    values_to = "Value")

# split day and night
day_data <- main_long %>%
  filter(day_night == "day")

night_data <- main_long %>%
  filter(day_night == "night")


# DAY — core characterization

day_core <- day_data %>%
  filter(Index %in% c("FI", "M_to_C"))

ggplot(day_core, aes(x = factor(date), y = Value, fill = treatment)) +
  geom_boxplot( alpha = 0.7,outlier.size = 1) +
  facet_wrap(~Index, scales = "free_y") +
  theme_bw() +
  scale_fill_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red", "black"))+
  labs(
    title = "Day fDOM — Core Characterization",
    x = "Date",
    y = "Value",
    fill = "Treatment") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(here("Output", "day_core_fdom.jpg"),
       height = 5, width = 10, units = "in")

outliers_day_core <- day_core %>%
  group_by(Index) %>%
  mutate(
    Q1 = quantile(Value, 0.25, na.rm = TRUE),
    Q3 = quantile(Value, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    lower = Q1 - 1.5 * IQR,
    upper = Q3 + 1.5 * IQR,
    is_outlier = Value < lower | Value > upper
  ) %>%
  filter(is_outlier)

# DAY — peak structure

day_peaks <- day_data %>%
  filter(Index %in% c("CobleA", "CobleB", "CobleT", "Fpeak"))

ggplot(day_peaks, aes(x = factor(date), y = Value, fill = treatment)) +
  geom_boxplot(alpha = 0.7,outlier.size = 1) +
  facet_wrap(~Index, scales = "free_y") +
  theme_bw() +
  scale_fill_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red", "black"))+
  labs(
    title = "Day fDOM — Peak Structure",
    x = "Date",
    y = "Value",
    fill = "Treatment") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(here("Output", "day_peak_fdom.jpg"),
       height = 6, width = 12, units = "in")

outliers_day_peaks <- day_peaks %>%
  group_by(Index) %>%
  mutate(
    Q1 = quantile(Value, 0.25, na.rm = TRUE),
    Q3 = quantile(Value, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    lower = Q1 - 1.5 * IQR,
    upper = Q3 + 1.5 * IQR,
    is_outlier = Value < lower | Value > upper
  ) %>%
  filter(is_outlier)

# NIGHT — core characterization

night_core <- night_data %>%
  filter(Index %in% c("FI", "M_to_C"))

ggplot(night_core, aes(x = factor(date), y = Value, fill = treatment)) +
  geom_boxplot( alpha = 0.7,outlier.size = 1) +
  facet_wrap(~Index, scales = "free_y") +
  theme_bw() +
  scale_fill_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red", "black"))+
  labs(
    title = "Night fDOM — Core Characterization",
    x = "Date",
    y = "Value",
    fill = "Treatment") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(here("Output", "night_core_fdom.jpg"),
       height = 5, width = 10, units = "in")

outliers_night_core <- night_core %>%
  group_by(Index) %>%
  mutate(
    Q1 = quantile(Value, 0.25, na.rm = TRUE),
    Q3 = quantile(Value, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    lower = Q1 - 1.5 * IQR,
    upper = Q3 + 1.5 * IQR,
    is_outlier = Value < lower | Value > upper
  ) %>%
  filter(is_outlier)

# NIGHT — peak structure

night_peaks <- night_data %>%
  filter(Index %in% c("CobleA", "CobleB", "CobleT", "Fpeak"))

ggplot(night_peaks, aes(x = factor(date), y = Value, fill = treatment)) +
  geom_boxplot( alpha = 0.7,outlier.size = 1) +
  facet_wrap(~Index, scales = "free_y") +
  theme_bw() +
  scale_fill_manual(values = c("steelblue","purple","lightgray","darkgray","lightgreen","red", "black"))+
  labs(
    title = "Night fDOM — Peak Structure",
    x = "Date",
    y = "Value",
    fill = "Treatment") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(here("Output", "night_peak_fdom.jpg"),
       height = 6, width = 12, units = "in")

outliers_night_peaks <- night_peaks %>%
  group_by(Index) %>%
  mutate(
    Q1 = quantile(Value, 0.25, na.rm = TRUE),
    Q3 = quantile(Value, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    lower = Q1 - 1.5 * IQR,
    upper = Q3 + 1.5 * IQR,
    is_outlier = Value < lower | Value > upper
  ) %>%
  filter(is_outlier)

outliers_day_core$source <- "day_core"
outliers_day_peaks$source <- "day_peak"
outliers_night_core$source <- "night_core"
outliers_night_peaks$source <- "night_peak"

all_outliers <- bind_rows(
  outliers_day_core,
  outliers_day_peaks,
  outliers_night_core,
  outliers_night_peaks
)

all_outliers %>%
  group_by(UniqueID) %>%
  summarise(
    n_outlier_flags = n(),
    indices = paste(unique(Index), collapse = ", "),
    sources = paste(unique(source), collapse = ", ")
  ) %>%
  arrange(desc(n_outlier_flags))

outlier_summary <- all_outliers %>%
  group_by(UniqueID) %>%
  summarise(
    n_outlier_rows = n(),
    indices = paste(unique(Index), collapse = ", "),
    sources = paste(unique(source), collapse = ", "),
    .groups = "drop"
  ) %>%
  mutate(
    qc_flag = case_when(
      n_outlier_rows >= 4 ~ "Strong QA/QC flag — inspect raw EEM and consider rerun",
      n_outlier_rows == 3 ~ "Inspect raw EEM and compare with related samples",
      n_outlier_rows == 2 ~ "Moderate deviation — likely biological but worth reviewing",
      TRUE ~ "Minor deviation — likely normal variability"
    )
  ) %>%
  arrange(desc(n_outlier_rows))
