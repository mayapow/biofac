
# define base directory (only thing to change per project if needed)
base_dir <- "Data/aqualog_fdom"

# load functions
source(file.path(base_dir, "aqualog", "process_aqualog_functions.R"))
source(file.path(base_dir, "aqualog", "eem_plot_functions.R"))




# process normal runs
run1 <- process_aqualog(
  data_directory = file.path(base_dir, "run1"),
  run_name = "run1",
  sample_key_file = "SampleDataSheet.txt"
)

run2 <- process_aqualog(
  data_directory = file.path(base_dir, "run2"),
  run_name = "run2",
  sample_key_file = "SampleDataSheet.txt"
)

run3 <- process_aqualog(
  data_directory = file.path(base_dir, "run3"),
  run_name = "run3",
  sample_key_file = "SampleDataSheet.txt"
)

# re-run with custom org sheet if needed to fix blank assignments
# uncomment only if needed

run1 <- process_aqualog(
  data_directory = file.path(base_dir, "run1"),
  run_name = "run1",
  sample_key_file = "SampleDataSheet.txt",
  org_file = "processed_data/run1_sample_sheet_clean.csv"
)

run2 <- process_aqualog(
  data_directory = file.path(base_dir, "run2"),
  run_name = "run2",
  sample_key_file = "SampleDataSheet.txt",
  org_file = "processed_data/run2_sample_sheet_clean.csv"
)

run3 <- process_aqualog(
  data_directory = file.path(base_dir, "run3"),
  run_name = "run3",
  sample_key_file = "SampleDataSheet.txt",
  org_file = "processed_data/run3_sample_sheet_clean.csv"
)

# automatically grab all run folders
run_dirs <- list.dirs(path = base_dir, full.names = FALSE, recursive = FALSE)

run_dirs <- run_dirs[grepl("^run", run_dirs)]

# sanity check
print(run_dirs)

# compile all runs
compiled <- compile_runs(
  run_dirs = file.path(base_dir, run_dirs),
  out_dir = file.path(base_dir, "compiled_runs")
)

# plot the first EEMs for checks and giggles
plot_eem(run1$EEMs[[1]], rows_as_names = T, 
         sample_name = names(run1$EEMs)[1])

plot_eem(run1$EEMs[[2]], rows_as_names = T, 
         sample_name = names(run1$EEMs)[2])


# check some indices (should have mostly normal distributions)
# if something is weird, first check that blanks are clear

hist(run1$indices$CobleA, breaks = 15)
hist(run1$indices$M_to_C, breaks = 15)
hist(run1$indices$CobleT, breaks = 15)


# fDOM INDICES – quick reference / what these actually mean


# -------------------------------
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
# - nice to pair with FI

# -------------------------------
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

# -------------------------------
# GENERAL NOTES
# -------------------------------

# - none of these are absolute → best used for comparisons
# - usually plotted as:
#     x = group / treatment / site
#     y = index value
# - FI + HIX + BIX together give a really solid snapshot of DOM source + processing
# ============================================================


library(dplyr)
library(tidyr)
library(ggplot2)

indices <- read.csv(file.path(base_dir, "compiled_runs", "all_sample_indices.csv"))

metadata <- read.csv(file.path(base_dir, "metadata.csv")) 
data <- left_join(indices, metadata, by = "UniqueID")
 data <- data %>%
   mutate(timepoint = as.POSIXct(timepoint, format = "%m/%d/%Y %H:%M"))
 data <- data %>%
   mutate(date = as.Date(date, format = "%m/%d/%Y"))
 
 main_data <- data %>%
   filter(date != as.Date("2026-04-06"))
 
fullday_data <- data %>%
   filter(date == as.Date("2026-04-06"))

fullday_long <- fullday_data %>%
  pivot_longer(
    cols = c(FI, Fpeak, CobleB, CobleA, CobleT, M_to_C),
    names_to = "Index",
    values_to = "Value"
  )

ggplot(fullday_long, aes(x = time, y = Value, fill = treatment)) +
  geom_boxplot() +
  facet_wrap(~Index, scales = "free_y") +
  theme_bw() +
  labs(
    title = "High-frequency sampling (April 6, 2026)",
    x = "Time",
    y = "Value"
  )+
 theme(axis.text.x = element_text(angle = 45, hjust = 1))

main_long <- main_data %>%
  pivot_longer(
    cols = c(FI, Fpeak, CobleB, CobleA, CobleT, M_to_C),
    names_to = "Index",
    values_to = "Value"
  )

 ggplot(main_long, aes(x = date, y = Value, fill = treatment)) +
   geom_boxplot(outlier.shape = NA, alpha = 0.7) +
   facet_grid(Index~day_night, scales = "free_y") +
   theme_bw() +
   labs(
     x = "Timepoint",
     y = "Value",
     fill = "Treatment",
     color = "Treatment"
   ) +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))
