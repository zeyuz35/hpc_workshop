# Read in Results ------------------------------------------------------
library(tidyverse)
library(stringr)
library(kableExtra)

results_names <- list.files("./results", full.names = TRUE)
# remove non RDS files
results_names <- results_names |> str_subset(".RDS") |> as.list()
# read in results
results_list <- lapply(results_names, readRDS)

# bind all results together
results_df <- bind_rows(results_list)

results_df

# Create nice looking summary statistics using group_by |> summarise

results_summary <- results_df |> 
  group_by(bigN, rho, gamma) |>
  summarise(
    beta_hat = mean(beta_hat), 
    beta_hat_se = mean(beta_hat_se)
  ) 

# Optionally convert to latex and cat out
results_summary |>
  kable(format = "latex") |>
  cat()
