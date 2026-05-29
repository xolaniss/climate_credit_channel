# Description
# Credit data from bank capital project - March 2026

# Find a way to up to 2024 or 2025

# Preliminaries -----------------------------------------------------------
library(here)

# Functions ---------------------------------------------------------------
source(here("packages.R"))
source(here("Functions", "fx_plot.R"))

# Import and cleaning -------------------------------------------------------------
credit_tbl <- read_rds(here("Data", "artifacts_combined_banks_monthly.rds")) |> 
  pluck(3) |> 
  relocate(Date, .before = Bank) |> 
  pivot_longer(-c(Date, Bank), names_to = "Series", values_to = "Value") |> 
  mutate(Series = str_replace_all(Series, "Non-financial ", "")) |> 
  mutate(Series = str_replace_all(Series, "Household sector", "Household")) |>
  mutate(Series = str_replace_all(Series, "corporate", "Corporate")) |> 
  pivot_wider(names_from = "Series", values_from = "Value") |> 
  # replace 0s with NA
  mutate(across(-c(Date, Bank), ~ if_else(.x == 0, NA, .x)))
  

lending_tbl <- read_rds(here("Data", "artifacts_BA930_futher_analysis.rds")) |> 
  pluck(1,2)


# Export ---------------------------------------------------------------
artifacts_credit_market <- list (
  credit_tbl = credit_tbl,
  lending_tbl = lending_tbl
)

write_rds(artifacts_credit_market, file = here("Outputs", "artifacts_credit_market.rds"))


