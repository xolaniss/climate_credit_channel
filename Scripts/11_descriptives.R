# Description ---------------------------------------------------------
# Descriptive statistics and plots for modelling dataset - May 2026
# summary statistics table, time-series plots, density plots, boxplots
# Notes from descriptives: 1. corporate_sector_mortgages, corporate_secured_credit, 
# household_secured_credit and households_residential_mortgages must have log(0) in the series.


# Preliminaries ---------------------------------
library(here)

# Functions -----------------------------------------------
source(here("packages.R"))
source(here("Functions", "fx_plot.R"))

# Import ----------------------------------------------
model_data_tbl <- read_rds(
  here("Outputs", "artifacts_model_data.rds")
) |>
  pluck(1)
# Descriptive statistics ------------------------------------

## Summary statistics table -------------
descriptive_stats_tbl <-
  model_data_tbl |>
  select(-ends_with("pos"), -ends_with("neg"), -ends_with("value"), -starts_with("land")) |> 
  pivot_longer(-c("date", "banks"), names_to = "credit_category", values_to = "value") |> 
  mutate(groups = if_else(str_ends(credit_category, "rate"), "Lending rates", 
                          if_else(str_ends(credit_category, "credit|mortgages"), "Credit growth", 
                                  if_else(str_ends(credit_category,"shock"), "Climate shock", "Monetary policy shock"))))|> 
  group_by(groups, credit_category) |> 
  drop_na() |> 
  summarise(
    across(
      .cols = -c(date, banks),
      .fns = list(
        mean   = ~ mean(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE),
        sd     = ~ sd(.x, na.rm = TRUE),
        min    = ~ min(.x, na.rm = TRUE),
        max    = ~ max(.x, na.rm = TRUE),
        n      = ~ n()
      ),
      .names = "{.fn}"
    )
  ) 


# Create artifacts object -----------------------
artifacts <- list(
  descriptive_stats_tbl = descriptive_stats_tbl
)

write_rds(
  artifacts,
  file = here(
    "Outputs",
    "artifacts_descriptive_statistics.rds"
  )
)
