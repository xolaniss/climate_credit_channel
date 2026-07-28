# Description
## type of shock extension

# Preliminaries -----------------------------------------------------------
library(here)

# Functions ---------------------------------------------------------------
source(here("packages.R"))
source(here("Functions", "fx_plot.R"))

# Import -------------------------------------------------------------
model_data_tbl <- read_rds(here("Outputs", "artifacts_model_data.rds")) |> 
  pluck(1)
  
# Positive-Negative Indicator ---------------------------------------------------------
model_data_type_of_shock_tbl <- model_data_tbl |> 
  mutate(pop_temp_shock_indicator = if_else(pop_temp_shock >= 0, 1, 0)) |> 
  mutate(pop_precip_shock_indicator = if_else(pop_precip_shock >= 0, 1, 0)) |> 
  mutate(winter_months_indicator = if_else(between(month(date), 6, 8), 1, 0)) |> 
  mutate(spring_months_indicator = if_else(between(month(date), 9, 11), 1, 0)) |> 
  mutate(autom_months_indicator = if_else(between(month(date), 3, 5), 1, 0)) |> 
  mutate(summer_months_indicator = if_else(month(date) %in% c(12, 1, 2), 1, 0))

# Export ---------------------------------------------------------------
artifacts_type_of_shock_extension <- list (
  model_data_type_of_shock_tbl = model_data_type_of_shock_tbl
)

write_rds(artifacts_type_of_shock_extension, file = here("Outputs", "artifacts_type_of_shock_extension.rds"))
  
model_data_type_of_shock_tbl |> 
  skim()
