# Description
# Combining data for modelling - April 2026

# Preliminaries -----------------------------------------------------------
library(here)

# Functions ---------------------------------------------------------------
source(here("packages.R"))
source(here("Functions", "fx_plot.R"))

# Import -------------------------------------------------------------
climate_shocks_tbl <- read_rds(here("Outputs", "artifacts_climate_shocks.rds")) |> 
  pluck(1)
credit_market <- read_rds(here("Outputs", "artifacts_credit_market.rds")) 
lending_tbl <- credit_market |>
  pluck(1) |> 
  mutate(Date = as.Date(Date)) |> 
  rename(date = 1) |> 
  janitor::clean_names() |> 
  dplyr::select(-other_assets) |> 
  group_by(bank) |> 
  mutate(across(.cols = c("corporate_unsecured_credit", 
                          "corporate_secured_credit", 
                          "corporate_sector_mortgages", 
                          "household_unsecured_credit", 
                          "household_secured_credit", 
                          "households_residential_mortgages"
                          ), 
                ~ log(.x) ))
lending_rates_tbl <- credit_market |>
  pluck(2) |> 
  mutate(Date = as.Date(Date)) |> 
  rename(date = 1) |> 
  janitor::clean_names()
surprises_tbl <- read_rds(here("Outputs", "artifacts_surprises.rds")) |> 
  pluck(2,1) |> 
  mutate(date = as.Date(date))

market_based_surprises_tbl <- read_rds(here("Outputs", "artifacts_market_based_surprises.rds")) |> 
  pluck(1)

# Transformations --------------------------------------------------------

## Climate shocks aggregation -------------
climate_temp_shocks_quarterly_tbl <- 
  climate_shocks_tbl |>
  dplyr::select(date, contains("temp")) |> 
  timetk::summarise_by_time(
    .date_var = date,
    .by = "quarter",
    across(.cols = contains("temp"),  .fns = mean)
  ) |> 
  mutate(date = date %-time% "1 day") 

climate_precip_shocks_quarterly_tbl <- 
climate_shocks_tbl |>
  dplyr::select(date, contains("precip")) |> 
  timetk::summarise_by_time(
    .date_var = date,
    .by = "quarter",
    across(.cols = contains("precip"),  .fns = sum)
  ) |> 
  mutate(date = date %-time% "1 day") 

## Lending aggregation ------------------
lending_quarterly_tbl <- 
  lending_tbl |>
  group_by(bank) |> 
  timetk::summarise_by_time(
    .date_var = date,
    .by = "quarter",
    across(.cols = everything() ,  .fns = sum) # maybe average
  ) |> 
  mutate(date = date %-time% "1 day") |> 
  ungroup()

lending_rates_quarterly_tbl <- 
  lending_rates_tbl |>
  group_by(bank) |> 
  timetk::summarise_by_time(
    .date_var = date,
    .by = "quarter",
    across(.cols = everything() ,  .fns = mean) 
  ) |> 
  mutate(date = date %-time% "1 day") |> 
  ungroup()

## Market-based Surprises aggregation ------------------
market_based_surprises_quarterly_tbl  <- 
  market_based_surprises_tbl |> 
  mutate(Quarter = as.yearqtr(Date)) |> 
  dplyr::select(-Date) |>
  group_by(Quarter) |>
  mutate(across(everything(), ~ mean(.x, na.rm = TRUE))) |> 
  ungroup() |>
  distinct(Quarter, .keep_all = TRUE) |> 
  rename(date = Quarter) |>
  mutate(date = as.Date(date)) |> 
  relocate(date, .before = everything()) |> 
  janitor::clean_names() |>
  mutate(date = date %-time% "1 day")  # check this again

## Making climate and surprises panel --------------------------
climate_temp_shocks_quarterly_panel_tbl <-
list(
  "ABSA BANK" = climate_temp_shocks_quarterly_tbl,
  "STANDARD BANK" = climate_temp_shocks_quarterly_tbl,
  "FIRSTRAND BANK" = climate_temp_shocks_quarterly_tbl,
  "NEDBANK" = climate_temp_shocks_quarterly_tbl,
  "CAPITEC BANK" = climate_temp_shocks_quarterly_tbl
) |> 
  bind_rows(.id = "bank") 

climate_precip_shocks_quarterly_panel_tbl <-
  list(
    "ABSA BANK" = climate_precip_shocks_quarterly_tbl,
    "STANDARD BANK" = climate_precip_shocks_quarterly_tbl,
    "FIRSTRAND BANK" = climate_precip_shocks_quarterly_tbl,
    "NEDBANK" = climate_precip_shocks_quarterly_tbl,
    "CAPITEC BANK" = climate_precip_shocks_quarterly_tbl 
  ) |>
  bind_rows(.id = "bank")

suprises_panel_tbl <-
  list(
    "ABSA BANK" = surprises_tbl,
    "STANDARD BANK" = surprises_tbl,
    "FIRSTRAND BANK" = surprises_tbl,
    "NEDBANK" = surprises_tbl,
    "CAPITEC BANK" = surprises_tbl 
  ) |>
  bind_rows(.id = "bank")

market_based_surprises_panel_tbl <-
  list(
    "ABSA BANK" = market_based_surprises_quarterly_tbl,
    "STANDARD BANK" = market_based_surprises_quarterly_tbl,
    "FIRSTRAND BANK" = market_based_surprises_quarterly_tbl,
    "NEDBANK" = market_based_surprises_quarterly_tbl,
    "CAPITEC BANK" = market_based_surprises_quarterly_tbl 
  ) |>
  bind_rows(.id = "bank")

# Combining ---------------------------------------------------------------
model_data_tbl <- 
  lending_quarterly_tbl |> 
  left_join(lending_rates_quarterly_tbl, by = c("bank", "date")) |> 
  left_join(climate_temp_shocks_quarterly_panel_tbl, by = c("bank", "date")) |> 
  left_join(climate_precip_shocks_quarterly_panel_tbl, by = c("bank", "date")) |> 
  left_join(suprises_panel_tbl, by = c("bank", "date")) |> 
  left_join(market_based_surprises_panel_tbl, by = c("bank", "date"))
  

# Export ---------------------------------------------------------------
artifacts <- list (
  model_data_tbl = model_data_tbl
)

write_rds(artifacts, file = here("Outputs", "artifacts_model_data.rds"))


