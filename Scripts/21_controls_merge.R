# Description ---------------------------------------------------------
# Merge controls - August 2026

# Preliminaries -------------------------------------------------------
library(here)

# Functions -----------------------------------------------------------
source(here("packages.R"))

# Import --------------------------------------------------------------
model_data_tbl <- read_rds(
  here("Outputs","artifacts_model_data.rds")
) |>
  pluck("model_data_tbl")

gdp_tbl <- read_rds(
  here("Outputs","artifacts_gdp.rds")
) |>
  pluck("gdp_tbl")

unemployment_tbl <- read_rds(
  here("Outputs","artifacts_unemployment.rds")
) |>
  pluck("unemployment_tbl")

exchange_rate_tbl <- read_rds(
  here("Outputs","artifacts_exchange_rate.rds")
) |>
  pluck("exchange_rate_tbl")

# Check date ranges ---------------------------------------------------
model_data_tbl |>
  summarise(
    min_date = min(date,na.rm = TRUE),
    max_date = max(date,na.rm = TRUE)
  )

gdp_tbl |>
  summarise(
    min_date = min(date,na.rm = TRUE),
    max_date = max(date,na.rm = TRUE)
  )

unemployment_tbl |>
  summarise(
    min_date = min(date,na.rm = TRUE),
    max_date = max(date,na.rm = TRUE)
  )

exchange_rate_tbl |>
  summarise(
    min_date = min(date,na.rm = TRUE),
    max_date = max(date,na.rm = TRUE)
  )

# Keep only variables required for the merge -------------------------
gdp_merge_tbl <- gdp_tbl |>
  select(
    date,
    gdp,
    log_gdp,
    gdp_growth,
    gdp_growth_yoy
  )

unemployment_merge_tbl <- unemployment_tbl |>
  select(
    date,
    unemployment_rate
  )

exchange_rate_merge_tbl <- exchange_rate_tbl |>
  select(
    date,
    usd_zar,
    log_usd_zar,
    usd_zar_change,
    usd_zar_change_yoy
  )

# Merge macroeconomic variables --------------------------------------
model_data_tbl <- model_data_tbl |>
  left_join(gdp_merge_tbl,by = "date") |>
  left_join(unemployment_merge_tbl,by = "date") |>
  left_join(exchange_rate_merge_tbl,by = "date")

# Check merged data ---------------------------------------------------
model_data_tbl |>
  summarise(
    min_date = min(date,na.rm = TRUE),
    max_date = max(date,na.rm = TRUE),
    n_rows = n(),
    n_banks = n_distinct(banks),
    missing_gdp = sum(is.na(gdp)),
    missing_unemployment = sum(is.na(unemployment_rate)),
    missing_exchange_rate = sum(is.na(usd_zar))
  )

# Check last observations --------------------------------------------
model_data_tbl |>
  select(
    banks,
    date,
    gdp,
    log_gdp,
    gdp_growth,
    gdp_growth_yoy,
    unemployment_rate,
    usd_zar,
    log_usd_zar,
    usd_zar_change,
    usd_zar_change_yoy
  ) |>
  arrange(date,banks) |>
  slice_tail(n = 20)

# Check Q1 2026 -------------------------------------------------------
model_data_tbl |>
  filter(date == as.Date("2026-03-31")) |>
  select(
    banks,
    date,
    gdp,
    log_gdp,
    gdp_growth,
    gdp_growth_yoy,
    unemployment_rate,
    usd_zar,
    log_usd_zar,
    usd_zar_change,
    usd_zar_change_yoy
  )

# Export --------------------------------------------------------------
artifacts <- list(
  model_data_tbl = model_data_tbl
)

write_rds(
  artifacts,
  file = here("Outputs","artifacts_model_data.rds")
)