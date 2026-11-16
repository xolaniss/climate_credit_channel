# Description
# Calculating alternative climate shocks - Xolani Sibande September 2025
# Preliminaries -----------------------------------------------------------
library(here)

# Functions ---------------------------------------------------------------
source(here("packages.R"))

# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))
# Shock = deviation from the rolling calendar-month mean, in rolling-sd units.
# The window holds the latest `window_years` observations of the same calendar
# month (including the current one), so 30 years = 360 months. Dates without a
# full window are NA and dropped.
month_based_shock <- function(data, variable_filter, window_years = 30){
  
  data |>
    dplyr::select(date, value = all_of(variable_filter)) |>
    mutate(month = month(date)) |>
    group_by(month) |>
    arrange(date, .by_group = TRUE) |>
    mutate(
      roll_mean = rollapplyr(value, width = window_years, FUN = mean, na.rm = TRUE, fill = NA),
      roll_sd   = rollapplyr(value, width = window_years, FUN = sd,   na.rm = TRUE, fill = NA),
      shock     = (value - roll_mean) / roll_sd
    ) |>
    ungroup() |>
    arrange(date) |>
    dplyr::select(date, value, roll_mean, roll_sd, shock) |>
    rename_with(~ paste0(variable_filter, "_", .x), .cols = -date) |>
    drop_na()
}

# Import -------------------------------------------------------------
climate_data_tbl <- read_rds(here("Outputs", "artifacts_climate_data.rds")) |>
  pluck(1)



climate_data_tbl |> 
  pivot_longer(cols = -c(date), names_to = "variable", values_to = "value") |>
  mutate(month = month(date)) 
  

# Climate shocks ---------------------------------------------------------------
args_list <- list(
  c("land_temp"),
  c("pop_temp"),
  c("land_precip"),
  c("pop_precip")
)


roll_window_years <- 30 # same-month observations per window (30 years = 360 months)

climate_shocks_list <-
  args_list |>
  map(
    ~ month_based_shock(
      data = climate_data_tbl,
      variable_filter = .x[1],
      window_years = roll_window_years
    )
  )

# Combine all shocks into one tbl --------------------------------------------
climate_shocks_full_tbl <-
  climate_shocks_list |>
  reduce(left_join, by = "date") |>
  ungroup() |>
  filter(date >= "2012-12-01")

# Kept apart from the shocks: 10_model_data.R aggregates every column whose
# name contains "temp" or "precip", so these must not enter climate_shocks_tbl.
climate_shocks_stats_tbl <-
  climate_shocks_full_tbl |>
  dplyr::select(date, ends_with("_roll_mean"), ends_with("_roll_sd"))

climate_shocks_tbl <-
  climate_shocks_full_tbl |>
  dplyr::select(-ends_with("_roll_mean"), -ends_with("_roll_sd")) |>
  
 
# Create positive and negative climate shock variables -----------------

mutate(
  
  across(
    ends_with("_shock"),
    ~ if_else(. > 0, ., 0),
    .names = "{.col}_pos"
  ),
  
  across(
    ends_with("_shock"),
    ~ if_else(. < 0, abs(.), 0),
    .names = "{.col}_abs_neg"
  )
  
)


# EDA ---------------------------------------------------------------------
climate_shocks_tbl |>
  skim()


# Example Graphing ---------------------------------------------------------------
climate_shocks_gg <-
  climate_shocks_tbl |>
  dplyr::select(date, ends_with("_shock")) |>
  pivot_longer(cols = -c(date), names_to = "variable", values_to = "value") |>
  mutate(
    variable = str_replace_all(
      variable,
      c("pop_temp_shock" = "Population Temperature Shock",
        "land_temp_shock" = "Land Temperature Shock",
        "pop_precip_shock" = "Population Precipitation Shock",
        "land_precip_shock" = "Land Precipitation Shock"
    ))) |>
  filter(
    !variable %in% c("Land Temperature Shock", "Land Precipitation Shock")
  ) |> 
  ggplot(aes(x = date, y = value, col = variable)) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y", ncol = 2) +
  labs(
    title = "",
    y = "Shock",
    x = "",
    col = "Shock"
  ) +
  theme_minimal(base_size = 8) +
  theme(legend.position = "") +
  scale_x_date(date_labels = "%Y", date_breaks = "4 years") +
  scale_color_manual(values = pnw_palette("Bay",4), labels = scales::label_wrap(20))


# Export ---------------------------------------------------------------
artifacts_climate_shocks <- list (
  climate_shocks_tbl = climate_shocks_tbl,
  climate_shocks_gg = climate_shocks_gg,
  climate_shocks_stats_tbl = climate_shocks_stats_tbl
)

write_rds(artifacts_climate_shocks, file = here("Outputs", "artifacts_climate_shocks.rds"))
