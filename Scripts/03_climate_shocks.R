# Description
# Calculating alternative climate shocks - Xolani Sibande September 2025
# Preliminaries -----------------------------------------------------------
library(here)

# Functions ---------------------------------------------------------------
source(here("packages.R"))

# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))
month_based_shock <- function(data, variable_filter){
  
  data |>
    pivot_longer(
      cols = -c(date),
      names_to = "variable",
      values_to = "value"
    ) |>
    mutate(month = month(date)) |>
    filter(variable == variable_filter) |>
    group_by(month) |>
    mutate(long_term_mean = mean(value, na.rm = TRUE), # This is based on the long-term mean (entire sample) for each month and country. This may change in the future
      long_term_sd   = sd(value, na.rm = TRUE), #month specifc standard deviation
      shock = (value - long_term_mean) / long_term_sd
    ) |>
    ungroup() |>
    pivot_wider(
      names_from = variable,
      values_from = c(value, shock),
      names_glue = "{variable}_{.value}"
    ) |>
    dplyr::select(-month, -long_term_mean, -long_term_sd) |>
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

climate_shocks_list <-
  args_list |>
  map(
  ~month_based_shock(
    data = climate_data_tbl,
    variable_filter = .x[1]
    )
  )

# Combine all shocks into one tbl --------------------------------------------
climate_shocks_tbl <-
  left_join(
    climate_shocks_list[[1]],
    climate_shocks_list[[2]],
    by = "date"
  ) |>
  left_join(
    climate_shocks_list[[3]],
    by = "date"
  ) |>
  left_join(
    climate_shocks_list[[4]],
    by = "date"
  ) |>
  ungroup() |>
  filter(date >= "2012-12-01") |>
  
 
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
  climate_shocks_gg = climate_shocks_gg
)

write_rds(artifacts_climate_shocks, file = here("Outputs", "artifacts_climate_shocks.rds"))


