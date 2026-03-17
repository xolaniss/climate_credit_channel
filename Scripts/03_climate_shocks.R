# Description
# Calculating alternative climate shocks - Xolani Sibande September 2025
# Preliminaries -----------------------------------------------------------
library(here)

# Functions ---------------------------------------------------------------
source(here("packages.R"))

# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))
month_based_shock <-
  function(data, variable_filter){
    data |>
      pivot_longer(cols = -c(date, country), names_to = "variable", values_to = "value") |>
      mutate(month = month(date)) |>
      relocate(month, .after = date) |>
      filter(variable == variable_filter) |>
      group_by(country, month) |>
      mutate(long_term_mean = mean(value, na.rm = TRUE)) |>
      ungroup() |>
      mutate(
        shock = (value - long_term_mean) / sd(value, na.rm = TRUE),
      ) |>
      pivot_wider(
        names_from = variable,
        values_from = c(value, shock),
        names_glue = "{variable}_{.value}"
      ) |>
      dplyr::select(-month, - long_term_mean) |>
      relocate(date, .before = country) |>
      drop_na()
  }

# Import -------------------------------------------------------------
climate_data_tbl <- read_rds(here("Outputs", "artifacts_climate_data.rds")) |>
  pluck(1)

# Climate shocks ---------------------------------------------------------------
args_list <- list(
  c("land_weighted_temp"),
  c("population_weighted_temp"),
  c("land_weighted_precip"),
  c("population_weighted_precip")
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
    by = c("date", "country")
  ) |>
  left_join(
    climate_shocks_list[[3]],
    by = c("date", "country")
  ) |>
  left_join(
    climate_shocks_list[[4]],
    by = c("date", "country")
  ) |>
  # group_by(country) |>
  # # remove bottom and top 5% values
  # mutate(across(.cols = -c(date, ends_with("_value")),
  #               .fns = ~ DescTools::Winsorize(.x, val = quantile(.x, probs = c(0.05, 0.95))))) |>
  ungroup() |>
  filter(date >= as.Date("2000-01-01")) |>
  filter(country %in% inflation_countries)   # keeping only countries with inflation data that are in TIVA


# EDA ---------------------------------------------------------------------
climate_shocks_tbl |>
  group_by(country) |>
  skim()


# Example Graphing ---------------------------------------------------------------
climate_shocks_gg <-
  climate_shocks_tbl |>
  dplyr::select(date, country, ends_with("_shock")) |>
  pivot_longer(cols = -c(date, country), names_to = "variable", values_to = "value") |>
  mutate(country = countrycode::countrycode(country, "iso3c", "country.name")) |>
  mutate(
    variable = str_replace_all(
      variable,
      c("population_weighted_temp_shock" = "Population weighted temperature shock",
        "land_weighted_temp_shock" = "Land weighted temperature shock",
        "population_weighted_precip_shock" = "Population weighted precipitation shock",
        "land_weighted_precip_shock" = "Land weighted precipitation shock"
    ))) |>
  ggplot(aes(x = date, y = value, col = variable)) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y", ncol = 2) +
  labs(
    title = "",
    y = "Shock",
    x = "",
    col = "Shock"
  ) +
  theme_minimal(base_size = 6) +
  theme(legend.position = "bottom") +
  theme(legend.position = "bottom") +
  scale_x_date(date_labels = "%Y", date_breaks = "4 years") +
  scale_color_manual(values = pnw_palette("Bay",4), labels = scales::label_wrap(20))


# Export ---------------------------------------------------------------
artifacts_climate_shocks <- list (
  climate_shocks_tbl = climate_shocks_tbl,
  climate_shocks_gg = climate_shocks_gg
)

write_rds(artifacts_climate_shocks, file = here("Outputs", "artifacts_climate_shocks.rds"))


