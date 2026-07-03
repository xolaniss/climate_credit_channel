# Description
# Weighted climate data from https://www.santannapisa.it/en/istituto/economia
# and https://weightedclimatedata.streamlit.app/Download_Data
# Preliminaries -----------------------------------------------------------
library(here)

# Functions ---------------------------------------------------------------
source(here("packages.R"))

# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))

# Import -------------------------------------------------------------

## Temperature ---------------------  # Change spreadsheets to 2024
land_weighted_temp_tbl <-
  read_csv(here("Data", "land_weighted_temp.csv")) |> 
  rename(
    date = 1,
    land_temp = 2
  ) 

population_weighted_temp_tbl <-
  read_csv(here("Data", "population_weighted_temp.csv")) |> 
  rename(date = 1, pop_temp = 2) 

## Precipitation ---------------------
land_weighted_precip_tbl <-
  read_csv(here("Data", "land_weighted_precip.csv")) |>
  rename(date = 1, land_precip = 2) 

population_weighted_precip_tbl <-
  read_csv(here("Data", "population_weighted_precip.csv")) |>
  rename(date = 1, pop_precip = 2) 

# Graphing ---------------------------------------------------------------
## Plotting the temperature -------
### Plotting the land weighted temperature -------
land_weighted_temp_gg <-
  land_weighted_temp_tbl |>
  ggplot(aes(x = date, y = land_temp)) +
  geom_line() +
  labs(
    title = "Land Weighted Temperature",
    x = "Date",
    y = "Temperature (C)"
  ) +
  theme_minimal() +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Plotting the population weighted temperature -------
population_weighted_temp_gg <-
  population_weighted_temp_tbl |>
  ggplot(aes(x = date, y = pop_temp)) +
  geom_line() +
  labs(
    title = "Population Weighted Temperature",
    x = "Date",
    y = "Temperature (C)"
  ) +
  theme_minimal() +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Plotting the precipitation -------
### Plotting the land weighted precipitation -------
land_weighted_precip_gg <-
  land_weighted_precip_tbl |>
  ggplot(aes(x = date, y = land_precip)) +
  geom_line() +
  labs(
    title = "Land Weighted Precipitation",
    x = "Date",
    y = "Precipitation (mm)"
  ) +
  theme_minimal() +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Plotting the population weighted precipitation -------
population_weighted_precip_gg <-
  population_weighted_precip_tbl |>
  ggplot(aes(x = date, y = pop_precip)) +
  geom_line() +
  labs(
    title = "Population Weighted Precipitation",
    x = "Date",
    y = "Precipitation (mm)"
  ) +
  theme_minimal() +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combined data --------------------
combined_climate_data_tbl <-
  land_weighted_temp_tbl |>
  left_join(population_weighted_temp_tbl, by = c("date")) |> 
  left_join(land_weighted_precip_tbl, by = c("date")) |> 
  left_join(population_weighted_precip_tbl, by = c("date"))

# eda ---------------------------------------------------------------------
combined_climate_data_tbl |>
  skimr::skim()


# Export ---------------------------------------------------------------
artifacts_climate_data <- list (
  combined_climate_data_tbl = combined_climate_data_tbl
)

write_rds(artifacts_climate_data, file = here("Outputs",
                                              "artifacts_climate_data.rds"))
