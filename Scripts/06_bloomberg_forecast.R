# Description
# dealing with forecasts from bloomberg - Xolani Sibande April 2026
# Preliminaries -----------------------------------------------------------
library(here)

# Functions ---------------------------------------------------------------
source(here("packages.R"))
source(here("Functions", "fx_plot.R"))

# Import -------------------------------------------------------------
bloomberg_quarterly_tbl <- read_excel(here("Data", "bloomberg_quarterly_forecast.xlsx")) |> 
  dplyr::select(-1, -2) 
  # janitor::clean_names() # what is reporting date? Tina



# EDA ---------------------------------------------------------------
bloomberg_quarterly_tbl |> skim()


# Graphing ---------------------------------------------------------------
bloomberg_quarterly_gg <-
  bloomberg_quarterly_tbl |>
  pivot_longer(-`Forecast Date`, names_to = "Series", values_to = "Value") |>
  ggplot(aes(x = `Forecast Date`, y = Value, , col = "Series")) +
  geom_line() +
  facet_wrap( ~ Series, scales = "free_y", ncol = 2) +
  labs(title = "", x = "Forecast Date", y = "") +
  theme_minimal(base_size = 8) +
  theme(legend.position = "") +
  scale_x_date(date_labels = "%Y", date_breaks = "4 years") +
  scale_color_manual(values = pnw_palette("Bay", 3),
                     labels = scales::label_wrap(20))



# Export ---------------------------------------------------------------
artifacts_bloomberg_forecast <- list (
  bloomberg_quarterly_tbl = bloomberg_quarterly_tbl,
  bloomberg_quarterly_gg = bloomberg_quarterly_gg
)

write_rds(artifacts_bloomberg_forecast, file = here("Outputs", "artifacts_bloomberg_forecast.rds"))


