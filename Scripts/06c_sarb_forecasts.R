# Description
# SARB forecasts - April 2026
# Preliminaries -----------------------------------------------------------
library(here)

# Functions ---------------------------------------------------------------
source(here("packages.R"))
source(here("Functions", "fx_plot.R"))

# Import -------------------------------------------------------------
sheet_names <- excel_sheets(here("Data", "TM_data_request.xlsx"))
sarb_forecasts_tbl <- 
  sheet_names |> 
  map(~ read_excel(here("Data", "TM_data_request.xlsx"), sheet = .x)) |> 
  set_names(sheet_names) |> 
  map(~ pivot_longer(.x, cols = -1, names_to = "mpc", values_to = "value")) |>
  map(~ rename(.x, date = 1)) |>
  map(~ mutate(.x, date = as.Date(parse_date_time(date, "yq")))) |> 
  bind_rows(.id = "variable") 

# EDA ---------------------------------------------------------------
sarb_forecasts_tbl |> group_by(variable) |> skimr::skim()

# Graphing ---------------------------------------------------------------
sarb_forecast_gg <- 
  sarb_forecasts_tbl |> 
  filter(!date >= as.Date("2030-01-01")) |> # check if this is correct
  ggplot(aes(x = date, y = value, col = mpc)) +
  geom_line() +
  theme_minimal() +
  labs(title = "SARB CPI forecasts",
       x = "",
       y = "",
       color = "MPC meeting")+
  facet_wrap(~ variable, scales = "free_y") +
  theme(legend.position = "") +
  scale_color_manual(values = pnw_palette("Bay", 190),
                     labels = scales::label_wrap(20))

# Export ---------------------------------------------------------------
artifacts_sarb_forecast <- list (
  sarb_forecasts_tbl = sarb_forecasts_tbl,
  sarb_forecast_gg = sarb_forecast_gg
)

write_rds(artifacts_sarb_forecast, file = here("Outputs", "artifacts_sarb_forecast.rds"))


