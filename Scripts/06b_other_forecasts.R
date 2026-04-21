# Description
# Other foreast - April 2026
# Preliminaries -----------------------------------------------------------
library(here)

# Functions ---------------------------------------------------------------
source(here("packages.R"))
source(here("Functions", "fx_plot.R"))

# Import -------------------------------------------------------------
sheet_names <-  excel_sheets(here("Data", "forecasts.xlsx"))
other_forecast_tbl <- 
  sheet_names |> 
  map(~ read_excel(here("Data", "forecasts.xlsx"), skip = 1, sheet = .x)) |> 
  set_names(sheet_names) |> 
  # remove columns 4:6 from first list element
  map_at("Inflation", ~ .x |> dplyr::select(-4: -6) |>  rename("Latest" = 2)) |> 
  bind_rows(.id = "Series") |> 
  relocate(Date, .before = Series)


# EDA ---------------------------------------------------------------
other_foreast_tbl |> group_by(Series) |>  skim()

# Graphing ---------------------------------------------------------------
other_forecast_gg <-
  other_forecast_tbl |>
  ggplot(aes(x = Date, y = Latest, col = Series)) +
  geom_line() +
  facet_wrap( ~ Series, scales = "free_y", ncol = 2) +
  labs(title = "", x = "Date", y = "") +
  theme_minimal(base_size = 8) +
  theme(legend.position = "") +
  scale_x_date(date_labels = "%Y", date_breaks = "4 years") +
  scale_color_manual(values = pnw_palette("Bay", 3),
                     labels = scales::label_wrap(20))

# Export ---------------------------------------------------------------
artifacts_other_forecast <- list (
  other_forecast_tbl = other_forecast_tbl,
  other_forecast_gg = other_forecast_gg
)

write_rds(artifacts_other_forecast, file = here("Outputs", "artifacts_other_forecast.rds"))


