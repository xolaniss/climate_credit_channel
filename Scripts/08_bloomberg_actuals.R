# Description
# Actuals - April 2026

# Preliminaries -----------------------------------------------------------
library(here)

# Functions ---------------------------------------------------------------
source(here("packages.R"))
source(here("Functions", "fx_plot.R"))

# Import -------------------------------------------------------------
actuals_tbl <- read_excel(here("Data", "cpi_gdp_repo_actuals.xlsx")) |> 
  dplyr::select(-2) |> 
  rename(Date = 1) 
  

# EDA ---------------------------------------------------------------
actuals_tbl  |>  skimr::skim()

# Graphing ---------------------------------------------------------------
actuals_gg <- 
  actuals_tbl |> 
  pivot_longer(-Date, names_to = "variable", values_to = "value") |> 
  ggplot(aes(x = Date, y = value, , col = variable)) +
  geom_line() +
  facet_wrap( ~ variable, scales = "free_y", ncol = 2) +
  labs(title = "", x = "date", y = "") +
  theme_minimal(base_size = 8) +
  theme(legend.position = "") +
  scale_x_date(date_labels = "%Y", date_breaks = "4 years") +
  scale_color_manual(values = pnw_palette("Bay", 3),
                     labels = scales::label_wrap(20))

# Export ---------------------------------------------------------------
artifacts_bloomberg_actuals <- list (
  actuals_gg = actuals_gg,
  actuals_tbl = actuals_tbl
)

write_rds(artifacts_bloomberg_actuals, file = here("Outputs", "artifacts_bloomberg_actuals.rds"))


