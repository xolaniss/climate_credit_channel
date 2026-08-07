# Description
# surprises table from IRFs.mat file - March 2026

# Preliminaries -----------------------------------------------------------
library(here)
library(R.matlab)

# Functions ---------------------------------------------------------------
source(here("packages.R"))
source(here("Functions", "fx_plot.R"))

# Import -------------------------------------------------------------
announcement_dates_tbl <- 
  read_excel(here("Data", "monetary_policy_announcement_dates.xlsx")) |> 
  rename(Date = 1) |> 
  mutate(Date = parse_date_time(Date, "dmy")) |> 
  mutate(Date = as.Date(Date)) |> 
  filter(Date >= "2002-06-01" & Date <= "2025-02-01" )

market_based_surprises_tbl <- 
  R.matlab::readMat(here("Data", "IRFs", "IRFs.mat")) |> 
  pluck("IRFs") |> 
  as.data.frame() |>
  as_tibble() |> 
  mutate(date = announcement_dates_tbl |> dplyr::select(Date)) |> 
  unnest(date) |> 
  relocate(Date, .before = "V1") |> 
  rename("Target" = 2, 
         "Forward Guidance" = 3, 
         "Central Bank Information" = 4, 
         "Country Risk" = 5) |> 
  filter(Date >= "2008-01-01" )
  

  
# Graph -------------------------------------------------------------------
market_based_surprises_gg <- 
  market_based_surprises_tbl |> 
  pivot_longer(-Date, names_to = "variable", values_to = "surprise") |> 
  ggplot(aes(x = Date, y = surprise, col = variable)) + # Change the variable names
  geom_line() +
  labs(
<<<<<<< Updated upstream
    # title = "SA Market-Based MPS",
    x = "",
    y = "Surprise"
=======
    title = "SA Market-Based MPS",
    x = "Date",
    y = " "
>>>>>>> Stashed changes
  ) +
  facet_wrap(~variable, scales = "free_y", ncol = 2) +
  theme_minimal(base_size = 8) +
  theme(legend.position = "none") +
  scale_x_date(date_labels = "%Y", date_breaks = "4 years") +
  scale_color_manual(values = pnw_palette("Bay",4), labels = scales::label_wrap(20))


# Export ---------------------------------------------------------------
artifacts_market_based_surprises <- list (
  market_based_surprises_tbl = market_based_surprises_tbl,
  market_based_surprises_gg = market_based_surprises_gg
  
)

write_rds(artifacts_market_based_surprises, file = here("Outputs", "artifacts_market_based_surprises.rds"))


