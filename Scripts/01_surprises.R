# Description
# surprises table from IRFs.mat file - March 2026

# Preliminaries -----------------------------------------------------------
library(here)
library(R.matlab)

# Functions ---------------------------------------------------------------
source(here("packages.R"))
source(here("Functions", "fx_plot.R"))

# Import -------------------------------------------------------------
sa_surprises_tbl <- 
  R.matlab::readMat(here("Data", "IRFs", "IRFs.mat")) |> 
  pluck("IRFs") |> 
  as.data.frame() |>
  as_tibble() # Find out the names and the dates of the surprises
  
# Graph -------------------------------------------------------------------
sa_surprises_gg <- 
  sa_surprises_tbl |> 
  rename(F1 = 1, F2 = 2, F3 = 3, F4 = 4) |>
  mutate(number = row_number()) |> 
  pivot_longer(-number, names_to = "variable", values_to = "surprise") |> 
  ggplot(aes(x = number, y = surprise)) + # Change the variable names
  geom_line() +
  labs(
    title = "SA MPS",
    x = "Date",
    y = ""
  ) +
  facet_wrap(~variable, scales = "free_y", ncol = 2) +
  theme_minimal(base_size = 6) +
  theme(legend.position = "bottom") +
  theme(legend.position = "bottom") +
  scale_x_date(date_labels = "%Y", date_breaks = "4 years") +
  scale_color_manual(values = pnw_palette("Bay",4), labels = scales::label_wrap(20)) +
  theme_minimal()


# Export ---------------------------------------------------------------
artifacts_sa_surprises <- list (
  sa_surprises_tbl = sa_surprises_tbl,
  sa_surprises_gg = sa_surprises_gg
  
)

write_rds(artifacts_sa_surprises, file = here("Outputs", "artifacts_sa_surprises.rds"))


