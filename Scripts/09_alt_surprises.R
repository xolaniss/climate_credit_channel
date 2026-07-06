# Description
# Structural surprises - April 2026
## Replicate Romer and Romer and Miyajima structural surprises
## Need: Romer : GDP Forecast, Repo Forecast, CPI Forecast, Actual repo.
##       Miyajima: Actual and forecasted GDP, Repo, and CPI

# Preliminaries -----------------------------------------------------------
library(here)

# Functions ---------------------------------------------------------------
source(here("packages.R"))
source(here("Functions", "fx_plot.R"))

# Import and Clean Data --------------------------------------------------------
forecast_tbl <- read_rds(here("Outputs", "artifacts_bloomberg_forecast.rds")) |> 
  pluck(1) |> 
  rename(
    date = `Forecast Date`,
    repo_forecast = `South Africa Repo Average Rate`,
    gdp_forecast  = `South Africa GDP Forecast YoY%`,
    cpi_forecast  = `South Africa CPI Forecast YoY%`
  ) 

actual_tbl <- read_rds(here("Outputs", "artifacts_bloomberg_actuals.rds")) |> 
  pluck(2) |> 
  rename(
    date =  Date,
    repo_actual = `South Africa Repo Average Rate`,
    gdp_actual  = `South Africa GDP Forecast YoY%`,
    cpi_actual  = `South Africa CPI Forecast YoY%`
  ) 


# Merge Datasets -----------------------------------------------------
combined_tbl <- forecast_tbl |> 
  left_join(actual_tbl, by = "date") 

## Construct Variables ------------------------------------------------

combined_romer_variables_tbl <- 
  combined_tbl |> 
  mutate(
    # Change in repo rate (dependent variable)
    change_repo = repo_actual - lag(repo_actual, n = 1),
    
    # Forecasted repo rate (level)
    repo_hat = repo_forecast,
    
    # GDP forecast: 2 quarters ahead change
    gdp_hat_lead_two = lead(gdp_forecast, n = 2),
    change_gdp_hat_two = gdp_hat_lead_two - lag(gdp_forecast, n = 1),
    
    # CPI forecast: 2 quarters ahead change
    cpi_hat_lead_two = lead(cpi_forecast, 2),
    change_cpi_hat_two = cpi_hat_lead_two - lag(cpi_forecast, n = 1)
  ) 
    
  
combined_miyajima_variables_tbl <-   
  combined_tbl |> 
  mutate(
    # Forecast Errors (FE = actual - forecast)
    fe_repo = repo_actual - repo_forecast,
    fe_gdp  = gdp_actual  - gdp_forecast,
    fe_cpi  = cpi_actual  - cpi_forecast
  ) 


## Graphing equation variables ---------------------------------------------
combined_romer_gg <- 
  combined_romer_variables_tbl |> 
  pivot_longer(-date, names_to = "variable", values_to = "value") |>
  ggplot(aes(x = date, y = value, col = variable)) +
  geom_line() +
  facet_wrap( ~ variable, scales = "free_y", ncol = 2) +
  theme_minimal(base_size = 8) +
  theme(legend.position = "") 


combined_miyajima_gg <- 
  combined_miyajima_variables_tbl |> 
  pivot_longer(-date, names_to = "variable", values_to = "value") |>
  ggplot(aes(x = date, y = value, col = variable)) +
  geom_line() +
  facet_wrap( ~ variable, scales = "free_y", ncol = 2) +
  theme_minimal(base_size = 8) +
  theme(legend.position = "") 


# Estimating surprises -------------------------------------------------------
## Estimate Romer & Romer Equation ------------------------------------
romer_suprise_tbl <- lm(
  change_repo ~ repo_hat + change_gdp_hat_two + change_cpi_hat_two,
  data = combined_romer_variables_tbl |> drop_na()) |> 
  resid() |> 
  tibble() |> 
  rename( "romer_surprise" = 1) |> 
  mutate(date = combined_romer_variables_tbl |>  drop_na() |> dplyr::select(date)) |> 
  relocate(date, .before = 1) |> 
  unnest(date) |> 
  rename("date" = 1) # hat is forecast


## Estimate Miyajima Equation ----------------------------------------
# FE(repo) = α + FE(inflation) + FE(gdp) + error

miyajima_surprise_tbl <- lm(
  fe_repo ~ fe_cpi + fe_gdp,
  data = combined_miyajima_variables_tbl |> drop_na()) |> 
  resid() |> 
  tibble() |> 
  rename( "miyajima_surprise" = 1) |>
  mutate(date = combined_miyajima_variables_tbl |>  drop_na() |> dplyr::select(date)) |>
  relocate(date, .before = 1) |>
  unnest(date) 

## Plots -----------------------------------------------------------

# Romer surprise
romer_surprise_gg <- 
  romer_suprise_tbl |> 
  ggplot(aes(x = date, y = romer_surprise)) +
  geom_line(color = "#0f85a0") +
  labs(
    title = "Romer & Romer Monetary Policy Surprise",
    x = " ",
    y = "Suprise"
  ) +
  theme_minimal(base_size = 8) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  scale_color_manual(labels = scales::label_wrap(20))

# Miyajima surprise
miyajima_suprise_gg <- 
  miyajima_surprise_tbl |>
  ggplot(aes(x = date, y = miyajima_surprise)) +
  geom_line(color = "#dd4124") +
  labs(
    title = "Miyajima Monetary Policy Surprise",
    x = " ",
    y = "Surprise"
  ) +
  theme_minimal(base_size = 8) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  scale_color_manual(labels = scales::label_wrap(20))


# Combining surprises data ---------------------------------------
combined_surprises_tbl <- 
  miyajima_surprise_tbl|> 
  left_join(romer_suprise_tbl, by = "date")


# Combining suprises ggs --------------------------------------------------
combined_gg <- romer_surprise_gg + miyajima_suprise_gg


# Save Outputs ---------------------------------------------------

# Create artifacts object
artifacts <- list(
  date  = list(
    combined_romer_variables_tbl = combined_romer_variables_tbl,
    combined_miyajima_variables_tbl = combined_miyajima_variables_tbl),
  surprises = list(
    combined_surprises_tbl = combined_surprises_tbl,
    romer_surprise_tbl = romer_suprise_tbl,
    miyajima_surprise_tbl = miyajima_surprise_tbl),
  plots = list(
    romer_surprise_gg = romer_surprise_gg,
    miyajima_surprise_gg = miyajima_suprise_gg,
    combined_gg = combined_gg
  )
)

# Save as RDS 
write_rds(artifacts, file = here("Outputs", "artifacts_surprises.rds"))


