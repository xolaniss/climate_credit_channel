# Description
# Structural surprises - Xolani April 2026
## Replicate Romer and Romer and Miyajima structural surprises
## Need: Romer : GDP Forecast, Repo Forecast, CPI Forecast, Actual repo.
##       Miyajima: Actual and forecasted GDP, Repo, and CPI

# Preliminaries -----------------------------------------------------------
library(here)

# Functions ---------------------------------------------------------------
source(here("packages.R"))
source(here("Functions", "fx_plot.R"))

# Import Data --------------------------------------------------------

# Forecast data
forecast <- read_excel(here("Data", "bloomberg_quarterly_forecast.xlsx"))

# Actual data
actual <- read_excel(here("Data", "cpi_gdp_repo_actuals.xlsx"))

# Clean Data ---------------------------------------------------------

clean_dates <- function(df) {
  df %>%
    mutate(
      Reporting_Date = as.Date(Reporting_Date, format = "%b %Y"),
      Quarter = as.yearqtr(Reporting_Date)
    )
}

forecast <- forecast %>%
  rename(
    Reporting_Date = `Reporting Date`,
    repo_forecast = `South Africa Repo Average Rate`,
    gdp_forecast  = `South Africa GDP Forecast YoY%`,
    cpi_forecast  = `South Africa CPI Forecast YoY%`
  ) %>%
  clean_dates()

actual <- actual %>%
  rename(
    Reporting_Date = `Reporting Date`,
    repo_actual = `South Africa Repo Average Rate`,
    gdp_actual  = `South Africa GDP Forecast YoY%`,
    cpi_actual  = `South Africa CPI Forecast YoY%`
  ) %>%
  clean_dates()

# Convert to numeric and handle missing values
forecast <- forecast %>%
  mutate(across(c(repo_forecast, gdp_forecast, cpi_forecast),
                ~as.numeric(na_if(as.character(.x), "#N/A"))))

actual <- actual %>%
  mutate(across(c(repo_actual, gdp_actual, cpi_actual),
                as.numeric))

# Merge Datasets -----------------------------------------------------

data <- forecast %>%
  left_join(actual, by = "Quarter") %>%
  arrange(Quarter)

# Construct Variables ------------------------------------------------

data <- data %>%
  mutate(

# Romer & Romer vars
   
    # Change in repo rate (dependent variable)
    d_repo = repo_actual - lag(repo_actual),
    
    # Forecasted repo rate (level)
    repo_hat = repo_forecast,
    
    # GDP forecast: 2 quarters ahead change
    gdp_hat_lead2 = lead(gdp_forecast, 2),
    d_gdp_hat = gdp_hat_lead2 - lag(gdp_forecast),
    
    # CPI forecast: 2 quarters ahead change
    cpi_hat_lead2 = lead(cpi_forecast, 2),
    d_cpi_hat = cpi_hat_lead2 - lag(cpi_forecast),

# Miyajima vars
    
    # Forecast Errors (FE = actual - forecast)
    fe_repo = repo_actual - repo_forecast,
    fe_gdp  = gdp_actual  - gdp_forecast,
    fe_cpi  = cpi_actual  - cpi_forecast
  )

# Estimate Romer & Romer Equation ------------------------------------

rr_model_data <- data %>%
  filter(!is.na(d_repo),
         !is.na(repo_hat),
         !is.na(d_gdp_hat),
         !is.na(d_cpi_hat))

rr_model <- lm(
  d_repo ~ repo_hat + d_gdp_hat + d_cpi_hat,
  data = rr_model_data
)

summary(rr_model)

# Extract Romer Surprise ---------------------------------------------

rr_model_data <- rr_model_data %>%
  mutate(
    romer_surprise = resid(rr_model)
  )

# Merge back
data <- data %>%
  left_join(
    rr_model_data %>% select(Quarter, romer_surprise),
    by = "Quarter"
  )

# Estimate Miyajima Equation ----------------------------------------
# FE(repo) = α + FE(inflation) + FE(gdp) + error

miyajima_model_data <- data %>%
  filter(!is.na(fe_repo),
         !is.na(fe_gdp),
         !is.na(fe_cpi))

miyajima_model <- lm(
  fe_repo ~ fe_cpi + fe_gdp,
  data = miyajima_model_data
)

summary(miyajima_model)

# Extract Miyajima Surprise ------------------------------------------

miyajima_model_data <- miyajima_model_data %>%
  mutate(
    miyajima_surprise = resid(miyajima_model)
  )

# Merge back
data <- data %>%
  left_join(
    miyajima_model_data %>% select(Quarter, miyajima_surprise),
    by = "Quarter"
  )

# Plots -----------------------------------------------------------

# Romer surprise
ggplot(data, aes(x = Quarter, y = romer_surprise)) +
  geom_line() +
  labs(
    title = "Romer & Romer Monetary Policy Surprise (South Africa)",
    x = "Quarter",
    y = "Shock"
  ) +
  theme_minimal()

# Miyajima surprise
ggplot(data, aes(x = Quarter, y = miyajima_surprise)) +
  geom_line() +
  labs(
    title = "Miyajima Monetary Policy Surprise (South Africa)",
    x = "Quarter",
    y = "Shock"
  ) +
  theme_minimal()

# Save Outputs ---------------------------------------------------

# Create artifacts object
artifacts_ <- list(
  data = data,
  rr_model = rr_model,
  miyajima_model = miyajima_model
)

# Save as RDS 
write_rds(artifacts_, file = here("Outputs", "artifacts_surprises.rds"))


