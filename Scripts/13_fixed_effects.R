# Description
# Running fixed effects regressions for all bank variables - May 2026
  ## NOTE 1 : Only bank fixed effects (no date fixed effects) because data 
  ## ...does not vary over time between banks so date fixed effects are colinear with explanatory variables
  ## NOTE 2: Consider splitting the climate variables between positive and negative shocks
  ## NOTE 3: Consider adding control variables 
  ## NOTE 4: Problem: Surprises are highly correlated with climate variables - consider using the error term of
  ## ... surprise = intercept + climate shock + residual. Use residual which shows surprises that cannot be explained by climate shocks.
  ## ...then main regression is: bank variable = climate + residual + (climate*residual) + bank FE + error
  ## NOTE 5: Some standard errors eg. secured credit rate are huge - consider logging dep. variable


# Preliminaries -----------------------------------------------------------
library(here)

# Functions ---------------------------------------------------------------
source(here("packages.R"))
source(here("Functions", "fx_plot.R"))

# Import ---------------------------------------------------------------
model_data_tbl <- read_rds(
  here("Outputs", "artifacts_model_data.rds")
) |>
  pluck("model_data_tbl")

# Data preparation -----------------------------------------------------

# Convert identifiers to factors
model_data_tbl <- model_data_tbl |>
  mutate(
    banks = as.factor(banks),
    date = as.factor(date)
  )

# Dependent variables --------------------------------------------------

dependent_vars <- c(
  "corporate_unsecured_credit",
  "corporate_secured_credit",
  "corporate_mortgages",
  "household_unsecured_credit",
  "household_secured_credit",
  "household_mortgages",
  "corporate_unsecured_credit_rate",
  "corporate_secured_credit_rate",
  "corporate_mortgage_rate",
  "household_unsecured_credit_rate",
  "household_secured_credit_rate",
  "household_mortgage_rate"
)

# Climate shock variables ----------------------------------------------

climate_vars <- c(
  # "land_temp_shock",
  "pop_temp_shock",
  # "land_precip_shock",
  "pop_precip_shock"
)

# Monetary policy surprises --------------------------------------------

surprise_vars <- c(
  "miyajima_surprise",
  "romer_surprise",
  "target",
  "forward_guidance",
  "central_bank_information",
  "country_risk"
)

# Variable labels ------------------------------------------------------

variable_labels <- c(
  
  # Climate variables
  # "land_temp_shock" =
  #   "Land-weighted temperature shock",
  
  "pop_temp_shock" =
    "Population-weighted temperature shock",
  
  # "land_precip_shock" =
  #   "Land-weighted precipitation shock",
  
  "pop_precip_shock" =
    "Population-weighted precipitation shock",
  
  # Surprise variables
  "miyajima_surprise" =
    "Miyajima surprise",
  
  "romer_surprise" =
    "Romer surprise",
  
  "target" =
    "Target factor",
  
  "forward_guidance" =
    "Forward guidance factor",
  
  "central_bank_information" =
    "Central bank information factor",
  
  "country_risk" =
    "Country risk factor",
  
  # Interaction terms --------------------------------------------------
  
  # "land_temp_shock:miyajima_surprise" =
  #   "Land temp shock × Miyajima surprise",
  # 
  # "land_temp_shock:romer_surprise" =
  #   "Land temp shock × Romer surprise",
  # 
  # "land_temp_shock:target" =
  #   "Land temp shock × Target factor",
  
  # "land_temp_shock:forward_guidance" =
  #   "Land temp shock × Forward guidance",
  # 
  # "land_temp_shock:central_bank_information" =
  #   "Land temp shock × Central bank information",
  # 
  # "land_temp_shock:country_risk" =
  #   "Land temp shock × Country risk",
  # 
  "pop_temp_shock:miyajima_surprise" =
    "Population temp shock × Miyajima surprise",
  
  "pop_temp_shock:romer_surprise" =
    "Population temp shock × Romer surprise",
  
  "pop_temp_shock:target" =
    "Population temp shock × Target factor",
  
  "pop_temp_shock:forward_guidance" =
    "Population temp shock × Forward guidance",
  
  "pop_temp_shock:central_bank_information" =
    "Population temp shock × Central bank information",
  
  "pop_temp_shock:country_risk" =
    "Population temp shock × Country risk",
  
  # "land_precip_shock:miyajima_surprise" =
  #   "Land precip shock × Miyajima surprise",
  # 
  # "land_precip_shock:romer_surprise" =
  #   "Land precip shock × Romer surprise",
  # 
  # "land_precip_shock:target" =
  #   "Land precip shock × Target factor",
  # 
  # "land_precip_shock:forward_guidance" =
  #   "Land precip shock × Forward guidance",
  # 
  # "land_precip_shock:central_bank_information" =
  #   "Land precip shock × Central bank information",
  # 
  # "land_precip_shock:country_risk" =
  #   "Land precip shock × Country risk",
  
  "pop_precip_shock:miyajima_surprise" =
    "Population precip shock × Miyajima surprise",
  
  "pop_precip_shock:romer_surprise" =
    "Population precip shock × Romer surprise",
  
  "pop_precip_shock:target" =
    "Population precip shock × Target factor",
  
  "pop_precip_shock:forward_guidance" =
    "Population precip shock × Forward guidance",
  
  "pop_precip_shock:central_bank_information" =
    "Population precip shock × Central bank information",
  
  "pop_precip_shock:country_risk" =
    "Population precip shock × Country risk"
)

# Create output folders ------------------------------------------------

dir.create(
  here("Outputs", "Fixed Effects Regression Tables"),
  recursive = TRUE,
  showWarnings = FALSE
)

# Regression estimation ------------------------------------------------

# Loop initial command 
all_models <- list()

for(dep in dependent_vars){
  dep_models <- list()
  for(climate in climate_vars){
    for(surprise in surprise_vars){
      regression_formula <- as.formula(      # Make formula
        paste0(
          dep,
          " ~ ",
          climate,
          " * ",
          surprise,
          " | banks "
        )
      )
      model <- feols(      # Estimate FE model
        regression_formula,
        data = model_data_tbl,
        cluster = ~banks
      )
      model_name <- paste(          # naming model
        climate,
        "x",
        surprise,
        sep = "_"
      )
      dep_models[[model_name]] <- model
    }
  }
  all_models[[dep]] <- dep_models        # Store models
}

# Export regression tables ---------------------------------------------

# Loop initial command 
for(dep in names(all_models)){
  models_tbl <- all_models[[dep]] # extracts all regression models associated with the current dependent variable
  
  # Create numbered column headings
  model_names <- paste0(
    "(",
    seq_along(models_tbl),
    ")"
  )
  names(models_tbl) <- model_names
  modelsummary(                 # Create table
    models_tbl,
    statistic = "std.error",  # Robust clustered SEs (estimated in feols)
    stars = c(
      "*" = .10,
      "**" = .05,
      "***" = .01
    ),
    gof_map = c(         # Goodness-of-fit statistics
      "nobs",
      "r.squared",
      "adj.r.squared",
      "within.r.squared"
    ),
    coef_map = variable_labels # Rename variables
    # output = here(              # Output file
    #   "Outputs",
    #   "Fixed Effects Regression Tables",
    #   paste0(dep, "_fe_regressions.html")
    # )
  )
  
 
}


# Extracting coeficients -----------------------------------------

estimates_tbl <- 
  map_dfr(models_tbl, modelsummary::get_estimates, .id = "model") |> 
  mutate(estimate = round(estimate, 3)) |> 
  select(-starts_with("conf"), -statistic, -df.error, -starts_with("s."), -group, -std.error) |> 
  mutate(
    stars = ifelse(p.value < 0.01, "***", 
                   ifelse(p.value < 0.05, "**", 
                          ifelse(p.value < 0.1, "*", "")))
  ) |> 
  mutate(estimate = paste0(estimate, stars)) |> 
  pivot_wider(names_from = model, values_from = estimate) |> 
  select(-p.value, -stars) 


# Save all models ------------------------------------------------------

write_rds(
  list(all_models = all_models, estimates_tbl = estimates_tbl),
  here("Outputs", "all_fe_models.rds")
)
