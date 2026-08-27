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
source(here("Functions", "group_ols_functions.R"))

# Import ---------------------------------------------------------------
model_data_tbl <- read_rds(
  here("Outputs", "artifacts_model_data.rds")
) |>
  pluck("model_data_tbl")

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
  "pop_temp_shock",
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
  # Climate shock
  "pop_temp_shock" =
    "Population-weighted temperature shock",
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
  
  # Interaction terms 
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

# Regression estimation ------------------------------------------------
formula <- as.formula(
  credit_value ~ mp_shock_value + climate_shock_value + 
    climate_shock_value * mp_shock_value + date |
    banks)


model_data_long_tbl <- 
  model_data_tbl |>
  select(-ends_with("neg"), -ends_with("pos")) |>
  pivot_longer(
    -c(
      "banks",
      "date",
      ends_with("shock"),
      ends_with("surprise"),
      "target",
      "forward_guidance",
      "central_bank_information",
      "country_risk"
    ),
    values_to = "credit_value",
    names_to = "credit"
  ) |> 
  pivot_longer(
    -c("banks", "date", "credit", "credit_value", ends_with("shock")),
    names_to = "mp_shock",
    values_to = "mp_shock_value"
  ) |> 
  pivot_longer(
    -c("banks", "date", "credit", "credit_value", "mp_shock", "mp_shock_value"),
    names_to = "climate_shock",
    values_to = "climate_shock_value"
  ) |> 
  relocate(
    c("mp_shock", "climate_shock"), .before = credit_value
  ) 
  

# Running regression workflow-----------------------------------------

## Regardless of type of climate and mp_shock --------------------
model_data_long_tbl |> 
  ols_nest_full_prep(group_vars = "credit") |> 
  ols_tidy_group_models(formula = formula) |> 
  ols_pretty_full_results(group_vars = "credit")
  

## Regardless of climate shock ------------
model_data_long_tbl |> 
  ols_nest_full_prep(group_vars = c("credit", "mp_shock")) |> 
  ols_tidy_group_models(formula = formula)  |> 
  ols_pretty_full_results(group_vars = c("credit", "mp_shock"))

  
## Full regression --------------
shock_models_tbl <-
  model_data_long_tbl |>
  ols_nest_full_prep(group_vars = c("credit", "mp_shock", "climate_shock")) |>
  ols_tidy_group_models(formula = formula)  |>
  ols_pretty_full_results(group_vars = c("credit", "mp_shock", "climate_shock")) 

shock_models_tbl

## Plots ----------------------
credit_labels <- c(
  corporate_unsecured_credit       = "Corp. unsecured credit",
  corporate_secured_credit         = "Corp. secured credit",
  corporate_mortgages              = "Corp. mortgages",
  household_unsecured_credit       = "HH unsecured credit",
  household_secured_credit         = "HH secured credit",
  household_mortgages              = "HH mortgages",
  corporate_unsecured_credit_rate  = "Corp. unsecured rate",
  corporate_secured_credit_rate    = "Corp. secured rate",
  corporate_mortgage_rate          = "Corp. mortgage rate",
  household_unsecured_credit_rate  = "HH unsecured rate",
  household_secured_credit_rate    = "HH secured rate",
  household_mortgage_rate          = "HH mortgage rate"
)

climate_labels <- c(
  pop_temp_shock   = "Temperature shock",
  pop_precip_shock = "Precipitation shock"
)

mp_labels2 <- c(
  miyajima_surprise        = "Miyajima",
  romer_surprise           = "Romer",
  target                   = "Target",
  forward_guidance         = "Fwd. guidance"
)

shock_model_data <-
  model_data_long_tbl |>
  filter(!mp_shock %in% c("country_risk", "central_bank_information", "forward_guidance")) |>
  ols_nest_full_prep(group_vars = c("credit", "mp_shock", "climate_shock")) |>
  ols_tidy_group_models(formula = formula) |>
  unnest(cols = models_coef, names_repair = "universal") |>
  select(credit, mp_shock, climate_shock, term, estimate, conf.low, conf.high, p.value) |>
  ungroup() |>
  filter(term == "mp_shock_value:climate_shock_value") |>
  mutate(
    group         = if_else(str_ends(credit, "credit|mortgages"), "Log of credit", "Lending rates"),
    credit        = factor(credit_labels[credit], levels = rev(credit_labels)),
    mp_shock      = factor(mp_labels2[mp_shock], levels = mp_labels2),
    climate_shock = factor(climate_labels[climate_shock], levels = climate_labels),
    sig           = if_else(p.value < 0.05, "p < 0.05", "p ≥ 0.05")
  )

fe_coef_plot <- function(data, title) {
  data |>
    ggplot(aes(x = estimate, y = credit, color = sig)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbar(aes(xmin = conf.low, xmax = conf.high),
                  orientation = "y", width = 0.35) +
    geom_point(size = 1.8) +
    facet_grid(climate_shock ~ mp_shock, scales = "free_x") +
    scale_x_continuous(breaks = scales::breaks_width(1)) +
    scale_color_manual(values = pnw_palette("Bay", 2),
                       labels = scales::label_wrap(20)) +
    labs(
      title    = title,
      subtitle = "Fixed effects regression (bank FE + year FE) | 95% CI, clustered SEs",
      x = "Coefficient (MP shock × Climate shock interaction)",
      y = NULL, color = NULL
    ) +
    theme_bw(base_size = 11) +
    theme(
      legend.position  = "bottom",
      strip.text       = element_text(size = 8.5),
      axis.text.y      = element_text(size = 8),
      axis.text.x      = element_text(size = 8),
      panel.grid.minor = element_blank(),
      strip.background = element_blank(),
      panel.background = element_blank(),
      panel.border     = element_rect(color = "grey80", fill = NA)
    )
}

shock_model_rates_gg <-
  shock_model_data |>
  filter(group == "Lending rates") |>
  fe_coef_plot(title = "Interaction effect: MP shock × Climate shock — Lending rates")

shock_model_credit_gg <-
  shock_model_data |>
  filter(group == "Log of credit") |>
  fe_coef_plot(title = "Interaction effect: MP shock × Climate shock — Log of credit")

# Keep combined for backwards compatibility
shock_model_gg <- shock_model_rates_gg
 
  

# Save all models ------------------------------------------------------

write_rds(
  list(shock_models_tbl    = shock_models_tbl,
       shock_model_rates_gg  = shock_model_rates_gg,
       shock_model_credit_gg = shock_model_credit_gg),
  here("Outputs", "all_fe_models.rds")
)
