# Description
# Running fixed effects regressions for all bank variables - May 2026
# PURGED version: monetary policy surprises are replaced by their purged
# (orthogonalised) residuals from surprise = intercept + climate shock + residual.
## NOTE 1 : Only bank fixed effects (no date fixed effects) because data 
## ...does not vary over time between banks so date fixed effects are colinear with explanatory variables
## NOTE 2: Consider splitting the climate variables between positive and negative shocks
## NOTE 3: Consider adding control variables 
## NOTE 4: Problem: Surprises are highly correlated with climate variables - consider using the error term of
## ... surprise = intercept + climate shock + residual. Use residual which shows surprises that cannot be explained by climate shocks.
## ...then main regression is: bank variable = climate + residual + (climate*residual) + bank FE + error
## ...--> IMPLEMENTED in this script: mp_shock_value holds the purged residual.
## NOTE 5: Some standard errors eg. secured credit rate are huge - consider logging dep. variable


# Preliminaries -----------------------------------------------------------
library(here)

# Functions ---------------------------------------------------------------
source(here("packages.R"))
source(here("Functions", "fx_plot.R"))
source(here("Functions", "group_ols_functions.R"))

# Import ---------------------------------------------------------------
## Purged model data: contains the original surprises AND the climate-specific
## residuals (<surprise>_<climate>_resid).
model_data_purge_tbl <- read_rds(
  here("Outputs", "artifacts_model_data_purged.rds")
) |>
  pluck("model_data_purge_tbl")

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
## Base names are kept for grouping and labelling; the values fed into the
## regression are the purged residuals paired with each climate shock.
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
    climate_shock_value * mp_shock_value |
    banks + lubridate::year(date))


# Long data ------------------------------------------------------------
## Each purged residual belongs to exactly one climate shock, so the long
## table is built over the valid (surprise, climate) pairs rather than a full
## cross of every MP column with every climate column.
purge_pairs_tbl <-
  tidyr::expand_grid(
    mp_shock      = surprise_vars,
    climate_shock = climate_vars
  ) |>
  mutate(
    resid_var = paste0(mp_shock, "_", climate_shock, "_resid")
  )

missing_resids <- setdiff(purge_pairs_tbl$resid_var, names(model_data_purge_tbl))
if (length(missing_resids) > 0) {
  stop(
    "Missing purged residual columns: ",
    paste(missing_resids, collapse = ", ")
  )
}

model_data_long_tbl <-
  purrr::pmap(
    list(
      purge_pairs_tbl$mp_shock,
      purge_pairs_tbl$climate_shock,
      purge_pairs_tbl$resid_var
    ),
    function(mp_nm, climate_nm, resid_nm) {
      model_data_purge_tbl |>
        dplyr::transmute(
          banks,
          date,
          mp_shock            = mp_nm,
          climate_shock       = climate_nm,
          mp_shock_value      = .data[[resid_nm]],
          climate_shock_value = .data[[climate_nm]],
          dplyr::across(all_of(dependent_vars))
        ) |>
        pivot_longer(
          cols      = all_of(dependent_vars),
          names_to  = "credit",
          values_to = "credit_value"
        )
    }
  ) |>
  bind_rows() |>
  relocate(
    c("credit", "mp_shock", "climate_shock"), .before = credit_value
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
  filter(!mp_shock %in% c("country_risk", "central_bank_information")) |>
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
      subtitle = "Fixed effects regression (bank FE + quarter FE) | 95% CI, clustered SEs",
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
  here("Outputs", "all_fe_models_purged.rds")
)
