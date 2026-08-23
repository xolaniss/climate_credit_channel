# Description ---------------------------------------------------------------
# Unsmoothed panel local projections (Jorda 2005) of bank variables on
# population-weighted climate shocks + ORIGINAL (unpurged) monetary policy
# surprises + interaction + controls, with lags (including of controls), fixed
# effects and Driscoll-Kraay standard errors.
# Results are very similar to the acse without controls
# Asymmetric (large-shock) version - August 2026

# Preliminaries -------------------------------------------------------------
library(here)

# Functions -----------------------------------------------------------------
source(here("packages.R"))
source(here("Functions", "fx_plot.R"))

# Import --------------------------------------------------------------------
## Unpurged model data: original MP surprises, not the orthogonalised resids.
model_data_tbl <- read_rds(
  here("Outputs", "artifacts_model_data.rds")
) |>
  pluck("model_data_tbl")

if (is.null(model_data_tbl)) {
  model_data_tbl <- read_rds(
    here("Outputs", "artifacts_model_data.rds")
  ) |>
    pluck("model_data_tbl")
}

# Define variables ----------------------------------------------------------

## Dependent (bank) variables ----
dep_vars <- c(
  "corporate_unsecured_credit",
  "corporate_secured_credit",
  "corporate_mortgages",
  "household_unsecured_credit",
  "household_secured_credit",
  "household_mortgages",
  "corporate_unsecured_credit_rate",
  "household_unsecured_credit_rate",
  "corporate_mortgage_rate",
  "household_mortgage_rate",
  "corporate_secured_credit_rate",
  "household_secured_credit_rate"
)

## Climate shocks (population-weighted only) ----
climate_shock_vars <- c(
  "pop_temp_shock",
  "pop_precip_shock"
)

## Original (unpurged) monetary policy surprises ----
mp_base_vars <- c(
  "miyajima_surprise",
  "romer_surprise",
  "target",
  "forward_guidance",
  "central_bank_information",
  "country_risk"
)

## Macroeconomic controls ----
control_vars <- c(
  "unemployment_rate",
  "usd_zar",
  "gdp_growth"
)

## Display labels -----------------------------------------------------------
mp_labels <- c(
  miyajima_surprise = "Miyajima surprise",
  romer_surprise = "Romer surprise",
  target = "Target",
  forward_guidance = "Forward guidance",
  central_bank_information = "Central bank information",
  country_risk = "Country risk"
)

climate_labels <- c(
  pop_temp_shock = "Population-weighted temperature shock",
  pop_precip_shock = "Population-weighted precipitation shock"
)

dep_labels <- c(
  corporate_unsecured_credit = "Corporate unsecured credit",
  corporate_secured_credit = "Corporate secured credit",
  corporate_mortgages = "Corporate mortgages",
  household_unsecured_credit = "Household unsecured credit",
  household_secured_credit = "Household secured credit",
  household_mortgages = "Household mortgages",
  corporate_unsecured_credit_rate = "Corporate unsecured credit rate",
  household_unsecured_credit_rate = "Household unsecured credit rate",
  corporate_mortgage_rate = "Corporate mortgage rate",
  household_mortgage_rate = "Household mortgage rate",
  corporate_secured_credit_rate = "Corporate secured credit rate",
  household_secured_credit_rate = "Household secured credit rate"
)

# LP settings ---------------------------------------------------------------
H <- 12
n_lags <- 2
dk_lag <- floor(1.5 * H)
ci_levels <- c(0.68, 0.90)

panel_id <- ~ banks + date

# Climate states at which the MP response is evaluated -----------------------
# Baseline (0) is the MP shock on its own; the extreme state is the overlap.
# Only one direction is of interest per climate variable:
#   temperature   -> +3 SD (extreme heat)
#   precipitation -> -3 SD (extreme dryness)
big_shock_sd <- 3

climate_direction <- c(
  pop_temp_shock = 1,
  pop_precip_shock = -1
)

comp_base <- "No climate shock"
comp_neg <- paste0("MP Shock with extreme dryness")
comp_pos <- paste0("MP Shock with extreme heat")

# Full label vocabulary (kept for the colour mapping and for the artifacts);
# each individual model uses only the baseline plus its own extreme state.
comp_levels <- c(comp_base, comp_neg, comp_pos)
comp_cols <- c("#00496f", "#dd4124", "#dd4124")
names(comp_cols) <- comp_levels

# Extreme-state label for a given climate variable
comp_shock_of <- function(climate) {
  if (climate_direction[[climate]] > 0) comp_pos else comp_neg
}

# Component levels used in a given model / plot (baseline first)
comp_levels_of <- function(climate) {
  c(comp_base, comp_shock_of(climate))
}

# Climate states, in the same order as comp_levels_of()
climate_values_of <- function(climate) {
  c(0, big_shock_sd * climate_direction[[climate]])
}

plot_caption <- paste0(
  "Dark blue: response to the MP shock on its own. ",
  "Red / light blue: response to the same MP shock when it coincides with an ",
  "extreme climate shock of ", big_shock_sd, " SD ",
  "(+", big_shock_sd, " SD temperature, i.e. extreme heat; ",
  "-", big_shock_sd, " SD precipitation, i.e. extreme dryness). ",
  "Bands: 68% and 90% Driscoll-Kraay confidence intervals."
)

# z of a two-sided level (0.68 -> 0.994, 0.90 -> 1.645)
z_of <- function(level) qnorm(1 - (1 - level) / 2)

# Build lag terms for a variable for LP --------------------------------------
lag_terms <- function(var, k) {
  if (k < 1) return(character(0))
  paste0("l(", var, ", 1:", k, ")")
}

# Estimation of LP for each horizon ------------------------------------------
# Returns the MP response evaluated at the baseline and at the extreme climate
# state of interest, with the delta method standard error for each linear
# combination.
estimate_h <- function(dat, dep, climate, mp_var, h) {
  states <- climate_values_of(climate)
  comps <- comp_levels_of(climate)
  lhs <- paste0("f(", dep, ", ", h, ") - l(", dep, ", 1)")
  rhs_shocks <- c(climate, mp_var, paste0(climate, ":", mp_var))
  rhs_lags <- c(
    lag_terms(dep, n_lags),
    lag_terms(climate, n_lags),
    lag_terms(mp_var, n_lags)
  )
  rhs_controls <- c(
    control_vars,
    unlist(lapply(control_vars, lag_terms, k = n_lags))
  )
  fe <- "banks"
  trend <- "+ as.numeric(date)"
  fml <- as.formula(
    paste0(
      lhs, " ~ ",
      paste(c(rhs_shocks, rhs_lags, rhs_controls), collapse = " + "),
      trend, " | ", fe
    )
  )
  mod <- tryCatch(
    feols(
      fml,
      data = dat,
      panel.id = panel_id,
      vcov = vcov_DK(lag = dk_lag)
    ),
    error = function(e) NULL
  )
  if (is.null(mod)) return(NULL)
  co <- broom::tidy(mod)
  b <- coef(mod)
  V <- vcov(mod)
  nm_r <- mp_var
  nm_i <- paste0(climate, ":", mp_var)
  if (!nm_i %in% names(b)) nm_i <- paste0(mp_var, ":", climate)
  have_r <- nm_r %in% names(b)
  have_i <- nm_i %in% names(b)
  if (!have_r) return(NULL)
  beta <- b[[nm_r]]
  var_beta <- V[nm_r, nm_r]
  if (have_i) {
    delta <- b[[nm_i]]
    var_delta <- V[nm_i, nm_i]
    cov_bd <- V[nm_r, nm_i]
  } else {
    delta <- 0
    var_delta <- 0
    cov_bd <- 0
  }
  irf <- beta + states * delta
  se <- sqrt(
    var_beta +
      (states^2) * var_delta +
      2 * states * cov_bd
  )
  tibble(
    h = rep(h, length(states)),
    climate_state = states,
    component = factor(comps, levels = comps),
    irf = irf,
    se = se,
    nobs = mod$nobs,
    tidy = rep(list(co), length(states))
  )
}

# Runner for one (dep, climate, mp_var) combination ---------------------------
run_lp <- function(dat, dep, climate, mp_var) {
  map(0:H, ~ estimate_h(dat, dep, climate, mp_var, .x)) |>
    compact() |>
    bind_rows()
}

# Output folders -------------------------------------------------------------
dir.create(here("Outputs", "LP_unsmoothed_asym_robust"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("Outputs", "LP_unsmoothed_asym_robust", "IRFs"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("Outputs", "LP_unsmoothed_asym_robust", "Plots"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("Outputs", "LP_unsmoothed_asym_robust", "Tables"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("Outputs", "LP_unsmoothed_asym_robust", "Combined_Plots"), showWarnings = FALSE, recursive = TRUE)

# Loop -----------------------------------------------------------------------
irf_store <- list()
table_store <- list()
plot_store <- list()

for (mp_base in mp_base_vars) {
  for (climate in climate_shock_vars) {
    mp_var <- mp_base   # original, unpurged MP surprise
    for (dep in dep_vars) {
      combo <- paste(dep, mp_base, climate, sep = "__")
      dat <- model_data_tbl |>
        dplyr::select(
          banks,
          date,
          all_of(dep),
          all_of(climate),
          all_of(mp_var),
          all_of(control_vars)
        ) |>
        dplyr::arrange(banks, date)
      res <- run_lp(dat, dep, climate, mp_var)
      if (nrow(res) == 0) next
      irf_tbl <- res |>
        mutate(
          dep = dep,
          mp_shock = mp_base,
          climate_shock = climate,
          model = combo,
          lo68 = irf - z_of(0.68) * se,
          hi68 = irf + z_of(0.68) * se,
          lo90 = irf - z_of(0.90) * se,
          hi90 = irf + z_of(0.90) * se
        ) |>
        dplyr::select(
          model,
          dep,
          mp_shock,
          climate_shock,
          component,
          climate_state,
          h,
          irf,
          se,
          lo68,
          hi68,
          lo90,
          hi90,
          nobs
        )
      irf_store[[combo]] <- irf_tbl
      reg_tbl <- res |>
        dplyr::filter(component == comp_base) |>
        dplyr::select(h, tidy) |>
        unnest(tidy) |>
        mutate(
          model = combo,
          dep = dep,
          mp_shock = mp_base,
          climate_shock = climate
        ) |>
        dplyr::select(
          model,
          dep,
          mp_shock,
          climate_shock,
          h,
          term,
          estimate,
          std.error,
          statistic,
          p.value
        )
      table_store[[combo]] <- reg_tbl
      p <- ggplot(
        irf_tbl,
        aes(x = h, y = irf, colour = component, fill = component)
      ) +
        geom_hline(yintercept = 0, linewidth = 0.4, colour = "grey40") +
        geom_ribbon(aes(ymin = lo90, ymax = hi90), alpha = 0.12, colour = NA) +
        geom_ribbon(aes(ymin = lo68, ymax = hi68), alpha = 0.25, colour = NA) +
        geom_line(linewidth = 0.9) +
        scale_colour_manual(values = comp_cols, drop = FALSE, name = NULL) +
        scale_fill_manual(values = comp_cols, drop = FALSE, name = NULL) +
        labs(
          title = paste0(dep_labels[[dep]]),
          subtitle = paste0(
            mp_labels[[mp_base]], "\n",
            climate_labels[[climate]]
          ),
          x = "Horizon (quarters)",
          y = "Coefficient"
        ) +
        theme_minimal(base_size = 6) +
        theme(
          legend.position = "bottom",
          plot.caption = element_text(hjust = 0, size = 6, colour = "grey30")
        ) +
        scale_x_continuous(breaks = scales::breaks_width(1))
      plot_store[[combo]] <- p
      ggsave(
        filename = here(
          "Outputs",
          "LP_unsmoothed_asym_robust",
          "Plots",
          paste0("irf_", combo, ".png")
        ),
        plot = p,
        width = 7,
        height = 5,
        dpi = 300
      )
    }
  }
}

# Bind & export --------------------------------------------------------------
irf_all <- bind_rows(irf_store)
table_all <- bind_rows(table_store)

# Combined figures -----------------------------------------------------------
# One figure per dependent variable and climate shock, faceted over all six
# MP surprise variables, with the baseline and the extreme climate state of
# interest shown in each panel.
for (dep in dep_vars) {
  for (climate in climate_shock_vars) {
    dat_grp <- irf_all |>
      filter(
        dep == .env$dep,
        climate_shock == .env$climate
      ) |>
      mutate(
        component = factor(component, levels = comp_levels_of(climate)),
        mp_shock = factor(
          mp_shock,
          levels = mp_base_vars,
          labels = mp_labels[mp_base_vars]
        )
      )
    if (nrow(dat_grp) == 0) next
    p_comb <- ggplot(
      dat_grp,
      aes(x = h, y = irf, colour = component, fill = component)
    ) +
      geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.4) +
      geom_ribbon(aes(ymin = lo90, ymax = hi90), alpha = 0.12, colour = NA) +
      geom_ribbon(aes(ymin = lo68, ymax = hi68), alpha = 0.25, colour = NA) +
      geom_line(linewidth = 0.9) +
      scale_colour_manual(values = comp_cols, drop = FALSE, name = NULL) +
      scale_fill_manual(values = comp_cols, drop = FALSE, name = NULL) +
      facet_wrap(~ mp_shock, scales = "free_y", ncol = 2) +
      labs(
        title = dep_labels[[dep]],
        subtitle = paste0(
          "Response to all six MP surprises | Climate shock: ",
          climate_labels[[climate]]
        ),
        caption = plot_caption,
        x = "Horizon (quarters)",
        y = "Coefficient"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        legend.position = "bottom",
        legend.direction = "vertical",
        strip.text = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0, size = 8, colour = "grey30")
      )
    ggsave(
      filename = here(
        "Outputs",
        "LP_unsmoothed_asym_robust",
        "Combined_Plots",
        paste0("combined_", dep, "__", climate, ".png")
      ),
      plot = p_comb,
      width = 11,
      height = 9.5,
      dpi = 300
    )
  }
}

# Per-model IRF and table CSVs ------------------------------------------------
walk(names(irf_store), function(nm) {
  readr::write_csv(
    irf_store[[nm]],
    here(
      "Outputs",
      "LP_unsmoothed_asym_robust",
      "IRFs",
      paste0("irf_", nm, ".csv")
    )
  )
  readr::write_csv(
    table_store[[nm]],
    here(
      "Outputs",
      "LP_unsmoothed_asym_robust",
      "Tables",
      paste0("reg_", nm, ".csv")
    )
  )
})

artifacts <- list(
  irf_unsmoothed = irf_all,
  tables_unsmoothed = table_all,
  settings = list(
    H = H,
    n_lags = n_lags,
    dk_lag = dk_lag,
    ci_levels = ci_levels,
    components = comp_levels,
    components_by_climate = map(
      set_names(climate_shock_vars),
      comp_levels_of
    ),
    climate_states = map(
      set_names(climate_shock_vars),
      climate_values_of
    ),
    climate_direction = climate_direction,
    big_shock_sd = big_shock_sd,
    climate_shocks = climate_shock_vars,
    mp_shocks = mp_base_vars,
    controls = control_vars
  ),
  plot_store = plot_store
)

write_rds(
  artifacts,
  file = here("Outputs", "artifacts_lp_unsmoothed_asym_robust.rds")
)
