# Description ---------------------------------------------------------------
# Unsmoothed panel local projections (Jorda 2005) of bank variables on
# population-weighted climate shocks + purged monetary policy surprises +
# interaction, with lags, fixed effects and Driscoll-Kraay standard errors.
# Asymmetric (large-shock) version - July 2026
#
# IRF plots show the MP shock response evaluated at different climate states:
#   (1) "MP shock alone (no climate shock)": beta, i.e. how the MP shock would
#       have manifested with the climate shock switched off;
#   (2) "MP shock during a large negative climate shock (-3 SD)": beta - 3*delta;
#   (3) "MP shock during a large positive climate shock (+3 SD)": beta + 3*delta,
#       i.e. how the same MP shock manifests when it overlaps with a large
#       climate shock in each direction.
# The distance between the baseline and the large-shock paths is the
# amplification (or dampening) caused by the climate shock overlap.

# Preliminaries -------------------------------------------------------------
library(here)

# Functions -----------------------------------------------------------------
source(here("packages.R"))
source(here("Functions", "fx_plot.R"))

# Import --------------------------------------------------------------------
model_data_purge_tbl <- read_rds(
  here("Outputs", "artifacts_model_data_purged.rds")
) |>
  pluck("model_data_purge_tbl")

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

## Purged (orthogonalised) monetary policy surprises ----
## Each resid is paired with its own population-weighted climate shock.
mp_base_vars <- c(
  "miyajima_surprise",
  "romer_surprise",
  "target",
  "forward_guidance",
  "central_bank_information",
  "country_risk"
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
dk_lag <- floor(1.5 * H) # Driscoll-Kraay lag length
ci_levels <- c(0.68, 0.90)

panel_id <- ~ banks + date

# Climate states at which the MP response is evaluated -----------------------
# Baseline (0) is the MP shock on its own; the large states are the overlap.
big_shock_sd <- 3
climate_values <- c(-big_shock_sd, 0, big_shock_sd)

comp_base <- "No climate shock"
comp_neg <- paste0("-", big_shock_sd, " SD")
comp_pos <- paste0("+", big_shock_sd, " SD")

comp_levels <- c(comp_base, comp_neg, comp_pos)
comp_cols <- c("#00496f", "#0f85a0", "#dd4124")
names(comp_cols) <- comp_levels

# Component in the same order as climate_values (-3, 0, +3)
comp_by_value <- c(comp_neg, comp_base, comp_pos)

plot_caption <- paste0(
  "Grey: response to the MP shock on its own. ",
  "Blue / red: response to the same MP shock when it coincides with a large ",
  "negative / positive climate shock (", big_shock_sd, " SD). ",
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
# Returns the MP response evaluated at each climate state, with the delta
# method standard error for that linear combination.
estimate_h <- function(dat, dep, climate, resid, h) {
  lhs <- paste0("f(", dep, ", ", h, ") - l(", dep, ", 1)")
  rhs_shocks <- c(climate, resid, paste0(climate, ":", resid))
  rhs_lags <- c(
    lag_terms(dep, n_lags),
    lag_terms(climate, n_lags),
    lag_terms(resid, n_lags)
  )
  fe <- "banks"
  # No time fixed effects: the MP surprises and climate shocks vary only over
  # time, not across banks.
  trend <- "+ as.numeric(date)"
  fml <- as.formula(
    paste0(
      lhs, " ~ ",
      paste(c(rhs_shocks, rhs_lags), collapse = " + "),
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
  nm_r <- resid
  nm_i <- paste0(climate, ":", resid)
  if (!nm_i %in% names(b)) nm_i <- paste0(resid, ":", climate)
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
  irf <- beta + climate_values * delta
  se <- sqrt(
    var_beta +
      (climate_values^2) * var_delta +
      2 * climate_values * cov_bd
  )
  tibble(
    h = rep(h, length(climate_values)),
    climate_state = climate_values,
    component = factor(comp_by_value, levels = comp_levels),
    irf = irf,
    se = se,
    nobs = mod$nobs,
    tidy = list(co, co, co)
  )
}

# Runner for one (dep, climate, resid) combination ---------------------------
run_lp <- function(dat, dep, climate, resid) {
  map(0:H, ~ estimate_h(dat, dep, climate, resid, .x)) |>
    compact() |>
    bind_rows()
}

# Output folders -------------------------------------------------------------
dir.create(here("Outputs", "LP_unsmoothed_asym"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("Outputs", "LP_unsmoothed_asym", "IRFs"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("Outputs", "LP_unsmoothed_asym", "Plots"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("Outputs", "LP_unsmoothed_asym", "Tables"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("Outputs", "LP_unsmoothed_asym", "Combined_Plots"), showWarnings = FALSE, recursive = TRUE)

# Loop -----------------------------------------------------------------------
irf_store <- list()
table_store <- list()
plot_store <- list()

for (mp_base in mp_base_vars) {
  for (climate in climate_shock_vars) {
    
    resid <- paste0(mp_base, "_", climate, "_resid")
    for (dep in dep_vars) {
      
      combo <- paste(dep, mp_base, climate, sep = "__")
      
      dat <- model_data_purge_tbl |>
        dplyr::select(
          banks, date,
          all_of(dep), all_of(climate), all_of(resid)
        ) |>
        dplyr::arrange(banks, date)
      
      res <- run_lp(dat, dep, climate, resid)
      if (nrow(res) == 0) next
      
      # Attach identifiers + CI bands
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
          model, dep, mp_shock, climate_shock, component, climate_state,
          h, irf, se, lo68, hi68, lo90, hi90, nobs
        )
      
      irf_store[[combo]] <- irf_tbl
      
      # Regression tables (tidy is duplicated per component)
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
      
      # IRF plot: MP shock alone vs MP shock during a large climate shock
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
          # caption = plot_caption,
          x = "Horizon (quaters)",
          y = "Coefficient"
        ) +
        theme_minimal(base_size = 6) +
        theme(
          legend.position = "bottom",
          # legend.direction = "vertical",
          plot.caption = element_text(hjust = 0, size = 6, colour = "grey30")
        ) +
        scale_x_continuous(breaks = scales::breaks_width(1))
      plot_store[[combo]] <- p
      
      ggsave(
        filename = here(
          "Outputs", "LP_unsmoothed_asym", "Plots",
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
# MP surprise variables, with every climate state shown in each panel.
for (dep in dep_vars) {
  for (climate in climate_shock_vars) {
    
    dat_grp <- irf_all |>
      filter(
        dep == .env$dep,
        climate_shock == .env$climate
      ) |>
      mutate(
        component = factor(component, levels = comp_levels),
        mp_shock = factor(mp_shock, levels = mp_base_vars, labels = mp_labels[mp_base_vars])
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
        x = "Horizon (quaters)",
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
        "Outputs", "LP_unsmoothed_asym", "Combined_Plots",
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
    here("Outputs", "LP_unsmoothed_asym", "IRFs", paste0("irf_", nm, ".csv"))
  )
  readr::write_csv(
    table_store[[nm]],
    here("Outputs", "LP_unsmoothed_asym", "Tables", paste0("reg_", nm, ".csv"))
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
    climate_states = climate_values,
    big_shock_sd = big_shock_sd,
    climate_shocks = climate_shock_vars,
    mp_shocks = mp_base_vars
  ),
  plot_store = plot_store
)

write_rds(
  artifacts,
  file = here("Outputs", "artifacts_lp_unsmoothed_asym.rds")
)
