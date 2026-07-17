# Description ---------------------------------------------------------------
# Unsmoothed panel local projections (Jorda 2005) of bank variables on
# climate shocks + purged monetary policy surprises + interaction,
# with lags, fixed effects, and Driscoll-Kraay standard errors - July 2026

# IRF plots show two coefficient paths on the same figure:
#   (1) the purged MP shock (_resid) coefficient, and
#   (2) the interaction (climate x _resid) coefficient,
# so the amplification/dampening of the MP shock by the climate shock is visible.
# WORK IN PROGRESS

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

## Climate shocks ----
climate_shock_vars <- c(
  "land_temp_shock",
  "pop_temp_shock",
  "land_precip_shock",
  "pop_precip_shock"
)

## Purged (orthogonalised) monetary policy surprises ----
## Each resid with its own climate shock.
mp_base_vars <- c(
  "miyajima_surprise",
  "romer_surprise",
  "target",
  "forward_guidance",
  "central_bank_information",
  "country_risk"
)

# LP settings ---------------------------------------------------------------
H         <- 12
n_lags    <- 2
dk_lag    <- floor(1.5 * H)  # Driscoll-Kraay lag length
ci_levels <- c(0.68, 0.90)

panel_id <- ~ banks + date

# Component labels + colours -------------------
comp_neg  <- "Climate = -3 SD"
comp_zero <- "Climate = 0"
comp_pos  <- "Climate = +3 SD"
comp_levels <- c(comp_neg, comp_zero,comp_pos)
comp_cols <- c("#1b6ca8","#444444","#c1272d")
names(comp_cols) <- comp_levels

# z of a two-sided level (0.68 -> 0.994, 0.90 -> 1.645)
z_of <- function(level) qnorm(1 - (1 - level) / 2)

# Build lag terms for a variable for LP --------------------
lag_terms <- function(var, k) {
  if (k < 1) return(character(0))
  paste0("l(", var, ", 1:", k, ")")
}

# Estimation of LP for each horizon funtion ------------
# Returns BOTH the _resid coefficient path and the interaction coefficient path.
estimate_h <- function(dat, dep, climate, resid, h){
  lhs <- paste0("f(", dep, ", ", h, ") - l(", dep, ", 1)")
  rhs_shocks <- c(climate, resid, paste0(climate, ":", resid))
  rhs_lags <- c(lag_terms(dep, n_lags), lag_terms(climate, n_lags), lag_terms(resid, n_lags))
  
  fe <- "banks"
  trend <- "+ as.numeric(date)" #no time fixed effects because the monetary policy surprises and climate shocks vary only over time, not across banks.
  fml <- as.formula(
    paste0(lhs," ~ ",
           paste(c(rhs_shocks, rhs_lags),collapse = " + "),
           trend, " | ", fe)
  )
  mod <- tryCatch(feols( fml,
                         data = dat,
                         panel.id = panel_id,
                         vcov = vcov_DK(lag = dk_lag)
  ),
  error = function(e) NULL
  )
  if(is.null(mod))
    return(NULL)
  co <- broom::tidy(mod)
  b <- coef(mod)
  V <- vcov(mod)
  nm_r <- resid
  nm_i <- paste0(climate, ":", resid)
  if(!nm_i %in% names(b))
    nm_i <- paste0(resid, ":", climate)
  have_r <- nm_r %in% names(b)
  have_i <- nm_i %in% names(b)
  
  if(!have_r)
    return(NULL)
  
  beta <- b[[nm_r]]
  var_beta <- V[nm_r,nm_r]
  
  if(have_i){delta <- b[[nm_i]]
  var_delta <- V[nm_i,nm_i]
  cov_bd <- V[nm_r,nm_i]
  
  }else{
    delta <- 0
    var_delta <- 0
    cov_bd <- 0
  }
  
  climate_values <- c(-3,0,3)
  
  irf <- beta + climate_values*delta
  se <- sqrt(var_beta + (climate_values^2)*var_delta + 2*climate_values*cov_bd)
  
  tibble(h = rep(h,3),
         component = factor(
           comp_levels,
           levels = comp_levels),
         
         irf = irf,
         se = se,
         nobs = mod$nobs,
         tidy = list(co,co,co)
  )
  
}

# Runner for one (dep, climate, resid) combination --------------------------
run_lp <- function(dat, dep, climate, resid) {
  map(0:H, ~ estimate_h(dat, dep, climate, resid, .x)) |>
    compact() |>
    bind_rows()
}

# Loop ----------------------------------------------------------------------
dir.create(here("Outputs", "LP_unsmoothed_asym"),         showWarnings = FALSE, recursive = TRUE)
dir.create(here("Outputs", "LP_unsmoothed_asym", "IRFs"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("Outputs", "LP_unsmoothed_asym", "Plots"),showWarnings = FALSE, recursive = TRUE)
dir.create(here("Outputs", "LP_unsmoothed_asym", "Tables"),showWarnings = FALSE, recursive = TRUE)

irf_store   <- list()
table_store <- list()

for (mp_base in mp_base_vars) {
  for (climate in climate_shock_vars) {
    
    resid <- paste0(mp_base, "_", climate, "_resid")
    if (!resid %in% names(model_data_purge_tbl)) {
      message("Skipping missing resid: ", resid)
      next
    }
    
    for (dep in dep_vars) {
      
      combo <- paste(dep, mp_base, climate, sep = "__")
      message("LP: ", combo)
      
      dat <- model_data_purge_tbl |>
        dplyr::select(banks, date,
                      all_of(dep), all_of(climate), all_of(resid)) |>
        dplyr::arrange(banks, date)
      
      res <- run_lp(dat, dep, climate, resid)
      if (nrow(res) == 0) next
      
      # Attach identifiers + CI bands 
      irf_tbl <- res |>
        mutate(dep = dep, mp_shock = mp_base, climate_shock = climate,
               model = combo) |>
        mutate(
          lo68 = irf - z_of(0.68) * se,
          hi68 = irf + z_of(0.68) * se,
          lo90 = irf - z_of(0.90) * se,
          hi90 = irf + z_of(0.90) * se
        ) |>
        dplyr::select(model, dep, mp_shock, climate_shock,
                      component, h, irf, se, lo68, hi68, lo90, hi90, nobs)
      
      irf_store[[combo]] <- irf_tbl
      
      # Regression tables
      reg_tbl <- res |>
        dplyr::filter(component == comp_zero) |>   # tidy is duplicated per component
        dplyr::select(h, tidy) |>
        unnest(tidy) |>
        mutate(model = combo, dep = dep,
               mp_shock = mp_base, climate_shock = climate) |>
        dplyr::select(model, dep, mp_shock, climate_shock, h,
                      term, estimate, std.error, statistic, p.value)
      
      table_store[[combo]] <- reg_tbl
      
      # IRF Plots
      p <- ggplot(irf_tbl, aes(x = h, y = irf,
                               colour = component, fill = component)) +
        geom_hline(yintercept = 0, linewidth = 0.4, colour = "grey40") +
        geom_ribbon(aes(ymin = lo90, ymax = hi90),
                    alpha = 0.12, colour = NA) +
        geom_ribbon(aes(ymin = lo68, ymax = hi68),
                    alpha = 0.25, colour = NA) +
        geom_line(linewidth = 0.9) +
        scale_colour_manual(values = comp_cols, drop = FALSE, name = NULL) +
        scale_fill_manual(values = comp_cols, drop = FALSE, name = NULL) +
        labs(
          title = paste0("Asymetric Unsmoothed LP: ", dep),
          subtitle = paste0("MP shock: ", mp_base,
                            "  |  Climate shock: ", climate,
                            "  (DK SE, 68%/90% bands)"),
          x = "Horizon", y = "Coefficient"
        ) +
        theme_minimal(base_size = 11) +
        theme(legend.position = "bottom")
      
      ggsave(
        filename = here("Outputs", "LP_unsmoothed_asym", "Plots",
                        paste0("irf_", combo, ".png")),
        plot = p, width = 7, height = 4.5, dpi = 300
      )
    }
  }
}

# Bind & export -------------------------------------------------------------
irf_all   <- bind_rows(irf_store)
table_all <- bind_rows(table_store)

# Per-model IRF and table CSVs
walk(names(irf_store), function(nm) {
  readr::write_csv(irf_store[[nm]],
                   here("Outputs", "LP_unsmoothed_asym", "IRFs", paste0("irf_", nm, ".csv")))
  readr::write_csv(table_store[[nm]],
                   here("Outputs", "LP_unsmoothed_asym", "Tables", paste0("reg_", nm, ".csv")))
})

artifacts <- list(
  irf_unsmoothed    = irf_all,
  tables_unsmoothed = table_all,
  settings = list(H = H, n_lags = n_lags,
                  dk_lag = dk_lag, ci_levels = ci_levels,
                  components = comp_levels)
)

write_rds(
  artifacts,
  file = here("Outputs", "artifacts_lp_unsmoothed_asym.rds")
)
