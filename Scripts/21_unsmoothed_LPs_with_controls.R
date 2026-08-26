# Description ---------------------------------------------------------------
# Unsmoothed panel local projections (Jorda 2005) of bank variables on
# population-weighted climate shocks + ORIGINAL (unpurged) monetary policy
# surprises + interaction, with lags, fixed effects, controls (2 lags of each
# control are also included) and Driscoll-Kraay standard errors.
# July 2026
#
# Results are generally similar to baseline case. Suggest using this as robustness

# Preliminaries -------------------------------------------------------------
library(here)

# Functions -----------------------------------------------------------------
source(here("packages.R"))
source(here("Functions","fx_plot.R"))

# Import --------------------------------------------------------------------
## Unpurged model data: original MP surprises, not the orthogonalised resids.
model_data_controls_tbl <- read_rds(
  here("Outputs","artifacts_model_data_controls.rds")
) |>
  pluck("model_data_controls_tbl") |> 
  mutate(
    fe_date = as.numeric(lubridate::quarter(date))
  )

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
ci_levels <- c(0.68,0.90)

panel_id <- ~ banks + date

# Component labels + colours ------------------------------------------------
comp_base <- "MP shock alone"
comp_climate <- "MP shock with climate shock"
comp_interaction <- "Interaction (climate amplification)"
comp_levels <- c(comp_base,comp_climate)
comp_cols <- c("#00496f","#dd4124")
names(comp_cols) <- comp_levels

# Create z of a two-sided level ----------------------------------------------
z_of <- function(level) qnorm(1 - (1 - level) / 2)

# Build lag terms for a variable for LP --------------------------------------
lag_terms <- function(var,k) {
  paste0("l(",var,", 1:",k,")")
}

# Estimation of LP for each horizon ------------------------------------------
# Returns the pure MP path (beta 1), the climate-overlap path (beta 1 + delta)
# and the interaction path (delta).
estimate_h <- function(dat,dep,climate,mp_var,h) {
  lhs <- paste0("f(",dep,", ",h,") - l(",dep,", 1)")
  rhs_shocks <- c(climate,mp_var,paste0(climate,":",mp_var))
  rhs_lags <- c(
    lag_terms(dep,n_lags),
    lag_terms(climate,n_lags),
    lag_terms(mp_var,n_lags)
  )
  rhs_controls <- unlist(
    lapply(control_vars,function(x) c(x,lag_terms(x,n_lags)))
  )
  fe <- "banks"
  trend <- "+ date"
  fml <- as.formula(
    paste0(
      lhs," ~ ",
      paste(c(rhs_shocks,rhs_lags,rhs_controls),collapse = " + "),
      trend," | ",fe
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
  nm_i <- paste0(climate,":",mp_var)
  if (!(nm_r %in% names(b))) return(NULL)
  beta <- b[[nm_r]]
  var_beta <- V[nm_r,nm_r]
  if (nm_i %in% names(b)) {
    delta <- b[[nm_i]]
    var_delta <- V[nm_i,nm_i]
    cov_bd <- V[nm_r,nm_i]
  } else {
    delta <- 0
    var_delta <- 0
    cov_bd <- 0
  }
  tibble(
    h = c(h,h,h),
    series = c("base","climate","interaction"),
    component = c(comp_base,comp_climate,comp_interaction),
    irf = c(beta,beta + delta,delta),
    se = c(
      sqrt(var_beta),
      sqrt(var_beta + var_delta + 2 * cov_bd),
      sqrt(var_delta)
    ),
    nobs = mod$nobs,
    tidy = list(co,co,co)
  )
}

# Estimate the complete local projection ------------------------------------
run_lp <- function(dat,dep,climate,mp_var) {
  map(0:H,~ estimate_h(dat,dep,climate,mp_var,.x)) |>
    compact() |>
    bind_rows()
}

# Output folders -------------------------------------------------------------
dir.create(here("Outputs","LP_unsmoothed_robust"),showWarnings = FALSE,recursive = TRUE)
dir.create(here("Outputs","LP_unsmoothed_robust","IRFs"),showWarnings = FALSE,recursive = TRUE)
dir.create(here("Outputs","LP_unsmoothed_robust","Plots"),showWarnings = FALSE,recursive = TRUE)
dir.create(here("Outputs","LP_unsmoothed_robust","Tables"),showWarnings = FALSE,recursive = TRUE)
dir.create(here("Outputs","LP_unsmoothed_robust","Combined_Plots"),showWarnings = FALSE,recursive = TRUE)

# Loop -----------------------------------------------------------------------
irf_store <- list()
table_store <- list()
plot_store <- list()

for (mp_base in mp_base_vars) {
  for (climate in climate_shock_vars) {
    mp_var <- mp_base   # original, unpurged MP surprise
    for (dep in dep_vars) {
      combo <- paste(dep,mp_base,climate,sep = "__")
      dat <- model_data_controls_tbl |>
        dplyr::select(
          banks,date,
          all_of(dep),all_of(climate),all_of(mp_var),
          all_of(control_vars)
        ) |>
        dplyr::arrange(banks,date)
      res <- run_lp(dat,dep,climate,mp_var)
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
        )
      irf_store[[combo]] <- irf_tbl
      reg_tbl <- res |>
        filter(series == "base") |>
        select(h,tidy) |>
        unnest(tidy) |>
        mutate(
          model = combo,
          dep = dep,
          mp_shock = mp_base,
          climate_shock = climate
        ) |>
        select(
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
      p <- irf_tbl |>
        filter(series != "interaction") |>
        mutate(component = factor(component,levels = comp_levels)) |>
        ggplot(aes(x = h,y = irf,colour = component,fill = component)) +
        geom_hline(yintercept = 0,linewidth = 0.4,colour = "grey40") +
        geom_ribbon(aes(ymin = lo90,ymax = hi90),alpha = 0.12,colour = NA) +
        geom_ribbon(aes(ymin = lo68,ymax = hi68),alpha = 0.25,colour = NA) +
        geom_line(linewidth = 0.9) +
        scale_colour_manual(values = comp_cols,name = NULL) +
        scale_fill_manual(values = comp_cols,name = NULL) +
        labs(
          title = paste0(dep_labels[[dep]]),
          subtitle = paste0(
            mp_labels[[mp_base]],
            "\n",climate_labels[[climate]]
          ),
          x = "Horizon (quaters)",
          y = "Coefficient"
        ) +
        theme_minimal(base_size = 6) +
        theme(
          legend.position = "bottom",
          plot.caption = element_text(hjust = 0,size = 6,colour = "grey30")
        ) +
        scale_x_continuous(breaks = scales::breaks_width(1))
      plot_store[[combo]] <- p
      p_int <- irf_tbl |>
        filter(series == "interaction") |>
        ggplot(aes(x = h,y = irf)) +
        geom_hline(yintercept = 0,linewidth = 0.4,colour = "grey40") +
        geom_ribbon(aes(ymin = lo90,ymax = hi90),fill = "#c1272d",alpha = 0.12) +
        geom_ribbon(aes(ymin = lo68,ymax = hi68),fill = "#c1272d",alpha = 0.25) +
        geom_line(colour = "#c1272d",linewidth = 0.9) +
        labs(
          title = paste0("Climate amplification of the MP shock: ",dep_labels[[dep]]),
          subtitle = paste0(
            "MP shock: ",mp_labels[[mp_base]],
            " | Climate shock: ",climate_labels[[climate]]
          ),
          caption = "Difference between the two IRF paths: the extra response attributable to the climate shock overlap.",
          x = "Horizon (quaters)",
          y = "Interaction coefficient"
        ) +
        theme_minimal(base_size = 11) +
        theme(plot.caption = element_text(hjust = 0,size = 8,colour = "grey30"))
      ggsave(
        filename = here(
          "Outputs","LP_unsmoothed_robust","Plots",
          paste0("irf_",combo,".png")
        ),
        plot = p,
        width = 7,
        height = 4.5,
        dpi = 300
      )
      ggsave(
        filename = here(
          "Outputs","LP_unsmoothed_robust","Plots",
          paste0("interaction_",combo,".png")
        ),
        plot = p_int,
        width = 7,
        height = 4.5,
        dpi = 300
      )
    }
  }
}

# Bind & export --------------------------------------------------------------
irf_all <- bind_rows(irf_store)
table_all <- bind_rows(table_store)

# Combined figures -----------------------------------------------------------
for (dep in dep_vars) {
  for (climate in climate_shock_vars) {
    dat_grp <- irf_all |>
      filter(
        dep == .env$dep,
        climate_shock == .env$climate,
        series != "interaction"
      ) |>
      mutate(
        component = factor(component,levels = comp_levels),
        mp_shock = factor(
          mp_shock,
          levels = mp_base_vars,
          labels = mp_labels[mp_base_vars]
        )
      )
    if (nrow(dat_grp) == 0) next
    p_comb <- ggplot(
      dat_grp,
      aes(x = h,y = irf,colour = component,fill = component)
    ) +
      geom_hline(yintercept = 0,colour = "grey40",linewidth = 0.4) +
      geom_ribbon(aes(ymin = lo90,ymax = hi90),alpha = 0.12,colour = NA) +
      geom_ribbon(aes(ymin = lo68,ymax = hi68),alpha = 0.25,colour = NA) +
      geom_line(linewidth = 0.9) +
      scale_colour_manual(values = comp_cols,name = NULL) +
      scale_fill_manual(values = comp_cols,name = NULL) +
      facet_wrap(~ mp_shock,scales = "free_y",ncol = 2) +
      labs(
        title = dep_labels[[dep]],
        subtitle = paste0(
          "Response to all six MP surprises | Climate shock: ",
          climate_labels[[climate]]
        ),
        caption = paste0(
          "Blue: response to the MP shock on its own. ",
          "Red: response to the same MP shock when it coincides with a 1 SD climate shock. ",
          "Bands: 68% and 90% Driscoll-Kraay confidence intervals."
        ),
        x = "Horizon (quaters)",
        y = "Coefficient"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        legend.position = "bottom",
        strip.text = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0,size = 8,colour = "grey30")
      )
    ggsave(
      filename = here(
        "Outputs","LP_unsmoothed_robust","Combined_Plots",
        paste0("combined_",dep,"__",climate,".png")
      ),
      plot = p_comb,
      width = 11,
      height = 9,
      dpi = 300
    )
  }
}

# Combined interaction figures ----------------------------------------------
for (dep in dep_vars) {
  for (climate in climate_shock_vars) {
    dat_int <- irf_all |>
      filter(
        dep == .env$dep,
        climate_shock == .env$climate,
        series == "interaction"
      ) |>
      mutate(
        mp_shock = factor(
          mp_shock,
          levels = mp_base_vars,
          labels = mp_labels[mp_base_vars]
        )
      )
    if (nrow(dat_int) == 0) next
    p_int_comb <- ggplot(dat_int,aes(x = h,y = irf)) +
      geom_hline(yintercept = 0,colour = "grey40",linewidth = 0.4) +
      geom_ribbon(aes(ymin = lo90,ymax = hi90),fill = "#c1272d",alpha = 0.12) +
      geom_ribbon(aes(ymin = lo68,ymax = hi68),fill = "#c1272d",alpha = 0.25) +
      geom_line(colour = "#c1272d",linewidth = 0.9) +
      facet_wrap(~ mp_shock,scales = "free_y",ncol = 2) +
      labs(
        title = paste0("Climate amplification: ",dep_labels[[dep]]),
        subtitle = paste0(
          "Interaction across all six MP surprises | Climate shock: ",
          climate_labels[[climate]]
        ),
        x = "Horizon (quaters)",
        y = "Interaction coefficient"
      ) +
      theme_minimal(base_size = 11) +
      theme(strip.text = element_text(face = "bold"))
    ggsave(
      filename = here(
        "Outputs","LP_unsmoothed_robust","Combined_Plots",
        paste0("combined_interaction_",dep,"__",climate,".png")
      ),
      plot = p_int_comb,
      width = 11,
      height = 9,
      dpi = 300
    )
  }
}

# Per-model IRF and table CSVs -----------------------------------------------
walk(names(irf_store),function(nm) {
  readr::write_csv(
    irf_store[[nm]],
    here("Outputs","LP_unsmoothed_robust","IRFs",paste0("irf_",nm,".csv"))
  )
  readr::write_csv(
    table_store[[nm]],
    here("Outputs","LP_unsmoothed_robust","Tables",paste0("reg_",nm,".csv"))
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
    components = c(comp_levels,comp_interaction),
    climate_shocks = climate_shock_vars,
    mp_shocks = mp_base_vars,
    controls = control_vars
  ),
  plot_store = plot_store
)

write_rds(
  artifacts,
  file = here("Outputs","artifacts_lp_unsmoothed_robust.rds")
)
