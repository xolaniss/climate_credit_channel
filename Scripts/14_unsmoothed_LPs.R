# Description ---------------------------------------------------------------
# Unsmoothed panel local projections (Jorda 2005) of bank variables on
# climate shocks + purged monetary policy surprises + interaction,
# with lags, fixed effects, and Driscoll-Kraay standard errors - July 2026

# IRF plots show two coefficient paths on the same figure:
#   (1) the purged MP shock (_resid) coefficient, and
#   (2) the interaction (climate x _resid) coefficient,
# so the amplification/dampening of the MP shock by the climate shock is visible.
#WORK IN PROGRESS

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
comp_base <- "MP shock only"
comp_climate <- "MP shock + 1 SD climate shock"
comp_levels <- c(comp_base, comp_climate)
comp_cols <- c("#1b6ca8","#c1272d")
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
  lhs <- paste0("f(",dep,", ",h,") - l(",dep,", 1)")
  rhs_shocks <- c(climate,resid,paste0(climate,":",resid))
  rhs_lags <- c(lag_terms(dep,n_lags),lag_terms(climate,n_lags),lag_terms(resid,n_lags))
  fe <- "banks"
  trend <- "+ as.numeric(date)"
  fml <- as.formula(
    paste0(lhs," ~ ",
           paste(c(rhs_shocks,rhs_lags),collapse=" + "),
           trend," | ",fe)
  )
  mod <- tryCatch(
    feols(
      fml,
      data=dat,
      panel.id=panel_id,
      vcov=vcov_DK(lag=dk_lag)
    ),
    error=function(e) NULL
  )
  if(is.null(mod)) return(NULL)
  co <- broom::tidy(mod)
  b <- coef(mod)
  V <- vcov(mod)
  nm_r <- resid
  nm_i <- paste0(climate,":",resid)
  if(!nm_i %in% names(b)) nm_i <- paste0(resid,":",climate)
  if(!nm_r %in% names(b)) return(NULL)
  beta <- b[[nm_r]]
  var_beta <- V[nm_r,nm_r]
  if(nm_i %in% names(b)){
    delta <- b[[nm_i]]
    var_delta <- V[nm_i,nm_i]
    cov_bd <- V[nm_r,nm_i]
  }else{
    delta <- 0
    var_delta <- 0
    cov_bd <- 0
  }
  tibble(
    h=c(h,h,h),
    series=c("base","climate","interaction"),
    component=c(comp_base,comp_climate,"Interaction"),
    irf=c(beta,beta+delta,delta),
    se=c(
      sqrt(var_beta),
      sqrt(var_beta+var_delta+2*cov_bd),
      sqrt(var_delta)
    ),
    nobs=mod$nobs,
    tidy=list(co,co,co)
  )
}

# Runner for one (dep, climate, resid) combination --------------------------
run_lp <- function(dat, dep, climate, resid) {
  map(0:H, ~ estimate_h(dat, dep, climate, resid, .x)) |>
    compact() |>
    bind_rows()
}

# Loop ----------------------------------------------------------------------
dir.create(here("Outputs", "LP_unsmoothed"),         showWarnings = FALSE, recursive = TRUE)
dir.create(here("Outputs", "LP_unsmoothed", "IRFs"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("Outputs", "LP_unsmoothed", "Plots"),showWarnings = FALSE, recursive = TRUE)
dir.create(here("Outputs", "LP_unsmoothed", "Tables"),showWarnings = FALSE, recursive = TRUE)
dir.create(here("Outputs","LP_unsmoothed","Combined_Plots"), showWarnings=FALSE, recursive=TRUE)

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
        mutate(
          dep=dep,
          mp_shock=mp_base,
          climate_shock=climate,
          model=combo,
          lo68=irf-z_of(0.68)*se,
          hi68=irf+z_of(0.68)*se,
          lo90=irf-z_of(0.90)*se,
          hi90=irf+z_of(0.90)*se
        )
      
      irf_store[[combo]] <- irf_tbl
      
      # Regression tables
      reg_tbl <- res |>
        filter(series == "base") |>
        select(h, tidy) |>
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
      
      # IRF Plots
p <- ggplot(
  filter(irf_tbl,series!="interaction"),
  aes(x=h,y=irf,colour=component,fill=component)
)+
  geom_hline(yintercept=0,linewidth=0.4,colour="grey40")+
  geom_ribbon(aes(ymin=lo90,ymax=hi90),alpha=0.12,colour=NA)+
  geom_ribbon(aes(ymin=lo68,ymax=hi68),alpha=0.25,colour=NA)+
  geom_line(linewidth=0.9)+
  scale_colour_manual(values=comp_cols,name=NULL)+
  scale_fill_manual(values=comp_cols,name=NULL)+
  labs(
    title=paste0("LP: ",dep),
    subtitle=paste0(
      "MP shock: ",mp_base,
      " | Climate shock: ",climate
    ),
    x="Horizon",
    y="Coefficient"
  )+
  theme_minimal(base_size=11)+
  theme(legend.position="bottom")

p_int <- ggplot(
  filter(irf_tbl,series=="interaction"),
  aes(x=h,y=irf)
)+
  geom_hline(yintercept=0,linewidth=0.4,colour="grey40")+
  geom_ribbon(aes(ymin=lo90,ymax=hi90),fill="#c1272d",alpha=0.12)+
  geom_ribbon(aes(ymin=lo68,ymax=hi68),fill="#c1272d",alpha=0.25)+
  geom_line(colour="#c1272d",linewidth=0.9)+
  labs(
    title=paste0("Interaction Effect: ",dep),
    subtitle=paste0(
      "MP shock: ",mp_base,
      " | Climate shock: ",climate
    ),
    x="Horizon",
    y="Interaction coefficient"
  )+
  theme_minimal(base_size=11)
      
      ggsave(
        filename = here("Outputs", "LP_unsmoothed", "Plots",
                        paste0("irf_", combo, ".png")),
        plot = p, width = 7, height = 4.5, dpi = 300
      )
      
      ggsave(
        filename=here(
          "Outputs",
          "LP_unsmoothed",
          "Plots",
          paste0("interaction_",combo,".png")
        ),
        plot=p_int,
        width=7,
        height=4.5,
        dpi=300
      )
    }
  }
}

#Define variable groups
plot_groups <- list(Corporate_Unsecured_Credit=c("corporate_unsecured_credit"),
  Corporate_Unsecured_Rates=c("corporate_unsecured_credit_rate"),
  Corporate_Secured_Credit=c("corporate_secured_credit"),
  Corporate_Secured_Rates=c("corporate_secured_credit_rate"),
  Corporate_Mortgages=c("corporate_mortgages"),
  Corporate_Mortgage_Rates=c("corporate_mortgage_rate"),
  Household_Unsecured_Credit=c("household_unsecured_credit"),
  Household_Unsecured_Rates=c("household_unsecured_credit_rate"),
  Household_Secured_Credit=c("household_secured_credit"),
  Household_Secured_Rates=c("household_secured_credit_rate"),
  Household_Mortgages=c("household_mortgages"),
  Household_Mortgage_Rates=c("household_mortgage_rate")
)

# Bind & export -------------------------------------------------------------
irf_all   <- bind_rows(irf_store)
table_all <- bind_rows(table_store)


# Create combined figures 
for(mp_base in mp_base_vars){
  for(climate in climate_shock_vars){
    
    dat_combo <- irf_all |>
      filter(mp_shock==mp_base,
             climate_shock==climate,
             series!="interaction")
    
    for(grp in names(plot_groups)){
      
      vars <- plot_groups[[grp]]
      
      dat_grp <- dat_combo |>
        filter(dep %in% vars)
      
      if(nrow(dat_grp)==0) next
      
      p <- ggplot(dat_grp,
                  aes(x=h,y=irf,colour=component,fill=component))+
        geom_hline(yintercept=0,colour="grey40",linewidth=0.4)+
        geom_ribbon(aes(ymin=lo90,ymax=hi90),alpha=0.12,colour=NA)+
        geom_ribbon(aes(ymin=lo68,ymax=hi68),alpha=0.25,colour=NA)+
        geom_line(linewidth=0.9)+
        scale_colour_manual(values=comp_cols,name=NULL)+
        scale_fill_manual(values=comp_cols,name=NULL)+
        facet_wrap(~dep,scales="free_y",ncol=2)+
        labs(title=grp,
             subtitle=paste0("MP shock: ",mp_base," | Climate shock: ",climate),
             x="Horizon",
             y="Coefficient")+
        theme_minimal(base_size=11)+
        theme(legend.position="bottom",
              strip.text=element_text(face="bold"))
      
      ggsave(
        here("Outputs",
             "LP_unsmoothed",
             "Combined_Plots",
             paste0(grp,"__",mp_base,"__",climate,".png")),
        plot=p,
        width=10,
        height=6,
        dpi=300
      )
    }
  }
}


# Per-model IRF and table CSVs
walk(names(irf_store), function(nm) {
  readr::write_csv(irf_store[[nm]],
                   here("Outputs", "LP_unsmoothed", "IRFs", paste0("irf_", nm, ".csv")))
  readr::write_csv(table_store[[nm]],
                   here("Outputs", "LP_unsmoothed", "Tables", paste0("reg_", nm, ".csv")))
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
  file = here("Outputs", "artifacts_lp_unsmoothed.rds")
)
