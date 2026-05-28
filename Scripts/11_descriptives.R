# Description ---------------------------------------------------------
# Descriptive statistics and plots for modelling dataset - May 2026
# summary statistics table, time-series plots, density plots, boxplots
# Notes from descriptives: 1. corporate_sector_mortgages, corporate_secured_credit, 
# household_secured_credit and households_residential_mortgages must have log(0) in the series.


# Preliminaries ---------------------------------
library(here)

# Functions -----------------------------------------------
source(here("packages.R"))
source(here("Functions", "fx_plot.R"))

# Import ----------------------------------------------
model_data_tbl <- read_rds(
  here("Outputs", "artifacts_model_data.rds")
) |>
  pluck(1)

# Variable selection --------------------------------------------------

## Variables for descriptive analysis -----------
variables_tbl <- model_data_tbl |>
  dplyr::select(
    -bank,
    -date
  )

## Variable names -------------------------
variable_names <- names(variables_tbl)

# Descriptive statistics ------------------------------------

## Summary statistics table -------------
descriptive_stats_tbl <-
  variables_tbl |>
  summarise(
    across(
      .cols = everything(),
      .fns = list(
        mean   = ~ mean(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE),
        sd     = ~ sd(.x, na.rm = TRUE),
        min    = ~ min(.x, na.rm = TRUE),
        max    = ~ max(.x, na.rm = TRUE),
        n      = ~ sum(!is.na(.x))
      ),
      .names = "{.col}_{.fn}"
    )
  ) |>
  pivot_longer(
    cols = everything(),
    names_to = c("variable", ".value"),
    names_sep = "_(?=[^_]+$)"
  ) |>
  arrange(variable)

# Create all plots ------------------------------------------------

## Create output folders --------------------
dir.create(
  here("Outputs", "Figures", "Descriptive"),
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  here("Outputs", "Tables"),
  recursive = TRUE,
  showWarnings = FALSE
)

# Time-series plots -------------------------------------------

for(var in variable_names){
  
  plot_tbl <-
    model_data_tbl |>
    dplyr::select(
      bank,
      date,
      all_of(var)
    )
  
  p <-
    ggplot(
      plot_tbl,
      aes(
        x = date,
        y = .data[[var]],
        colour = bank
      )
    ) +
    geom_line(linewidth = 0.8) +
    labs(
      title = paste("Time Series:", var),
      x = NULL,
      y = var,
      colour = "Bank"
    ) +
    theme_minimal()
  
  ggsave(
    filename = here(
      "Outputs",
      "Figures",
      "Descriptive",
      paste0(var, "_timeseries.png")
    ),
    plot = p,
    width = 10,
    height = 6
  )
}

# Density plots -------------------------------------------------------

for(var in variable_names){
  
  plot_tbl <-
    model_data_tbl |>
    dplyr::select(
      bank,
      all_of(var)
    )
  
  p <-
    ggplot(
      plot_tbl,
      aes(
        x = .data[[var]],
        fill = bank
      )
    ) +
    geom_density(
      alpha = 0.4
    ) +
    labs(
      title = paste("Density Plot:", var),
      x = var,
      y = "Density",
      fill = "Bank"
    ) +
    theme_minimal()
  
  ggsave(
    filename = here(
      "Outputs",
      "Figures",
      "Descriptive",
      paste0(var, "_density.png")
    ),
    plot = p,
    width = 10,
    height = 6
  )
}

# Boxplots --------------------------------------------

for(var in variable_names){
  
  plot_tbl <-
    model_data_tbl |>
    dplyr::select(
      bank,
      all_of(var)
    )
  
  p <-
    ggplot(
      plot_tbl,
      aes(
        x = bank,
        y = .data[[var]],
        fill = bank
      )
    ) +
    geom_boxplot() +
    labs(
      title = paste("Boxplot:", var),
      x = NULL,
      y = var,
      fill = "Bank"
    ) +
    theme_minimal()
  
  ggsave(
    filename = here(
      "Outputs",
      "Figures",
      "Descriptive",
      paste0(var, "_boxplot.png")
    ),
    plot = p,
    width = 10,
    height = 6
  )
}

# Export --------------------------------------------

## Export descriptive statistics ----------------
write_csv(
  descriptive_stats_tbl,
  file = here(
    "Outputs",
    "Tables",
    "descriptive_statistics.csv"
  )
)

## Save artifacts ---------------------
artifacts <- list(
  descriptive_stats_tbl = descriptive_stats_tbl,
  correlation_tbl = correlation_tbl
)

write_rds(
  artifacts, file = here(
    "Outputs",
    "artifacts_descriptive_statistics.rds"
  )
)