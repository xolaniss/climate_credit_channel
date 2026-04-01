# Description
# Combined data
# Preliminaries -----------------------------------------------------------
library(here)

# Functions ---------------------------------------------------------------
source(here("packages.R"))
source(here("Functions", "fx_plot.R"))

# Import -------------------------------------------------------------
climate_data_tbl <- read_rds(here("Outputs", "artifacts_climate_data.rds")) |> pluck(1)
climate_shocks_tbl <- read_rds(here("Outputs", "artifacts_climate_shocks.rds")) |> pluck(1)
credit_extension_tbl <- read_rds(here("Outputs", "artifacts_credit_market.rds")) |> pluck(1)
lending_tbl <- read_rds(here("Outputs", "artifacts_credit_market.rds")) |> pluck(2)
sa_surprises_tbl <- read_rds(here("Outputs", "artifacts_sa_surprises.rds")) |> pluck(1)


# Cleaning -----------------------------------------------------------------

# Transformations --------------------------------------------------------


# EDA ---------------------------------------------------------------


# Graphing ---------------------------------------------------------------


# Export ---------------------------------------------------------------
artifacts_ <- list (

)

write_rds(artifacts_, file = here("Outputs", "artifacts_.rds"))


