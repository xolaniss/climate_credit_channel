# Description
# Creation of surprises

# Preliminaries -----------------------------------------------------------
library(here)
library(R.matlab)

# Functions ---------------------------------------------------------------
source(here("packages.R"))
source(here("Functions", "fx_plot.R"))

# Import -------------------------------------------------------------
sa_surprises_tbl <- 
  R.matlab::readMat(here("Data", "IRFs", "IRFs.mat")) |> 
  as_tibble() # Find out the names and the dates of the surprises

# Export ---------------------------------------------------------------
artifacts_sa_surprises <- list (
  sa_surprises_tbl = sa_surprises_tbl
)

write_rds(artifacts_sa_surprises, file = here("Outputs", "artifacts_sa_surprises.rds"))


