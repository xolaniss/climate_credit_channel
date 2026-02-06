# Description
# Generate folder to share latex version of the paper

# Packages -----------------------------------------------------------
library(here)
library(tidyverse)
library(fs)


# Delete current latex folder ----------------------------------------
if (dir.exists(here("Latex_version"))) {
  dir_delete(here("Latex_version"))
}

# Create folder ------------------------------------------------------
dir.create(here("Latex_version"))

# Copy text files ---------------------------------------------------------
list.files(path = ".", pattern = "tex", full.names = TRUE ) %>%
  file.copy(to = here("Latex_version"), overwrite = TRUE)
file.copy(here("references.bib"), here("Latex_version"), overwrite = TRUE)

# Copy resources folder ---------------------------------------------
folder_path <- list.dirs(path = ".", recursive = FALSE) %>%
  str_subset("files")
folder_name <- str_remove(folder_path, "./")
dir_copy(folder_path, here("Latex_version", folder_name), overwrite = TRUE)
