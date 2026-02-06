suppressWarnings(suppressMessages(suppressPackageStartupMessages({
  # core
  library(tidyverse)
  library(readr)
  library(readxl)
  library(lubridate)
  library(xts)
  library(broom)
  library(glue)
  library(scales)
  library(kableExtra)
  library(pins)
  library(timetk)
  library(uniqtag)
  library(quantmod)
  library(pins)
  library(flextable)
  library(gt)
  library(officer)
  library(officedown)
  library(modelsummary)
  library(sandwich)
  library(lmtest)
  library(stargazer)
  library(conflicted)
  conflict_prefer("select", "dplyr")
  conflict_prefer("filter", "dplyr")
  conflict_prefer("lag", "dplyr")
  
  # graphs
  library(PNWColors)
  library(patchwork)
  
  # eda
  library(psych)
  library(DataExplorer)
  library(skimr)
  
  # econometrics
  library(tseries)
  library(strucchange)
  library(vars)
  library(urca)
  library(mFilter)
  library(car)
  
  # Parallel processing
  library(furrr)
  library(parallel)
  library(tictoc)
  
})))
