# Description
# incorporating updated lending rate data - June 2026

# Preliminaries -----------------------------------------------------------
library(here)

# Functions ---------------------------------------------------------------
source(here("packages.R"))
source(here("Functions", "fx_plot.R"))

# Import and Cleaning -----------------------------------------------------------------
lending_rate_2008_2021_tbl <- read_rds(here("Outputs", "artifacts_credit_market.rds")) |> 
  pluck(2) |> 
  janitor::clean_names() |> 
  mutate(date = as.Date(date)) |> 
  filter(date < "2022-01-01") |> 
  filter(banks != "TOTAL BANKS") |> 
  mutate(banks = str_replace(banks, "FNB", "FIRSTRAND BANK")) |> 
  mutate(banks = str_replace(banks, "CAPITEC", "CAPITEC BANK"))


sheet_names <- c("Absa Bank", "Capitec Bank", "Firstrand Bank", "Nedbank LTD", "The Standard Bank of SA")
corrected_sheet_names <- c("Absa Bank", "Capitec Bank", "FirstRand Bank", "Nedbank", "Standard Bank")
updated_lending_rate_data_tbl <- 
  sheet_names |>
  set_names(corrected_sheet_names) |> 
  map(
  ~read_excel(here("Data", "BA930_Multiple_banks_data_2023-2026.xlsx"), skip = 2406, sheet = .x)) |> 
  map(~drop_na(.x, "Weighted average rate (%)")) |> 
  map(~select(.x, -4, -6, -7)) |> 
  bind_rows(.id = "Bank") |> 
  relocate(Date, .before = 1) |> 
  relocate(`Line Item`, .before = 3) |> 
  mutate(
    `Line Item` = if_else(`Line Item` <= 56, "Corporate", 
                       if_else(`Line Item` >=57 & `Line Item` <= 66, "Household", "Other")
                       )
    ) |> 
  rename("Sector" = `Line Item`) |> 
  rename("Credit Type"  = `Line Item Description`) |> 
  filter(Sector != "Other") |> 
  janitor::clean_names() |> 
  filter(credit_type != "fixed rate") |> 
  mutate(
    credit_type = str_remove(credit_type, ":  -   flexible rate")
  ) |> 
  mutate(
    credit_type = str_remove(credit_type, ":")
  ) |> 
  pivot_wider(
              id_cols = c(date, bank, sector), 
              names_from = credit_type, 
              values_from = weighted_average_rate_percent
              ) |> 
  janitor::clean_names() |> 
  mutate(bank = str_to_upper(bank)) |> 
  mutate(date = parse_datetime(date, "%Y-%m")) |> 
  mutate(date = as.Date(date)) 
  
# EDA ---------------------------------------------------------------------
updated_lending_rate_data_tbl  |> 
  skim()

# Calculating hybrid rates ---------------------------------------------
lending_rate_2022_2026_tbl <- 
  updated_lending_rate_data_tbl |> 
  mutate(
    unsecured_credit_rate = (overdrafts + credit_cards + other)/3, # equal share, # change them
    secured_credit_rate = (instalment_sale_agreements + leasing_transactions) / 2,
    mortgage_rate = mortgage_advances
  ) |> 
  dplyr::select(-overdrafts, - instalment_sale_agreements, -leasing_transactions, -mortgage_advances, -credit_cards, -other) |> 
  pivot_longer(-c("date", "bank", "sector"), names_to = "credit_type", values_to = "rate") |> 
  mutate(
    sector_credit_type = paste0(sector, "_", credit_type) |> str_to_lower()
  ) |> 
  dplyr::select(-sector, -credit_type) |> 
  tidyr::pivot_wider(id_cols = c(date, bank), names_from = sector_credit_type, values_from = rate) |> 
  rename("banks" = bank)


# Combine -----------------------------------------------------------------
lending_rate_tbl <- bind_rows(lending_rate_2008_2021_tbl, lending_rate_2022_2026_tbl)


# Export ---------------------------------------------------------------
artifacts_ <- list (

)

write_rds(artifacts_, file = here("Outputs", "artifacts_.rds"))


