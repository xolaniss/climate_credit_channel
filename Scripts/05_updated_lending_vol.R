# Description
# Updating BA900 to 2026 - Xolani Sibande
# Preliminaries -----------------------------------------------------------
library(here)

# Functions ---------------------------------------------------------------
source(here("packages.R"))
source(here("Functions", "fx_plot.R"))

# Import -------------------------------------------------------------
lending_vol_2008_2021_tbl <- 
  read_rds(here("Outputs", "artifacts_credit_market.rds")) |> 
  pluck(1) |> 
  janitor::clean_names() |> 
  filter(date < "2022-01-01")


BA900 <- read_excel(here("Data", "BA900_big_banks.xlsx")) 

# Cleaning -----------------------------------------------------------------
lending_vol_tbl <- 
  BA900 |>
  filter(...1 %in%
           c(142, 
             143, # UNSECURED CREDIT: 142 and 143 are instalment sales
             169, 
             170, # 169 and 170 are credit cards
             183,  # 183 to  188 are overdrafts
             184, 
             185, 
             186, 
             190, # other loans
             191, 
             192, 
             193,
             147,  # SECURED CREDIT:  147 and 148 are leasing transactions
             148,
             156, # MORTGAGES
             157, 
             158, 
             163, 
             164, 
             165 
             )) |> 
  rename(
     "line_item" = 1,
     "credit_sector" = 2,
     "date" = 3
  ) |>  
  mutate(date = parse_date(date, "%Y-%m")) |>
  mutate(credit_category = case_when(
    line_item %in% c(142, 143)             ~ "Instalment Sales",
    line_item %in% c(169, 170)             ~ "Credit Cards",
    line_item %in% c(183, 184, 185, 186)   ~ "Overdrafts",
    line_item %in% c(190, 191, 192, 193)   ~ "Other Loans",
    line_item %in% c(147, 148)             ~ "Leasing Transactions",
    line_item %in% c(156, 157, 158, 163, 164, 165)  ~ "Mortgages",
    .default = NA_character_
  )) |> 
  mutate(broad_credit_category = case_when(
    credit_category %in% c("Credit Cards", "Overdrafts", "Other Loans")     ~ "Unsecured Credit",
    credit_category %in% c("Instalment Sales", "Leasing Transactions")      ~ "Secured Credit",
    credit_category %in% c("Mortgages")   ~ "Mortgages",
    .default = NA_character_
  )) |> 
  select(!(`...46`:`...59`)) |>
  pivot_longer(cols = starts_with("."),
               names_to = "variable",
               values_to = "value") |>
  group_by(date, line_item) |>
  mutate(bank = rep(
    c(
      "Absa Bank",
      "Capitec",
      "FNB",
      "Investec",
      "Nedbank",
      "Standard Bank",
      "Total"
    ),
    each = 6
  )) |> 
  mutate(asset_type = rep(
    c(
      "Domestic assets",
      "Domestic assets: Of which: foreign currency",
      "Foreign assets",
      "Foreign assets: Of which: foreign currency",
      "TOTAL ASSETS (Col 1 plus col 3)",
      "Of which: under repurchase agreements"
    ), times = 7)) |>
  ungroup() |> 
  select(-variable) |> 
  relocate(
    date, .before = line_item
  ) |> 
  relocate(
    bank, .after = date
  ) |> 
  relocate(
    value, .after = asset_type
  ) 
  

# Transformations --------------------------------------------------------



# Graphing ---------------------------------------------------------------


# Export ---------------------------------------------------------------
artifacts_ <- list ( )

write_rds(artifacts_, file = here("Outputs", "artifacts_.rds"))


