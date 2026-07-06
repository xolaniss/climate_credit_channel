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
lending_rate_2022_2026_tbl <- 
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

lending_vol_tbl <- read_rds(here("Outputs", "artifacts_lending_volume.rds")) |> 
  pluck(1)


# EDA ---------------------------------------------------------------------
lending_rate_2022_2026_tbl  |> 
  skim()

# Calculating hybrid rates ---------------------------------------------
lending_rate_2022_2026_tbl <- 
  lending_rate_2022_2026_tbl |> 
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
lending_rate_tbl <- bind_rows(lending_rate_2008_2021_tbl, lending_rate_2022_2026_tbl) |> 
  filter(!banks == "CAPITEC BANK") |> 
  filter(date > "2012-12-01")

# Graphing ----------------------------------------------------------------
lending_rate_gg <- 
  lending_rate_tbl |> 
  pivot_longer(cols = - c(date, banks), values_to = "rate", names_to = "credit_category") |> 
  mutate(
    credit_category = str_replace_all(credit_category, "_", " ") |> str_to_title()
  ) |> 
  ggplot(aes(x = date, y = rate, color = credit_category)) + 
  geom_line() + 
  labs(x = " ", y = "Rate (%)") + 
  theme_minimal(base_size = 8) +
  theme(legend.position = "none") +
  facet_wrap(credit_category ~ banks, scales = "free_y", ncol = 4) +
  scale_color_manual(values = pnw_palette("Bay",6), labels = scales::label_wrap(20))
  
lending_rate_gg

# Export ------------------------------------------------------------------
artifacts_lending_rates <- 
  list (
    lending_rate_tbl = lending_rate_tbl,
    lending_rate_gg = lending_rate_gg
  )

write_rds(artifacts_lending_rates, file = here("Outputs", "artifacts_lending_rates.rds"))


