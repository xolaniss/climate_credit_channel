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
  filter(date < "2022-01-01") |>
  rename("banks" = bank) 
  # filter(date > "2009-12-01")

BA900_2022_2026 <- read_excel(here("Data", "BA900_big_banks.xlsx")) 

# Cleaning -----------------------------------------------------------------
lending_vol_2022_2026_tbl <- 
  BA900_2022_2026 |>
  filter(...1 %in%
           c(142, 
             143, # UNSECURED CREDIT: 142 and 143 are instalment sales
             168, 
             169, # 168 and 169 are credit cards
             183,  # 183 to  188 are overdrafts
             185, 
             190, # other loans
             192, 
             147,  # SECURED CREDIT:  147 and 148 are leasing transactions
             148,
             152, # MORTGAGES
             153,
             156, 
             157, 
             163, 
             164
             )) |> 
  rename(
     "line_item" = 1,
     "credit_sector" = 2,
     "date" = 3
  ) |>  
  mutate(date = parse_date(date, "%Y-%m")) |>
  mutate(credit_category = case_when(
    line_item %in% c(142, 143)             ~ "Instalment Sales",
    line_item %in% c(168, 169)             ~ "Credit Cards",
    line_item %in% c(183, 185)            ~ "Overdrafts",
    line_item %in% c(190, 192)            ~ "Other Loans",
    line_item %in% c(147, 148)             ~ "Leasing Transactions",
    line_item %in% c(152, 153, 156, 157, 163, 164)  ~ "Mortgages",
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
  mutate(banks = rep(
    c(
      "ABSA BANK",
      "CAPITEC BANK",
      "FIRSTRAND BANK",
      "INVESTEC",
      "NEDBANK",
      "STANDARD BANK",
      "TOTAL"
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
    banks, .after = date
  ) |> 
  relocate(
    value, .after = asset_type
  ) |> 
  mutate(value = as.numeric(value)) |> 
  mutate(credit_sector = str_to_lower(credit_sector)) |> 
  mutate(credit_sector = dplyr::recode_values(credit_sector,
    "non-financial corporate sector"                    ~ "corporate",
    "private non-financial corporate sector"            ~ "corporate",
    "unincorporated business enterprises of households" ~ "household",
    "households"                                        ~ "household",
    "non-profit organisations serving households"        ~ "household",
    "corporate sector"                                  ~"corporate",
    "household sector"                                  ~"household"
  )) |> 
  filter(asset_type ==  "TOTAL ASSETS (Col 1 plus col 3)") |>
  # filter(str_detect(asset_type, "TOTAL")) |>
  mutate(value = as.numeric(value)*1000) |>
  summarise(total = sum(value, na.rm = TRUE), .by = c(date, banks, credit_sector, broad_credit_category)) |> 
  arrange(date, banks, credit_sector, broad_credit_category) |> 
  mutate(broad_credit_category = str_to_lower(broad_credit_category)) |> 
  mutate(broad_credit_category = str_replace_all(broad_credit_category, " ", "_")) |> 
  mutate(credit_category = paste0(credit_sector,  "_", broad_credit_category)) |> 
  relocate(credit_category , .before = credit_sector) |> 
  select(-credit_sector, -broad_credit_category) |> 
  pivot_wider(names_from = credit_category, values_from = total)
  
# Combined --------------------------------------------------------
lending_vol_tbl <- bind_rows(lending_vol_2008_2021_tbl, lending_vol_2022_2026_tbl) |> 
  filter(!banks %in% c("INVESTEC", "TOTAL", "CAPITEC BANK"))
lending_growth_tbl <- 
  lending_vol_tbl |> 
  group_by(banks) |> 
  mutate(across(-c("date"), ~ ((.x - lag(.x, 1))/lag(.x, 1)))*100) |> 
  filter(date > "2012-12-01") # starting in 2010
  
# Graphing ---------------------------------------------------------------
lending_vol_gg <- 
  lending_vol_tbl |> 
  pivot_longer(-c("date", "banks"), values_to = "value", names_to = "credit_category") |> 
  ggplot(aes(x = date, y = value, color = credit_category)) + 
  geom_line() + 
  scale_y_continuous(labels = scales::label_number(scale = 1e-9, suffix = "B"), limits = c(0, NA)) +
  labs(x = " ", y = "Lending Growth (%)") + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "none")  +
  facet_wrap(banks~credit_category, scales = "free_y") 

lending_growth_gg <- 
  lending_growth_tbl |> 
  mutate(
    banks = replace_values(
      banks,
      "ABSA BANK" ~ "BANK A",
      "FIRSTRAND BANK" ~ "BANK B",
      "NEDBANK" ~ "BANK C",
      "STANDARD BANK" ~ "BANK D"
    )
  ) |> 
  ungroup() |> 
  pivot_longer(-c("date", "banks"), values_to = "value", names_to = "credit_category") |> 
  mutate(
    credit_category = str_replace_all(credit_category, "_", " ") |> str_to_title()
  ) |> 
  ggplot(aes(x = date, y = value, color = credit_category)) + 
  geom_line() + 
  labs(x = " ", y = "Lending Growth (%)") + 
  theme_minimal(base_size = 8) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "none")  +
  facet_wrap(credit_category ~ banks, scales = "free_y", ncol = 4) +
  scale_color_manual(values = pnw_palette("Bay",6), labels = scales::label_wrap(20))

lending_growth_gg
  
# Export --------------------------------------------------------------
artifacts_lending_volume <- 
  list (
    lending_vol_tbl = lending_vol_tbl,
    lending_growth_tbl = lending_growth_tbl,
    lending_growth_gg = lending_growth_gg 
    )

write_rds(artifacts_lending_volume, file = here("Outputs", "artifacts_lending_volume.rds"))


