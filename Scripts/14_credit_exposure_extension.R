# Description
# credit exposure data
# Preliminaries -----------------------------------------------------------
library(here)

# Functions ---------------------------------------------------------------
source(here("packages.R"))
source(here("Functions", "fx_plot.R"))

# Import and clean -------------------------------------------------------------
model_data_tbl <- read_rds(here("Outputs", 'artifacts_model_data.rds')) |> 
  pluck(1)

names_vec <- excel_sheets(here("Data", "Credit_exposure_by_sector_february_2022.xlsx")) 

sector_credit_exposure_tbl <- 
  names_vec |> 
  map( ~ read_excel(here("Data", "Credit_exposure_by_sector_february_2022.xlsx"), 
                    skip = 43,
                    sheet = .x
                    )
  ) |> 
  set_names(names_vec) |> 
  bind_rows(.id = "banks") |> 
  filter(banks %in% c("absa", "firstrand", "nedbank", "standardbank")) |> 
  mutate(banks = str_replace_all(banks,
    c(
      "absa" = "ABSA BANK", 
      "firstrand" =  "FIRSTRAND BANK", 
      "nedbank" = "NEDBANK", 
      "standardbank" = "STANDARD BANK")
    )) |> 
  rename("sector" = 2) |> 
  dplyr::select(-3, -4) |>  
  pivot_longer(-c("banks", "sector"), names_to = "date", values_to = "value") |> 
  pivot_wider(names_from = sector, values_from = value) |> 
  mutate(date = parse_date(date, "%b %Y")) |> 
  relocate(date, .before = banks)
  


# EDA ---------------------------------------------------------------
sector_credit_exposure_tbl |> 
  janitor::clean_names() |> 
  skim()

# Graphing ---------------------------------------------------------------
sector_credit_exposure_gg <- 
  sector_credit_exposure_tbl |> 
  pivot_longer(-c("banks", "date"), names_to = "sector", values_to = "value") |> 
  ggplot(aes(x = date, y = value, color = banks)) +
  geom_line() +
  facet_wrap(~sector, scales = "free_y", ncol = 2) +
  labs(
    title = "",
    y = "Exposure",
    x = "",
    col = "Bank"
  ) +
  theme_minimal(base_size = 8) +
  theme(legend.position = "bottom") +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  scale_color_manual(values = pnw_palette("Bay",4), labels = scales::label_wrap(20)) +
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = "B"))

# Export ---------------------------------------------------------------
artifacts_sector_credit_exposure <- list (
 sector_credit_exposure_tbl = sector_credit_exposure_tbl,
 sector_credit_exposure_gg = sector_credit_exposure_gg
)

write_rds(artifacts_sector_credit_exposure, 
          file = here("Outputs", "artifacts_sector_credit_exposure.rds"))


