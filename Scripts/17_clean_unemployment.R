# Description
# Cleaning QLFS unemployment rate (LU1) - August 2026 (Datasource: StatsSA)

# Preliminaries -----------------------------------------------------------
library(here)

# Functions ---------------------------------------------------------------
source(here("packages.R"))
source(here("Functions", "fx_plot.R"))

qlfs_path  <- here("Data", "QLFS.xlsx")
qlfs_sheet <- "Table 2.3"

# Import -------------------------------------------------------------

# Construct QLFS quarterly labels from Jan-Mar 2008 to Jan-Mar 2026
quarter_starts <- seq.Date(
  from = as.Date("2008-01-01"),
  to   = as.Date("2026-01-01"),
  by   = "3 months"
)

quarter_labels <- paste0(
  c("Jan-Mar", "Apr-Jun", "Jul-Sep", "Oct-Dec")[
    (seq_along(quarter_starts) - 1) %% 4 + 1
  ],
  " ",
  format(quarter_starts, "%Y")
)

# Read QLFS data without assigning column names
qlfs_raw_tbl <-
  readxl::read_excel(
    qlfs_path,
    sheet     = qlfs_sheet,
    skip      = 3,
    col_names = FALSE,
    col_types = "text",
    na        = c("", " -", "-", "*", "..")
  )

# Assign the correct column names
names(qlfs_raw_tbl) <- c("indicator", quarter_labels)

# Transformations ---------------------------------------------------------

unemployment_long_tbl <- qlfs_raw_tbl |>
  mutate(indicator_clean = str_replace_all(indicator, "\u00A0", " "),# Replace non-breaking spaces with ordinary spaces
    indicator_clean = str_squish(indicator_clean) # Clean whitespace
    ) |>
  
  # Keep only national LU1 row
  filter(
    indicator_clean == "LU1- Unemployment rate",
    row_number() == min(
      which(
        str_squish(str_replace_all(indicator, "\u00A0", " ")) ==
          "LU1- Unemployment rate"
      )
    )
  ) |>
  
  pivot_longer(cols = all_of(quarter_labels), names_to  = "quarter", values_to = "unemployment_rate") |>
  mutate(region = "South Africa",
    unemployment_rate = as.numeric(unemployment_rate),
    
# Convert quarter labels to quarter-end dates
    date = case_when(
      str_detect(quarter, "^Jan-Mar") ~
        make_date(year  = as.integer(str_extract(quarter, "\\d{4}")),
          month = 3, day = 31
        ),
      str_detect(quarter, "^Apr-Jun") ~
        make_date(
          year  = as.integer(str_extract(quarter, "\\d{4}")),
          month = 6, day = 30
        ),
      str_detect(quarter, "^Jul-Sep") ~
        make_date( year  = as.integer(str_extract(quarter, "\\d{4}")),
          month = 9, day = 30
        ),
      str_detect(quarter, "^Oct-Dec") ~
        make_date( year  = as.integer(str_extract(quarter, "\\d{4}")),
          month = 12, day = 31))
  ) |>
  dplyr::select(region, date, quarter, unemployment_rate) |>
  arrange(date)

# National series ----------------------------------------------------------

unemployment_tbl <-
  unemployment_long_tbl |>
  dplyr::select(
    date,
    unemployment_rate
  ) |>
  drop_na()

# Graph --------------------------------------------------------
unemployment_gg <-
  unemployment_tbl |>
  ggplot(aes(x = date, y = unemployment_rate)) +
  geom_line(col = pnw_palette("Bay", 4)[1]) +
  labs(
    title = "",
    y     = "Unemployment rate (%)",
    x     = ""
  ) +
  theme_minimal(base_size = 8) +
  theme(legend.position = "") +
  scale_x_date(date_labels = "%Y", date_breaks = "4 years")

# Export ------------------------------------------------------------------
artifacts_unemployment <- list(
  unemployment_tbl         = unemployment_tbl,
  unemployment_gg          = unemployment_gg
)

write_rds(artifacts_unemployment, file = here("Outputs", "artifacts_unemployment.rds"))
