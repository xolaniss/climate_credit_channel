# Description
# Cleaning daily USD/ZAR exchange rate into quarterly averages - August 2026
# SARB: Rand per US Dollar, weighted average of banks' daily rates at ~10:30am

# Preliminaries -----------------------------------------------------------
library(here)

# Functions ---------------------------------------------------------------
source(here("packages.R"))
source(here("Functions", "fx_plot.R"))

# Import ------------------------------------------------------------------
exchange_rate_raw_tbl <- readxl::read_excel(
  here("Data", "Exchange_Rate.xlsx"),
  sheet = "Sheet 1 - HistoricalRateDetail ",
  skip = 4,
  col_names = c("date", "value"),
  col_types = c("text", "text")
)

# Transformations ---------------------------------------------------------
exchange_rate_daily_tbl <- exchange_rate_raw_tbl |>
  filter(str_detect(date, "^\\d{4}-\\d{2}-\\d{2}$")) |>
  mutate(
    date = ymd(date),
    usd_zar = as.numeric(value)
  ) |>
  arrange(date) |> # the file arrives newest-first
  drop_na(date, usd_zar) |>
  select(date, usd_zar)

# Quarterly average -------------------------------------------------------
exchange_rate_tbl <- exchange_rate_daily_tbl |>
  mutate(
    year = year(date),
    qtr = quarter(date)
  ) |>
  group_by(year, qtr) |>
  summarise(
    usd_zar = mean(usd_zar, na.rm = TRUE),
    n_days = dplyr::n(),
    .groups = "drop"
  ) |>
  filter(n_days >= 40) |> #No quarter is fewer than this many days
  mutate(
    date = make_date(
      year = year,
      month = qtr * 3L,
      day = 1L
    ) %m+% months(1) - days(1) # quarter-end, matching the GDP data
  ) |>
  arrange(date) |>
  mutate(
    log_usd_zar = log(usd_zar),
    usd_zar_change = (usd_zar / lag(usd_zar) - 1) * 100, # qoq percentage
    usd_zar_change_yoy = (usd_zar / lag(usd_zar, 4) - 1) * 100 # yoy percentage
  ) |>
  select(date, usd_zar, log_usd_zar, usd_zar_change, usd_zar_change_yoy)

# Graph --------------------------------------------------------
exchange_rate_gg <-
  exchange_rate_tbl |>
  select(date, log_usd_zar, usd_zar_change_yoy) |>
  pivot_longer(
    cols = -date,
    names_to = "variable",
    values_to = "value"
  ) |>
  mutate(
    variable = str_replace_all(
      variable,
      c(
        "log_usd_zar" = "Rand per US Dollar (log)",
        "usd_zar_change_yoy" = "Rand depreciation (% y/y)"
      )
    )
  ) |>
  ggplot(aes(x = date, y = value, col = variable)) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y", ncol = 2) +
  labs(
    title = "",
    y = "",
    x = "",
    col = ""
  ) +
  theme_minimal(base_size = 8) +
  theme(legend.position = "") +
  scale_x_date(date_labels = "%Y", date_breaks = "4 years") +
  scale_color_manual(
    values = pnw_palette("Bay", 4),
    labels = scales::label_wrap(20)
  )

# Export ------------------------------------------------------------------
artifacts_exchange_rate <- list(
  exchange_rate_tbl = exchange_rate_tbl,
  exchange_rate_daily_tbl = exchange_rate_daily_tbl,
  exchange_rate_gg = exchange_rate_gg
)

write_rds(
  artifacts_exchange_rate,
  file = here("Outputs", "artifacts_exchange_rate.rds")
)
