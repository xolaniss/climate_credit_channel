# Description
# Cleaning quarterly real GDP - August 2026
# SARB: GDP at market prices, constant 2015 prices, seasonally adjusted at annual rate

# Preliminaries -----------------------------------------------------------
library(here)

# Functions ---------------------------------------------------------------
source(here("packages.R"))
source(here("Functions", "fx_plot.R"))

# Import ------------------------------------------------------------------
gdp_raw_tbl <- readxl::read_excel(
  here("Data", "GDP_per_capita.xlsx"),
  sheet = "Sheet 1 - SDDSDetail",
  skip = 4,
  col_names = c("quarter", "value"),
  col_types = c("text", "numeric")
)

# Transformations ---------------------------------------------------------
gdp_tbl <- gdp_raw_tbl |>
  filter(str_detect(quarter, "^Q[1-4]/\\d{2}$")) |>
  mutate(
    year = 2000L + as.integer(str_sub(quarter, 4, 5)),
    qtr = as.integer(str_sub(quarter, 2, 2)),
    date = make_date(
      year = year,
      month = qtr * 3L,
      day = 1L
    ) %m+% months(1) - days(1),
    gdp = as.numeric(value)
  ) |>
  arrange(date) |>
  drop_na(date, gdp) |>
  mutate(
    log_gdp = log(gdp),
    gdp_growth = (gdp / lag(gdp) - 1) * 100, #qoq percentage
    gdp_growth_yoy = (gdp / lag(gdp, 4) - 1) * 100 #yoy percentage
  ) |>
  select(date, gdp, log_gdp, gdp_growth, gdp_growth_yoy)


# Graph --------------------------------------------------------
gdp_gg <-
  gdp_tbl |>
  select(date, log_gdp, gdp_growth_yoy) |>
  pivot_longer(
    cols = -date,
    names_to = "variable",
    values_to = "value"
  ) |>
  mutate(
    variable = str_replace_all(
      variable,
      c(
        "log_gdp" = "Real GDP (log)",
        "gdp_growth_yoy" = "Real GDP growth (% y/y)"
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
artifacts_gdp <- list(
  gdp_tbl = gdp_tbl,
  gdp_gg = gdp_gg
)

write_rds(
  artifacts_gdp,
  file = here("Outputs", "artifacts_gdp.rds")
)