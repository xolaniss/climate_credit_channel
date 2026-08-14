# Preliminaries -------------------------------------------------------------
library(here)

# Functions -----------------------------------------------------------------
source(here("packages.R"))
source(here("Functions", "fx_plot.R"))

# Import --------------
irf_standard <- read_rds(here("Outputs", "artifacts_lp_unsmoothed.rds")) |>
  pluck("irf_unsmoothed")
irf_asym <- read_rds(here("Outputs", "artifacts_lp_unsmoothed_asym.rds")) |> 
  pluck("irf_unsmoothed")

# Timing ------------------------------
timing_stats <- 
  irf_standard |>
  filter(component == "Interaction (climate amplification)") |> 
  mutate(
    sig68       = lo68 > 0 | hi68 < 0,
    borrower    = if_else(str_detect(dep, "household"), "Household", "Corporate"),
    credit_type = if_else(str_detect(dep, "_rate"), "Rate", "Volume"),
    climate_label = if_else(climate_shock == "pop_temp_shock", "Temperature", "Precipitation")
  ) |> 
  group_by(dep, mp_shock, climate_shock, borrower, credit_type, climate_label) |>
  summarise(
    peak_horizon  = h[which.max(abs(irf))],
    n_sig         = sum(sig68),
    direction     = if_else(mean(irf[sig68]) > 0, "Amplifies", "Dampens"),
    .groups = "drop"
  )

## Persistence ----

p_persist_gg <- timing_stats |>
  ggplot(aes(x = n_sig, fill = borrower)) +
  geom_histogram(binwidth = 2, position = "dodge", alpha = 0.85) +
  facet_grid(credit_type ~ climate_label) +
  scale_x_continuous(breaks = seq(0, 13, 2)) +
  scale_fill_manual(values = c("Corporate" = "#00496f", "Household" = "#dd4124")) +
  labs(
    title = "Persistence of interaction effect",
    subtitle = "(quarters significant at 68% CI)",
    x = "Quarters significant", y = "Count", fill = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position  = "bottom",
    panel.grid.minor = element_blank(),
    strip.text       = element_text(size = 9)
  )

medians_persist_tbl <- timing_stats |>
  group_by(credit_type, climate_label, borrower) |>
  summarise(med = median(n_sig), .groups = "drop")

p_persist_gg <- timing_stats |>
  ggplot(aes(x = n_sig, fill = borrower)) +
  geom_histogram(binwidth = 2, position = "dodge", alpha = 0.85) +
  geom_vline(
    data = medians_persist_tbl,
    aes(xintercept = med, color = borrower),
    linetype = "dashed", linewidth = 0.7
  ) +
  facet_grid(credit_type ~ climate_label) +
  scale_x_continuous(breaks = seq(0, 13, 2)) +
  scale_fill_manual(values  = c("Corporate" = "#00496f", "Household" = "#dd4124")) +
  scale_color_manual(values = c("Corporate" = "#00496f", "Household" = "#dd4124")) +
  labs(
    title = "Persistence of interaction effect",
    subtitle = "Quarters significant at 68% CI | dashed = median",
    x = "Quarters significant", y = "Count", fill = NULL, color = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position  = "bottom",
    panel.grid.minor = element_blank(),
    strip.text       = element_text(size = 9)
  )


# Peaks -----
medians_peak_tbl <- timing_stats |>
  group_by(credit_type, climate_label, borrower) |>
  summarise(med = median(peak_horizon), .groups = "drop")

p_peak_gg <- timing_stats |>
  ggplot(aes(x = peak_horizon, fill = borrower)) +
  geom_histogram(binwidth = 1, position = "dodge", alpha = 0.85) +
  geom_vline(
    data = medians_peak_tbl,
    aes(xintercept = med, color = borrower),
    linetype = "dashed", linewidth = 0.7
  ) +
  facet_grid(credit_type ~ climate_label) +
  scale_x_continuous(breaks = seq(0, 12, 2)) +
  scale_fill_manual(values  = c("Corporate" = "#00496f", "Household" = "#dd4124")) +
  scale_color_manual(values = c("Corporate" = "#00496f", "Household" = "#dd4124")) +
  labs(
    title = "Peak horizon of interaction effect",
    x = "Horizon (quarters)", y = "Count", fill = NULL, color = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position  = "bottom",
    panel.grid.minor = element_blank(),
    strip.text       = element_text(size = 9)
  )

## Combined ------------
combined_gg <- 
  p_peak_gg + p_persist_gg +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")



## Direction -------------
mp_type_map <- c(
  romer_surprise           = "Theory-based",
  miyajima_surprise        = "Theory-based",
  target                   = "Market-based",
  forward_guidance         = "Market-based",
  central_bank_information = "Market-based",
  country_risk             = "Market-based"
)

mp_label_map <- c(
  romer_surprise           = "Romer",
  miyajima_surprise        = "Miyajima",
  target                   = "Target",
  forward_guidance         = "Fwd. guidance"
)

direction_tbl <- irf_standard |>
  filter(!mp_shock %in% c("central_bank_information", "country_risk")) |> 
  filter(component == "Interaction (climate amplification)") |>
  mutate(sig68 = lo68 > 0 | hi68 < 0) |>
  group_by(dep, mp_shock, climate_shock) |>
  summarise(
    median_irf = median(irf[sig68], na.rm = TRUE),
    n_sig      = sum(sig68),
    .groups    = "drop"
  ) |>
  mutate(
    direction = case_when(
      is.nan(median_irf) ~ "Insignificant",
      median_irf >  0    ~ "Amplifies",
      median_irf <= 0    ~ "Dampens"
    ),
    borrower      = if_else(str_detect(dep, "household"), "Household", "Corporate"),
    credit_type   = if_else(str_detect(dep, "_rate"), "Rate", "Volume"),
    credit_label  = dep |>
      str_remove("corporate_|household_") |>
      str_remove("_rate") |>
      str_replace_all("_", " ") |>
      str_to_title(),
    dep_label    = paste0(borrower, " ", credit_label),
    climate_label = if_else(climate_shock == "pop_temp_shock",
                            "Temperature", "Precipitation"),
    mp_label = factor(mp_label_map[mp_shock], levels = mp_label_map),
    mp_type  = mp_type_map[mp_shock]
  )


short_dep_labels <- c(
  corporate_mortgage_rate          = "Corp. mortgage",
  corporate_secured_credit_rate    = "Corp. secured",
  corporate_unsecured_credit_rate  = "Corp. unsecured",
  household_mortgage_rate          = "HH mortgage",
  household_secured_credit_rate    = "HH secured",
  household_unsecured_credit_rate  = "HH unsecured",
  corporate_mortgages              = "Corp. mortgage",
  corporate_secured_credit         = "Corp. secured",
  corporate_unsecured_credit       = "Corp. unsecured",
  household_mortgages              = "HH mortgage",
  household_secured_credit         = "HH secured",
  household_unsecured_credit       = "HH unsecured"
)

rate_order   <- c("Corp. mortgage", "Corp. secured", "Corp. unsecured",
                  "HH mortgage",    "HH secured",    "HH unsecured")

direction_gg <- 
  direction_tbl |>
  mutate(
    dep_label     = factor(short_dep_labels[dep], levels = rev(rate_order)),
    median_capped = pmax(pmin(median_irf, 0.5), -0.5)
  ) |>
  ggplot(aes(x = mp_label, y = dep_label, fill = median_capped)) +
  geom_tile(colour = "white", linewidth = 0.6) +
  geom_text(aes(label = if_else(direction == "Amplifies", "+", "−")),
            size = 4.5, fontface = "bold", colour = "black") +
  facet_grid(credit_type ~ climate_label) +
  scale_fill_distiller(
    palette   = "RdBu",
    direction = -1,
    limits    = c(-0.5, 0.5),
    oob       = scales::squish,
    name      = "← dampens | amplifies →",
    breaks    = c(-0.5, -0.25, 0, 0.25, 0.5),
    labels    = c("≤ −0.5", "−0.25", "0", "+0.25", "≥ +0.5")
  ) +
  scale_x_discrete(limits = c("Romer", "Miyajima", "Target",
                               "Fwd. guidance")) +
  labs(
    title    = "Direction of climate–MP interaction on credit",
    subtitle = "+ amplifies MP transmission  |  − dampens  |  colour intensity = magnitude",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x       = element_text(angle = 30, hjust = 1, size = 9),
    axis.text.y       = element_text(size = 9),
    strip.text        = element_text(face = "bold", size = 9),
    legend.position   = "right",
    legend.key.height = unit(1.4, "cm"),
    panel.grid        = element_blank(),
    plot.subtitle     = element_text(size = 8, colour = "grey40")
  )


## Persistence and Peak by credit product -----

short_dep_labels2 <- c(
  corporate_mortgage_rate         = "Corp. mortgage",
  corporate_secured_credit_rate   = "Corp. secured",
  corporate_unsecured_credit_rate = "Corp. unsecured",
  household_mortgage_rate         = "HH mortgage",
  household_secured_credit_rate   = "HH secured",
  household_unsecured_credit_rate = "HH unsecured",
  corporate_mortgages             = "Corp. mortgage",
  corporate_secured_credit        = "Corp. secured",
  corporate_unsecured_credit      = "Corp. unsecured",
  household_mortgages             = "HH mortgage",
  household_secured_credit        = "HH secured",
  household_unsecured_credit      = "HH unsecured"
)

product_order <- c(
  "Corp. mortgage", "Corp. secured", "Corp. unsecured",
  "HH mortgage",    "HH secured",    "HH unsecured"
)

product_summary_tbl <- timing_stats |>
  mutate(dep_label = factor(short_dep_labels2[dep], levels = rev(product_order))) |>
  group_by(dep_label, borrower, credit_type, climate_label) |>
  summarise(
    med_peak = median(peak_horizon),
    lo_peak  = quantile(peak_horizon, 0.25),
    hi_peak  = quantile(peak_horizon, 0.75),
    med_sig  = median(n_sig),
    lo_sig   = quantile(n_sig, 0.25),
    hi_sig   = quantile(n_sig, 0.75),
    .groups  = "drop"
  )

product_cols <- c("Corporate" = "#00496f", "Household" = "#dd4124")

p_product_peak_gg <- product_summary_tbl |>
  ggplot(aes(x = med_peak, y = dep_label, colour = borrower,
             xmin = lo_peak, xmax = hi_peak)) +
  geom_vline(xintercept = median(timing_stats$peak_horizon),
             linetype = "dashed", colour = "grey60", linewidth = 0.5) +
  geom_pointrange(size = 0.55, linewidth = 0.9) +
  facet_grid(credit_type ~ climate_label, scales = "free_y", space = "free_y") +
  scale_x_continuous(breaks = seq(0, 12, 2), limits = c(0, 13)) +
  scale_colour_manual(values = product_cols) +
  labs(
    title  = "Peak horizon by credit product",
    subtitle = "Median across MP shock measures | range = IQR | dashed = overall median",
    x = "Peak horizon (quarters)", y = NULL, colour = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    strip.text         = element_text(size = 9, face = "bold"),
    legend.position    = "bottom"
  )

p_product_sig_gg <- product_summary_tbl |>
  ggplot(aes(x = med_sig, y = dep_label, colour = borrower,
             xmin = lo_sig, xmax = hi_sig)) +
  geom_vline(xintercept = median(timing_stats$n_sig),
             linetype = "dashed", colour = "grey60", linewidth = 0.5) +
  geom_pointrange(size = 0.55, linewidth = 0.9) +
  facet_grid(credit_type ~ climate_label, scales = "free_y", space = "free_y") +
  scale_x_continuous(breaks = seq(0, 13, 2), limits = c(0, 13)) +
  scale_colour_manual(values = product_cols) +
  labs(
    title    = "Persistence by credit product",
    subtitle = "Quarters significant at 68% CI | range = IQR | dashed = overall median",
    x = "Quarters significant", y = NULL, colour = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    strip.text         = element_text(size = 9, face = "bold"),
    legend.position    = "bottom"
  )

combined_product_gg <-
  p_product_peak_gg / p_product_sig_gg +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")


## Climate state -----------

asym_timing <- irf_asym |>
  filter(component != "No climate shock") |>
  mutate(
    sig68       = lo68 > 0 | hi68 < 0,
    borrower    = if_else(str_detect(dep, "household"), "Household", "Corporate"),
    credit_type = if_else(str_detect(dep, "_rate"), "Rate", "Volume"),
    climate_label = if_else(climate_shock == "pop_temp_shock", "Temperature", "Precipitation"),
    mp_type     = mp_type_map[mp_shock],
    mp_label    = factor(mp_label_map[mp_shock],
                         levels = c("Romer", "Miyajima", "Target",
                                    "Fwd. guidance", "CB information", "Country risk"))
  ) |>
  group_by(dep, mp_shock, mp_label, mp_type, climate_shock, climate_label,
           borrower, credit_type, component) |>
  summarise(
    peak_horizon = h[which.max(abs(irf))],
    n_sig        = sum(sig68),
    .groups      = "drop"
  )

asym_timing |> count(component)



# Summary by mp_label × credit_type × climate_label × climate state
asym_summary_tbl <- asym_timing |>
  group_by(mp_label, mp_type, credit_type, climate_label, component) |>
  summarise(
    med_peak = median(peak_horizon),
    lo_peak  = quantile(peak_horizon, 0.25),
    hi_peak  = quantile(peak_horizon, 0.75),
    med_sig  = median(n_sig),
    lo_sig   = quantile(n_sig, 0.25),
    hi_sig   = quantile(n_sig, 0.75),
    .groups  = "drop"
  ) |> 
  drop_na()

state_cols <- c("-3 SD" = "#2166ac", "+3 SD" = "#d6604d")

p_asym_peak_gg <- asym_summary_tbl |>
  ggplot(aes(x = med_peak, y = mp_label, colour = component,
             xmin = lo_peak, xmax = hi_peak,
             shape = component)) +
  geom_vline(xintercept = median(asym_timing$peak_horizon),
             linetype = "dashed", colour = "grey60", linewidth = 0.5) +
  geom_pointrange(position = position_dodge(width = 0.5),
                  size = 0.5, linewidth = 0.8) +
  facet_grid(credit_type ~ climate_label) +
  scale_x_continuous(breaks = seq(0, 12, 2), limits = c(0, 13)) +
  scale_colour_manual(values = state_cols) +
  scale_shape_manual(values = c("-3 SD" = 16, "+3 SD" = 17)) +
  labs(
    title  = "Peak horizon by climate state",
    x = "Peak horizon (quarters)", y = NULL,
    colour = "Climate state", shape = "Climate state"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    strip.text         = element_text(size = 9),
    legend.position    = "bottom"
  )

p_asym_sig_gg <- asym_summary_tbl |>
  ggplot(aes(x = med_sig, y = mp_label, colour = component,
             xmin = lo_sig, xmax = hi_sig,
             shape = component)) +
  geom_vline(xintercept = median(asym_timing$n_sig),
             linetype = "dashed", colour = "grey60", linewidth = 0.5) +
  geom_pointrange(position = position_dodge(width = 0.5),
                  size = 0.5, linewidth = 0.8) +
  facet_grid(credit_type ~ climate_label) +
  scale_x_continuous(breaks = seq(0, 13, 2), limits = c(0, 13)) +
  scale_colour_manual(values = state_cols) +
  scale_shape_manual(values = c("-3 SD" = 16, "+3 SD" = 17)) +
  labs(
    title    = "Persistence by climate state",
    subtitle = "Quarters significant at 68% CI",
    x = "Quarters significant", y = NULL,
    colour = "Climate state", shape = "Climate state"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    strip.text         = element_text(size = 9),
    legend.position    = "bottom"
  )

combined_asym_gg <- 
  p_asym_peak_gg + p_asym_sig_gg +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")



# Export ------------------------------------------------------------------

artifacts_extensions <- list(
  direction_gg         = direction_gg,
  combined_gg          = combined_gg,
  combined_product_gg  = combined_product_gg,
  combined_asym_gg     = combined_asym_gg
)

write_rds(artifacts_extensions, here("Outputs", "artifacts_extensions.rds"))
