# Makefile for climate credit channel analysis pipeline
# Usage:
#   make        - build all outputs
#   make -j4    - parallel build (stages 0 and 4b run concurrently)
#   make clean  - remove all generated artifacts

R = Rscript

# ─── Top-level target ─────────────────────────────────────────────────────────

all: Outputs/artifacts_extensions.rds \
     Outputs/all_fe_models.rds \
     Outputs/artifacts_descriptive_statistics.rds \
     Outputs/artifacts_sarb_forecast.rds \
     Outputs/artifacts_lp_unsmoothed_robust.rds \
     Outputs/artifacts_lp_unsmoothed_asym_robust.rds \
     Outputs/all_fe_models_purged.rds \
     Outputs/artifacts_lp_unsmoothed_purged.rds \
     Outputs/artifacts_lp_unsmoothed_asym_purged.rds

# ─── Stage 0: No upstream R-script dependencies ───────────────────────────────

Outputs/artifacts_market_based_surprises.rds: Scripts/01_surprises.R \
    Data/monetary_policy_announcement_dates.xlsx
	$(R) $<

Outputs/artifacts_climate_data.rds: Scripts/02_climate_data.R \
    Data/population_weighted_temp.csv \
    Data/population_weighted_precip.csv \
    Data/land_weighted_temp.csv \
    Data/land_weighted_precip.csv
	$(R) $<

Outputs/artifacts_credit_market.rds: Scripts/04_previous_credit_data.R \
    Data/artifacts_combined_banks_monthly.rds \
    Data/artifacts_combined_lending.rds
	$(R) $<

Outputs/artifacts_bloomberg_forecast.rds: Scripts/07_bloomberg_forecast.R \
    Data/bloomberg_quarterly_forecast.xlsx
	$(R) $<

Outputs/artifacts_sarb_forecast.rds: Scripts/07_sarb_forecasts.R \
    Data/TM_data_request.xlsx
	$(R) $<

Outputs/artifacts_bloomberg_actuals.rds: Scripts/08_bloomberg_actuals.R \
    Data/cpi_gdp_repo_actuals.xlsx
	$(R) $<

# ─── Stage 0b: Control variable cleaning (parallel with Stage 0) ──────────────

Outputs/artifacts_unemployment.rds: Scripts/17_clean_unemployment.R \
    Data/QLFS.xlsx
	$(R) $<

Outputs/artifacts_gdp.rds: Scripts/18_clean_GDP_per_capita.R \
    Data/GDP_per_capita.xlsx
	$(R) $<

Outputs/artifacts_exchange_rate.rds: Scripts/19_clean_exchange_rate.R \
    Data/Exchange_Rate.xlsx
	$(R) $<

# ─── Stage 1: Depends on Stage 0 ─────────────────────────────────────────────

Outputs/artifacts_climate_shocks.rds: Scripts/03_climate_shocks.R \
    Outputs/artifacts_climate_data.rds
	$(R) $<

Outputs/artifacts_lending_volume.rds: Scripts/05_updated_lending_vol.R \
    Outputs/artifacts_credit_market.rds \
    Data/BA900_big_banks.xlsx
	$(R) $<

Outputs/artifacts_surprises.rds: Scripts/09_alt_surprises.R \
    Outputs/artifacts_bloomberg_forecast.rds \
    Outputs/artifacts_bloomberg_actuals.rds
	$(R) $<

# ─── Stage 2: Depends on Stage 1 ─────────────────────────────────────────────

Outputs/artifacts_lending_rates.rds: Scripts/06_updated_lending_rates.R \
    Outputs/artifacts_credit_market.rds \
    Outputs/artifacts_lending_volume.rds \
    Data/BA930_Multiple_banks_data_2023-2026.xlsx
	$(R) $<

# ─── Stage 3: Final data integration ─────────────────────────────────────────

Outputs/artifacts_model_data.rds: Scripts/10_model_data.R \
    Outputs/artifacts_climate_shocks.rds \
    Outputs/artifacts_lending_volume.rds \
    Outputs/artifacts_lending_rates.rds \
    Outputs/artifacts_surprises.rds \
    Outputs/artifacts_market_based_surprises.rds
	$(R) $<

# ─── Stage 4: Baseline modelling ─────────────────────────────────────────────

Outputs/artifacts_descriptive_statistics.rds: Scripts/11_descriptives.R \
    Outputs/artifacts_model_data.rds
	$(R) $<

Outputs/artifacts_model_data_purged.rds: Scripts/12_purged_mp_surprise.R \
    Outputs/artifacts_model_data.rds
	$(R) $<

Outputs/all_fe_models.rds: Scripts/13_fixed_effects.R \
    Outputs/artifacts_model_data.rds
	$(R) $<

# ─── Stage 5: Baseline local projections ─────────────────────────────────────

Outputs/artifacts_lp_unsmoothed.rds: Scripts/14_unsmoothed_LPs.R \
    Outputs/artifacts_model_data_purged.rds
	$(R) $<

Outputs/artifacts_lp_unsmoothed_asym.rds: Scripts/15_asymetric_unsmoothed_LPs.R \
    Outputs/artifacts_model_data_purged.rds
	$(R) $<

# ─── Stage 5b: Purged robustness (FE + LPs on purged data) ───────────────────

Outputs/all_fe_models_purged.rds: Scripts/23_purged_FE.R \
    Outputs/artifacts_model_data_purged.rds
	$(R) $<

Outputs/artifacts_lp_unsmoothed_purged.rds: Scripts/24_purged_LP.R \
    Outputs/artifacts_model_data_purged.rds
	$(R) $<

Outputs/artifacts_lp_unsmoothed_asym_purged.rds: Scripts/25_purged_asymetric_LP.R \
    Outputs/artifacts_model_data_purged.rds
	$(R) $<

# ─── Stage 6: Extensions (baseline) ──────────────────────────────────────────

Outputs/artifacts_extensions.rds: Scripts/16_extensions.R \
    Outputs/artifacts_lp_unsmoothed.rds \
    Outputs/artifacts_lp_unsmoothed_asym.rds
	$(R) $<

# ─── Stage 6b: Controls merge ─────────────────────────────────────────────────
# Script 20 augments artifacts_model_data.rds in-place (overwrites it with
# controls appended). A stamp file ensures the baseline scripts (11, 13) finish
# on the original model_data before augmentation, and that 12 (purging) also
# completes so the purged data is not affected by the controls merge.

Outputs/.controls_merged: Scripts/20_controls_merge.R \
    Outputs/artifacts_model_data.rds \
    Outputs/artifacts_descriptive_statistics.rds \
    Outputs/all_fe_models.rds \
    Outputs/artifacts_model_data_purged.rds \
    Outputs/artifacts_unemployment.rds \
    Outputs/artifacts_gdp.rds \
    Outputs/artifacts_exchange_rate.rds
	$(R) $<
	touch $@

# ─── Stage 7: Controls local projections ─────────────────────────────────────

Outputs/artifacts_lp_unsmoothed_robust.rds: Scripts/21_unsmoothed_LPs_with_controls.R \
    Outputs/.controls_merged
	$(R) $<

Outputs/artifacts_lp_unsmoothed_asym_robust.rds: Scripts/22_asym_LPs_with_controls.R \
    Outputs/.controls_merged
	$(R) $<

# ─── Utilities ────────────────────────────────────────────────────────────────

.PHONY: all clean

clean:
	rm -f Outputs/artifacts_*.rds Outputs/all_fe_models*.rds Outputs/.controls_merged
