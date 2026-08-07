<<<<<<< Updated upstream
.PHONY: all

all:
	Rscript Scripts/01_surprises.R
	Rscript Scripts/02_climate_data.R
	Rscript Scripts/03_climate_shocks.R
	Rscript Scripts/04_previous_credit_data.R
	Rscript Scripts/05_updated_lending_vol.R
	Rscript Scripts/06_updated_lending_rates.R
	Rscript Scripts/07_bloomberg_forecast.R
	Rscript Scripts/07_sarb_forecasts.R
	Rscript Scripts/08_bloomberg_actuals.R
	Rscript Scripts/09_alt_surprises.R
	Rscript Scripts/10_model_data.R
	Rscript Scripts/11_descriptives.R
	Rscript Scripts/12_purged_mp_surprise.R
	Rscript Scripts/13_fixed_effects.R
	Rscript Scripts/14_unsmoothed_LPs.R
	Rscript Scripts/15_asymetric_unsmoothed_LPs.R
	Rscript Scripts/16_credit_exposure_extension.R
	Rscript Scripts/17_type_of_shock_extension.R
	@echo "All scripts completed successfully."
=======
# Makefile for climate credit channel analysis pipeline
# Usage:
#   make          - build all outputs
#   make clean    - remove all generated artifacts

R = Rscript

# ─── Top-level target ────────────────────────────────────────────────────────
# Depends on the final integrated dataset plus the standalone SARB forecast
# (which does not feed into 10_model_data.R but is part of the pipeline).

all: Outputs/artifacts_model_data.rds \
     Outputs/artifacts_sarb_forecast.rds

# ─── Stage 1: No upstream script dependencies ────────────────────────────────

Outputs/artifacts_market_based_surprises.rds: Scripts/01_surprises.R \
    Data/IRFs/IRFs.mat \
    Data/monetary_policy_announcement_dates.xlsx
	$(R) $<

Outputs/artifacts_climate_data.rds: Scripts/02_climate_data.R \
    Data/_data_gadm0_era_tmp_cropland_2000_monthly.csv \
    Data/_data_gadm0_era_tmp_pop_2000_monthly.csv \
    Data/_data_gadm0_era_pre_cropland_2000_monthly.csv \
    Data/_data_gadm0_era_pre_pop_2000_monthly.csv
	$(R) $<

Outputs/artifacts_credit_market.rds: Scripts/04_credit_data.R \
    Data/artifacts_combined_banks_monthly.rds \
    Data/artifacts_combined_lending.rds
	$(R) $<

Outputs/artifacts_bloomberg_forecast.rds: Scripts/06_bloomberg_forecast.R \
    Data/bloomberg_quarterly_forecast.xlsx
	$(R) $<

Outputs/artifacts_sarb_forecast.rds: Scripts/07_sarb_forecasts.R \
    Data/TM_data_request.xlsx
	$(R) $<

Outputs/artifacts_bloomberg_actuals.rds: Scripts/08_bloomberg_actuals.R \
    Data/cpi_gdp_repo_actuals.xlsx
	$(R) $<

# ─── Stage 2: Depends on Stage 1 outputs ─────────────────────────────────────

Outputs/artifacts_climate_shocks.rds: Scripts/03_climate_shocks.R \
    Outputs/artifacts_climate_data.rds
	$(R) $<

Outputs/artifacts_surprises.rds: Scripts/09_alt_surprises.R \
    Outputs/artifacts_bloomberg_forecast.rds \
    Outputs/artifacts_bloomberg_actuals.rds
	$(R) $<

# ─── Stage 3: Final integration ──────────────────────────────────────────────

Outputs/artifacts_model_data.rds: Scripts/10_model_data.R \
    Outputs/artifacts_climate_shocks.rds \
    Outputs/artifacts_credit_market.rds \
    Outputs/artifacts_surprises.rds \
    Outputs/artifacts_market_based_surprises.rds
	$(R) $<

# ─── Utilities ───────────────────────────────────────────────────────────────

.PHONY: all clean

clean:
	rm -f Outputs/artifacts_*.rds
>>>>>>> Stashed changes
