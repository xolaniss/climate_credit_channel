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
