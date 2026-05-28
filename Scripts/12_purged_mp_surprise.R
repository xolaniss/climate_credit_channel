# Description ---------------------------------------------------------
# Orthogonalising monetary policy shocks to climate shocks and storing residuals in model_data_tbl_2 - April 2026

# Preliminaries -----------------------------------------------------------
library(here)

# Functions ---------------------------------------------------------------
source(here("packages.R"))
source(here("Functions", "fx_plot.R"))

# Import -------------------------------------------------------------
model_data_tbl<-read_rds(here("Outputs","artifacts_model_data.rds"))|>
  pluck(1)

# Define variables ----------------------------------------------------

mp_shock_vars<-c(
  "miyajima_surprise",
  "romer_surprise",
  "target",
  "forward_guidance",
  "central_bank_information",
  "country_risk"
)

climate_shock_vars<-c(
  "population_weighted_precip_shock",
  "land_weighted_precip_shock",
  "population_weighted_temp_shock",
  "land_weighted_temp_shock"
)

# Generate and store residuals from orthogonalisation ---------------------------------------

model_data_tbl_2<-model_data_tbl

## Loop over all mp and climate shocks -------
for(mp_var in mp_shock_vars){
  for(climate_var in climate_shock_vars){
    
    temp_tbl<-model_data_tbl|>
      dplyr::select(bank,date,all_of(mp_var),all_of(climate_var))|>
      tidyr::drop_na()
    
    if(nrow(temp_tbl)>5){
      #Run OLS regression
      model<-lm(
        as.formula(paste0(mp_var,"~",climate_var)),
        data=temp_tbl
      )
      
      resid_name<-paste0(mp_var,"_",climate_var,"_resid")
      
      temp_tbl[[resid_name]]<-resid(model)
      
      residual_tbl<-temp_tbl|>
        dplyr::select(bank,date,all_of(resid_name))
      
      model_data_tbl_2<-model_data_tbl_2|>
        left_join(residual_tbl,by=c("bank","date"))
    }
  }
}

# Export --------------------------------------------------------------

artifacts<-list(
  model_data_tbl_2=model_data_tbl_2
)

write_rds(
  artifacts,
  file=here("Outputs","artifacts_model_data_orthogonalised.rds")
)