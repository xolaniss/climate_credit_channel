modelsummary_word <- function(models = models,
                              title = "Insert title", 
                              coef_map = NULL, 
                              coef_rename  = NULL,
                              stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01), 
                              decimals = 3,
                              vcov =  NULL,
                              gof_omit = "AIC|BIC|Std|Log",
                              coef_omit = NULL,
                              add_rows = NULL,
                              gof_map =  gof_map,
                              output = "flextable"
) {
  modelsummary(
    models,
    output = output,
    statistic = NULL,
    stars = stars,
    fmt = decimals,
    vcov =  vcov,
    title = title,
    gof_omit = gof_omit ,
    coef_map = coef_map,
    coef_rename = coef_rename,
    coef_omit = coef_omit,
    gof_map = "all",
    add_rows = add_rows
    
  ) %>% 
    theme_booktabs() %>% 
    border_inner(border = fp_border(color = "white"), part = "all")
}


modelsummary_basic <- function(models = models,
                              title = "Insert title", 
                              coef_map = NULL, 
                              coef_rename  = NULL,
                              stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01), 
                              decimals = 2,
                              vcov =  NULL,
                              gof_omit = "AIC|BIC|Std|Log",
                              coef_omit = NULL,
                              add_rows = NULL,
                              gof_map =  gof_map,
                              output = "dataframe"
) {
  modelsummary(
    models,
    output = output,
    statistic = NULL,
    stars = stars,
    fmt = decimals,
    vcov =  vcov,
    title = title,
    gof_omit = gof_omit ,
    coef_map = coef_map,
    coef_rename = coef_rename,
    coef_omit = coef_omit,
    gof_map = "all",
    add_rows = add_rows
    
  ) 
}
