ols_nest_full_prep <-
  function(data, group_vars) {
    data |>
      group_by(across(all_of(group_vars))) |>
      nest()
  }

ols_tidy_group_models <-
  function(nested_data, formula) {
    nested_data |>
      mutate(models = map(data, ~feols(formula, data = .x, cluster = ~banks))) |>
      mutate(models_coef = map(models, ~tidy(.)))
  }

ols_pretty_full_results <-
  function(data_fitted_models, group_vars) {
    data_fitted_models |>
      unnest(cols = models_coef, names_repair = "universal") |>
      dplyr::select(all_of(group_vars), term, estimate, p.value) |>
      mutate(
        stars = ifelse(p.value < 0.001, "***", ifelse(p.value < 0.01, "**", ifelse(p.value < 0.05, "*", "")))
      ) |>
      mutate(across(estimate, ~strtrim(., 6))) |>
      mutate(Estimate = paste0(estimate, stars)) |>
      dplyr::select(-estimate, -p.value, -stars) |>
      pivot_longer(-c(all_of(group_vars), term)) |>
      spread(key = term, value = value) |>
      dplyr::select(-name)
  }

ols_group_full_workflow <-
  function(data, formula, group_vars) {
    data |>
      ols_nest_full_prep(group_vars) |>
      ols_tidy_group_models(formula) |>
      ols_pretty_full_results(group_vars)
  }
