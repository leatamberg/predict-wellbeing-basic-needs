get_error_per_test_region <- function(cv_results, single_region_folds, data) {
  fold_regions <- 
    tidy(single_region_folds) %>% 
    filter(Data == "Assessment") %>% 
    group_by(Fold) %>% 
    summarise(region = data$region[first(Row)], n_countries = n())
  
  error_per_test_region <- 
    cv_results %>% 
    collect_metrics(summarize = FALSE) %>% 
    filter(.metric == 'rmse') %>% 
    rename(Fold = 'id', rmse = '.estimate' ) %>% 
    select(Fold, rmse) %>% 
    full_join(fold_regions, by = "Fold") %>%  
    arrange(desc(rmse))
}


# A function to pull out the model and tidy it
get_model_stats <- function(x) {
  # x is a workflow, so pull the model out
  mod <- extract_fit_parsnip(x)
  # return the stats in a good format
  tidy(mod)
}

get_error_per_country_cv <- function(cv_results, fit_on_all_samples, data, true_values){
  error_per_country <- 
    cv_results %>% 
    collect_predictions() %>% 
    full_join(data %>% 
                select(country_name, country_code, region, population) %>% 
                rownames_to_column(".row") %>% 
                mutate(.row = as.numeric(.row)), 
              by = ".row") %>% 
    full_join(fit_on_all_samples %>% 
                predict(data) %>% 
                rename(pred_fit_all = ".pred") %>% 
                rownames_to_column(".row") %>% 
                mutate(.row = as.numeric(.row)),
              by = ".row") %>% 
    transmute(country_name, pred_cv = .pred, 
              true = {{true_values}}, diff_cv = .pred - {{true_values}}, 
              pred_fit_all, diff_fit_all = pred_fit_all - {{true_values}}, 
              id, region, population, country_code) %>% 
    arrange(diff_cv)
  
}

get_error_per_country <- function(fit_on_all_samples, data){
  error_per_country <- 
    fit_on_all_samples %>% 
    predict(data) %>% 
    rename(pred = ".pred") %>% 
    rownames_to_column(".row") %>% 
    mutate(.row = as.numeric(.row)) %>% 
    full_join(data %>% 
                select(country_name, country_code, region, {{true_values}}) %>% 
                rownames_to_column(".row") %>% 
                mutate(.row = as.numeric(.row)), 
              by = ".row"
              ) %>% 
    transmute(country_name, region, true = {{true_values}}, 
              pred, diff = pred - {{true_values}}, country_code) %>% 
    arrange(diff)
  return(error_per_country)
}

moran_neighbors <- function(data, variable, country_neighbors, summarize = TRUE, same_region = FALSE){

    data %>% 
    select(country_code, {{variable}}) %>% 
    inner_join(country_neighbors, by="country_code") %>% 
    inner_join(data %>% 
                 transmute(country_code_neighbor = country_code, 
                           variable_neighbor = {{variable}}),
                           by = "country_code_neighbor") %>% 
    {if(same_region) filter(.,region == region_neighbor) else .} %>% 
    {if(summarize) . else group_by(.,region)} %>% 
    summarize(moran_index = cov({{variable}}, variable_neighbor)/var({{variable}}), n_country_pairs = n()) 

}

workflow_residual_moran_neighbors <- function(workflow, data, country_neighbors, summarize = TRUE, same_region = FALSE, name_true_values){
  prediction <- 
    workflow %>% 
    fit(data=data) %>% 
    predict(new_data = data)
  
  data["residual"] <- prediction - data[name_true_values]
  moran_neighbors(data, residual, country_neighbors, summarize = summarize, same_region = same_region)
}

lm_residual_moran_neighbors <- function(model_fit, data, country_neighbors, summarize = TRUE, same_region = FALSE, name_true_values){
  prediction <- 
    model_fit %>% 
    predict(new_data = data)
  
  data["residual"] <- prediction - data[name_true_values]
  moran_neighbors(data, residual, country_neighbors, summarize = summarize, same_region = same_region)
}

moran_region <- function(data, variable){
  
  data %>% 
    select(region,country_code, {{variable}}) %>% 
    full_join(data %>% 
                 transmute(country_code_neighbor = country_code, 
                           variable_neighbor = {{variable}},
                           region),
               by = "region"
              ) %>% 
    filter(country_code != country_code_neighbor) %>% 
    summarize(moran_index = cov({{variable}}, variable_neighbor)/var({{variable}}), n_country_pairs = n()) 
  
}

workflow_residual_moran_region <- function(workflow, data, name_true_values){
  prediction <- 
    workflow %>% 
    fit(data=data) %>% 
    predict(new_data = data)
  
  data["residual"] <- prediction - data[name_true_values]
  moran_region(data, residual)
}

get_coefficients_per_fold <- function(cv_results, with_statistics = TRUE){
  coefficients <- 
    cv_results %>% 
    select(starts_with("id"), .extracts) %>% 
    bind_rows() %>% 
    unnest(".extracts") %>%  
    unnest(".extracts")
  
  if(with_statistics){
    coefficients %>% 
      select(-c(.config)) %>% 
      pivot_wider(names_from = id, values_from = estimate)
  }else{
    coefficients %>% 
      select(-c(.config, std.error, statistic, p.value)) %>% 
      pivot_wider(names_from = id, values_from = estimate)
  }
  
}

# helper function to compare fit on different continents

fit_single_continent <- function(workflow, data){
  
  fit_america <- 
    workflow %>% 
    fit(data = data %>%
          filter(continent %in% c("Americas", "Oceania"))) %>% 
    tidy() %>% 
    transmute(term, americas = estimate)
  
  fit_europe <- workflow %>% 
    fit(data = data %>%
          filter(continent %in% c("Europe"))) %>% 
    tidy() %>% 
    transmute(term, europe = estimate)
  
  fit_asia <- workflow %>% 
    fit(data = data %>% 
          filter(continent %in% c("Asia"))) %>% 
    tidy() %>% 
    transmute(term, asia = estimate)
  
  fit_africa <- workflow %>% 
    fit(data = data %>% 
          filter(continent %in% c("Africa"))) %>% 
    tidy() %>% 
    transmute(term, africa = estimate)
  
  fit_europe %>% 
    full_join(fit_asia, by="term") %>% 
    full_join(fit_america, by="term") %>% 
    full_join( fit_africa, by="term")
  
}

fit_single_continent_lm <- function(formula, data){
  
  
  fit_america <- 
    data.frame(
      america = coefficients(
        lm(formula, data = data %>%
             filter(continent %in% c("Americas", "Oceania"))))) %>% 
    rownames_to_column(var = "term")
  
  fit_europe <- 
    data.frame(
      europe = coefficients(
        lm(formula, data = data %>%
             filter(continent %in% c("Europe"))))) %>% 
    rownames_to_column(var = "term")
  
  fit_asia <- 
    data.frame(
      asia = coefficients(
        lm(formula, data = data %>%
             filter(continent %in% c("Asia"))))) %>% 
    rownames_to_column(var = "term")
  
  fit_africa <- 
    data.frame(
      africa = coefficients(
        lm(formula, data = data %>%
             filter(continent %in% c("Africa"))))) %>% 
    rownames_to_column(var = "term")
  
  fit_europe %>% 
    full_join(fit_asia, by="term") %>% 
    full_join(fit_america, by="term") %>% 
    full_join( fit_africa, by="term")
  
}


