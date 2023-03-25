step_select_lasso <- function(
    recipe, 
    ..., 
    role = "predictor", 
    trained = FALSE, 
    outcome = NULL,
    keep_always = NULL,
    penalty = 0.05,
    options = list(), 
    exclude = NULL,
    skip = FALSE,
    id = rand_id("p_value")
) {
  
  
  
  ## The variable selectors are not immediately evaluated by using
  ##  the `quos()` function in `rlang`. `ellipse_check()` captures 
  ##  the values and also checks to make sure that they are not empty.  
  terms <- ellipse_check(...) 
  
  add_step(
    recipe, 
    step_select_lasso_new(
      terms = terms, 
      role = role,
      trained = trained,
      outcome = outcome,
      keep_always = keep_always,
      penalty = penalty,
      options = options,
      exclude = exclude,
      skip = skip,
      id = id
    )
  )
}

# wrapper around 'step' function that sets the class of new step objects
#' @importFrom recipes step
step_select_lasso_new <- 
  function(terms, role, trained, outcome, keep_always, penalty, options, exclude, skip, id) {
    step(
      subclass = "select_lasso", 
      terms = terms,
      role = role,
      trained = trained,
      outcome = outcome,
      keep_always = keep_always,
      penalty = penalty,
      options = options,
      exclude = exclude,
      skip = skip,
      id = id
    )
  }


prep.step_select_lasso <- function(x, training, info = NULL, ...) {
  
  # translate the terms arguments
  x_names <- recipes::recipes_eval_select(x$terms, training, info = info)
  y_name <- x$outcome #recipes::terms_select(x$outcome, info = info)
  #y_name <- y_name[1]
  
  # training = data_2015
  # x_names = c("gdp", "HIV_unaids")
  # y_name = "life_expect"
  
  X <- training[, x_names]
  y <- training[[y_name]]
  
  linear_model <- 
    linear_reg(penalty = x$penalty, mixture = 1) %>% 
    set_engine("glmnet")
  
  model_fit <- tidy(fit_xy(linear_model, X, y))
  
  exclude <- (model_fit %>% filter(estimate == 0 & !term %in% c("(Intercept)", x$keep_always)))$term
  
  
  step_select_lasso_new(
    terms = x$terms, 
    trained = TRUE,
    outcome = y_name,
    keep_always = x$keep_always,
    role = x$role, 
    penalty = x$penalty,
    options = x$options,
    exclude = exclude,
    skip = x$skip,
    id = x$id
  )
  
}

bake.step_select_lasso <- function(object, new_data, ...) {
  if (length(object$exclude) > 0) {
    new_data <- new_data[, !colnames(new_data) %in% object$exclude]
  }
  
  as_tibble(new_data)
}


tunable.step_select_lasso <- function(x, ...) {
  tibble(
    name = c("penalty"),
    call_info = list(
      list(pkg = "dials", fun = "penalty", range = c(-5, 0),
           trans = scales::log10_trans())
    ),
    source = "recipe",
    component = "step_select_lasso",
    component_id = x$id
  )
}