step_select_p_value <- function(
    recipe, 
    ..., 
    role = "predictor", 
    trained = FALSE, 
    outcome = NULL,
    keep_always = NULL,
    threshold = 0.05,
    options = list(), 
    exclude = NULL,
    skip = FALSE,
    id = rand_id("p_value")
) {
  
  
  if ((threshold > 1 | threshold <= 0)&threshold != tune()) {
    rlang::abort("`threshold` should be on (0, 1].")
  }
  
  ## The variable selectors are not immediately evaluated by using
  ##  the `quos()` function in `rlang`. `ellipse_check()` captures 
  ##  the values and also checks to make sure that they are not empty.  
  terms <- ellipse_check(...) 
  
  add_step(
    recipe, 
    step_select_p_value_new(
      terms = terms, 
      role = role,
      trained = trained,
      outcome = outcome,
      keep_always = keep_always,
      threshold = threshold,
      options = options,
      exclude = exclude,
      skip = skip,
      id = id
    )
  )
}

# wrapper around 'step' function that sets the class of new step objects
#' @importFrom recipes step
step_select_p_value_new <- 
  function(terms, role, trained, outcome, keep_always, threshold, options, exclude, skip, id) {
    step(
      subclass = "select_p_value", 
      terms = terms,
      role = role,
      trained = trained,
      outcome = outcome,
      keep_always = keep_always,
      threshold = threshold,
      options = options,
      exclude = exclude,
      skip = skip,
      id = id
    )
  }


prep.step_select_p_value <- function(x, training, info = NULL, ...) {
  
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
    linear_reg() %>% 
    set_engine("lm")
  
  model_fit <- tidy(fit_xy(linear_model, X, y))
  
  exclude <- (model_fit %>% filter(p.value > x$threshold & !term %in% c("(Intercept)", x$keep_always)))$term
    
  
  step_select_p_value_new(
    terms = x$terms, 
    trained = TRUE,
    outcome = y_name,
    keep_always = x$keep_always,
    role = x$role, 
    threshold = x$threshold,
    options = x$options,
    exclude = exclude,
    skip = x$skip,
    id = x$id
  )
  
}

bake.step_select_p_value <- function(object, new_data, ...) {
  if (length(object$exclude) > 0) {
    new_data <- new_data[, !colnames(new_data) %in% object$exclude]
  }
  
  as_tibble(new_data)
}

tunable.step_select_p_value <- function(x, ...) {
  tibble(
    name = c("threshold"),
    call_info = list(
      list(pkg = "dials", fun = "threshold", range = c(0.00001, 0.2))
    ),
    source = "recipe",
    component = "step_select_p_value",
    component_id = x$id
  )
}