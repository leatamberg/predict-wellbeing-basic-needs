step_select_p_value_iterative <- function(
    recipe, 
    ..., 
    role = "predictor", 
    trained = FALSE, 
    outcome = NULL,
    keep_always = NULL,
    threshold = 0.05,
    initial_threshold = 0.5,
    options = list(), 
    exclude = NULL,
    skip = FALSE,
    id = rand_id("p_value_iterative")
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
    step_select_p_value_iterative_new(
      terms = terms, 
      role = role,
      trained = trained,
      outcome = outcome,
      keep_always = keep_always,
      threshold = threshold,
      initial_threshold = initial_threshold,
      options = options,
      exclude = exclude,
      skip = skip,
      id = id
    )
  )
}

# wrapper around 'step' function that sets the class of new step objects
#' @importFrom recipes step
step_select_p_value_iterative_new <- 
  function(terms, role, trained, outcome, keep_always, threshold, initial_threshold, options, exclude, skip, id) {
    step(
      subclass = "select_p_value_iterative", 
      terms = terms,
      role = role,
      trained = trained,
      outcome = outcome,
      keep_always = keep_always,
      threshold = threshold,
      initial_threshold = initial_threshold,
      options = options,
      exclude = exclude,
      skip = skip,
      id = id
    )
  }


prep.step_select_p_value_iterative <- function(x, training, info = NULL, ...) {
  
  #verbose = x$options$verbose
  
  # translate the terms arguments
  x_names <- recipes::recipes_eval_select(x$terms, training, info = info)
  y_name <- x$outcome 
  
  
  X <- training[, x_names]
  y <- training[[y_name]]
  
  linear_model <- 
    linear_reg() %>% 
    set_engine("lm")

    
  exclude <- character()
  #if(verbose){print(exclude)}
  model_fit <- fit_xy(linear_model, X, y)
  
  exclude <- c(exclude, 
               (model_fit %>% tidy() %>%  filter(p.value > x$initial_threshold & !term %in% c("(Intercept)", x$keep_always)))$term)
  
  X <- X[,!colnames(X) %in% exclude]
  
  model_fit <- fit_xy(linear_model, X, y)
  
  worst_p <- max((model_fit %>% tidy() %>%  filter(!term %in% c("(Intercept)", x$keep_always)))$p.value)
  
  
  while(worst_p>x$threshold & 
        nrow(tidy(model_fit)) > (1 + length(x$keep_always))){
  
    exclude <- c(exclude, 
                 (model_fit %>% tidy() %>% filter(p.value == worst_p & !term %in% c("(Intercept)", x$keep_always)))$term)
    
    #if(verbose){print(exclude)}
  
    X <- X[,!colnames(X) %in% exclude]
    
    model_fit <- fit_xy(linear_model, X, y)
    
    #if(verbose){print(model_fit %>% extract_fit_engine() %>% summary)}
    
    
   worst_p <- max((model_fit %>% tidy() %>%  filter(!term %in% c("(Intercept)", x$keep_always)))$p.value)
}
  

  
  step_select_p_value_iterative_new(
    terms = x$terms, 
    trained = TRUE,
    outcome = y_name,
    keep_always = x$keep_always,
    role = x$role, 
    threshold = x$threshold,
    initial_threshold = x$initial_threshold,
    options = x$options,
    exclude = exclude,
    skip = x$skip,
    id = x$id
  )
  
}

bake.step_select_p_value_iterative <- function(object, new_data, ...) {
  if (length(object$exclude) > 0) {
    new_data <- new_data[, !colnames(new_data) %in% object$exclude]
  }
  
  as_tibble(new_data)
}

tunable.step_select_p_value_iterative <- function(x, ...) {
  tibble(
    name = c("threshold"),
    call_info = list(
      list(pkg = "dials", fun = "threshold", range = c(0.001, 0.5))
    ),
    source = "recipe",
    component = "step_select_p_value_iterative",
    component_id = x$id
  )
}