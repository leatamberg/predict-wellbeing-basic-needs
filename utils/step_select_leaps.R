step_select_leaps <- function(
    recipe, 
    ..., 
    role = "predictor", 
    trained = FALSE, 
    outcome = NULL,
    keep_always = NULL,
    method = "exhaustive",
    eval_criterion = "BIC",
    num_terms_max = NULL,
    options = list(), 
    include = NULL,
    skip = FALSE,
    id = rand_id("p_value")
) {
  
  if(!eval_criterion %in% c("BIC", "Cp")){
    stop("evaluation criterion not implemented")
  }
  
  
  ## The variable selectors are not immediately evaluated by using
  ##  the `quos()` function in `rlang`. `ellipse_check()` captures 
  ##  the values and also checks to make sure that they are not empty.  
  terms <- ellipse_check(...) 
  
  add_step(
    recipe, 
    step_select_leaps_new(
      terms = terms, 
      role = role,
      trained = trained,
      outcome = outcome,
      keep_always = keep_always,
      method = method,
      eval_criterion = eval_criterion,
      num_terms_max = num_terms_max,
      options = options,
      include = include,
      skip = skip,
      id = id
    )
  )
}

# wrapper around 'step' function that sets the class of new step objects
#' @importFrom recipes step
step_select_leaps_new <- 
  function(terms, role, trained, outcome, keep_always, method, eval_criterion, num_terms_max, options, include, skip, id) {
    step(
      subclass = "select_leaps", 
      terms = terms,
      role = role,
      trained = trained,
      outcome = outcome,
      keep_always = keep_always,
      method = method,
      eval_criterion = eval_criterion,
      num_terms_max = num_terms_max,
      options = options,
      include = include,
      skip = skip,
      id = id
    )
  }


prep.step_select_leaps <- function(x, training, info = NULL, ...) {
  
  # translate the terms arguments
  x_names <- recipes::recipes_eval_select(x$terms, training, info = info)
  y_name <- x$outcome 
  
  # training = data_2015
  # x_names = c("electricity", "sanitation_basic",
  #             "unmet_contraception", "health_expenditure",
  #             "voice", "stability", "alcohol", "gdp")
  # y_name = "life_expect"
  
  X <- training[, x_names]
  y <- training[[y_name]]
  
  if(is.integer(x$num_terms_max)){
    num_terms_max <- x$num_terms_max
  }else{
    num_terms_max <- length(x_names)
  }
  
  search_results <- leaps::regsubsets(x=X,y=y, method = x$method, nbest = 1, nvmax = num_terms_max)
  summary_search_results <- summary(search_results,all_best = FALSE, matrix.logical = TRUE)
  
  # find model performing best according to Cp or BIC
  if(x$eval_criterion == "BIC"){
    pos_best_model <- which.min(summary_search_results$bic) # which min gives first appearance of minimum, 
                                                            # which in our case corresponds to smallest model
  }else if(x$eval_criterion == "Cp"){
    pos_best_model <- which.min(summary_search_results$cp)
  }
  
  best_model <- summary_search_results$which[pos_best_model,][-1] # we don't want to include the intercept
  
  include <- union(union(names(best_model)[best_model], x$keep_always), x$outcome)
  
  step_select_leaps_new(
    terms = x$terms, 
    trained = TRUE,
    outcome = y_name,
    keep_always = x$keep_always,
    method = x$method,
    eval_criterion = x$eval_criterion,
    num_terms_max = num_terms_max,
    role = x$role, 
    options = x$options,
    include = include,
    skip = x$skip,
    id = x$id
  )
  
}

bake.step_select_leaps <- function(object, new_data, ...) {
  
  if (length(object$include) > 0) {
    new_data <- new_data[, colnames(new_data) %in% object$include ]
  }
  
  as_tibble(new_data)
}

