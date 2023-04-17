if (FALSE) {
  training_df = dat
  selected_algorithm = "random_forest"
  model_mode = "classification" 
  model_label="demo"
  model_specifications = model_specifications
  num_folds = 3 
  grid_size = 10
  num_bootstraps = 4
  outcome_var = "Category"
  features = colnames(dat) %>% .[. != "Category"]
  do_parallel = FALSE
  show_plots = TRUE
  error_metric = "roc_auc"
}

# training_df: Pre-processed dataset, ready for ML modeling
train_ml_model <- function(training_df, selected_algorithm = "gbm", model_mode = "classification", model_label="",
                           model_specifications = NULL, num_folds = 5, grid_size = 10, do_bootstraps = FALSE, num_bootstraps = 20,
                           outcome_var = NULL, features = NULL, do_parallel = FALSE, show_plots = TRUE, 
                           error_metric = "rmse"
) {
  
  log_info("Get relevant data subset")
  dat_analysis <- training_df %>%
    select_at(c(outcome_var, features))
  
  log_info("Split the data into training/validation + testing sets")
  # TO DO: For classification, stratified split on outcome variable levels
  set.seed(94263)
  dat_split <- initial_split(dat_analysis, prop = 0.85)
  dat_train_and_val <- training(dat_split)
  dat_test <- testing(dat_split)
  
  # Examine distribution of the outcome variable in training/validation and testing sets
  # to do
  tryCatch({
    if (do_parallel) {
      log_info("Set up parallel backend")
      cores <- parallelly::availableCores(logical = FALSE)
      cl <- parallel::makePSOCKcluster(cores)
      doParallel::registerDoParallel(cl)
    }
    
    
    log_info("Generate k folds for CV")
    # TO DO: For classification, stratified split on outcome variable levels
    dat_folds <- vfold_cv(dat_train_and_val, v = num_folds)
    
    log_info("Config: Save the assessment set results")
    ctrl <- control_grid(save_pred = TRUE, allow_par=TRUE, parallel_over = "everything", 
                         verbose=TRUE)
    
    log_info("Model formula")
    model_formula <- as.formula(paste(outcome_var, '~ .'))
    
    log_info("Get config for selected algorithm")
    model_config <- get_model_config(model_formula, model_specifications, selected_algorithm, model_mode)
    
    log_info("Get model workflow")
    model_spec <- model_config$model_spec
    model_wflow <- model_config$model_wflow
    model_param_grid <- model_config$model_param_grid
    
    
    
    log_info("Tune the hyperparameters using k-fold cross validation")
    set.seed(3820983)
    model_train_time <- system.time({
      model_res <-
        model_wflow %>%
        tune_grid(resamples = dat_folds, 
                  grid = model_param_grid %>% grid_max_entropy(size = grid_size),
                  control = ctrl)
    })
    
    log_info("How long did it take to run?")
    print(model_train_time)
  },
  
  error = function(e) {
    log_error(as.character(e))
  }, 
  
  finally = function() {
    if (do_parallel) {
      log_info("Shut down the parallel backend")
      foreach::registerDoSEQ()
      parallel::stopCluster(cl)
    }
  }
  
  
  )
  log_info("Collect predictions")
  predictions <- model_res %>% collect_predictions()
  
  log_info("Hyperparameter names")
  param_names <- model_wflow %>% 
    extract_parameter_set_dials() %>% 
    select(name) %>% unlist %>% as.vector
  
  log_info("Compute labels associated with each parameter combination for the plots")
  param_scenario_lbl <- paste(param_names, "={", param_names, "}", sep="") %>%
    paste(., collapse="; ")
  
  log_info("Collect metrics across the CV folds")
  tune_metrics <- model_res %>% 
    collect_metrics() %>%
    mutate(scenario = glue::glue(param_scenario_lbl))
  
  log_info("Visual summary: model performance across tuning scenarios")
  plot_tune_metrics <- tune_metrics %>%
    ggplot(aes(x=scenario)) + 
    geom_point(aes(x=scenario, y=mean)) +
    geom_errorbar(aes(ymin=mean-std_err, ymax=mean+std_err), width=.2,
                  position=position_dodge(0.05)) +
    facet_wrap(~.metric) +
    coord_flip()
  
  log_info("Visual summary: model performance by individual param values")
  #metric_name <- ifelse(model_mode == 'classification', 'roc_auc', 'rmse')
  plot_perf_by_param <- lapply(param_names, function(nm) plot_param(tune_metrics, nm, error_metric))
  
  log_info("Hyperparameter combo that has the lowest value of `{error_metric}`")
  optimal_config <- select_best(model_res, metric = error_metric)
  
  log_info("Workflow associated with the optimal set of parameters")
  best_wflow <- model_wflow %>% 
    finalize_workflow(optimal_config)
  
  log_info("The final model fit")
  final_model_fit <- best_wflow %>% 
    fit(data = dat_train_and_val)
  
  log_info("Engine-specific model fit")
  engine_specific_model_fit <- final_model_fit %>%
    extract_fit_engine()
  
  if (do_bootstraps) {
    ## Bootstrap model fits (for prediction intervals)
    ## Reference: https://www.tidymodels.org/learn/statistics/bootstrap/
    
    log_info("Get bootstrap resamples")
    set.seed(27)
    boot_samples <- bootstraps(dat_train_and_val, times = num_bootstraps, apparent = TRUE)
    
    log_info("Fit the best model on all bootstrap resamples")
    boot_models <-
      boot_samples %>% 
      mutate(model = map(splits, fit_model_on_bootstrap, best_wflow = best_wflow))
  }
  
  log_info("Variable importance")
  varimp_res <- get_varimp(selected_algorithm, final_model_fit, engine_specific_model_fit)
  df_varimp <- varimp_res$df_varimp
  plot_varimp <- varimp_res$plot_varimp
  
  ## Explain model predictions
  # will be implemented later
  
  log_info("Plot preds from top model")
  if (model_mode == "classification") {
    pred_df <- predictions %>%
      filter(.config == optimal_config$.config) 
    
    plot_predictions <- plot_confusion_matrix()
  }
  
  if (model_mode == "regression") {
    plot_predictions <- predictions %>%
      filter(.config == optimal_config$.config) %>%
      ggplot(aes_string(x = outcome_var, y = ".pred")) +
      geom_point() + 
      geom_abline(slope=1, intercept=0, linetype=2) + 
      theme_bw() +
      ggtitle(paste0(toupper(selected_algorithm), ' - ', model_label, "\nPredicted vs Actual in Out-of-Bag Sample"))
  }
  
  log_info("List of objects to be returned by the function")
  rtn <- list(
    selected_algorithm = selected_algorithm, 
    model_mode = model_mode, 
    model_label = model_label,
    model_specifications = model_specifications, 
    num_folds = num_folds, 
    grid_size = grid_size, 
    do_bootstraps = do_bootstraps, 
    num_bootstraps = num_bootstraps,
    outcome_var = outcome_var, 
    features = features, 
    do_parallel = do_parallel, 
    show_plots = show_plots, 
    error_metric = error_metric,
    dat_split = dat_split,
    dat_train_and_val = dat_train_and_val,
    dat_test = dat_test,
    dat_folds = dat_folds,
    model_formula = model_formula,
    model_spec = model_spec,
    model_wflow = model_wflow,
    model_param_grid = model_param_grid,
    model_res = model_res,
    model_train_time = model_train_time,
    param_names = param_names,
    tune_metrics = tune_metrics,
    plot_tune_metrics = plot_tune_metrics,
    plot_perf_by_param = plot_perf_by_param,
    optimal_config = optimal_config,
    best_wflow = best_wflow,
    final_model_fit = final_model_fit,
    engine_specific_model_fit = engine_specific_model_fit,
    df_varimp = df_varimp,
    plot_varimp = plot_varimp,
    plot_predictions = plot_predictions
  )
  
  if (do_bootstraps) {
    rtn[["boot_samples"]] <- boot_samples
    rtn[["fit_model_on_bootstrap"]] <- fit_model_on_bootstrap
    rtn[["boot_models"]] <- boot_models
  }
  
  if (show_plots) {
    log_info("Display summary plots")
    plot_list_vars <- names(rtn)[grepl(pattern="plot_", names(rtn))]
    plot_list <- rtn[plot_list_vars]
    print(plot_list)
  }
  
  log_info("Save model results to disk")
  timestamp <- Sys.time()
  filename <- file.path(run_results_folder, 
                        glue::glue("model_res-{selected_algorithm}-{model_label}-{timestamp}.rds")) %>% 
    gsub(pattern=":| ", replacement=".", x=.)
  
  saveRDS(rtn, file=filename)
  log_info("Model results saved to {filename}.")
  
  
  log_info("Save model object to disk")
  timestamp <- Sys.time()
  filename <- file.path(run_results_folder, 
                        glue::glue("model_object-{selected_algorithm}-{model_label}-{timestamp}.rds")) %>% 
    gsub(pattern=":| ", replacement=".", x=.)
  
  if (selected_algorithm == 'gbm') {
    saveRDS.lgb.Booster(engine_specific_model_fit, file=filename)
  } else {
    saveRDS(engine_specific_model_fit, file=filename)
  }
  log_info("Model object saved to {filename}.")
  
  return(rtn)
}
