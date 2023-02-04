
zero_variance <- function(vals) {
vals %>% 
  length(unique(.)) == 1
}

remove_zero_variance_fields <- function(dat) {
  zv_fields <- sapply(dat, zero_variance) %>% .[. == TRUE] %>% names
  
  if (length(zv_fields) == 0) {
    log_info("All fields had some variability. Returning dataframe with no changes.")
    return(dat)
  } else {
    log_info("The following fields were identified as having zero variance: {paste(zv_fields, collapse=', ')}")
    fields_to_keep <- colnames(dat)[!(colnames(dat) %in% zv_fields)]
    dat <- dat %>% select_at(fields_to_keep)
    log_info("Fields successfully removed")
  }
  
  return(dat)
}


model_specifications <- list(
  "xgboost" = boost_tree(mode = 'regression', engine = 'xgboost', trees = tune(),
                         tree_depth = tune(), min_n = tune(), learn_rate = tune(), 
                         mtry = tune()),
  
  "gbm" = boost_tree(mode = 'regression', engine = 'lightgbm', trees = tune(),
                     tree_depth = tune(), min_n = tune(), learn_rate = tune(), 
                     mtry = tune()),
  
  "random_forest" = rand_forest(mode = 'regression', trees = tune(),
                     min_n = tune(), mtry = tune()) %>%
    set_engine("ranger", importance = "impurity")
    # set_engine("randomForest", importance = TRUE)
)



get_model_config <- function(model_formula, model_specifications, selected_algorithm) {
  
  model_spec <- model_specifications[[selected_algorithm]]
  
  model_wflow <- workflow(model_formula, model_spec)
  
  if (selected_algorithm == "xgboost") {
    model_param_grid <- model_wflow %>% 
      extract_parameter_set_dials() %>% 
      update(
        trees = trees(c(100, 500)),
        learn_rate = learn_rate(c(.00005, .12), trans= NULL),
        tree_depth = tree_depth(c(6, 20)),
        min_n = min_n(c(10, 60)),
        mtry = mtry(c(5, 40))
      )
  }
  
  if (selected_algorithm == "gbm") {
    model_param_grid <- model_wflow %>% 
      extract_parameter_set_dials() %>% 
      update(
        trees = trees(c(100, 500)),
        learn_rate = learn_rate(c(.00005, .12), trans= NULL),
        tree_depth = tree_depth(c(6, 20)),
        min_n = min_n(c(10, 60)),
        mtry = mtry(c(5, 40))
      )
  }
  
  
  if (selected_algorithm == "random_forest") {
    model_param_grid <- model_wflow %>% 
      extract_parameter_set_dials() %>% 
      update(
        trees = trees(c(100, 500)),
        min_n = min_n(c(10, 60)),
        mtry = mtry(c(5, 40))
      )
  }
  
  
  rtn <- list(
    model_spec = model_spec,
    model_wflow = model_wflow,
    model_param_grid = model_param_grid
  )
  
  return(rtn)
  
}



# modeling_dataset: Pre-processed dataset, ready for ML modeling

train_ml_model <- function(dat_analysis, selected_algorithm = "xgboost", model_label="",
                           model_specifications = NULL, num_folds = 5, grid_size = 10, num_bootstraps = 20,
                           outcome_var = NULL, features = NULL, do_parallel = FALSE, show_plots = TRUE
                           ) {
  # Get relevant data subset
  dat_analysis <- dat_analysis %>%
      select_at(c(outcome_var, features))
  
  # Split the data into training/validation + testing sets
  set.seed(123)
  dat_split <- initial_split(dat_analysis, prop = 0.85)
  
  # Get training and testing dataframes
  dat_train_and_val <- training(dat_split)
  dat_test <- testing(dat_split)
  
  # Examine distribution of the outcome variable in training/validation and testing sets
  # to do
  
  if (do_parallel) {
    # Set up parallel backend
    cores <- parallelly::availableCores(logical = FALSE)
    cl <- parallel::makePSOCKcluster(cores)
    doParallel::registerDoParallel(cl)
  }
  
  
  # Generate k folds for CV
  dat_folds <- vfold_cv(dat_train_and_val, v = num_folds)
  
  # Save the assessment set results
  ctrl <- control_grid(save_pred = TRUE, allow_par=TRUE, parallel_over = "everything", 
                       verbose=TRUE)
  
  # Model formula
  model_formula <- as.formula(paste(outcome_var, '~ .'))
  
  # Get config for selected algorithm
  model_config <- get_model_config(model_formula, model_specifications, selected_algorithm)
  
  # Model workflow
  model_spec <- model_config$model_spec
  model_wflow <- model_config$model_wflow
  model_param_grid <- model_config$model_param_grid
  
  
  
  # Tune the hyperparameters using k-fold cross validation
  set.seed(95920)
  tm <- system.time({
    model_res <-
      model_wflow %>%
      tune_grid(resamples = dat_folds, 
                grid = model_param_grid %>% grid_max_entropy(size = grid_size),
                control = ctrl)
  })
  
  # How long did it take to run?
  print(tm)
  
  if (do_parallel) {
    # Shut down the parallel backend
    foreach::registerDoSEQ()
    parallel::stopCluster(cl)
  }
  
  # Collect predictions
  predictions <- model_res %>% collect_predictions()
  
  
  # Create dataframe of performance metrics across various parameter settings
  param_names <- model_wflow %>% 
    extract_parameter_set_dials() %>% 
    select(name) %>% unlist %>% as.vector
  
  # Compute labels associated with each parameter combination for the plots
  param_scenario_lbl <- paste(param_names, "={", param_names, "}", sep="") %>%
    paste(., collapse="; ")
  
  # Collect metrics across the CV folds
  tune_metrics <- model_res %>% 
    collect_metrics() %>%
    mutate(scenario = glue::glue(param_scenario_lbl))
  
  # Visual summary: model performance across tuning scenarios
  plot_tune_metrics <- tune_metrics %>%
    ggplot(aes(x=scenario)) + 
    geom_point(aes(x=scenario, y=mean)) +
    geom_errorbar(aes(ymin=mean-std_err, ymax=mean+std_err), width=.2,
                  position=position_dodge(0.05)) +
    facet_wrap(~.metric) +
    coord_flip()
  
  # Visual summary: model performance by indiv param values
  plot_perf_by_param <- lapply(param_names, function(nm) plot_param(tune_metrics, nm))
  
  # Model that has the lowest RMSE
  best_rmse <- select_best(model_res, metric = "rmse")
  
  # Workflow associated with the best model (lowest RMSE)
  best_wflow <- model_wflow %>% 
    finalize_workflow(best_rmse)
  
  # The final model fit
  final_model_fit <- 
    best_wflow %>% 
    fit(data = dat_train_and_val)
  print(final_model_fit)
  
  # Engine-specific model fit
  engine_specific_model_fit <- final_model_fit %>%
    extract_fit_engine()
  
  ## Bootstrap model fits (for prediction intervals)
  ## Reference: https://www.tidymodels.org/learn/statistics/bootstrap/
  
  # Get bootstrap resamples
  set.seed(27)
  boot_samples <- bootstraps(dat_train_and_val, times = num_bootstraps, apparent = TRUE)
  
  # Fit the best model on all bootstrap resamples
  boot_models <-
    boot_samples %>% 
    mutate(model = map(splits, fit_model_on_bootstrap, best_wflow = best_wflow))
  
  # Variable importance plot
  if (selected_algorithm %in% c("xgboost")) {
    df_varimp <- final_model_fit %>%
      pull_workflow_fit() %>%
      vi %>%
      mutate(PctImportance = round(Importance / sum(Importance) * 100, 2))
    
    plot_varimp <- final_model_fit %>%
      pull_workflow_fit() %>%
      vip(geom = "col") +
      theme_bw()
  }
  
  
  if (selected_algorithm %in% c("random_forest")) {
    # ranger varimp
    df_varimp <- final_model_fit %>%
      pull_workflow_fit() %>%
      vi %>%
      mutate(PctImportance = round(Importance / sum(Importance) * 100, 2))

    plot_varimp <- final_model_fit %>%
      pull_workflow_fit() %>%
      vip(geom = "col") +
      theme_bw()
    
    
    # randomForest varimp
    # type = either 1 or 2, specifying the type of importance measure 
    # (1 = mean decrease in accuracy, 2 = mean decrease in node impurity).
    # df_varimp <- engine_specific_model_fit %>%
    #   importance(type=2) %>% 
    #   data.frame(Variable = rownames(.), .) %>% 
    #   set_colnames(c("Variable", "Importance")) %>%
    #   mutate(PctImportance = round(Importance / sum(Importance) * 100, 2)) %>%
    #   arrange(desc(PctImportance))
    # 
    # plot_varimp <- df_varimp %>%
    #   head(10) %>%
    #   ggplot(aes(x = reorder(Variable, PctImportance), y = PctImportance)) +
    #   geom_bar(stat = "identity", col = "black", show.legend = F) +
    #   coord_flip() +
    #   scale_fill_grey() +
    #   theme_bw() + 
    #   ggtitle("Top 10 attributes") + 
    #   xlab("") + ylab("% importance")
  }
  
  
  
  if (selected_algorithm == "gbm") {
    # Applying varimp utils specific to lightgbm
    tree_imp <- engine_specific_model_fit %>%
      lgb.importance(percentage = TRUE)
    
    df_varimp <- tree_imp %>%
      rename(Variable = Feature, Importance = Gain) %>%
      select(Variable, Importance) %>%
      mutate(PctImportance = round(Importance / sum(Importance) * 100, 2)) %>%
      arrange(desc(PctImportance))
    
    plot_varimp <- df_varimp %>%
      head(10) %>%
      ggplot(aes(x = reorder(Variable, PctImportance), y = PctImportance)) +
      geom_bar(stat = "identity", col = "black", show.legend = F) +
      coord_flip() +
      scale_fill_grey() +
      theme_bw() + 
      ggtitle("Top 10 attributes") + 
      xlab("") + ylab("% importance")
  }
  
  
  ## Explain model predictions
  # will be implemented later
  
  
  
  # Plot preds from top model
  plot_predictions <- predictions %>%
    filter(.config == best_rmse$.config) %>%
    ggplot(aes_string(x = outcome_var, y = ".pred")) +
    geom_point() + 
    geom_abline(slope=1, intercept=0, linetype=2) + 
    theme_bw() +
    ggtitle(paste0(toupper(selected_algorithm), ' - ', model_label, "\nPredicted vs actual"))
  
  rtn <- list(
    dat_split = dat_split,
    dat_train_and_val = dat_train_and_val,
    dat_test = dat_test,
    dat_folds = dat_folds,
    model_formula = model_formula,
    model_spec = model_spec,
    model_wflow = model_wflow,
    model_param_grid = model_param_grid,
    model_res = model_res,
    param_names = param_names,
    tune_metrics = tune_metrics,
    plot_tune_metrics = plot_tune_metrics,
    plot_perf_by_param = plot_perf_by_param,
    best_rmse = best_rmse,
    best_wflow = best_wflow,
    final_model_fit = final_model_fit,
    engine_specific_model_fit = engine_specific_model_fit,
    boot_samples = boot_samples,
    fit_model_on_bootstrap = fit_model_on_bootstrap,
    boot_models = boot_models,
    df_varimp = df_varimp,
    plot_varimp = plot_varimp,
    plot_predictions = plot_predictions
  )
  
  if (show_plots) {
    plot_list_vars <- names(rtn)[grepl(pattern="plot_", names(rtn))]
    plot_list <- rtn[plot_list_vars]
    print(plot_list)
  }
  
  # Save results to disk
  filename <- file.path(run_results_folder, 
                        paste0("model_res", "-", selected_algorithm, "-", model_label, "-", Sys.time(), ".rds")) %>% 
                gsub(pattern=":| ", replacement=".", x=.)
  
  saveRDS(rtn, file=filename)
  log_info("Model results saved to {filename}.")
  return(rtn)
  
  
}







plot_param <- function(metric_df, param, metric_name='rmse') {
  metric_df %>%
    filter(.metric == metric_name) %>%
    arrange_at('mean') %>%
    ggplot(aes_string(x=param, y='mean')) +
    geom_point() + 
    xlab(param) + 
    ylab('') + 
    ggtitle(glue::glue("{metric_name} vs {param}"))
}









# Helper function to get a single model fit on a bootstrap resample
fit_model_on_bootstrap <- function(split, best_wflow) {
  best_wflow %>% 
    fit(data = analysis(split))
}



# Helper function to get prediction intervals
# boot_model <- boot_models$model[[1]]
# input_data <- dat_train_and_val[1:3,]
# predict(boot_model, new_data = input_data)
bootstrap_pred_intervals <- function(boot_models, input_data, lower_pct = .05, upper_pct = 0.95) {
  # Get predictions on all input cases using all bootstrap models
  pred_df <- boot_models %>%
    mutate(preds = map(model, \(mod) predict(mod, new_data=input_data)))
  
  # Combine predictions across bootstraps into a matrix
  pred_matrix <- bind_cols(pred_df$preds, .name_repair="minimal") %>%
    as.matrix %>% t
  
  # Compute upper and lower confidence bounds
  pred_intervals <- pred_matrix %>% apply(2, quantile, probs=c(lower_pct, upper_pct)) %>% t
  
  return(pred_intervals)
}

# bootstrap_pred_intervals(boot_models, input_data, lower_pct = .05, upper_pct = 0.95)





