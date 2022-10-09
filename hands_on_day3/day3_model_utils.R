plot_tune_grid_metrics <- function(tune_metrics) {
  tune_metrics %>%
    ggplot(aes(x=scenario)) + 
    geom_point(aes(x=scenario, y=mean)) +
    geom_errorbar(aes(ymin=mean-std_err, ymax=mean+std_err), width=.2,
                  position=position_dodge(0.05)) +
    facet_wrap(~.metric, scales = 'free_x') +
    coord_flip()
}


plot_param <- function(tune_metrics, param, metric_type = NULL) {
  plot_title <-  glue::glue("{metric_type} vs {param}")
  tune_metrics %>%
    filter(.metric == metric_type) %>%
    arrange_at('mean') %>%
    ggplot(aes_string(x=param, y='mean')) +
    geom_point() + 
    xlab(param) + 
    ylab('') + 
    ggtitle(plot_title)
}



plot_pred_vs_obs_scatter <- function(preds, plot_title='') {
  # Get predictions
  #preds <- augment(model_fit, new_data = dat) 
  
  # Make scatterplot
  preds %>%
    ggplot(aes_string(x=selected_outcome, y='.pred')) +
    geom_point() + 
    geom_abline(slope = 1, intercept = 0, linetype=2) + 
    xlab('observed') +
    ylab('predicted') + 
    ggtitle(plot_title)
}
