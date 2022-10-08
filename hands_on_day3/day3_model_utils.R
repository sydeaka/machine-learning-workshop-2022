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