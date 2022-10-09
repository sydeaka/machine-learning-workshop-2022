

logger::log_info("Helper fn: Load and transform the ACTG175 data")
actg175_load_and_transform_data <- function() {
  logger::log_info("Get the raw data from the speff2trial package")
  data(ACTG175)
  dat_raw <- ACTG175
  
  
  logger::log_info("Apply data transformations")
  dat_clean <- dat_raw %>%
    mutate(treatment_arm = case_when(
      arms == 0 ~ 'zidovudine',
      arms == 1 ~ 'zidovudine and didanosine',
      arms == 2 ~ 'zidovudine and zalcitabine',
      arms == 3 ~ 'didanosine',
      TRUE ~ NA_character_
    )) %>%
    mutate(
      hemo = case_when(
        hemo == 0 ~ 'no',
        hemo == 1 ~ 'yes',
        TRUE ~ NA_character_
      ),
      lgbtq = case_when(
        homo == 0 ~ 'no',
        homo == 1 ~ 'yes',
        TRUE ~ NA_character_
      ),
      hist_intra_drug_use = case_when(
        drugs == 0 ~ 'no',
        drugs == 1 ~ 'yes',
        TRUE ~ NA_character_
      ),
      prior_nz_art = case_when(
        oprior == 0 ~ 'no',
        oprior == 1 ~ 'yes',
        TRUE ~ NA_character_
      ),
      prior_z_30days = case_when(
        z30 == 0 ~ 'no',
        z30 == 1 ~ 'yes',
        TRUE ~ NA_character_
      ),
      prior_z = case_when(
        zprior == 0 ~ 'no',
        zprior == 1 ~ 'yes',
        TRUE ~ NA_character_
      ),
      days_prior_art = preanti,
      race = case_when(
        race == 0 ~ 'white',
        race == 1 ~ 'non-white',
        TRUE ~ NA_character_
      ),
      gender = case_when(
        gender == 0 ~ 'female',
        gender == 1 ~ 'male',
        TRUE ~ NA_character_
      ),
      prior_art = case_when(
        str2 == 0 ~ 'naive',
        str2 == 1 ~ 'experienced',
        TRUE ~ NA_character_
      ),
      strat_hist_art = case_when(
        strat == 1 ~ 'antiretroviral naive',
        strat == 2 ~ '> 1 but <= 52 weeks of prior antiretroviral therapy',
        strat == 3 ~ '> 52 weeks',
        TRUE ~ NA_character_
      ),
      symptom = case_when(
        symptom == 0 ~ 'asymptomatic',
        symptom == 1 ~ 'symptomatic',
        TRUE ~ NA_character_
      ),
      prior_z = case_when(
        zprior == 0 ~ 'no',
        zprior == 1 ~ 'yes',
        TRUE ~ NA_character_
      ),
      prior_z = case_when(
        zprior == 0 ~ 'no',
        zprior == 1 ~ 'yes',
        TRUE ~ NA_character_
      ),
      zidovudine_indicator = treat,
      surv_days = days,
      surv_event = cens,
      event = ifelse(surv_event == 0, 'no', 'yes') %>% factor,
      baseline_cd4 = cd40,
      baseline_cd8 = cd80
    ) %>%
    select(
      # identifier
      pidnum, 
      # treatment assignment 
      zidovudine_indicator, treatment_arm, offtrt, 
      # baseline predictors
      age, wtkg, hemo, lgbtq, hist_intra_drug_use, karnof, prior_nz_art, prior_z_30days, prior_z, days_prior_art, race, gender, prior_art, strat_hist_art, symptom, baseline_cd4, baseline_cd8, 
      # survival outcome
      event, surv_event, surv_days,
      # other outcomes
      cd420, cd496, r, cd820
    ) %>%
    mutate_if(is.character, factor) 
  
  logger::log_info("ACTG175 data load and transformation is complete.")
  
  return(
    list(
      dat_raw = dat_raw, 
      dat_clean = dat_clean
    )
  )
  
}


logger::log_info("Helper fn: exploratory data analysis for ACTG175 study")
# Variable types, missing data summary, relationships between predictors, preds vs outcomes, etc
actg175_eda <- function(dat_raw, dat_clean) {
  logger::log_info("Specify variable types")
  var_outcomes <- c('event', 'surv_event', 'surv_days', 'cd420', 'cd496', 'r', 'cd820')
  excluded_cols <- c('pidnum', 'zidovudine_indicator', 'offtrt')
  classes <- sapply(dat_clean, class)
  vars_factor <- classes[classes == 'factor'] %>% 
    names %>% 
    .[!(. %in% c('progression', 'event'))]
  vars_cont <- classes[(classes  %in% c('integer', 'numeric'))] %>% 
    names %>% 
    .[!(. %in% c(var_outcomes, excluded_cols))]
  
  logger::log_info("Missing data summary")
  missing_data_summary_df <- sapply(dat_raw, function(dat_col) {
    num_missing <- length(which(is.na(dat_col)))
    pct_missing <- round(100 * num_missing / nrow(dat_raw), 2)
    return(c(num_missing = num_missing, pct_missing = pct_missing))
  }) %>% 
    t %>% 
    data.frame %>%
    mutate(var_name = rownames(.))
  
  vars_w_cols_missing <- missing_data_summary_df %>%
    filter(num_missing > 0)
  
  plot_missing_data_pct <- missing_data_summary_df %>%
    ggplot(aes(x=var_name, y=pct_missing)) + 
    ylim(0, 100) +
    geom_bar(stat='identity') + 
    coord_flip() + 
    xlab('') + ylab('') + ggtitle("Missing data percentages for ACTG175 study data columns")
  
  missing_data_summary <- list(
    missing_data_summary_df = missing_data_summary_df,
    vars_w_cols_missing = vars_w_cols_missing,
    plot_missing_data_pct = plot_missing_data_pct
  )
  
  plot_list <- list()
  
  logger::log_info("Bar plot of event counts")
  plot_list[["event_counts"]] <- dat_clean %>%
    count(event) %>%
    ggplot(aes(x = event, y = n)) + 
    geom_bar(stat = 'identity') + 
    ylab('') + ggtitle('Patient counts by event status')
  
  logger::log_info("Bar plot of prior_z counts")
  plot_list[["prior_z_counts"]] <- dat_clean %>%
    count(prior_z) %>%
    ggplot(aes(x = prior_z, y = n)) + 
    geom_bar(stat = 'identity') +
    xlab('') + ylab('') + ggtitle('Patient counts by prior zidovudine history')
  
  
  logger::log_info("Bar plot of prior_nz_art counts")
  plot_list[["prior_nz_art_counts"]] <- dat_clean %>%
    count(prior_nz_art) %>%
    ggplot(aes(x = prior_nz_art, y = n)) + 
    geom_bar(stat = 'identity') +
    xlab('') + ylab('') + ggtitle('Patient counts by prior non-zidovudine history')
  
  logger::log_info("Boxplot: CD4 @ 20 wks by treatment arm")
  plot_list[["cd420_vs_treatment_arm"]] <- dat_clean %>%
    ggplot(aes(x=treatment_arm, y=cd420)) +
    geom_boxplot()
  
  logger::log_info("Boxplot: CD8 @ 20 wks by treatment arm")
  plot_list[["cd820_vs_treatment_arm"]] <- dat_clean %>%
    ggplot(aes(x=treatment_arm, y=cd820)) +
    geom_boxplot()
  
  logger::log_info("Exploratory data analysis of ACTG study data is complete.")
  
  return(list(
    artifacts = list(var_outcome = var_outcomes, 
                     excluded_cols = excluded_cols, 
                     classes = classes, 
                     vars_factor = vars_factor, 
                     vars_cont = vars_cont),
    summaries = list(missing_data_summary = missing_data_summary),
    plot_list = plot_list
  ))
}


logger::log_info("ACTG175 env setup complete")
