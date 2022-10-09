
logger::log_info("Load the packages")
suppressPackageStartupMessages(library(speff2trial))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(testthat))
suppressPackageStartupMessages(library(tidymodels))
suppressPackageStartupMessages(library(yardstick))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(recipes))
suppressPackageStartupMessages(library(themis))
suppressPackageStartupMessages(library(DALEXtra))
suppressPackageStartupMessages(library(pins))
suppressPackageStartupMessages(library(plumber))
suppressPackageStartupMessages(library(rapidoc))
suppressPackageStartupMessages(library(vetiver))




logger::log_info("Load Helper Fns")
# Helper function: Pretty data table
# Table fmt ref: https://stackoverflow.com/questions/44368572/dt-showing-more-rows-in-dt
pretty_dt <- function(tbl, num_digits = 3, num_rows=20) {
  dt <- tbl %>% 
    head(num_rows) %>%
    datatable(
      fillContainer = FALSE, 
      options = list(pageLength = 5, 
                     autoWidth = TRUE,
                     dom = 'tip', # table only
                     scrollX = TRUE,
                     scrollY = TRUE,
                     initComplete = JS(
                       "function(settings, json) {",
                       "$(this.api().table().header()).css({'color': 'white'});",
                       "}")
      )
    )
  
  # Round float columns (numeric but not integer)
  cols_numeric <- colnames(tbl)[sapply(tbl, is.numeric)]
  cols_integer <- colnames(tbl)[sapply(tbl, is.integer)]
  cols_float <- cols_numeric[!(cols_numeric %in% cols_integer)]
  if (length(cols_float) > 0) {
    dt <- dt %>% formatRound(cols_float, num_digits)
  }
  
  return(dt)
}


logger::log_info("Workshop environment setup is complete.")