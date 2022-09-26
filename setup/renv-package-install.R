# install.packages('renv')
# renv::init()
# renv::activate()

renv::install("tidymodels")
#renv::install("tidyverse")
renv::install("openxlsx")
renv::install("ggplot2")
renv::install("here")
renv::install("testthat")
renv::install("plotly")
renv::install("speff2trial")
renv::install("tidymodels/censored")
renv::install("gridExtra")
renv::install("survminer")
renv::install('glue')
renv::install("ranger")
renv::install("xgboost")
renv::install("readxl")
#renv::install("")
#renv::install("")
#renv::install("")



renv::status()

renv::snapshot()

renv::status()
