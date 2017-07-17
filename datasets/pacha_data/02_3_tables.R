# Open the project datasets.Rproj three levels up to run this script

# 1: Set parameters

rm(list = ls())

envir = as.environment(1)

parameters <- function() {
  assign("classification", readline(prompt="Enter classification: "), envir = envir)
  assign("rev", readline(prompt="Enter revision: "), envir = envir)
  assign("digits", readline(prompt="Enter digits: "), envir = envir)
  assign("initialyear", readline(prompt="Enter initial year: "), envir = envir)
  assign("finalyear", readline(prompt="Enter final year: "), envir = envir)
  
  assign("years",seq(initialyear,finalyear), envir = envir)
  
  assign("years_missing_t_minus_1",years[1], envir = envir)
  assign("years_missing_t_minus_5",(years[1]+1):(years[1]+4), envir = envir)
  assign("years_full",(years[1]+5):(years[length(years)]), envir = envir)

  source("pacha_data/01_1_scripts/3_tables/01_libraries.R")
  source("pacha_data/01_1_scripts/3_tables/02_extract.R")
  source("pacha_data/01_1_scripts/3_tables/03_0_create_yodp.R")
  source("pacha_data/01_1_scripts/3_tables/04_0_create_yd.R")
  source("pacha_data/01_1_scripts/3_tables/05_0_create_ydp.R")
  source("pacha_data/01_1_scripts/3_tables/06_0_create_yod.R")
  source("pacha_data/01_1_scripts/3_tables/07_0_create_yp.R")
  source("pacha_data/01_1_scripts/3_tables/08_0_create_yo.R")
  source("pacha_data/01_1_scripts/3_tables/09_0_create_yop.R")
  source("pacha_data/01_1_scripts/3_tables/10_remove_csv_files.R")
}

parameters()
