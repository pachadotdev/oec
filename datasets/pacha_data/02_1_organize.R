# Open the project datasets.Rproj three levels up to run this script

# 1: Set parameters

rm(list = ls())

envir = as.environment(1)

parameters <- function() {
  assign("classification", readline(prompt="Enter classification: "), envir = envir)
  assign("rev", readline(prompt="Enter revision: "), envir = envir)
  assign("initialyear", readline(prompt="Enter initial year: "), envir = envir)
  assign("finalyear", readline(prompt="Enter final year: "), envir = envir)
  
  assign("years",seq(initialyear,finalyear), envir = envir)
  
  source("pacha_data/01_1_scripts/1_organize/1_libraries.R")
  source("pacha_data/01_1_scripts/1_organize/2_extract.R")
  source("pacha_data/01_1_scripts/1_organize/3_rearrange.R")
  source("pacha_data/01_1_scripts/1_organize/4_verify.R")
  source("pacha_data/01_1_scripts/1_organize/5_compress.R")
}

parameters()
