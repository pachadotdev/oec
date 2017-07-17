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
  
  source("pacha_data/01_1_scripts/2_indicators/01_libraries.R")
  source("pacha_data/01_1_scripts/2_indicators/02_extract.R")
  source("pacha_data/01_1_scripts/2_indicators/03_1_compute_rca_exports.R")
  source("pacha_data/01_1_scripts/2_indicators/03_2_compute_rca_imports.R")
  source("pacha_data/01_1_scripts/2_indicators/04_1_compute_rca_exports_smooth.R")
  source("pacha_data/01_1_scripts/2_indicators/04_2_compute_rca_imports_smooth.R")
  source("pacha_data/01_1_scripts/2_indicators/05_compute_rca_matrix.R")
  source("pacha_data/01_1_scripts/2_indicators/06_compute_pci.R")
  
  if(classification == "sitc" & rev == 2) {
    source("pacha_data/01_1_scripts/2_indicators/07_compute_eci.R")
  }
  
  source("pacha_data/01_1_scripts/2_indicators/08_join_pci_rankings.R")
  
  if(classification == "sitc" & rev == 2) {
    source("pacha_data/01_1_scripts/2_indicators/09_join_eci_rankings.R")
  }
  
  source("pacha_data/01_1_scripts/2_indicators/10_compress.R")
}

parameters()
