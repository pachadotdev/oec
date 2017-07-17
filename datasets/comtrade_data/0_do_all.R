# Open the project datasets.Rproj one level up to run this script

# 1: Set parameters

rm(list = ls())

envir = as.environment(1)

parameters <- function() {
  assign("download_country_codes", readline(prompt="Download country codes (y/n: "), envir = envir)
  assign("download_product_codes", readline(prompt="Download product codes (y/n: "), envir = envir)
  assign("download_sitc_rev1", readline(prompt="Download SITC rev1 datasets (y/n: "), envir = envir)
  assign("download_sitc_rev2", readline(prompt="Download SITC rev2 datasets (y/n: "), envir = envir)
  assign("download_sitc_rev3", readline(prompt="Download SITC rev3 datasets (y/n: "), envir = envir)
  assign("download_sitc_rev4", readline(prompt="Download SITC rev4 datasets (y/n: "), envir = envir)
  assign("download_hs_rev1992", readline(prompt="Download HS rev1992 datasets (y/n: "), envir = envir)
  assign("download_hs_rev1996", readline(prompt="Download HS rev1996 datasets (y/n: "), envir = envir)
  assign("download_hs_rev2002", readline(prompt="Download HS rev2002 datasets (y/n: "), envir = envir)
  assign("download_hs_rev2007", readline(prompt="Download HS rev2007 datasets (y/n: "), envir = envir)
  assign("download_hs_rev2012", readline(prompt="Download HS rev2012 datasets (y/n: "), envir = envir)
  
  token = "replace_with_your_token"
  
  source("comtrade_data/0_scripts/1_libraries.R")
  
  if(download_country_codes = "y"){source("comtrade_data/0_scripts/1_download_country_codes.R")}
  
  if(download_product_codes = "y"){source("comtrade_data/0_scripts/2_download_product_codes.R")}
  
  if(download_sitc_rev1 = "y"){
    years = as.list(seq(1999,2015,1))
    
    zip_folder = paste0("comtrade_data/sitc_rev",rev,"_bulk/zip/")
    classification = paste0("S",rev)
    classification2 = paste0("sitc_rev",rev,"_bulk_")
    zip_folder = paste0("comtrade_data/sitc_rev",rev,"_bulk/zip/")
    csv_folder = paste0("comtrade_data/sitc_rev",rev,"_bulk/csv/")
    
    source("comtrade_data/0_scripts/download_sitc/1_download.R")
    source("comtrade_data/0_scripts/download_sitc/2_extract.R")
    source("comtrade_data/0_scripts/download_sitc/3_rename.R")
    source("comtrade_data/0_scripts/download_sitc/4_compress.R")
  }
  
  if(download_sitc_rev2 = "y"){
    years = as.list(seq(1999,2015,1))
    
    zip_folder = paste0("comtrade_data/sitc_rev",rev,"_bulk/zip/")
    classification = paste0("S",rev)
    classification2 = paste0("sitc_rev",rev,"_bulk_")
    zip_folder = paste0("comtrade_data/sitc_rev",rev,"_bulk/zip/")
    csv_folder = paste0("comtrade_data/sitc_rev",rev,"_bulk/csv/")
    
    source("comtrade_data/0_scripts/download_sitc/1_download.R")
    source("comtrade_data/0_scripts/download_sitc/2_extract.R")
    source("comtrade_data/0_scripts/download_sitc/3_rename.R")
    source("comtrade_data/0_scripts/download_sitc/4_compress.R")
  }
  
  if(download_sitc_rev3 = "y"){
    years = as.list(seq(1999,2015,1))
    
    zip_folder = paste0("comtrade_data/sitc_rev",rev,"_bulk/zip/")
    classification = paste0("S",rev)
    classification2 = paste0("sitc_rev",rev,"_bulk_")
    zip_folder = paste0("comtrade_data/sitc_rev",rev,"_bulk/zip/")
    csv_folder = paste0("comtrade_data/sitc_rev",rev,"_bulk/csv/")
    
    source("comtrade_data/0_scripts/download_sitc/1_download.R")
    source("comtrade_data/0_scripts/download_sitc/2_extract.R")
    source("comtrade_data/0_scripts/download_sitc/3_rename.R")
    source("comtrade_data/0_scripts/download_sitc/4_compress.R")
  }
  
  if(download_sitc_rev4 = "y"){
    years = as.list(seq(2007,2015,1))
    
    zip_folder = paste0("comtrade_data/sitc_rev",rev,"_bulk/zip/")
    classification = paste0("S",rev)
    classification2 = paste0("sitc_rev",rev,"_bulk_")
    zip_folder = paste0("comtrade_data/sitc_rev",rev,"_bulk/zip/")
    csv_folder = paste0("comtrade_data/sitc_rev",rev,"_bulk/csv/")
    
    source("comtrade_data/0_scripts/download_sitc/1_download.R")
    source("comtrade_data/0_scripts/download_sitc/2_extract.R")
    source("comtrade_data/0_scripts/download_sitc/3_rename.R")
    source("comtrade_data/0_scripts/download_sitc/4_compress.R")
  }
  
  if(download_hs_rev1992 = "y"){
    years = as.list(seq(1992,2015,1))
    
    zip_folder = paste0("comtrade_data/hs_rev",rev,"_bulk/zip/")
    classification = "H0"
    classification2 = paste0("hs_rev",rev,"_bulk_")
    zip_folder = paste0("comtrade_data/hs_rev",rev,"_bulk/zip/")
    csv_folder = paste0("comtrade_data/hs_rev",rev,"_bulk/csv/")
    
    source("comtrade_data/0_scripts/download_sitc/1_download.R")
    source("comtrade_data/0_scripts/download_sitc/2_extract.R")
    source("comtrade_data/0_scripts/download_sitc/3_rename.R")
    source("comtrade_data/0_scripts/download_sitc/4_compress.R")
  }
  
  if(download_hs_rev1996 = "y"){
    years = as.list(seq(1996,2015,1))
    
    zip_folder = paste0("comtrade_data/hs_rev",rev,"_bulk/zip/")
    classification = "H1"
    classification2 = paste0("hs_rev",rev,"_bulk_")
    zip_folder = paste0("comtrade_data/hs_rev",rev,"_bulk/zip/")
    csv_folder = paste0("comtrade_data/hs_rev",rev,"_bulk/csv/")
    
    source("comtrade_data/0_scripts/download_sitc/1_download.R")
    source("comtrade_data/0_scripts/download_sitc/2_extract.R")
    source("comtrade_data/0_scripts/download_sitc/3_rename.R")
    source("comtrade_data/0_scripts/download_sitc/4_compress.R")
  }
  
  if(download_hs_rev2002 = "y"){
    years = as.list(seq(2002,2015,1))
    
    zip_folder = paste0("comtrade_data/hs_rev",rev,"_bulk/zip/")
    classification = "H2"
    classification2 = paste0("hs_rev",rev,"_bulk_")
    zip_folder = paste0("comtrade_data/hs_rev",rev,"_bulk/zip/")
    csv_folder = paste0("comtrade_data/hs_rev",rev,"_bulk/csv/")
    
    source("comtrade_data/0_scripts/download_sitc/1_download.R")
    source("comtrade_data/0_scripts/download_sitc/2_extract.R")
    source("comtrade_data/0_scripts/download_sitc/3_rename.R")
    source("comtrade_data/0_scripts/download_sitc/4_compress.R")
  }
  
  if(download_hs_rev2007 = "y"){
    years = as.list(seq(2007,2015,1))
    
    zip_folder = paste0("comtrade_data/hs_rev",rev,"_bulk/zip/")
    classification = "H3"
    classification2 = paste0("hs_rev",rev,"_bulk_")
    zip_folder = paste0("comtrade_data/hs_rev",rev,"_bulk/zip/")
    csv_folder = paste0("comtrade_data/hs_rev",rev,"_bulk/csv/")
    
    source("comtrade_data/0_scripts/download_sitc/1_download.R")
    source("comtrade_data/0_scripts/download_sitc/2_extract.R")
    source("comtrade_data/0_scripts/download_sitc/3_rename.R")
    source("comtrade_data/0_scripts/download_sitc/4_compress.R")
  }
  
  if(download_hs_rev2012 = "y"){
    years = as.list(seq(2012,2015,1))
    
    zip_folder = paste0("comtrade_data/hs_rev",rev,"_bulk/zip/")
    classification = "H4"
    classification2 = paste0("hs_rev",rev,"_bulk_")
    zip_folder = paste0("comtrade_data/hs_rev",rev,"_bulk/zip/")
    csv_folder = paste0("comtrade_data/hs_rev",rev,"_bulk/csv/")
    
    source("comtrade_data/0_scripts/download_sitc/1_download.R")
    source("comtrade_data/0_scripts/download_sitc/2_extract.R")
    source("comtrade_data/0_scripts/download_sitc/3_rename.R")
    source("comtrade_data/0_scripts/download_sitc/4_compress.R")
  }
}