# Open the project datasets.Rproj three levels up to run this script

#1: load functions

source("pacha_data/01_1_scripts/3_tables/99_helpers.R")

#2: read replacement codes

countries = tbl_dt(fread("oec_data/country_names.tsv")) %>% 
  select(-name)

products = tbl_dt(fread(paste0("oec_data/",classification,"_rev",rev,"_product_names.tsv"))) %>% 
  setNames(., c("prod_id","id","name")) %>% 
  select(-name)

#3: read tables accordingly to 1 year and 5 years data availability

for(t in years_missing_t_minus_1) {
  if(file.exists(paste0(yodp_folder,classification,"_rev",rev,"_yodp_",digits,"_",t,".zip")) |
     file.exists(paste0(yodp_folder,classification,"_rev",rev,"_yodp_",digits,"_",t,".csv"))) {
    messageline() 
    message(paste("YODP table for the year", t, "exists. Skipping."))
  } else {
    messageline() 
    message(paste("Creating YODP table for the year", t))
    source("pacha_data/01_1_scripts/3_tables/03_1_yodp_missing_t_minus_1.R")
  }
}

for(t in years_missing_t_minus_5) {
  if(file.exists(paste0(yodp_folder,classification,"_rev",rev,"_yodp_",digits,"_",t,".zip")) |
     file.exists(paste0(yodp_folder,classification,"_rev",rev,"_yodp_",digits,"_",t,".csv"))) {
    messageline() 
    message(paste("YODP table for the year", t, "exists. Skipping."))
  } else {
    messageline() 
    message(paste("Creating YODP table for the year", t))
    source("pacha_data/01_1_scripts/3_tables/03_2_yodp_missing_t_minus_5.R")
  }
}

for(t in years_full) {
  if(file.exists(paste0(yodp_folder,classification,"_rev",rev,"_yodp_",digits,"_",t,".zip")) |
     file.exists(paste0(yodp_folder,classification,"_rev",rev,"_yodp_",digits,"_",t,".csv"))) {
    messageline() 
    message(paste("YODP table for the year", t, "exists. Skipping."))
  } else {
    messageline() 
    message(paste("Creating YODP table for the year", t))
    source("pacha_data/01_1_scripts/3_tables/03_3_yodp_full.R")
  }
}

#4: delete imports and exports folders

if(file.exists(temporal_csv_folder)) {
  unlink(temporal_csv_folder, recursive = TRUE)
}

#5: compress YODP tables

source("pacha_data/01_1_scripts/3_tables/03_4_yodp_compress.R")
