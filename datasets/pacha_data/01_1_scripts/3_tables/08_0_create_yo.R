# Open the project datasets.Rproj three levels up to run this script

#1: load functions

source("pacha_data/01_1_scripts/3_tables/99_helpers.R")

#2: extract YODP tables

zip_list = list.files(path = yodp_folder, pattern = "\\.zip")

if(length(zip_list) > 0) {
  for(i in 1:length(zip_list)) {
    if(!file.exists(paste0(yodp_folder,classification,"_rev",rev,"_yodp_",digits,"_",years[[i]],".csv")) &
       !file.exists(paste0(yp_folder,classification,"_rev",rev,"_yp_",digits,"_",years[[i]],".zip"))) {
      messageline() 
      message(paste("Unzipping YODP table for the year",years[[i]]))
      system(paste0("7z e ",yodp_folder,zip_list[[i]]," -oc:",yodp_folder))
    } else {
      messageline() 
      message(paste0("YODP table exists. No need to extract YODP table for the year ",years[[i]],". Skipping."))
    }
  }
}

#3: create YO table

for(t in years_missing_t_minus_1) {
  if(file.exists(paste0(yo_folder,classification,"_rev",rev,"_yo_",digits,"_",t,".zip")) |
     file.exists(paste0(yo_folder,classification,"_rev",rev,"_yo_",digits,"_",t,".csv"))) {
    messageline() 
    message(paste("YO table for the year",t,"exists. Skipping."))
  } else {
    messageline() 
    message(paste("Creating YO table for the year", t))
    source("pacha_data/01_1_scripts/3_tables/08_1_yo_missing_t_minus_1.R")
  }
}

for(t in years_missing_t_minus_5) {
  if(file.exists(paste0(yo_folder,classification,"_rev",rev,"_yo_",digits,"_",t,".zip")) |
     file.exists(paste0(yo_folder,classification,"_rev",rev,"_yo_",digits,"_",t,".csv"))) {
    messageline() 
    message(paste("YO table for the year",t,"exists. Skipping."))
  } else {
    messageline() 
    message(paste("Creating YO table for the year", t))
    source("pacha_data/01_1_scripts/3_tables/08_2_yo_missing_t_minus_5.R")
  }
}

for(t in years_full) {
  if(file.exists(paste0(yo_folder,classification,"_rev",rev,"_yo_",digits,"_",t,".zip")) |
     file.exists(paste0(yo_folder,classification,"_rev",rev,"_yo_",digits,"_",t,".csv"))) {
    messageline() 
    message(paste("YO table for the year",t,"exists. Skipping."))
  } else {
    messageline() 
    message(paste("Creating YO table for the year", t))
    source("pacha_data/01_1_scripts/3_tables/08_3_yo_full.R")
  }
}

gc()

#4: compress YO tables

source(paste0("pacha_data/01_1_scripts/3_tables/08_4_yo_compress.R"))
