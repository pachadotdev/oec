# Open the project datasets.Rproj three levels up to run this script

#1: load functions

source("pacha_data/01_1_scripts/3_tables/99_helpers.R")

#2: extract YODP tables

zip_list = list.files(path = yodp_folder, pattern = "\\.zip")

if(length(zip_list) > 0) {
  for(i in 1:length(zip_list)) {
    if(!file.exists(paste0(yodp_folder,classification,"_rev",rev,"_yodp_",digits,"_",years[[i]],".csv")) &
       !file.exists(paste0(ydp_folder,classification,"_rev",rev,"_ydp_",digits,"_",years[[i]],".zip"))) {
      messageline() 
      message(paste("Unzipping YODP table for the year",years[[i]]))
      system(paste0("7z e ",yodp_folder,zip_list[[i]]," -oc:",yodp_folder))
    } else {
      messageline() 
      message(paste0("YODP table exists. No need to extract YODP table for the year ",years[[i]],". Skipping."))
    }
  }
}

#3: create YDP table

for(t in years_missing_t_minus_1) {
  if(file.exists(paste0(ydp_folder,classification,"_rev",rev,"_ydp_",digits,"_",t,".zip")) |
     file.exists(paste0(ydp_folder,classification,"_rev",rev,"_ydp_",digits,"_",t,".csv"))) {
    messageline() 
    message(paste("YDP table for the year",t,"exists. Skipping."))
  } else {
    messageline() 
    message(paste("Creating YDP table for the year", t))
    source("pacha_data/01_1_scripts/3_tables/05_1_ydp_missing_t_minus_1.R")
  }
}

for(t in years_missing_t_minus_5) {
  if(file.exists(paste0(ydp_folder,classification,"_rev",rev,"_ydp_",digits,"_",t,".zip")) |
     file.exists(paste0(ydp_folder,classification,"_rev",rev,"_ydp_",digits,"_",t,".csv"))) {
    messageline() 
    message(paste("YDP table for the year",t,"exists. Skipping."))
  } else {
    messageline() 
    message(paste("Creating YDP table for the year", t))
    source("pacha_data/01_1_scripts/3_tables/05_2_ydp_missing_t_minus_5.R")
  }
}

for(t in years_full) {
  if(file.exists(paste0(ydp_folder,classification,"_rev",rev,"_ydp_",digits,"_",t,".zip")) |
     file.exists(paste0(ydp_folder,classification,"_rev",rev,"_ydp_",digits,"_",t,".csv"))) {
    messageline() 
    message(paste("YDP table for the year",t,"exists. Skipping."))
  } else {
    messageline() 
    message(paste("Creating YDP table for the year", t))
    source("pacha_data/01_1_scripts/3_tables/05_3_ydp_full.R")
  }
}

gc()

#4: compress YDP tables

source("pacha_data/01_1_scripts/3_tables/05_4_ydp_compress.R")
