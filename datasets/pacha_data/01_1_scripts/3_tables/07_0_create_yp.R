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

#3: create YP table

for(t in years_missing_t_minus_1) {
  if(file.exists(paste0(yp_folder,classification,"_rev",rev,"_yp_",digits,"_",t,".zip")) |
     file.exists(paste0(yp_folder,classification,"_rev",rev,"_yp_",digits,"_",t,".csv"))) {
    messageline() 
    message(paste("YP table for the year",t,"exists. Skipping."))
  } else {
    messageline() 
    message(paste("Creating YP table for the year", t))
    source("pacha_data/01_1_scripts/3_tables/07_1_yp_missing_t_minus_1.R")
  }
}

for(t in years_missing_t_minus_5) {
  if(file.exists(paste0(yp_folder,classification,"_rev",rev,"_yp_",digits,"_",t,".zip")) |
     file.exists(paste0(yp_folder,classification,"_rev",rev,"_yp_",digits,"_",t,".csv"))) {
    messageline() 
    message(paste("YP table for the year",t,"exists. Skipping."))
  } else {
    messageline() 
    message(paste("Creating YP table for the year", t))
    source("pacha_data/01_1_scripts/3_tables/07_2_yp_missing_t_minus_5.R")
  }
}

for(t in years_full) {
  if(file.exists(paste0(yp_folder,classification,"_rev",rev,"_yp_",digits,"_",t,".zip")) |
     file.exists(paste0(yp_folder,classification,"_rev",rev,"_yp_",digits,"_",t,".csv"))) {
    messageline() 
    message(paste("YP table for the year",t,"exists. Skipping."))
  } else {
    messageline() 
    message(paste("Creating YP table for the year", t))
    source("pacha_data/01_1_scripts/3_tables/07_3_yp_full.R")
  }
}

gc()

#4: delete PCI folder

if(file.exists(pci_folder)) {
  unlink(pci_folder, recursive = TRUE)
}

#5: compress YP tables

source("pacha_data/01_1_scripts/3_tables/07_4_yp_compress.R")
