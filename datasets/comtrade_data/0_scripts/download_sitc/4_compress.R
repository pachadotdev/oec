# Open the project datasets.Rproj two levels up to run this script

if(length(csv_list) > 0) {
  for(t in 1:length(csv_list)) {
    if(file.exists(paste0(csv_folder,"sitc_rev1_bulk_",years[[t]],".csv"))) {
      file.remove(paste0(zip_folder,"sitc_rev1_bulk_",years[[t]],".zip"))
      system(paste0("7z a \"",getwd(),"/",zip_folder,"sitc_rev1_bulk_",years[[t]],".zip\""," \"",getwd(),"/",csv_folder,"sitc_rev1_bulk_",years[[t]],".csv\""))
    }
  }
}

if(file.exists(csv_folder)) {
  unlink(csv_folder, recursive = TRUE)
}
