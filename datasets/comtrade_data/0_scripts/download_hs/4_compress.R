# Open the project datasets.Rproj two levels up to run this script

csv_list = list.files(path = csv_folder, pattern = "\\.csv")

if(length(csv_list) > 0) {
  for(t in 1:length(csv_list)) {
    if(file.exists(paste0(csv_folder,"hs_rev1992_bulk_",years[[t]],".csv"))) {
      file.remove(paste0(zip_folder,"hs_rev1992_bulk_",years[[t]],".zip"))
      system(paste0("7z a \"",getwd(),"/",zip_folder,"hs_rev1992_bulk_",years[[t]],".zip\""," \"",getwd(),"/",csv_folder,"hs_rev1992_bulk_",years[[t]],".csv\""))
    }
  }
}

if(file.exists(csv_folder)) {
  unlink(csv_folder, recursive = TRUE)
}
