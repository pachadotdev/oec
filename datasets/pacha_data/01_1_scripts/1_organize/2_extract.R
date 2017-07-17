# Open the project datasets.Rproj two levels up to run this script

#1: Extract raw files to a temporal folder

zip_folder = paste0("comtrade_data/",classification,"_rev",rev,"_bulk/zip/")
zip_list = list.files(path = zip_folder, pattern = "\\.zip")

rev_folder = paste0("pacha_data/",classification,"_rev",rev,"/")
verified_csv_folder = paste0(rev_folder,"1_",classification,"_rev",rev,"_verified_data/")

temporal_raw_csv_folder = paste0(verified_csv_folder,"temporal_raw_csv/")
temporal_raw_csv_folder_2 = paste0(verified_csv_folder,"temporal_raw_csv_2/")

if(file.exists(temporal_raw_csv_folder)) {
  print("The raw files exist. No extraction needed.")
} else {
  try(dir.create(rev_folder))
  try(dir.create(verified_csv_folder))
  try(dir.create(temporal_raw_csv_folder))
  
  for(t in 1:length(zip_list)) {
    if(file.exists(paste0(temporal_raw_csv_folder,classification,"_rev",rev,"_bulk_",years[[t]],".csv"))) {
      print("File exists. Skippping.") 
    } else {
      print(paste("Unzipping year",years[[t]]))
      system(paste0("7z e ",zip_folder,zip_list[[t]]," -oc:",temporal_raw_csv_folder))
    }
  }
  gc()
}
