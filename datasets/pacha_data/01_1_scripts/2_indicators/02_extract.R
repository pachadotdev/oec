# Open the project datasets.Rproj two levels up to run this script

#1: extract raw data to a folder that we delete when the process is completed

zip_folder = paste0("pacha_data/",classification,"_rev",rev,"/1_",classification,"_rev",rev,"_verified_data","/trade_",digits,"/")

indicators_folder = paste0("pacha_data/",classification,"_rev",rev,"/2_",classification,"_rev",rev,"_indicators/")
indicators_code_folder = paste0(indicators_folder,"indicators_",digits,"/")

temporal_csv_folder = paste0(indicators_code_folder,"temporal_csv_folder/")
rca_exports_folder = paste0(indicators_code_folder,"1_1_",classification,"_rev",rev,"_",digits,"_rca_exports/")
rca_imports_folder = paste0(indicators_code_folder,"1_2_",classification,"_rev",rev,"_",digits,"_rca_imports/")

rca_exports_smooth_folder = paste0(indicators_code_folder,"2_1_",classification,"_rev",rev,"_",digits,"_rca_exports_smooth/")
rca_imports_smooth_folder = paste0(indicators_code_folder,"2_2_",classification,"_rev",rev,"_",digits,"_rca_imports_smooth/")

rca_matrices_folder = paste0(indicators_code_folder,"3_",classification,"_rev",rev,"_",digits,"_matrices/")
pci_rankings_folder = paste0(indicators_code_folder,"4_1_",classification,"_rev",rev,"_",digits,"_pci_rankings/")
eci_rankings_folder = paste0(indicators_code_folder,"5_1_",classification,"_rev",rev,"_",digits,"_eci_rankings/")
joined_pci_rankings_folder = paste0(indicators_code_folder,"4_2_",classification,"_rev",rev,"_",digits,"_joined_pci_rankings/")
joined_eci_rankings_folder = paste0(indicators_code_folder,"5_2_",classification,"_rev",rev,"_",digits,"_joined_eci_rankings/")

if(file.exists(rca_exports_folder) & file.exists(rca_imports_folder)) {
  print("RCA folder exists. No extraction needed.") 
} else {
  try(dir.create(indicators_folder))
  try(dir.create(indicators_code_folder))
  try(dir.create(temporal_csv_folder))
  
  zip_list = list.files(path = zip_folder, pattern = "\\.zip")
  
  for(t in 1:length(zip_list)) {
    if(!file.exists(paste0(temporal_csv_folder,classification,"_rev",rev,"_",years[[t]],"_",digits,".csv"))) {
      print(paste("Unzipping year",zip_list[[t]]))
      system(paste0("7z e ",zip_folder,zip_list[[t]]," -oc:",temporal_csv_folder))
    } else {
      print("File exists. Skippping.")
    }
  }
  
  rm(zip_list,zip_folder)
  gc()
}
