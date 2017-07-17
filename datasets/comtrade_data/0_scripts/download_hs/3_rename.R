# Open the project datasets.Rproj two levels up to run this script

csv_list = list.files(path = csv_folder, pattern = "\\.csv")

for(t in 1:length(csv_list)) {
  if(!file.exists(paste0(csv_folder,gsub("_pub.*","",csv_list[[t]]),".csv"))) {
    print(paste("Renaming year",years[[t]]))
    file.rename(paste0(csv_folder,csv_list[[t]]),paste0(csv_folder,gsub("_pub.*","",csv_list[[t]]),".csv"))
  } else {
    print("File exists. Skippping.")
  }
}

for(t in 1:length(csv_list)) {
  if(!file.exists(paste0(csv_folder,"hs_rev1992_bulk_",years[[t]],".csv")) &
     file.exists(paste0(csv_folder,"type-C_r-ALL_ps-",years[t],"_freq-A_px-H0.csv"))) {
    print(paste("Renaming year",years[[t]]))
    file.rename(paste0(csv_folder,"type-C_r-ALL_ps-",years[t],"_freq-A_px-H0.csv"),paste0(csv_folder,"hs_rev1992_bulk_",years[[t]],".csv"))
  } else {
    print("File exists. Skippping.")
  }
}
