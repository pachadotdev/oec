# Open the project datasets.Rproj two levels up to run this script

if(!file.exists(csv_folder)) {
  dir.create(csv_folder)
}

zip_list = list.files(path = zip_folder, pattern = "\\.zip")

for(t in 1:length(zip_list)) {
  if(!file.exists(paste0(csv_folder,"hs_rev1992_bulk_",years[[t]],".csv"))) {
    print(paste("Unzipping year",years[[t]]))
    system(paste0("7z e ",zip_folder,zip_list[[t]]," -oc:",csv_folder))
  } else {
    print("File exists. Skippping.")
  }
}
