# Open the project datasets.Rproj two levels up to run this script

# 1: Delete temporal folders

try(unlink(temporal_raw_csv_folder, recursive = TRUE))
try(unlink(temporal_raw_csv_folder_2, recursive = TRUE))

# 2: List verified files

verified_csv_list = list.files(path = verified_csv_folder, pattern = ".csv", recursive = TRUE) %>% 
  gsub("trade",paste0(getwd(),"/",verified_csv_folder,"trade"),.)
  
verified_zip_list = verified_csv_list %>% 
  gsub("csv","zip",.)

#3: Compress verified imports files and delete csv files

for(k in 1:length(verified_zip_list)) {
  if(file.exists(verified_zip_list[[k]])) {
    print("The compressed section file exist. Skipping.")
  } else {
    system(paste("7z a",verified_zip_list[[k]],verified_csv_list[[k]]))
    file.remove(verified_csv_list[[k]])
  }
}
