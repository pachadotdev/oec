# Open the project datasets.Rproj three levels up to run this script

#1: remove csv files in each folder

if(length(yodp_folder_list) > 0) {
  for(i in 1:length(yodp_folder_list)) {
    try(file.remove(paste0(yodp_folder,yodp_folder_list[[i]])))
  }
}

if(length(yd_folder_list) > 0) {
  for(i in 1:length(yd_folder_list)) {
    try(file.remove(paste0(yd_folder,yd_folder_list[[i]])))
  }
}

if(length(ydp_folder_list) > 0) {
  for(i in 1:length(ydp_folder_list)) {
    try(file.remove(paste0(ydp_folder,ydp_folder_list[[i]])))
  }
}

if(length(yod_folder_list) > 0) {
  for(i in 1:length(yod_folder_list)) {
    try(file.remove(paste0(yod_folder,yod_folder_list[[i]])))
  }
}

if(length(yp_folder_list) > 0) {
  for(i in 1:length(yp_folder_list)) {
    try(file.remove(paste0(yp_folder,yp_folder_list[[i]])))
  }
}

if(length(yo_folder_list) > 0) {
  for(i in 1:length(yo_folder_list)) {
    try(file.remove(paste0(yo_folder,yo_folder_list[[i]])))
  }
}

if(length(yop_folder_list) > 0) {
  for(i in 1:length(yop_folder_list)) {
    try(file.remove(paste0(yop_folder,yop_folder_list[[i]])))
  }
}
