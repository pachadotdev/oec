# Open the project datasets.Rproj three levels up to run this script

#1: compress yodp tables

yodp_folder_list = list.files(path = yodp_folder, pattern = "\\.csv")

if(length(yodp_folder_list) > 0) {
  for(t in 1:length(yodp_folder_list)) {
    if(file.exists(paste0(yodp_folder,classification,"_rev",rev,"_yodp_",digits,"_",years[[t]],".csv")) &
       !file.exists(paste0(yodp_folder,classification,"_rev",rev,"_yodp_",digits,"_",years[[t]],".zip"))) {
      messageline() 
      message(paste0("Compressing YODP table for the year ",years[[t]]))
      system(paste0("7z a ",yodp_folder,classification,"_rev",rev,"_yodp_",digits,"_",years[[t]],".zip"," ",yodp_folder,classification,"_rev",rev,"_yodp_",digits,"_",years[[t]],".csv"))
    } else {
      messageline() 
      message(paste("YODP table for the year",years[[t]],"is already compressed. Skipping."))
    }
  }
}
