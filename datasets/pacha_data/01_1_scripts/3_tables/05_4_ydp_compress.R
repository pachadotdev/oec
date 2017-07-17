# Open the project datasets.Rproj three levels up to run this script

#1: compress YDP tables

ydp_folder_list = list.files(path = ydp_folder, pattern = "\\.csv")

if(length(ydp_folder_list) > 0) {
  for(t in 1:length(ydp_folder_list)) {
    if(file.exists(paste0(ydp_folder,classification,"_rev",rev,"_ydp_",digits,"_",years[[t]],".csv")) &
       !file.exists(paste0(ydp_folder,classification,"_rev",rev,"_ydp_",digits,"_",years[[t]],".zip"))) {
      messageline() 
      message(paste0("Compressing YDP table for the year ",years[[t]]))
      system(paste0("7z a ",ydp_folder,classification,"_rev",rev,"_ydp_",digits,"_",years[[t]],".zip"," ",ydp_folder,classification,"_rev",rev,"_ydp_",digits,"_",years[[t]],".csv"))
    } else {
      messageline() 
      message(paste("YDP table for the year",years[[t]],"is already compressed. Skipping."))
    }
  }
}
