# Open the project datasets.Rproj three levels up to run this script

#1: compress yd tables

yd_folder_list = list.files(path = yd_folder, pattern = "\\.csv")

if(length(yd_folder_list) > 0) {
  for(t in 1:length(yd_folder_list)) {
    if(file.exists(paste0(yd_folder,classification,"_rev",rev,"_yd_",digits,"_",years[[t]],".csv")) &
       !file.exists(paste0(yd_folder,classification,"_rev",rev,"_yd_",digits,"_",years[[t]],".zip"))) {
      messageline() 
      message(paste0("Compressing YD table for the year ",years[[t]]))
      system(paste0("7z a ",yd_folder,classification,"_rev",rev,"_yd_",digits,"_",years[[t]],".zip"," ",yd_folder,classification,"_rev",rev,"_yd_",digits,"_",years[[t]],".csv"))
    } else {
      messageline() 
      message(paste("YD table for the year",years[[t]],"is already compressed. Skipping."))
    }
  }
}
