# Open the project datasets.Rproj three levels up to run this script

#1: compress YOD tables

yod_folder_list = list.files(path = yod_folder, pattern = "\\.csv")

if(length(yod_folder_list) > 0) {
  for(t in 1:length(yod_folder_list)) {
    if(file.exists(paste0(yod_folder,classification,"_rev",rev,"_yod_",digits,"_",years[[t]],".csv")) &
       !file.exists(paste0(yod_folder,classification,"_rev",rev,"_yod_",digits,"_",years[[t]],".zip"))) {
      messageline() 
      message(paste0("Compressing YOD table for the year ",years[[t]]))
      system(paste0("7z a ",yod_folder,classification,"_rev",rev,"_yod_",digits,"_",years[[t]],".zip"," ",yod_folder,classification,"_rev",rev,"_yod_",digits,"_",years[[t]],".csv"))
    } else {
      messageline() 
      message(paste("YOD table for the year",years[[t]],"is already compressed. Skipping."))
    }
  }
}
