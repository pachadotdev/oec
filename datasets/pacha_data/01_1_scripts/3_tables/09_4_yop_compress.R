# Open the project datasets.Rproj three levels up to run this script

#1: compress YOP tables

yop_folder_list = list.files(path = yop_folder, pattern = "\\.csv")

if(length(yop_folder_list) > 0) {
  for(t in 1:length(yop_folder_list)) {
    if(file.exists(paste0(yop_folder,classification,"_rev",rev,"_yop_",digits,"_",years[[t]],".csv")) &
       !file.exists(paste0(yop_folder,classification,"_rev",rev,"_yop_",digits,"_",years[[t]],".zip"))) {
      messageline() 
      message(paste0("Compressing YOP table for the year ",years[[t]]))
      system(paste0("7z a \"",getwd(),"/",yop_folder,classification,"_rev",rev,"_yop_",digits,"_",years[[t]],".zip\""," \"",getwd(),"/",yop_folder,classification,"_rev",rev,"_yop_",digits,"_",years[[t]],".csv\""))
    } else {
      messageline() 
      message(paste("YOP table for the year",years[[t]],"is already compressed. Skipping."))
    }
  }
}
