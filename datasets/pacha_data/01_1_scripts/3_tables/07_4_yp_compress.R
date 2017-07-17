# Open the project datasets.Rproj three levels up to run this script

#1: compress YP tables

yp_folder_list = list.files(path = yp_folder, pattern = "\\.csv")

if(length(yp_folder_list) > 0) {
  for(t in 1:length(yp_folder_list)) {
    if(file.exists(paste0(yp_folder,classification,"_rev",rev,"_yp_",digits,"_",years[[t]],".csv")) &
       !file.exists(paste0(yp_folder,classification,"_rev",rev,"_yp_",digits,"_",years[[t]],".zip"))) {
      messageline() 
      message(paste0("Compressing YP table for the year ",years[[t]]))
      system(paste0("7z a \"",getwd(),"/",yp_folder,classification,"_rev",rev,"_yp_",digits,"_",years[[t]],".zip\""," \"",getwd(),"/",yp_folder,classification,"_rev",rev,"_yp_",digits,"_",years[[t]],".csv\""))
    } else {
      messageline() 
      message(paste("YP table for the year",years[[t]],"is already compressed. Skipping."))
    }
  }
}
