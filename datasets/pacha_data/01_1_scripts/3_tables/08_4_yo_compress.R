# Open the project datasets.Rproj three levels up to run this script

#1: compress YO tables

yo_folder_list = list.files(path = yo_folder, pattern = "\\.csv")

if(length(yo_folder_list) > 0) {
  for(t in 1:length(yo_folder_list)) {
    if(file.exists(paste0(yo_folder,classification,"_rev",rev,"_yo_",digits,"_",years[[t]],".csv")) &
       !file.exists(paste0(yo_folder,classification,"_rev",rev,"_yo_",digits,"_",years[[t]],".zip"))) {
      messageline() 
      message(paste0("Compressing YO table for the year ",years[[t]]))
      system(paste0("7z a \"",getwd(),"/",yo_folder,classification,"_rev",rev,"_yo_",digits,"_",years[[t]],".zip\""," \"",getwd(),"/",yo_folder,classification,"_rev",rev,"_yo_",digits,"_",years[[t]],".csv\""))
    } else {
      messageline() 
      message(paste("YO table for the year",years[[t]],"is already compressed. Skipping."))
    }
  }
}
