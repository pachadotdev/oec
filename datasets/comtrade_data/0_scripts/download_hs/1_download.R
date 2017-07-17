# Open the project datasets.Rproj two levels up to run this script

if(!file.exists(zip_folder)) {
  dir.create(zip_folder)
}

for(t in 1:length(years)) {
  url = paste0("https://comtrade.un.org/api/get/bulk/C/A/",years[[t]],"/ALL/",classification,"?token=",token)
  zip = paste0(zip_folder,classification2,years[[t]],".zip")
  if(!file.exists(zip)) {
    print(paste("Downloading",zip))
    download.file(url, zip, method="wget", extra = "--no-check-certificate")
  } else {
    print(paste(zip,"exists. Skiping."))
  }
}
