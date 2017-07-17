# Open the project datasets.Rproj two levels up to run this script

#1: Import product codes

if(!file.exists("comtrade_data/product_codes/official_list_of_comtrade_codes.xlsx")) {
  try(dir.create("comtrade_data/product_codes"))
  url = "https://unstats.un.org/unsd/tradekb/Attachment439.aspx?AttachmentType=1"
  xlsx = "comtrade_data/product_codes/official_list_of_comtrade_codes.xlsx"
  download.file(url,xlsx)
}