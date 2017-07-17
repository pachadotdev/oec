# Open the project datasets.Rproj two levels up to run this script

#1: Import country codes

url = "https://comtrade.un.org/data/cache/reporterAreas.json"
json = "comtrade_data/country_codes/country_codes.json"
if(!file.exists(json)){
  try(dir.create("country_codes"))
  download.file(url, json, method="wget")
}

url = "http://unstats.un.org/unsd/tradekb/Attachment321.aspx?AttachmentType=1"
xls = "comtrade_data/country_codes/country_codes.xls"
if(!file.exists(xls)){
  download.file(url, xls, method="wget")
}
