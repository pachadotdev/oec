# Open the project datasets.Rproj one level up to run this script

###########################
# 2. Download datasets
###########################

# 2.1: download sitc network (SITC rev.2 4 characters)

download_network_sitc_rev2_4char_json = function(year){
  url = "https://raw.githubusercontent.com/alexandersimoes/oec/master/oec/static/json/network_sitc.json"
  folder = paste0("asimoes_data")
  json = paste0(folder,"/network_sitc_rev2_4char.json")
  
  if(!file.exists(folder)) {
    dir.create(folder)
  }  
  
  if(!file.exists(json)) {
    print("downloading network_sitc.json")
    download.file(url, json, method="curl")
  }
}

download_network_sitc_rev2_4char_json()

rm(download_network_sitc_rev2_4char_json)

# 2.2: download hs92 network (HS92 4 characters)

download_network_hs92_4char_json = function(year){
  url = "https://raw.githubusercontent.com/alexandersimoes/oec/master/oec/static/json/network_hs4.json"
  folder = paste0("asimoes_data")
  json = paste0(folder,"/network_hs92_4char.json")

  if(!file.exists(folder)) {
    dir.create(folder)
  }  
  
  if(!file.exists(json)) {
    print("downloading network_hs4.json")
    download.file(url, json, method="curl")
  }
}

download_network_hs92_4char_json()

rm(download_network_hs92_4char_json)

# 2.3: download hs92 network (HS92 6 characters)

download_network_hs92_6char_json = function(year){
  url = "https://raw.githubusercontent.com/alexandersimoes/oec/master/oec/static/json/network_hs6.json"
  folder = paste0("asimoes_data")
  json = paste0(folder,"/network_hs92_6char.json")
  
  if(!file.exists(folder)) {
    dir.create(folder)
  }  
  
  if(!file.exists(json)) {
    print("downloading network_hs8.json")
    download.file(url, json, method="curl")
  }
}

download_network_hs92_6char_json()

rm(download_network_hs92_6char_json)
