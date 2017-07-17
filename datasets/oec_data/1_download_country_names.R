# Open the project datasets.Rproj three levels up to run this script

#1: download country names

oec_data = "oec_data"
try(dir.create(oec_data))

countries_rankings = function(){
  url = "http://atlas.media.mit.edu/static/db/raw/country_names.tsv.bz2"
  bz2 = "oec_data/country_names.tsv.bz2"
  
  if(!file.exists(bz2)) {
    download.file(url, bz2, method = "wget")
  }
  
  if(!file.exists("oec_data/country_names.tsv")){
    system(paste0("7z e ",bz2," -oc:",oec_data))
  }
}
