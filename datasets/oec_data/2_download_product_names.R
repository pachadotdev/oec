# Open the project datasets.Rproj three levels up to run this script

#1: download product names (SITC rev 2, HS92/96/02/07)

oec_data = "oec_data"
try(dir.create(oec_data))

download_product_names = function() {
  if(classification == "sitc") {
    url = "http://atlas.media.mit.edu/static/db/raw/products_sitc_rev2.tsv.bz2"
    bz2 = "oec_data/sitc_rev2_product_names.tsv.bz2"
    tsv = "oec_data/sitc_rev2_product_names.tsv"
  }
  
  if(classification == "hs92") {
    url = "http://atlas.media.mit.edu/static/db/raw/products_hs_92.tsv.bz2"
    bz2 = "oec_data/hs_rev1992_product_names.tsv.bz2"
    tsv = "oec_data/hs_rev1992_product_names.tsv"
  }
  
  if(classification == "hs96") {
    url = "http://atlas.media.mit.edu/static/db/raw/products_hs_96.tsv.bz2"
    bz2 = "oec_data/hs_rev1996_product_names.tsv.bz2"
    tsv = "oec_data/hs_rev1996_product_names.tsv"
  }
  
  if(classification == "hs02") {
    url = "http://atlas.media.mit.edu/static/db/raw/products_hs_02.tsv.bz2"
    bz2 = "oec_data/hs_rev2002_product_names.tsv.bz2"
    tsv = "oec_data/hs_rev2002_product_names.tsv"
  }
  
  if(classification == "hs07") {
    url = "http://atlas.media.mit.edu/static/db/raw/products_hs_07.tsv.bz2"
    bz2 = "oec_data/hs_rev2007_product_names.tsv.bz2"
    tsv = "oec_data/hs_rev2007_product_names.tsv"
  }
  
  if(!file.exists(bz2)) {
    download.file(url, bz2, method = "wget")
  }
  
  if(!file.exists(tsv)){
    system(paste0("7z e ",bz2," -oc:",oec_data))
  }
}
