library(jsonlite)
library(dplyr)
library(data.table)

# Obtain from DB

url = "http://atlas.media.mit.edu/static/db/raw/country_names.tsv.bz2"
bz2 = "country_names_db.tsv.bz2"

if(!file.exists(bz2)) {
  download.file(url, bz2, method = "wget")
}

if(!file.exists("country_names_db.tsv")){
  system(paste0("7z e ",bz2," -oc:",getwd()))
}

countries_db = as_tibble(fread("country_names_db.tsv", sep = "\t")) %>% 
  rename(country = name, country_code = id_3char) %>% 
  select(country,country_code)

# Obtain from API

countries_api = fromJSON("http://atlas.media.mit.edu/attr/country/")

countries_api = as_tibble(countries_api[[1]]) %>% 
  select(name,display_id,id) %>% 
  mutate(display_length = nchar(id)) %>% 
  filter(display_length == 5) %>% 
  rename(country = name, country_code = display_id) %>% 
  select(country,country_code) %>% 
  filter(!is.na(country_code))

# Check both sources match

countries_api %>% 
  anti_join(countries_db)

countries_db %>% 
  anti_join(countries_api)

###

# Add "The World"

countries_list = countries_api %>% 
  rbind(c("all","the World")) %>% 
  arrange(country)

save(countries_list, file = "countries_list.RData")
