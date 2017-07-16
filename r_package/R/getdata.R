#' Downloads and processes the data from the API
#' @export
#' @param origin Country code of origin (e.g. "chl" for Chile)
#' @param dest Country code of destination (e.g. "chn" for China)
#' @param classification Trade classification that can be "1" (HS92 4 characters since year 1995), "2" (SITC rev.2 4 characters since year 1962) or "3" (HS92 6 characters since year 1995)
#' @param year The OEC's API ranges from 1962 to 2015
#' @import dplyr curl
#' @importFrom readr write_csv
#' @importFrom jsonlite fromJSON write_json
#' @importFrom servr httw
#' @examples
#' # Run countries_list() to display the full list of countries
#' # For the example Chile is "chl" and China is "chn"
#'
#' # Download trade between Chile and China
#' # Year 2015 (HS92 4 characters)
#' # getdata("chl", "chn", 2014)
#' # getdata("chl", "chn", 2014, 1) # equivalent to last command
#'
#' # Download trade between Chile and China
#' # Year 2015 (SITC rev2 4 characters)
#' # getdata("chl", "chn", 2015, 2)
#'
#' # Download trade between Chile and China
#' # Year 2015 (HS92 6 characters)
#' # getdata("chl", "chn", 2015, 3)
#' @keywords functions

globalVariables(c("export_val","import_val","sitc_id","origin_id","dest_id","country","sitc",
                  "group_id","product_name","hs92_id_len","hs92_id","origin_name","dest_name",
                  "origin_total_export_val","world_total_export_val","rca","sitc_id_len","top_importer","top_exporter"))

getdata = function(origin, dest, year, classification) {
  
  if(missing(classification)) {classification = 1}
  
  if(origin %in% countries_list$country_code & dest %in% countries_list$country_code){
    print("Valid country codes. Proceeding...")
  } else {
    print("Error. Invalid country codes, see 'countries_list'.")
    stop()
  }
  
  if(year < 1961 | year > 2015) {
    print("The data is only available from 1962 to 2015.")
    stop()
  } else {
    if((classification == 1 | classification == 3) & year < 1995) {
      print("HS92 classification is only available from the year 1995 and ongoing.")
      stop()
    } else {
      if(classification == 1 | classification == 2 | classification == 3){
        if(classification == 1){
          classification = "hs92"
          characters = 4
          print("Using HS92 classification (4 characters)...")
        }
        if(classification == 2){
          classification = "sitc"
          characters = 4
          print("Using SITC rev.2 classification (4 characters)...")
        }
        if(classification == 3) {
          classification = "hs92"
          characters = 6
          print("Using HS92 classification (6 characters)")
        }
      }
      
      output = paste(origin, dest, year, classification, characters, sep = "_")
      
      if(classification == "sitc" | classification == "hs92") {
        if(classification == "sitc") {
          if(!file.exists(paste0(output,".csv")) | !file.exists(paste0(output,".json"))) {
            print(paste0("Processing SITC rev.2 (",characters," characters) files..."))
            
            origin_dest_year = fromJSON(paste("http://atlas.media.mit.edu/sitc/export", year, origin, dest, "show/", sep = "/"))
            origin_dest_year = as_tibble(origin_dest_year[[1]])
            
            origin_dest_year = origin_dest_year %>%
              mutate(trade_exchange_val = export_val + import_val) %>%
              rename(id = sitc_id) %>%
              mutate(sitc = substr(id,3,6)) %>%
              mutate(origin_id = substr(origin_id,3,5),
                     dest_id = substr(dest_id,3,5)) %>%
              left_join(countries_list, by = c("origin_id" = "country_code")) %>%
              rename(origin_name = country) %>%
              left_join(countries_list, by = c("dest_id" = "country_code")) %>%
              rename(dest_name = country)
            
            world_world_year = fromJSON(paste("http://atlas.media.mit.edu/sitc/export", year, "all/all/show/", sep = "/"))
            world_world_year = as_tibble(world_world_year[[1]])
            
            world_world_year = world_world_year %>%
              rename(id = sitc_id) %>%
              rename(world_total_export_val = export_val,
                     world_total_import_val = import_val) %>%
              mutate(sitc = substr(id,3,6)) %>%
              select(sitc,contains("world_total_"),contains("pci"),contains("top_"))
            
            origin_world_year = fromJSON(paste("http://atlas.media.mit.edu/sitc/export", year, origin, "all/show/", sep = "/"))
            origin_world_year = as_tibble(origin_world_year[[1]])
            
            origin_world_year = origin_world_year %>%
              select(export_val,sitc_id) %>%
              rename(origin_total_export_val = export_val) %>%
              rename(id = sitc_id) %>%
              mutate(sitc = substr(id,3,6)) %>%
              select(-id)
            
            origin_dest_year = origin_dest_year %>%
              left_join(world_world_year, by = "sitc") %>%
              left_join(origin_world_year, by = "sitc") %>%
              mutate(rca = (origin_total_export_val/sum(origin_total_export_val, na.rm=TRUE))/(world_total_export_val/sum(world_total_export_val, na.rm=TRUE)),
                     rca = round(rca,3)) %>%
              select(year,origin_id,dest_id,id,sitc,contains("export_"),contains("import_"),everything()) %>%
              select(-sitc_id_len)
            
            rm(world_world_year,origin_world_year)
            
            names(countries_list) = c("top_importer","top_importer_code")
            
            origin_dest_year = origin_dest_year %>%
              mutate(top_importer_code = substr(top_importer, 3, 5)) %>%
              select(-top_importer) %>%
              left_join(countries_list, by="top_importer_code")
            
            names(countries_list) = c("top_exporter","top_exporter_code")
            
            origin_dest_year = origin_dest_year %>%
              mutate(top_exporter_code = substr(top_exporter, 3, 5)) %>%
              select(-top_exporter) %>%
              left_join(countries_list, by="top_exporter_code")
            
            rm(countries_list)
            
            origin_dest_year = origin_dest_year %>%
              left_join(sitc, by = "sitc") %>%
              mutate(icon = paste0("d3plus-1.9.8/icons/sitc/sitc_", group_id, ".png")) %>%
              select(year,origin_id,dest_id,product_name,id,sitc,contains("product_"),contains("export_"),contains("import_"),everything())
            
            print("Writing SITC rev.2 (4 characters) CSV file...")
            
            origin_dest_year %>%
              write_csv(paste0(output,".csv")) %>%
              write_json(paste0(output,".json"))
            
            envir = as.environment(1)
            assign(paste(origin, dest, year, classification, characters, sep = "_"), origin_dest_year, envir = envir)
            
          } else {
            envir = as.environment(1)
            print("The file you want to download is in the working folder. Reading JSON...")
            assign(paste(origin, dest, year, classification, characters, sep = "_"), as_tibble(fromJSON(paste0(output,".json"))), envir = envir)
          }
        }
        
        if(classification == "hs92") {
          if(characters == 4 | characters == 6) {
            if(!file.exists(paste0(output,".csv")) | !file.exists(paste0(output,".json"))) {
              print(paste0("Processing HS92 (",characters," characters) files..."))
              
              origin_dest_year = fromJSON(paste("http://atlas.media.mit.edu/hs92/export", year, origin, dest, "show/", sep = "/"))
              origin_dest_year = as_tibble(origin_dest_year[[1]])
              
              origin_dest_year = origin_dest_year %>%
                filter(hs92_id_len == characters + 2) %>%
                mutate(trade_exchange_val = export_val + import_val) %>%
                rename(id = hs92_id) %>%
                mutate(hs92 = substr(id,3,characters + 2)) %>%
                mutate(origin_id = substr(origin_id,3,5),
                       dest_id = substr(dest_id,3,5)) %>%
                left_join(countries_list, by = c("origin_id" = "country_code")) %>%
                rename(origin_name = country) %>%
                left_join(countries_list, by = c("dest_id" = "country_code")) %>%
                rename(dest_name = country)
              
              world_world_year = fromJSON(paste("http://atlas.media.mit.edu/hs92/export", year, "all/all/show/", sep = "/"))
              world_world_year = as_tibble(world_world_year[[1]])
              
              world_world_year = world_world_year %>%
                filter(hs92_id_len == characters + 2) %>%
                rename(id = hs92_id) %>%
                rename(world_total_export_val = export_val,
                       world_total_import_val = import_val) %>%
                mutate(hs92 = substr(id,3,characters + 2)) %>%
                select(hs92,contains("world_total_"),contains("pci"),contains("top_"))
              
              origin_world_year = fromJSON(paste("http://atlas.media.mit.edu/hs92/export", year, origin, "all/show/", sep = "/"))
              origin_world_year = as_tibble(origin_world_year[[1]])
              
              origin_world_year = origin_world_year %>%
                filter(hs92_id_len == characters + 2) %>%
                select(export_val,hs92_id) %>%
                rename(origin_total_export_val = export_val) %>%
                rename(id = hs92_id) %>%
                mutate(hs92 = substr(id,3,characters + 2)) %>%
                select(-id)
              
              origin_dest_year = origin_dest_year %>%
                left_join(world_world_year, by = "hs92") %>%
                left_join(origin_world_year, by = "hs92") %>%
                mutate(rca = (origin_total_export_val/sum(origin_total_export_val, na.rm=TRUE))/(world_total_export_val/sum(world_total_export_val, na.rm=TRUE)),
                       rca = round(rca,3)) %>%
                select(year,origin_name,dest_name,origin_id,dest_id,id,hs92,contains("export_"),contains("import_"),everything()) %>%
                select(-hs92_id_len)
              
              rm(world_world_year,origin_world_year)
              
              names(countries_list) = c("top_importer","top_importer_code")
              
              origin_dest_year = origin_dest_year %>%
                mutate(top_importer_code = substr(top_importer, 3, 5)) %>%
                select(-top_importer) %>%
                left_join(countries_list, by="top_importer_code")
              
              names(countries_list) = c("top_exporter","top_exporter_code")
              
              origin_dest_year = origin_dest_year %>%
                mutate(top_exporter_code = substr(top_exporter, 3, 5)) %>%
                select(-top_exporter) %>%
                left_join(countries_list, by="top_exporter_code")
              
              rm(countries_list)
              
              hs92 = hs92 %>%
                filter(nchar(hs92) == characters)
              
              origin_dest_year = origin_dest_year %>%
                left_join(hs92, by = "hs92") %>%
                mutate(icon = paste0("d3plus-1.9.8/icons/hs/hs_", group_id, ".png")) %>%
                select(year,origin_id,dest_id,product_name,id,hs92,contains("product_"),contains("export_"),contains("import_"),everything())
              
              rm(hs92)
              
              print("Writing SITC rev.2 (4 characters) CSV and JSON files...")
              
              origin_dest_year %>%
                write_csv(paste0(output,".csv")) %>%
                write_json(paste0(output,".json"))
              
              envir = as.environment(1)
              assign(paste(origin, dest, year, classification, characters, sep = "_"), origin_dest_year, envir = envir)
              
            } else {
              envir = as.environment(1)
              print("The file you want to download is in the working folder. Reading JSON...")
              assign(paste(origin, dest, year, classification, characters, sep = "_"), as_tibble(fromJSON(paste0(output,".json"))), envir = envir)
            }
          }
        }
      } else {
        print('Error. The worldowed classifications can be "1" (HS92 4 characters) or "3" (HS92 6 characters) for the year 1995 and going or "2" (SITC rev.2 4 characters) for the year 1962 and ongoing.')
        stop()
      }
    }
  }
}
