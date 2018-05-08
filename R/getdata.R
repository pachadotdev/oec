#' Downloads and processes the data from the API
#' @export
#' @param origin ISO code for country of origin (e.g. \code{chl} for Chile). Run \code{countries_list} in case of doubt.
#' @param destination ISO code for country of destination (e.g. \code{chn} for China). Run \code{countries_list} in case of doubt.
#' @param year The OEC's API ranges from 1962 to 2016
#' @param classification Trade classification that can be \code{1} (HS92 4 characters since year 1995), 
#'     \code{2} (SITC rev.2 4 characters since year 1962) or 
#'     \code{3} (HS92 6 characters since year 1995). By default set to \code{1}.
#' @param write Write to user's filespace (by default set to \code{FALSE})
#' @param wrapper Argument used by \code{getdata_interval} (by default set to \code{FALSE})
#' @examples
#' \dontrun{
#' # Run `countries_list` to display the full list of countries
#' # What does Chile export to China?  year 2015 - classification HS92 6 characters
#' getdata("chl", "chn", 2015, 3)
#' }
#' 
#' @keywords functions

getdata <- function(origin, destination, year, classification, write, wrapper) {
  if (missing(classification)) { classification <- 1 }
  if (missing(write)) { write <- FALSE }
  if (missing(wrapper)) { wrapper <- FALSE }
  
  if (origin %in% countries_list$country_code & 
      destination %in% countries_list$country_code) {
    message("Valid country codes. Proceeding...")
  } else {
    stop("Invalid country codes, see `countries_list`.")
  }
  
  if (year < 1961 | year > 2016) {
    stop("The data is only available from 1962 to 2016.")
  } else {
    if ((classification == 1 | classification == 3) & year < 1995) {
      stop("HS92 classification is only available from the year 1995 and ongoing.")
    } else {
      if (classification == 1 |
          classification == 2 | 
          classification == 3) {
        if (classification == 1) {
          classification <- "hs92"
          characters <- 4
          message("Using HS92 classification (4 characters)...")
        }
        if (classification == 2) {
          classification <- "sitc"
          characters <- 4
          message("Using SITC rev.2 classification (4 characters)...")
        }
        if (classification == 3) {
          classification <- "hs92"
          characters <- 6
          message("Using HS92 classification (6 characters)")
        }
      }
      
      output <- paste(origin, destination, year, classification, characters, sep = "_")
      
      if (classification == "sitc" | classification == "hs92") {
        
        if (classification == "sitc") { sitc <- oec::sitc }
        if (classification == "hs92") { hs92 <- oec::hs92 }
        
        if (!exists(output)) {
          message("Processing data...")
          
          # product origin-destination data -----------------------------------------
          
          origin_destination_year <- fromJSON(
            sprintf(
              "https://atlas.media.mit.edu/%s/export/%s/%s/%s/show/",
              classification,
              year,
              origin,
              destination
            )
          )
          
          origin_destination_year <- as_tibble(origin_destination_year[[1]])
          
          if (nrow(origin_destination_year) == 0) { 
            stop("No data available. Try changing year or trade classification.") 
          }
          
          if (origin != "all" & destination != "all") {
            origin_destination_year <- origin_destination_year %>%
              rename(destination_id = !!sym("dest_id")) %>% 
              mutate(trade_exchange_val = 
                       !!sym("export_val") + !!sym("import_val"))
            
            if (classification == "sitc") {
              origin_destination_year <- origin_destination_year %>% 
                rename(id = !!sym("sitc_id")) %>% 
                mutate(
                  sitc = substr(!!sym("id"), 3, 6)
                )
            } else {
              if (characters == 4) {
                origin_destination_year <- origin_destination_year %>% 
                  rename(id = !!sym("hs92_id")) %>% 
                  mutate(
                    hs92 = substr(!!sym("id"), 3, 6)
                  )
              } else {
                origin_destination_year <- origin_destination_year %>% 
                  rename(id = !!sym("hs92_id")) %>% 
                  mutate(
                    hs92 = substr(!!sym("id"), 3, 8)
                  )
              }
            }
            
            origin_destination_year <- origin_destination_year %>% 
              mutate(
                origin_id = substr(!!sym("origin_id"), 3, 5),
                destination_id = substr(!!sym("destination_id"), 3, 5)
              ) %>%
              left_join(
                countries_list,
                by = c("origin_id" = "country_code")
              ) %>%
              rename(origin_name = !!sym("country")) %>%
              left_join(
                countries_list, 
                by = c("destination_id" = "country_code")
              ) %>%
              rename(destination_name = !!sym("country"))
          }
          
          if (origin != "all" & destination == "all") {
            origin_destination_year <- origin_destination_year %>%
              mutate(trade_exchange_val = 
                       !!sym("export_val") + !!sym("import_val"))
            
            if (classification == "sitc") {
              origin_destination_year <- origin_destination_year %>% 
                rename(id = !!sym("sitc_id")) %>% 
                mutate(
                  sitc = substr(!!sym("id"), 3, 6)
                )
            } else {
              if (characters == 4) {
                origin_destination_year <- origin_destination_year %>% 
                  rename(id = !!sym("hs92_id")) %>% 
                  mutate(
                    hs92 = substr(!!sym("id"), 3, 6)
                  )
              } else {
                origin_destination_year <- origin_destination_year %>% 
                  rename(id = !!sym("hs92_id")) %>% 
                  mutate(
                    hs92 = substr(!!sym("id"), 3, 8)
                  )
              }
            }
            
            origin_destination_year <- origin_destination_year %>% 
              mutate(
                origin_id = substr(!!sym("origin_id"), 3, 5),
                destination_id = "all"
              ) %>%
              left_join(
                countries_list,
                by = c("origin_id" = "country_code")
              ) %>%
              rename(origin_name = !!sym("country")) %>%
              left_join(
                countries_list, 
                by = c("destination_id" = "country_code")
              ) %>%
              rename(destination_name = !!sym("country"))
          }
          
          if (origin == "all" & destination != "all") {
            origin_destination_year <- origin_destination_year %>%
              mutate(trade_exchange_val = 
                       !!sym("export_val") + !!sym("import_val"))
            
            if (classification == "sitc") {
              origin_destination_year <- origin_destination_year %>% 
                rename(id = !!sym("sitc_id")) %>% 
                mutate(
                  sitc = substr(!!sym("id"), 3, 6)
                )
            } else {
              if (characters == 4) {
                origin_destination_year <- origin_destination_year %>% 
                  rename(id = !!sym("hs92_id")) %>% 
                  mutate(
                    hs92 = substr(!!sym("id"), 3, 6)
                  )
              } else {
                origin_destination_year <- origin_destination_year %>% 
                  rename(id = !!sym("hs92_id")) %>% 
                  mutate(
                    hs92 = substr(!!sym("id"), 3, 8)
                  )
              }
            }
            
            origin_destination_year <- origin_destination_year %>% 
              mutate(
                origin_id = "all",
                destination_id = substr(!!sym("destination_id"), 3, 5)
              ) %>%
              left_join(countries_list,
                        by = c("origin_id" = "country_code")) %>%
              rename(origin_name = !!sym("country")) %>%
              left_join(
                countries_list, 
                by = c("destination_id" = "country_code")
              ) %>%
              rename(destination_name = !!sym("country"))
          }
          
          if (origin == "all" & destination == "all") {
            origin_destination_year <- origin_destination_year %>%
              mutate(trade_exchange_val = 
                       !!sym("export_val") + !!sym("import_val"))
            
            if (classification == "sitc") {
              origin_destination_year <- origin_destination_year %>% 
                rename(id = !!sym("sitc_id")) %>% 
                mutate(
                  sitc = substr(!!sym("id"), 3, 6)
                )
            } else {
              if (characters == 4) {
                origin_destination_year <- origin_destination_year %>% 
                  rename(id = !!sym("hs92_id")) %>% 
                  mutate(
                    hs92 = substr(!!sym("id"), 3, 6)
                  )
              } else {
                origin_destination_year <- origin_destination_year %>% 
                  rename(id = !!sym("hs92_id")) %>% 
                  mutate(
                    hs92 = substr(!!sym("id"), 3, 8)
                  )
              }
            }
            
            origin_destination_year <- origin_destination_year %>% 
              mutate(
                origin_id = "all",
                destination_id = "all"
              ) %>%
              left_join(
                countries_list,
                by = c("origin_id" = "country_code")
              ) %>%
              rename(origin_name = !!sym("country")) %>%
              left_join(
                countries_list, 
                by = c("destination_id" = "country_code")
              ) %>%
              rename(destination_name = !!sym("country"))
          }
          
          # product world-world data ------------------------------------------------
          
          world_world_year <- fromJSON(
            sprintf(
              "https://atlas.media.mit.edu/%s/export/%s/all/all/show/",
              classification,
              year
            )
          )
          
          world_world_year <- as_tibble(world_world_year[[1]])
          
          world_world_year <- world_world_year %>%
            rename(
              world_total_export_val = !!sym("export_val"),
              world_total_import_val = !!sym("import_val")
            )
          
          if (classification == "sitc") {
            world_world_year <- world_world_year %>% 
              rename(id = !!sym("sitc_id")) %>% 
              mutate(
                sitc = substr(!!sym("id"), 3, 6)
              ) %>% 
              select(
                !!sym("sitc"),
                contains("pci"),
                contains("top_")
              )
          } else {
            if (characters == 4) {
              world_world_year <- world_world_year %>% 
                rename(id = !!sym("hs92_id")) %>% 
                mutate(
                  hs92 = substr(!!sym("id"), 3, 6)
                ) %>% 
                select(
                  !!sym("hs92"),
                  contains("pci"),
                  contains("top_")
                )
            } else {
              world_world_year <- world_world_year %>% 
                rename(id = !!sym("hs92_id")) %>% 
                mutate(
                  hs92 = substr(!!sym("id"), 3, 8)
                ) %>% 
                select(
                  !!sym("hs92"),
                  contains("pci"),
                  contains("top_")
                )
            }
          }
          
          # product origin-world data -----------------------------------------------
          
          origin_world_year <- fromJSON(
            sprintf(
              "https://atlas.media.mit.edu/%s/export/%s/%s/all/show/",
              classification,
              year,
              origin
            )
          )
          
          origin_world_year <- as_tibble(origin_world_year[[1]])
          
          if (classification == "sitc") {
            origin_world_year <- origin_world_year %>%
              select(!!sym("sitc_id"), contains("_rca")) %>%
              mutate(sitc = substr(!!sym("sitc_id"), 3, 6)) %>%
              select(-!!sym("sitc_id"))
            
            origin_destination_year <- origin_destination_year %>%
              left_join(origin_world_year, by = "sitc") %>%
              left_join(world_world_year, by = "sitc") %>%
              select(
                !!!syms(c(
                  "year",
                  "origin_id",
                  "destination_id",
                  "origin_name",
                  "destination_name",
                  "id",
                  "sitc"
                )),
                contains("export_"),
                contains("import_"),
                everything()
              ) %>%
              select(-!!sym("sitc_id_len"))
          } else {
            if (characters == 4) {
              origin_world_year <- origin_world_year %>%
                select(!!sym("hs92_id"), contains("_rca")) %>%
                mutate(hs92 = substr(!!sym("hs92_id"), 3, 6)) %>%
                select(-!!sym("hs92_id"))
            } else {
              origin_world_year <- origin_world_year %>%
                select(!!sym("hs92_id"), contains("_rca")) %>%
                mutate(hs92 = substr(!!sym("hs92_id"), 3, 8)) %>%
                select(-!!sym("hs92_id"))
            }
            
            origin_destination_year <- origin_destination_year %>%
              left_join(origin_world_year, by = "hs92") %>%
              left_join(world_world_year, by = "hs92") %>%
              select(
                !!!syms(c(
                  "year",
                  "origin_id",
                  "destination_id",
                  "origin_name",
                  "destination_name",
                  "id",
                  "hs92"
                )),
                contains("export_"),
                contains("import_"),
                everything()
              ) %>%
              select(-!!sym("hs92_id_len"))
          }
          
          rm(world_world_year, origin_world_year)
          
          names(countries_list) <-
            c("top_importer_code", "top_importer")
          
          origin_destination_year <- origin_destination_year %>%
            rename(top_importer_code = !!sym("top_importer")) %>% 
            mutate(top_importer_code = substr(!!sym("top_importer_code"), 3, 5)) %>%
            left_join(countries_list, by = "top_importer_code")
          
          names(countries_list) <-
            c("top_exporter_code", "top_exporter")
          
          origin_destination_year <- origin_destination_year %>%
            rename(top_exporter_code = !!sym("top_exporter")) %>% 
            mutate(top_exporter_code = substr(!!sym("top_exporter_code"), 3, 5)) %>%
            left_join(countries_list, by = "top_exporter_code")
          
          rm(countries_list)
          
          if (classification == "sitc") {
            origin_destination_year <- origin_destination_year %>%
              left_join(sitc, by = "sitc") %>%
              select(
                !!!syms(c(
                  "year",
                  "origin_id",
                  "destination_id",
                  "origin_name",
                  "destination_name",
                  "id",
                  "sitc",
                  "product_name"
                )),
                contains("product_"),
                contains("export_"),
                contains("import_"),
                everything()
              )
          } else {
            origin_destination_year <- origin_destination_year %>%
              left_join(hs92, by = "hs92") %>%
              select(
                !!!syms(c(
                  "year",
                  "origin_id",
                  "destination_id",
                  "origin_name",
                  "destination_name",
                  "id",
                  "hs92",
                  "product_name"
                )),
                contains("product_"),
                contains("export_"),
                contains("import_"),
                everything()
              )
          }
          
          if (write == TRUE) {
            message("Writing SITC rev.2 (4 characters) CSV file...")
            
            origin_destination_year %>%
              write_csv(paste0(output, ".csv")) %>%
              write_json(paste0(output, ".json"))
          }
          
          if (wrapper == FALSE) {
            envir <- as.environment(1)
            assign(
              sprintf(
                "%s_%s_%s_%s_%s",
                origin,
                destination,
                year,
                classification,
                characters
              ),
              origin_destination_year,
              envir = envir
            )
          } else {
            return(origin_destination_year)
          }
          
        } else {
          if (file.exists(paste0(output, ".json"))) {
            message("The file you want to download is in the working folder. Reading JSON...")
            if (wrapper == FALSE) {
              envir <- as.environment(1)
              assign(
                sprintf(
                  "%s_%s_%s_%s_%s",
                  origin,
                  destination,
                  year,
                  classification,
                  characters
                ),
                as_tibble(fromJSON(paste0(output, ".json"))),
                envir = envir
              )
            }
          } else {
            message("The data is in the working space.")
            return(output)
          }
        }
        
      } else {
        stop('Check classification.')
      }
    }
  }
}
