#' Downloads and processes the data from the API
#' @export
#' @param origin Country code of origin (e.g. "chl" for Chile)
#' @param destination Country code of destination (e.g. "chn" for China)
#' @param initial_year The OEC's API ranges from 1942 to 2014. This needs to be lower than `final_year`
#' @param final_year The OEC's API ranges from 1942 to 2014. This needs to be greater than `initial_year`
#' @param classification Trade classification that can be "1" (HS92 4 characters since year 1995), "2" (SITC rev.2 4 characters since year 1962) or "3" (HS92 6 characters since year 1995)
#' @param interval is an optional parameter to define the distance between years (by default set to 1)
#' @import curl data.table jsonlite plyr servr
#' @importFrom utils write.csv
#' @examples
#' # Run countries_list() to display the full list of countries
#' # Chile is "chl" and China is "chn"
#'
#' # Download trade data from OEC's API (HS92 4 characters product list)
#' # for Chile and China in the years 2010 to 2014
#' # getdata_interval("chl", "chn", 2011, 2014)
#' # is the same as
#' # getdata_interval("chl", "chn", 2011, 2014, 1, 1)
#'
#' # Download trade data from OEC's API (HS92 4 characters product list)
#' # for Chile and China in the years 2010, 2012 and 2014
#' # getdata_interval("chl", "chn", 2011, 2014, 1, 2)
#'
#' # Download trade data from OEC's API (SITC rev.2 4 characters product list)
#' # for Chile and China in the years 2010, 2012 and 2014
#' # getdata_interval("chl", "chn", 2011, 2014, 2, 2)
#'
#' # Download trade data from OEC's API (HS92 6 characters product list)
#' # for Chile and China in the years 2010, 2012 and 2014
#' # getdata_interval("chl", "chn", 2011, 2014, 3, 2)
#' @keywords functions

getdata_interval <- function(origin, destination, initial_year, final_year, classification, interval) {

  countries_list <- oec::countries_list

  if(missing(classification)) {classification = 1}

  if(missing(interval)) {interval = 1}

  if(origin %in% countries_list$country_code & destination %in% countries_list$country_code){
    print("Valid country codes...")
  } else {
    print("Error. Invalid country codes, see 'countries_list'.")
    stop()
  }

  if(initial_year > final_year) {
    print("The initial year needs to be a lower value than the last year.")
    stop()
  } else {
    years <- seq(initial_year, final_year, interval)

    if(classification == 1) {
      output <- paste(origin, destination, initial_year, final_year, interval, "4char_hs92", sep = "_")
      char <- "4char"
      code <- "hs92"
    }
    if(classification == 2) {
      output <- paste(origin, destination, initial_year, final_year, interval, "4char_sitc_rev2", sep = "_")
      char <- "4char"
      code <- "sitc_rev2"
    }
    if(classification == 3) {
      output <- paste(origin, destination, initial_year, final_year, interval, "6char_hs92", sep = "_")
      char <- "6char"
      code <- "hs92"
    }

    if(!file.exists(paste0(output,".csv")) | !file.exists(paste0(output,".json"))) {
      datalist <- list(1:length(years))
      for(t in 1:length(years)) {
        print(paste("Processing SITC rev.2 (4 characters) files for the year",years[[t]],"..."))
        envir = as.environment(1)
        getdata(origin, destination, years[[t]], classification)
        datalist[[t]] <- paste(origin, destination, years[[t]], char, code, sep = "_")
      }
      envir = as.environment(1)
      datalist <- lapply(datalist, get)
      getdata_interval_all <- do.call("rbind",datalist)
      assign(output, getdata_interval_all, envir = envir)
      write.csv(getdata_interval_all, paste0(output,".csv"))
      output_json <- toJSON(getdata_interval_all, pretty = TRUE)
      write(output_json, file=paste0(output,".json"))
    } else {
      envir = as.environment(1)
      print("The file you want to download is in the working folder. Reading JSON...")
      assign(output, fromJSON(paste0(output,".json")), envir = envir)
    }
  }
}
