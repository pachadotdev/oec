#' Downloads and processes the data from the API
#' @export
#' @param origin Country code of origin (e.g. "chl" for Chile)
#' @param dest Country code of destination (e.g. "chn" for China)
#' @param initial_year The OEC's API ranges from 1942 to 2015. This needs to be lower than `final_year`
#' @param final_year The OEC's API ranges from 1942 to 2015. This needs to be greater than `initial_year`
#' @param classification Trade classification that can be "1" (HS92 4 characters since year 1995), "2" (SITC rev.2 4 characters since year 1962) or "3" (HS92 6 characters since year 1995)
#' @param interval is an optional parameter to define the distance between years (by default set to 1)
#' @examples
#' # Run countries_list() to display the full list of countries
#' # For the example Chile is "chl" and China is "chn"
#' # Download trade between Chile and China
#' # Years 2010-2015 (HS92 4 characters)
#' # getdata_interval("chl", "chn", 2010, 2015)
#' # getdata_interval("chl", "chn", 2010, 2015, 1, 1) # equivalent to last command
#'
#' # Download trade between Chile and China
#' # Years 2010, 2012 and 2014 from OEC's API (HS92 4 characters)
#' # getdata_interval("chl", "chn", 2010, 2015, 1, 2)
#'
#' # Download trade between Chile and China
#' # Years 2010, 2012 and 2014 from OEC's API (SITC rev2 4 characters)
#' # getdata_interval("chl", "chn", 2010, 2014, 2, 2)
#'
#' # Download trade between Chile and China
#' # Years 2010, 2012 and 2014 from OEC's API (HS92 6 characters)
#' # getdata_interval("chl", "chn", 2010, 2014, 3, 2)
#' @keywords functions

getdata_interval = function(origin, dest, initial_year, final_year, classification, interval) {

  if(missing(classification)) {classification = 1}

  if(missing(interval)) {interval = 1}

  if(origin %in% countries_list$country_code & dest %in% countries_list$country_code){
    print("Valid country codes...")
  } else {
    print("Error. Invalid country codes, see 'countries_list'.")
    stop()
  }

  if(initial_year > final_year) {
    print("The initial year needs to be a lower value than the last year.")
    stop()
  } else {
    years = seq(initial_year, final_year, interval)

    if(classification == 1) {classification = "hs92"; characters = 4}
    if(classification == 2) {classification = "sitc"; characters = 4}
    if(classification == 3) {classification = "hs92"; characters = 6}

    output = paste(origin, dest, initial_year, final_year, interval, classification, characters, sep = "_")

    if(!file.exists(paste0(output,".csv")) | !file.exists(paste0(output,".json"))) {
      datalist = list(1:length(years))
      for(t in 1:length(years)) {
        print(paste("Processing SITC rev.2 (4 characters) files for the year",years[[t]],"..."))
        envir = as.environment(1)

        if(classification == "hs92" & characters == 4) {classification = 1}
        if(classification == "sitc" & characters == 4) {classification = 2}
        if(classification == "hs92" & characters == 6) {classification = 3}

        getdata(origin, dest, years[[t]], classification)

        if(classification == 1) {classification = "hs92"; characters = 4}
        if(classification == 2) {classification = "sitc"; characters = 4}
        if(classification == 3) {classification = "hs92"; characters = 6}
        datalist[[t]] = paste(origin, dest, years[[t]], classification, characters, sep = "_")
      }

      envir = as.environment(1)
      datalist = lapply(datalist, get)
      getdata_interval_all = bind_rows(datalist)
      assign(output, getdata_interval_all, envir = envir)
      write_csv(getdata_interval_all, paste0(output,".csv"))
      write_json(getdata_interval_all, paste0(output,".json"))
    } else {
      envir = as.environment(1)
      print("The file you want to download is in the working folder. Reading JSON...")
      assign(output, fromJSON(paste0(output,".json")), envir = envir)
    }
  }
}
