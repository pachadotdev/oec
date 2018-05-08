#' Convenient wrapper for getdata
#' @export
#' @param origin ISO code for country of origin (e.g. \code{chl} for Chile). Run \code{countries_list} in case of doubt.
#' @param destination ISO code for country of destination (e.g. \code{chn} for China). Run \code{countries_list} in case of doubt.
#' @param initial_year The OEC's API ranges from 1962 to 2016. This needs to be lower than `final_year`.
#' @param final_year The OEC's API ranges from 1962 to 2016. This needs to be greater than `initial_year`.
#' @param classification Trade classification that can be \code{1} (HS92 4 characters since year 1995), 
#'     \code{2} (SITC rev.2 4 characters since year 1962) or 
#'     \code{3} (HS92 6 characters since year 1995). By default set to \code{1}.
#' @param interval is an optional parameter to define the distance between years (by default set to 1)
#' @param write Write to user's filespace (by default set to \code{FALSE})
#' @examples
#' \dontrun{
#' # Run `countries_list` to display the full list of countries
#' # What does Chile export to Peru? years 2000 to 2015 - classification HS92 6 characters
#' getdata_interval("chl", "chn", 2000, 2015, 3)
#' }
#' 
#' @keywords functions

getdata_interval <- function(origin, destination, initial_year, final_year, classification, interval, write) {
  if (missing(classification)) { classification <- 1 }
  if (missing(interval)) { interval <- 1 }
  if (missing(write)) { write <- FALSE }
  
  countries_list <- oec::countries_list
  
  if (origin %in% countries_list$country_code & 
      destination %in% countries_list$country_code) {
    message("Valid country codes...")
  } else {
    message("Error. Invalid country codes, see 'countries_list'.")
    stop()
  }
  
  if (initial_year > final_year) {
    message("The initial year needs to be a lower value than the last year.")
    stop()
  } else {
    years <- seq(initial_year, final_year, interval)
    
    if (classification == 1) {
      classification <- "hs92"
      characters <- 4
    }
    if (classification == 2) {
      classification <- "sitc"
      characters <- 4
    }
    if (classification == 3) {
      classification <- "hs92"
      characters <- 6
    }
    
    output <- paste(
      origin, destination, initial_year, final_year, interval, classification, characters,
      sep = "_"
    )
    
    if (!exists(output)) {
      getdata_interval_all <- list(1)
      for (t in 1:length(years)) {
        message(paste("Year", years[[t]], "..."))
        if (classification == "hs92" &
            characters == 4) {
          classification <- 1
        }
        if (classification == "sitc" &
            characters == 4) {
          classification <- 2
        }
        if (classification == "hs92" &
            characters == 6) {
          classification <- 3
        }
        
        getdata_interval_all[[t]] <- getdata(origin, destination, years[[t]], classification, 
                                 write = FALSE, wrapper = TRUE)
      }
      
      getdata_interval_all <- map_df(getdata_interval_all, .f = bind_rows)
      envir <- as.environment(1)
      assign(output, getdata_interval_all, envir = envir)
      
      if (write == TRUE) {
        write_csv(getdata_interval_all, paste0(output, ".csv"))
        write_json(getdata_interval_all, paste0(output, ".json"))
      }
    } else {
      if (file.exists(paste0(output, ".json"))) {
        message("The file you want to download is in the working folder. Reading JSON...")
        envir <- as.environment(1)
        assign(output, fromJSON(paste0(output, ".json")), envir = envir)
      } else {
        message("The data is in the working space.")
      }
    }
  }
}
