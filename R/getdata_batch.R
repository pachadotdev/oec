#' Convenient wrapper for getdata
#' @description Returns a tibble containing imports, exports and relevant metrics 
#'     (e.g. exports growth w/r to last year) for a range of years as the result of different API calls 
#'     and data transforming. 
#'     The API returns data for different trade classifications: 
#'     (i) SITC (1962-2016); (ii) HS92 (1992-2016); (iii) HS96 (1996-2016); (iv) HS02 (2002-2016); 
#'     (v) HS07 (2007-2016).
#' @param origin ISO code for country of origin (e.g. \code{chl} for Chile). 
#'     Run \code{country_codes} in case of doubt.
#' @param destination ISO code for country of destination (e.g. \code{chn} for China). 
#'     Run \code{country_codes} in case of doubt.
#' @param initial_year Numeric value greater or equal to 1962 and lower of equal to 2016. 
#'     This needs to be lower or equal to \code{final_year}.
#' @param final_year Numeric value greater or equal to 1962 and lower of equal to 2016. 
#'     This needs to be greater or equal to \code{initial_year}.
#' @param classification Any of the available trade classifications in the OEC (\code{sitc}, \code{hs92}, 
#'     \code{hs96}, \code{hs02} or \code{hs07}). Default set to \code{sitc}.
#' @param by Numeric value to define the increment of the sequence of years. Default set to \code{1}.
#' @seealso \code{getdata}
#' @export
#' @examples
#' \dontrun{
#' # The next examples can take more than 5 seconds to compute, and specially for large economies so 
#' # these are just shown without evaluation according to CRAN rules
#' 
#' # Run `country_codes` to display the full table of countries
#' 
#' # What does Chile export to China?  
#' # years 2010 to 2015 - SITC (4 characters)
#' getdata_batch("chl", "chn", 2010, 2015)
#' # or with explicit parameters
#' getdata_batch("chl", "chn", 2010, 2015, "sitc", 1)
#' 
#' # What does Chile export to China?  
#' # years 2010 to 2015 - HS07 (4 and 6 characters)
#' getdata_batch("chl", "chn", 2010, 2015, "hs07")
#' 
#' # What does Chile export to China?  
#' # years 2010 and 2012 - HS07 (4 and 6 characters)
#' getdata_batch("chl", "chn", 2010, 2012, "hs07", 2)
#' }
#' @keywords functions

getdata_batch <- function(origin, destination, initial_year, final_year, classification, by) {
  # Checks ------------------------------------------------------------------
  stopifnot(all(c(origin, destination) %in% oec::country_codes$country_code))
  stopifnot(all(c(initial_year, final_year) %in% 1962:2016))
  stopifnot(initial_year < final_year)
  
  if (missing(classification)) { classification <- "sitc" }
  stopifnot(classification %in% c("sitc", "hs92", "hs96", "hs02", "hs07"))
  
  if (missing(by)) { by <- 1 }
  stopifnot(is.numeric(by))

  # Output ------------------------------------------------------------------
  output <- paste(
    origin, destination, initial_year, final_year, by, classification, sep = "_"
  )
  stopifnot(is_true(!exists(output)))
  
  # Pass arguments to getdata -----------------------------------------------
  years <- seq(initial_year, final_year, by)
  
  data <- list(1)
  for (t in 1:length(years)) {
    data[[t]] <- 
      getdata(origin, destination, years[t], classification, wrapper = TRUE)
  }
  
  data <- map_df(data, .f = bind_rows)
  envir <- as.environment(1)
  assign(output, data, envir = envir)
}
