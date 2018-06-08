#' Downloads and processes the data from the API
#' @description Returns a tibble containing imports, exports and relevant metrics 
#'     (e.g. exports growth w/r to last year) for a  year as the result of different API calls 
#'     and data transforming. 
#'     The API returns data for different trade classifications: 
#'     (i) SITC (1962-2016); (ii) HS92 (1992-2016); (iii) HS96 (1996-2016); (iv) HS02 (2002-2016); 
#'     (v) HS07 (2007-2016).
#' @param origin ISO code for country of origin (e.g. \code{chl} for Chile). 
#' Run \code{country_codes} in case of doubt.
#' @param destination ISO code for country of destination (e.g. \code{chn} for China). 
#' Run \code{country_codes} in case of doubt.
#' @param years Numeric value greater or equal to 1962 and lower of equal to 2016.
#' @param classification Any of the available trade classifications in the OEC (\code{sitc}, \code{hs92}, 
#'     \code{hs96}, \code{hs02} or \code{hs07}). Default set to \code{sitc}.
#' @seealso \code{getdata_batch}
#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble select filter mutate contains 
#'     everything left_join bind_rows rename matches
#' @importFrom stringr str_sub str_length
#' @importFrom curl curl new_handle handle_setheaders has_internet
#' @importFrom jsonlite fromJSON
#' @importFrom rlang sym syms is_true
#' @importFrom purrr map_df flatten_df
#' @export
#' @examples
#' \dontrun{
#' # The next examples can take more than 5 seconds to compute, and specially for large economies so 
#' # these are just shown without evaluation according to CRAN rules
#' 
#' # Run `country_codes` to display the full table of countries
#' 
#' # What does Chile export to China?  
#' # year 2015 - SITC (4 characters)
#' getdata("chl", "chn", 2015)
#' # or with explicit parameter
#' getdata("chl", "chn", 2015, "sitc")
#' 
#' # What does Chile export to China?  
#' # years 2010, 2011 and 2015 - HS07 (4 and 6 characters)
#' getdata("chl", "chn", c(2010, 2011, 2015), "hs07")
#' }
#' @keywords functions

getdata <- function(origin, destination, years, classification) {
  # Checks ------------------------------------------------------------------
  if(has_internet() != TRUE) 
    stop("No internet connection.")
  
  if(all(c(origin, destination) %in% oec::country_codes$country_code) != TRUE) 
    stop("Please verify that you wrote the origin and destination countries correctly.")
  
  if(all(years %in% 1962:2016) != TRUE) 
    stop("Please verify that you are requesting data contained within the years 1962-2016.")
  
  if (missing(classification)) { classification <- "sitc" }
  
  if(classification %in% c("sitc", "hs92", "hs96", "hs02", "hs07") != TRUE)
    stop("Please verify that you wrote a valid trade classification.")
  
  if (classification == "sitc") { 
    if(all(years >= 1962) != TRUE)
      stop("Provided that you requested SITC data please verify that the data you are requesting is contained within the years 1962-2016.")
  }
  
  if (classification == "hs92") { 
    if(all(years >= 1992) != TRUE)
      stop("Provided that you requested HS92 data please verify that the data you are requesting is contained within the years 1992-2016.")
  }
  
  if (classification == "hs96") { 
    if(all(years >= 1996) != TRUE)
      stop("Provided that you requested HS96 data please verify that the data you are requesting is contained within the years 1996-2016.")
  }
  
  if (classification == "hs02") { 
    if(all(years >= 2002) != TRUE)
      stop("Provided that you requested HS02 data please verify that the data you are requesting is contained within the years 2002-2016.")
  }
  
  if (classification == "hs07") { 
    if(all(years >= 2007) != TRUE)
      stop("Provided that you requested HS07 data please verify that the data you are requesting is contained within the years 2007-2016.")
  }
  
  # Valid input message -----------------------------------------------------
  message(
    sprintf("Valid input. Processing %s data...", years)
  )
  
  # Output and package data -------------------------------------------------
  output <- paste(origin, destination, years, classification, sep = "_")
  
  if (classification == "sitc") { product_codes <- oec::sitc }
  if (classification == "hs92") { product_codes <- oec::hs92 }
  if (classification == "hs96") { product_codes <- oec::hs96 }
  if (classification == "hs02") { product_codes <- oec::hs02 }
  if (classification == "hs07") { product_codes <- oec::hs07 }
  
  # Origin-destination flows ------------------------------------------------
  read_from_api_od <- function(t) {
    url <- sprintf(
      "https://atlas.media.mit.edu/%s/export/%s/%s/%s/show/",
      classification,
      years[t],
      origin,
      destination
    )
    
    data <- flatten_df(fromJSON(url))

    return(data)
  }
  
  origin_destination <- map_df(1:length(years), read_from_api_od)

  # No data in API message --------------------------------------------------
  if (nrow(origin_destination) == 0) {
    stop("No data available. Try changing years or trade classification.") 
  }
  
  # compute trade balance
  origin_destination <- mutate(origin_destination, 
    trade_exchange_val = !!sym("export_val") + !!sym("import_val")
  )
  
  # convert ids to standard hs/sitc
  origin_destination <- origin_destination %>% 
    rename(
      id = !!sym(sprintf("%s_id", classification)),
      id_len = !!sym(sprintf("%s_id_len", classification))
    ) %>% 
    mutate(
      id = str_sub(!!sym("id"), 3),
      id_len = str_length(!!sym("id"))
    )
  
  # convert country codes to standard iso3
  if (destination == "all") {
    origin_destination <- mutate(origin_destination, dest_id = "xxall")
  }
  
  if (origin == "all") {
    origin_destination <- mutate(origin_destination, origin_id = "xxall")
  }
  
  # include countries (official names)
  origin_destination <- origin_destination %>% 
    rename(destination_id = !!sym("dest_id")) %>% 
    mutate(
      origin_id = str_sub(!!sym("origin_id"), 3),
      destination_id = str_sub(!!sym("destination_id"), 3)
    ) %>%
    left_join(country_codes, by = c("origin_id" = "country_code")) %>%
    rename(origin_name = !!sym("country")) %>%
    left_join(country_codes, by = c("destination_id" = "country_code")) %>%
    rename(destination_name = !!sym("country"))
  
  # remove RCAs (if applicable, or this will have duplicates, 
  # not all queries return RCAs)
  origin_destination <- select(origin_destination, -matches("rca"))
  
  # Origin-world flows ------------------------------------------------------
  read_from_api_ow <- function(t) {
    url <- sprintf(
      "https://atlas.media.mit.edu/%s/export/%s/%s/%s/show/",
      classification,
      years[t],
      origin,
      "all"
    )
    
    data <- flatten_df(fromJSON(url))
    
    return(data)
  }
  
  origin_world <- map_df(1:length(years), read_from_api_ow)
  
  # extract RCAs
  origin_world <- origin_world %>%
    rename(
      id = !!sym(sprintf("%s_id", classification)),
      id_len = !!sym(sprintf("%s_id_len", classification))
    ) %>% 
    mutate(id = str_sub(!!sym("id"), 3)) %>% 
    select(!!sym("id"), contains("_rca"))
  
  # World-world flows -------------------------------------------------------
  read_from_api_ww <- function(t) {
    url <- sprintf(
      "https://atlas.media.mit.edu/%s/export/%s/%s/%s/show/",
      classification,
      years[t],
      "all",
      "all"
    )
    
    data <- flatten_df(fromJSON(url))
    
    return(data)
  }
  
  world_world <- map_df(1:length(years), read_from_api_ww)
  
  world_world <- rename(world_world,
    world_total_export_val = !!sym("export_val"),
    world_total_import_val = !!sym("import_val")
  )
  
  # extract ECI and ranks
  world_world <- world_world %>% 
    rename(
      id = !!sym(sprintf("%s_id", classification)),
      id_len = !!sym(sprintf("%s_id_len", classification))
    ) %>% 
    mutate(id = str_sub(!!sym("id"), 3)) %>% 
    select(
      !!sym("id"),
      contains("pci"),
      contains("top_")
    )
  
  # Join trade flows --------------------------------------------------------
  origin_destination <- origin_destination %>%
    left_join(origin_world, by = "id") %>%
    left_join(world_world, by = "id") %>%
    select(
      !!!syms(c(
        "year",
        "origin_id",
        "destination_id",
        "origin_name",
        "destination_name",
        "id",
        "id_len"
      )),
      contains("export_"),
      contains("import_"),
      everything()
    )
  
  names(country_codes) <- c("top_importer_code", "top_importer")
  
  origin_destination <- origin_destination %>%
    rename(top_importer_code = !!sym("top_importer")) %>% 
    mutate(top_importer_code = str_sub(!!sym("top_importer_code"), 3)) %>%
    left_join(country_codes, by = "top_importer_code")
  
  names(country_codes) <- c("top_exporter_code", "top_exporter")
  
  origin_destination <- origin_destination %>%
    rename(top_exporter_code = !!sym("top_exporter")) %>% 
    mutate(top_exporter_code = str_sub(!!sym("top_exporter_code"), 3)) %>%
    left_join(country_codes, by = "top_exporter_code")
  
  origin_destination <- origin_destination %>%
    left_join(product_codes, by = "id") %>%
    select(
      !!!syms(c(
        "year",
        "origin_id",
        "destination_id",
        "origin_name",
        "destination_name",
        "id",
        "id_len",
        "product_name",
        "group_id",
        "group_name"
      )),
      contains("product_"),
      contains("export_"),
      contains("import_"),
      everything()
    )
  
  return(origin_destination)
}
