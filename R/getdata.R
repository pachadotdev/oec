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
#' @param year Numeric value greater or equal to 1962 and lower of equal to 2016.
#' @param classification Any of the available trade classifications in the OEC (\code{sitc}, \code{hs92}, 
#'     \code{hs96}, \code{hs02} or \code{hs07}). Default set to \code{sitc}.
#' @param wrapper Argument used by \code{getdata_batch}. Default set to \code{FALSE}.
#' @seealso \code{getdata_batch}
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
#' # year 2015 - HS07 (4 and 6 characters)
#' getdata("chl", "chn", 2015, "hs07")
#' }
#' @keywords functions

getdata <- function(origin, destination, year, classification, wrapper) {
  # Checks ------------------------------------------------------------------
  stopifnot(all(c(origin, destination) %in% oec::country_codes$country_code))
  stopifnot(year %in% 1962:2016)
  
  if (missing(classification)) { classification <- "sitc" }
  stopifnot(classification %in% c("sitc", "hs92", "hs96", "hs02", "hs07"))
  if (classification == "sitc") { stopifnot(year >= 1962) }
  if (classification == "hs92") { stopifnot(year >= 1992) }
  if (classification == "hs96") { stopifnot(year >= 1996) }
  if (classification == "hs02") { stopifnot(year >= 2002) }
  if (classification == "hs07") { stopifnot(year >= 2007) }
  
  if (missing(wrapper)) { wrapper <- FALSE }
  
  # Valid input message -----------------------------------------------------
  message(
    sprintf("Processing %s data. The classification is %s.", year, classification)
  )
  
  # Output and package data -------------------------------------------------
  output <- paste(origin, destination, year, classification, sep = "_")
  stopifnot(is_true(!exists(output)))
  
  if (classification == "sitc") { product_codes <- oec::sitc }
  if (classification == "hs92") { product_codes <- oec::hs92 }
  if (classification == "hs96") { product_codes <- oec::hs96 }
  if (classification == "hs02") { product_codes <- oec::hs02 }
  if (classification == "hs07") { product_codes <- oec::hs07 }
  
  # Origin-destination flows ------------------------------------------------
  origin_destination <- fromJSON(
    sprintf(
      "https://atlas.media.mit.edu/%s/export/%s/%s/%s/show/",
      classification,
      year,
      origin,
      destination
    )
  )
  
  origin_destination <- as_tibble(origin_destination[[1]])
  
  # No data in API message --------------------------------------------------
  if (nrow(origin_destination) == 0) {
    stop("No data available. Try changing year or trade classification.") 
  }
  
  # compute trade balance
  origin_destination <- origin_destination %>%
    mutate(trade_exchange_val = 
             !!sym("export_val") + !!sym("import_val"))
  
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
    origin_destination <- origin_destination %>% 
      mutate(dest_id = "xxall")
  }
  
  if (origin == "all") {
    origin_destination <- origin_destination %>% 
      mutate(origin_id = "xxall")
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
  
  # remove RCAs (if applicable or this will have duplicates, 
  # not all queries return RCAs)
  origin_destination <- origin_destination %>% 
    select(-matches("rca"))
  
  # Origin-world flows ------------------------------------------------------
  origin_world <- fromJSON(
    sprintf(
      "https://atlas.media.mit.edu/%s/export/%s/%s/all/show/",
      classification,
      year,
      origin
    )
  )
  
  origin_world <- as_tibble(origin_world[[1]])
  
  # extract RCAs
  origin_world <- origin_world %>%
    rename(
      id = !!sym(sprintf("%s_id", classification)),
      id_len = !!sym(sprintf("%s_id_len", classification))
    ) %>% 
    mutate(id = str_sub(!!sym("id"), 3)) %>% 
    select(!!sym("id"), contains("_rca"))

  # World-world flows -------------------------------------------------------
  world_world <- fromJSON(
    sprintf(
      "https://atlas.media.mit.edu/%s/export/%s/all/all/show/",
      classification,
      year
    )
  )
  
  world_world <- as_tibble(world_world[[1]])
  
  world_world <- world_world %>%
    rename(
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

  rm(world_world, origin_world)
  
  names(country_codes) <-
    c("top_importer_code", "top_importer")
  
  origin_destination <- origin_destination %>%
    rename(top_importer_code = !!sym("top_importer")) %>% 
    mutate(top_importer_code = str_sub(!!sym("top_importer_code"), 3)) %>%
    left_join(country_codes, by = "top_importer_code")
  
  names(country_codes) <-
    c("top_exporter_code", "top_exporter")
  
  origin_destination <- origin_destination %>%
    rename(top_exporter_code = !!sym("top_exporter")) %>% 
    mutate(top_exporter_code = str_sub(!!sym("top_exporter_code"), 3)) %>%
    left_join(country_codes, by = "top_exporter_code")
  
  rm(country_codes)
  
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
  
  if (wrapper == FALSE) {
    envir <- as.environment(1)
    assign(
      sprintf(
        "%s_%s_%s_%s",
        origin,
        destination,
        year,
        classification
      ),
      origin_destination,
      envir = envir
    )
  } else {
    return(origin_destination)
  }
}
