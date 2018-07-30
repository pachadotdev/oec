#' Downloads and processes the data from the API
#' @description This function accesses \code{atlas.media.mit.edu}and perfoms different API 
#' calls to return tidy data. and data transforming.
#' @param origin ISO code for country of origin (e.g. \code{chl} for Chile). 
#' Default set to \code{all}.
#' Run \code{country_codes} in case of doubt.
#' @param destination ISO code for country of destination (e.g. \code{chn} for China). 
#' Default set to \code{all}.
#' Run \code{country_codes} in case of doubt.
#' @param years Numeric value greater or equal to 1962 and lower of equal to 2016. 
#' Default set to 2000.
#' @param classification Any of the available trade classifications in the OEC (\code{sitc}, \code{hs92},
#' \code{hs96}, \code{hs02} or \code{hs07}). Default set to \code{sitc}.
#' @return A tibble that describes bilateral trade metrics (imports, exports, trade balance
#' and relevant metrics
#' such as exports growth w/r to last year) between an \code{origin} and \code{destination}
#' country.
#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble select filter mutate contains
#' everything left_join bind_rows rename matches
#' @importFrom stringr str_sub str_length
#' @importFrom jsonlite fromJSON
#' @importFrom curl curl has_internet handle_setheaders new_handle
#' @importFrom httr http_error GET status_code
#' @importFrom rlang sym syms
#' @importFrom purrr flatten_df map_df
#' @export
#' @examples
#' \dontrun{
#' # The next examples can take more than 5 seconds to compute, and specially for large 
#' economies so these are just shown without evaluation according to CRAN rules
#'
#' # Run `country_codes` to display the full table of countries
#'
#' # What does Chile export to China?
#' # year 2015 - SITC (4 characters)
#' get_data("chl", "chn", 2015)
#' # or with explicit parameter
#' get_data("chl", "chn", 2015, "sitc")
#'
#' # What does Chile export to China?
#' # years 2010, 2011 and 2015 - HS07 (4 and 6 characters)
#' get_data("chl", "chn", c(2010, 2011, 2015), "hs07")
#' }
#' @keywords functions

get_data <- function(origin = "all", destination = "all", years = 2000, classification = "sitc") {
  # Check connections -------------------------------------------------------
  if (has_internet() != TRUE) {
    stop("No internet connection. Please retry again later.")
  }

  if (http_error("https://atlas.media.mit.edu/attr/country/") != FALSE) {
    stop("There is a server problem. Please retry again later.")
  }

  # Check classification and years ------------------------------------------
  match.arg(classification, c("sitc", "hs92", "hs96", "hs02", "hs07"))

  if (all(years %in% 1962:2016) != TRUE) {
    stop("Please verify that you are requesting data
         contained within the years 1962-2016.")
  }

  if (classification == "sitc" & all(years >= 1962) != TRUE) {
    stop("Provided that you requested SITC data please
         verify that the data you are requesting is
         contained within the years 1962-2016.")
  }

  if (classification == "hs92" & all(years >= 1992) != TRUE) {
    stop("Provided that you requested HS92 data please
         verify that the data you are requesting is
         contained within the years 1992-2016.")
  }

  if (classification == "hs96" & all(years >= 1996) != TRUE) {
    stop("Provided that you requested HS96 data please
         verify that the data you are requesting is
         contained within the years 1996-2016.")
  }

  if (classification == "hs02" & all(years >= 2002) != TRUE) {
    stop("Provided that you requested HS02 data please
         verify that the data you are requesting is
         contained within the years 2002-2016.")
  }

  if (classification == "hs07" & all(years >= 2007) != TRUE) {
    stop("Provided that you requested HS07 data please
         verify that the data you are requesting is
         contained within the years 2007-2016.")
  }

  # Package data ------------------------------------------------------------
  product_codes <- switch(classification,
    "hs92" = oec::hs92,
    "hs96" = oec::hs96,
    "hs02" = oec::hs02,
    "hs07" = oec::hs07,
    "sitc" = oec::sitc
  )

  country_codes <- oec::country_codes

  # Check origin and destination --------------------------------------------
  if (!origin %in% country_codes$country_code) {
    origin <- get_countrycode(origin)
  }

  if (!destination %in% country_codes$country_code) {
    destination <- get_countrycode(destination)
  }

  match.arg(origin, country_codes$country_code)
  match.arg(destination, country_codes$country_code)

  # Valid input message -----------------------------------------------------
  message(
    sprintf("\nProcessing %s data...", years)
  )

  # Read from API -----------------------------------------------------------
  read_from_api <- function(t, flow, attempts_left = 5) {
    stopifnot(attempts_left > 0)

    url <- switch(flow,
      "origin-destination" = sprintf(
        "https://atlas.media.mit.edu/%s/export/%s/%s/%s/show/",
        classification,
        years[t],
        origin,
        destination
      ),
      "origin-world" = sprintf(
        "https://atlas.media.mit.edu/%s/export/%s/%s/all/show/",
        classification,
        years[t],
        origin
      ),
      "world-world" = sprintf(
        "https://atlas.media.mit.edu/%s/export/%s/all/all/show/",
        classification,
        years[t]
      )
    )

    resp <- GET(url)

    # on a successful GET, return the response
    if (status_code(resp) == 200) {
      data <- try(
        flatten_df(fromJSON(resp$url))
      )

      if (!is.data.frame(data)) {
        stop("It wasn't possible to obtain data.
             Provided this function tests your internet connection there was 
             a server problem.
             Please try again later.")
      }

      return(data)
    } else if (attempts_left == 0) {
      # when attempts run out, stop with an error
      stop("Cannot connect to the API")
    } else {
      # otherwise, sleep a second and try again
      Sys.sleep(1)
      read_from_api(t, flow = "origin-destination", attempts_left = attempts_left - 1)
    }
  }

  # Origin-destination flows ------------------------------------------------
  origin_destination <- map_df(seq_along(years), read_from_api, flow = "origin-destination")

  # no data in API message
  if (nrow(origin_destination) == 0) {
    stop("No data available. Try changing years or trade classification.")
  }

  # compute trade balance
  origin_destination <- mutate(origin_destination,
    trade_exchange_val = !!sym("export_val") +
      !!sym("import_val")
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
  if (origin != "all" | destination != "all") {
    origin_world <- map_df(seq_along(years), read_from_api, flow = "origin-world")

    # extract RCAs
    origin_world <- origin_world %>%
      rename(
        id = !!sym(sprintf("%s_id", classification)),
        id_len = !!sym(sprintf("%s_id_len", classification))
      ) %>%
      mutate(id = str_sub(!!sym("id"), 3)) %>%
      select(!!sym("id"), contains("_rca"))
  }

  # World-world flows -------------------------------------------------------
  if (all(c(origin, destination) %in% "all") != TRUE) {
    world_world <- map_df(seq_along(years), read_from_api, flow = "world-world")

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
  }

  # Join trade flows --------------------------------------------------------
  if (destination != "all") {
    origin_destination <- origin_destination %>%
      left_join(origin_world, by = "id")
  }

  if (all(c(origin, destination) %in% "all") != TRUE) {
    origin_destination <- origin_destination %>%
      left_join(world_world, by = "id")
  }

  origin_destination <- origin_destination %>%
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

  names(country_codes) <- c("top_importer", "top_importer_code")

  origin_destination <- origin_destination %>%
    rename(top_importer_code = !!sym("top_importer")) %>%
    mutate(top_importer_code = str_sub(!!sym("top_importer_code"), 3)) %>%
    left_join(country_codes, by = "top_importer_code")

  names(country_codes) <- c("top_exporter", "top_exporter_code")

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