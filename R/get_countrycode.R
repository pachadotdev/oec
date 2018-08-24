#' Obtain valid country codes in ISO-3 format
#' @description This function takes a text string and searches within the package data for 
#' a country code in the context of valid API country codes.
#' @param countryname A text string such as "Chile", "CHILE" or "CHL".
#' @return A single character if there is a exact match (e.g. \code{get_countrycode("Chile")}) or a 
#' tibble in case of multiple matches (e.g. \code{get_countrycode("Germany")})
#' @importFrom magrittr %>%
#' @importFrom dplyr select filter
#' @importFrom rlang sym
#' @importFrom purrr as_vector
#' @importFrom stringr str_detect str_to_lower
#' @export
#' @examples
#' get_countrycode("world")
#' @keywords functions

get_countrycode <- function(countryname = "world") {
  countrycode <- oec::country_codes %>%
    filter(
      str_detect(
        str_to_lower(!!sym("country")), str_to_lower(countryname)
      )
    ) %>%
    select(!!sym("country_code")) %>%
    as_vector()
  
  countryname <- switch(countryname,
    "us" = "united states",
    "usa" = "united states",
    "america" = "united states",
    "united states of america" = "united states",
    "uk" = "united kingdom",
    "england" = "united kingdom",
    "scotland" = "united kingdom",
    "holland" = "netherlands",
    "ussr" = "russia",
    "myanmar" = "burma",
    "persia" = "iran",
    "siam" = "thailand",
    "indochina" = "vietnam",
    "rhodesia" = "zimbabwe",
    "british honduras" = "belice",
    "bengal" = "bangladesh",
    "east pakistan" = "bangladesh",
    "zaire" = "democratic republic of the congo",
    countryname
  )


  if (length(countrycode) == 0) {
    message("There is no match for your search. Please check the spelling or 
            explore country_codes package provided within this package.")
  }

  if (length(countrycode) > 1) {
    message("There is more than one match for your search. Please try again using
            one of these codes:")

    print(
      oec::country_codes %>%
        filter(
          str_detect(
            str_to_lower(!!sym("country")), str_to_lower(countryname)
          )
        )
    )
  } else {
    return(countrycode)
  }
}