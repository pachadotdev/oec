#' aaa
#' @description aaa
#' @param countryname aaa
#' @return aaa
#' @importFrom magrittr %>%
#' @importFrom dplyr select filter
#' @importFrom rlang sym syms
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
    as_vector
  
  stopifnot(length(countrycode) != 0)
  
  if (length(countrycode) > 1) {
    message("There is more than one match for your search. Here's the output")
    
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
