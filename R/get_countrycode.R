#' aaa
#' @description aaa
#' @param country_name aaa
#' @return aaa
#' @importFrom magrittr %>%
#' @importFrom dplyr select filter
#' @importFrom rlang sym syms
#' @importFrom purrr as_vector
#' @export
#' @examples
#' get_countrycode("world")
#' @keywords functions

get_countrycode <- function(country_name = "world") {
  countrycode <- oec::country_codes %>% 
    
    mutate(
      country2 = tolower(!!sym("country"))
    ) %>% 
    
    filter(
      grepl(tolower(country_name), !!sym("country2"))
    ) %>% 
    
    select(!!sym("country_code"), -!!sym("country")) %>% 
    as_vector
  
  stopifnot(length(countrycode) != 0)
  
  if (length(countrycode) > 1) {
    message("There is more than one match for your search. Here's the output")
    
    print(
      oec::country_codes %>% 
        
        mutate(
          country2 = iconv(tolower(!!sym("country")))
        ) %>% 
        
        filter(
          grepl(tolower(country_name), !!sym("country2"))
        ) %>% 
        
        select(-!!sym("country2"))
    )
  } else {
    return(countrycode)
  }
}
