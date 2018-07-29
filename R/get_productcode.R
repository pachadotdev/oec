#' Obtain a valid product codes
#' @description This function takes a text string and searches within the package data for
#' all matching product codes in the context of valid API product codes.
#' @param productname A text string such as "Animals", "COPPER" or "fruits".
#' @return A tibble with all possible matches (no uppercase distinction) showing the product
#' name, product code and corresponding trade classification (e.g. HS92 or SITC)
#' @importFrom magrittr %>%
#' @importFrom dplyr select filter
#' @importFrom rlang sym
#' @importFrom purrr map map2_dfr as_vector
#' @importFrom stringr str_detect str_subset str_to_lower
#' @importFrom utils data
#' @export
#' @examples
#' get_productcode("animals")
#' @keywords functions

get_productcode <- function(productname = "animals") {
  # Grab all the names of all product datasets
  all_datastr <-
    str_subset(
      data(package = "oec")$results[, "Item"],
      "hs[0-9]|sitc"
    )

  # get the datasets, create the type_product column, bind them all together
  # and do the search
  all_datastr %>%
    map(get) %>%
    map2_dfr(all_datastr, ~ {
      .x$type_product <- .y
      .x
    }) %>%
    filter(
      str_detect(
        str_to_lower(!!sym("product_name")), productname
      )
    )
}