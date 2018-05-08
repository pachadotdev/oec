#' oec exported operators and S3 methods
#' The following functions are imported and then re-exported
#' from the oec package to avoid listing Depends of oec
#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble select filter mutate contains 
#'     everything left_join bind_rows rename
#' @importFrom readr write_csv
#' @importFrom purrr map_df
#' @importFrom curl curl new_handle handle_setheaders
#' @importFrom jsonlite fromJSON write_json
#' @importFrom rlang sym syms quo
#' @name oec-exports
NULL