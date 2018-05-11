#' oec exported operators and S3 methods
#' The following functions are imported and then re-exported
#' from the oec package to avoid listing Depends of oec
#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble select filter mutate contains 
#'     everything left_join bind_rows rename matches
#' @importFrom stringr str_sub str_length
#' @importFrom purrr map_df
#' @importFrom curl curl new_handle handle_setheaders
#' @importFrom jsonlite fromJSON
#' @importFrom rlang sym syms is_true
#' @name oec-exports
#' @keywords internal
NULL