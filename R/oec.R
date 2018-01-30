#' The Observatory of Economic Complexity
#'
#' Package's details.
#'
#' This package was created to simplify user interaction with the OEC's API. It will download trade data from MIT Media Lab servers and it will save that both in CSV and JSON formats.
#'
#' You can use this package just to download information but it also creates D3Plus visualizations that are suitable for presentations or a context where you need to show data. These visualizations do not need internet connection after you obtain the data.
#'
#' All of the datasets provided within this package provide data that cannot be obtained from the API and do help creating better visualizations.
#'
#' The functions provided within this package are:
#'
#' \link{getdata} Downloads and processes the data from the API for a certain year.
#'
#' \link{getdata_interval} Downloads and processes the data from the API for an interval of years.
#'
#' The datasets provided within this package are:
#'
#' \link{countries_list}	A list of all the countries in the world and its respective country code.
#'
#' \link{hs92} HS92 products and groups (4 and 6 characters codes).
#'
#' \link{sitc} SITC rev.2 products and groups (4 characters codes).
#'
#' @name oec-package
#' @aliases oec
#' @docType package
NULL
