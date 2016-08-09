#' Creates a treemap for a given year
#' @export
#' @return Creates an \code{HTML} file with a treemap visualization for a given year.
#' @param origin is the country code of origin (e.g. "chl" for Chile)
#' @param destination is the country code of origin (e.g. "chn" for China)
#' @param variable is the variable to visualize and it can be "imports", "exports" or "exchange" (trade exchange)
#' @param classification refers to the trade classification that can be "6" (HS92 6 characters) or "8" (HS92 8 characters) for the year 1995 and going or "4" (SITC rev.2 4 characters) for the year 1962 and ongoing
#' @param year is the year and the OEC's API ranges from 1962 to 2014
#' @param depth can be 0 or 1 (explain this better!) and its optional
#' @examples
#' treemap("chl", "chn", "exports", 6, 2014)
#' @keywords functions

treemap <- function(origin, destination, variable, classification, year, depth) {

  d3_folder <- paste0(getwd(), "/d3plus")
  if(!file.exists(d3_folder)){
    print("D3plus not installed... installing using install_d3plus()...")
    install_d3plus()
  }

  if(missing(depth)) {
    depth = 1
  }

  input <- paste(origin, destination, year, classification, sep = "_")
  input <- paste0(input, "char")

  getdata(origin, destination, classification, year)

  if(classification == 4) {
    code_display <- "SITC code"
    char <- "4char"
  } else {
    if(classification == 6) {
      code_display <- "HS92 code"
      char <- "6char"
    } else {
      if(classification == 8) {
        code_display <- "HS92 code"
        char <- "8char"
      } else {
        print('classification only admits "sitc" for SITC rev.2 and "hs6"/"hs8" for H292.')
      }
    }
  }

  grouped_val <- 1
  variablecol <- ifelse(variable == "imports", "import_val",
                        ifelse(variable == "exports", "export_val",
                               ifelse(variable == "exchange", "trade_exchange_val", message("input only allows \"imports\", \"exports\" and \"exchange\""))))
  variablename <- ifelse(variable == "imports", "Import",
                         ifelse(variable == "exports", "Export",
                                ifelse(variable == "exchange", "Trade exchange", message('input only allows "imports", "exports" and "exchange" (trade exchange)'))))

  output <- input
  html_file <- paste0(output, "_treemap_", variable, ".html")
  if(!file.exists(html_file)){
      print("creating treemap...")
      treemap_template <- paste(readLines(system.file("extdata", "treemap_template.html", package = "oec"), warn = F), collapse = "\n")
      treemap_template <- gsub("json_file", paste0(output, ".json"), treemap_template)
      treemap_template <- gsub("variablecol", variablecol, treemap_template)
      treemap_template <- gsub("variablename", variablename, treemap_template)
      treemap_template <- gsub("code_display", code_display, treemap_template)
      treemap_template <- gsub("depth_val", depth, treemap_template)
      print("writing html file...")
      writeLines(treemap_template, paste0(output, "_treemap_", variable, ".html"))
      print("opening html files in the browser.")
      httw(pattern = NULL, daemon = TRUE)
    } else {
      print("html treemap file already exists. skipping.")
      print("opening html files in the browser.")
      httw(pattern = NULL, daemon = TRUE)
    }
}
