#' Creates an animated treemap.
#' @export
#' @return Creates an \code{HTML} file in the working directory with a tree map visualization.
#' @param ORIGIN is the country code of origin (e.g. "chl" for Chile)
#' @param DESTINATION is the country code of origin (e.g. "chn" for China)
#' @param VARIABLE is the variable to visualize and it can be "imports", "exports" or "exchange" (trade exchange)
#' @param CLASSIFICATION refers to the trade classification that can be "6" (HS92 6 characters) or "8" (HS92 8 characters) for the year 1995 and going or "4" (SITC rev.2 4 characters) for the year 1962 and ongoing
#' @param YEAR is the year and the OEC's API ranges from 1962 to 2014
#' @examples
#' treemap("chl", "chn", "exports", 6, 2004)
#' @keywords functions

treemap <- function(ORIGIN, DESTINATION, VARIABLE, CLASSIFICATION, YEAR) {

  d3_folder <- paste0(getwd(),"/d3plus")
  if(!file.exists(d3_folder)){
    print("d3plus not installed... installing using d3plus()...")
    d3plus()
  }

  INPUT = paste(ORIGIN,DESTINATION,YEAR,CLASSIFICATION, sep="_")
  INPUT = paste0(INPUT,"char")

    getdata(ORIGIN, DESTINATION, CLASSIFICATION, YEAR)

    if(CLASSIFICATION == 4) {
      code_display = "SITC code"
      CHAR = "4char"
    } else {
      if(CLASSIFICATION == 6) {
        code_display = "HS92 code"
        CHAR = "6char"
      } else {
        if(CLASSIFICATION == 8) {
          code_display = "HS92 code"
          CHAR = "8char"
        } else {
          print('classification only admits sitc for SITC rev.2 and hs6/hs8 for H292.')
        }
      }
    }

    width = 1000
    height = 600

    #grouped_val <- ifelse(GROUPED == "yes", 0, ifelse(GROUPED == "no", 1, message("*** Input only allows \"yes\" and \"no\" ***")))
    grouped_val = 1
    variablecol <- ifelse(VARIABLE == "imports", "import_val",
                          ifelse(VARIABLE == "exports", "export_val",
                                 ifelse(VARIABLE == "exchange", "trade_exchange_val", message("Input only allows \"imports\", \"exports\" and \"exchange\""))))
    variablename <- ifelse(VARIABLE == "imports", "Import",
                           ifelse(VARIABLE == "exports", "Export",
                                  ifelse(VARIABLE == "exchange", "Trade exchange", message('Input only allows "imports", "exports" and "exchange" (trade exchange)'))))

    OUTPUT = INPUT
    html_file <- paste0(OUTPUT, "_treemap_", VARIABLE, ".html")
      if(!file.exists(html_file)){
        print("creating treemap...")
        treemap_template <- paste(readLines(system.file("extdata", "treemap_template.html", package = "oec"), warn = F), collapse = "\n")
        treemap_template = gsub("json_file", paste0(OUTPUT, ".json"), treemap_template)
        treemap_template = gsub("variablecol", variablecol, treemap_template)
        treemap_template = gsub("variablename", variablename, treemap_template)
        treemap_template = gsub("code_display", code_display, treemap_template)
        print("writing html file...")
        writeLines(treemap_template, paste0(OUTPUT, "_treemap_", VARIABLE, ".html"))
        print("opening html files in the browser.")
        httw(pattern = NULL, daemon = TRUE)
      } else {
        print("html treemap file already exists. skipping.")
        print("opening html files in the browser.")
        httw(pattern = NULL, daemon = TRUE)
      }
}
