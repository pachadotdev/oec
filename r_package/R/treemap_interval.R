#' Creates a treemap for a given given period of years
#' @export
#' @return Creates an \code{HTML} file with a treemap visualization for a given period of years.
#' @param origin is the country code of origin (e.g. "chl" for Chile)
#' @param destination is the country code of origin (e.g. "chn" for China)
#' @param variable is the variable to visualize and it can be "imports", "exports" or "exchange" (trade exchange)
#' @param classification Trade classification that can be "1" (HS92 4 characters since year 1995), "2" (SITC rev.3 4 characters since year 1962) or "3" (HS92 6 characters since year 1995)
#' @param initial_year is the initial year and the OEC's API ranges from 1942 to 2014
#' @param final_year is the final year and the OEC's API ranges from 1942 to 2014
#' @param interval is an optional parameter to define the distance between years (by default set to 1)
#' @param depth is an optional parameter that can take values "0" (group's detail) or "1" (product's detail), by defaults its set to 1
#' @examples
#' # Run countries_list() to display the full list of countries
#' # Chile is "chl" and China is "chn"
#' # Visualize trade data from OEC's API (HS92 4 characters product list)
#' # for Chile and China in the years 2011 to 2014
#' # treemap_interval("chl", "chn", "exports", 2011, 2014, 1, 1 ,1)
#' # is the same as
#' # treemap_interval("chl", "chn", "exports", 2011, 2014)
#' @keywords functions

treemap_interval <- function(origin, destination, variable, initial_year, final_year, interval, classification, depth) {

  countries_list <-  oec::countries_list

  d3_folder <- paste0(getwd(), "/d3plus-1.9.6")
  if(!file.exists(d3_folder)){
    print("D3plus is not installed. Installing...")
    install_d3plus()
  }

  if(missing(interval)) {interval = 1}

  if(missing(classification)) {classification = 1}

  if(missing(depth)) {depth = 1}

  getdata_interval(origin, destination, initial_year, final_year, classification, interval)

  if(classification == 1) {
    code_display <- "HS92 code"
    product_display <- "HS92 product"
    char <- "4char"
    input <- paste(origin, destination, initial_year, final_year, interval, char, "hs92", sep = "_")
  } else {
    if(classification == 2) {
      code_display <- "SITC code"
      product_display <- "SITC product"
      char <- "4char"
      input <- paste(origin, destination, initial_year, final_year, interval, char, "sitc_rev2", sep = "_")
    } else {
      if(classification == 3) {
        code_display <- "HS92 code"
        product_display <- "HS92 product"
        char <- "6char"
        input <- paste(origin, destination, initial_year, final_year, interval, char, "hs92", sep = "_")
      } else {
        print('Classification can be "1" (HS92 4 characters since year 1995), "2" (SITC rev.3 4 characters since year 1962) or "3" (HS92 6 characters since year 1995.')
      }
    }
  }

  grouped_val <- 1
  replacement_variable <- ifelse(variable == "imports", "import_val",
                        ifelse(variable == "exports", "export_val",
                               ifelse(variable == "exchange", "trade_exchange_val", message("input only allows \"imports\", \"exports\" and \"exchange\""))))
  replacement_name <- ifelse(variable == "imports", "Import",
                         ifelse(variable == "exports", "Export",
                                ifelse(variable == "exchange", "Trade exchange", message('input only allows "imports", "exports" and "exchange" (trade exchange)'))))

  output <- input
  html_file <- paste0(output, "_treemap_", variable, ".html")
  if(!file.exists(html_file)){
    print("Creating treemap...")
    treemap_interval_template <- paste(readLines(system.file("extdata", "treemap_template.html", package = "oec"), warn = F), collapse = "\n")
    treemap_interval_template <- gsub("json_file", paste0(output, ".json"), treemap_interval_template)
    treemap_interval_template <- gsub("replace_variable", replacement_variable, treemap_interval_template)
    treemap_interval_template <- gsub("replace_name", replacement_name, treemap_interval_template)
    treemap_interval_template <- ifelse(classification == 1 | classification == 3, gsub("replace_group_id", "hs92_group_id", treemap_interval_template), gsub("replace_group_id", "sitc_rev2_group_id", treemap_interval_template))
    treemap_interval_template <- ifelse(classification == 1 | classification == 3, gsub("replace_group_name", "hs92_group_name", treemap_interval_template), gsub("replace_group_name", "sitc_rev2_group_name", treemap_interval_template))
    treemap_interval_template <- ifelse(classification == 1 | classification == 3, gsub("replace_product_id", "hs92_product_id", treemap_interval_template), gsub("replace_product_id", "sitc_rev2_product_id", treemap_interval_template))
    treemap_interval_template <- ifelse(classification == 1 | classification == 3, gsub("replace_product_name", "hs92_product_name", treemap_interval_template), gsub("replace_product_name", "sitc_rev2_product_name", treemap_interval_template))
    treemap_interval_template <- ifelse(classification == 1 | classification == 3, gsub("replace_color", "hs92_color", treemap_interval_template), gsub("replace_color", "sitc_rev2_color", treemap_interval_template))
    treemap_interval_template <- ifelse(classification == 1 | classification == 3, gsub("replace_icon", "hs92_icon", treemap_interval_template), gsub("replace_icon", "sitc_rev2_icon", treemap_interval_template))
    treemap_interval_template <- gsub("code_display", code_display, treemap_interval_template)
    treemap_interval_template <- gsub("product_display", product_display, treemap_interval_template)
    treemap_interval_template <- gsub("depth_val", depth, treemap_interval_template)
    treemap_interval_template <- gsub("replace_origin", countries_list[countries_list$country_code == origin, 1], treemap_interval_template)
    treemap_interval_template <- gsub("replace_destination", countries_list[countries_list$country_code == destination, 1], treemap_interval_template)
    treemap_interval_template <- ifelse(variable == "exports", gsub("replace_action", "export to", treemap_interval_template),
                               ifelse(variable == "imports", gsub("replace_action", "import from", treemap_interval_template),
                                      "exchange with"))
    treemap_interval_template <- gsub("replace_years", paste0(initial_year," - ",final_year), treemap_interval_template)
    treemap_interval_template <- gsub("replace_timeline", paste0('.time({"value": "year", "solo":', initial_year, '})'), treemap_interval_template)
    print("Writing HTML file...")
    writeLines(treemap_interval_template, paste0(output, "_treemap_", variable, ".html"))
    print("Opening HTML files in the browser.")
    httw(pattern = NULL, daemon = TRUE)
  } else {
    print("HTML treemap file already exists. Skipping.")
    print("Opening HTML file in the browser.")
    httw(pattern = NULL, daemon = TRUE)
  }
}
