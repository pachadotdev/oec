#' Creates a treemap for a given period of years
#' @export
#' @return Creates an \code{HTML} file with a treemap visualization for a given period of years.
#' @param origin is the country code of origin (e.g. "chl" for Chile)
#' @param dest is the country code of destination (e.g. "chn" for China)
#' @param variable is the variable to visualize and it can be "imports", "exports" or "exchange" (trade exchange)
#' @param classification Trade classification that can be "1" (HS92 4 characters since year 1995), "2" (SITC rev.3 4 characters since year 1962) or "3" (HS92 6 characters since year 1995)
#' @param initial_year is the initial year and the OEC's API ranges from 1942 to 2014
#' @param final_year is the final year and the OEC's API ranges from 1942 to 2014
#' @param interval is an optional parameter to define the distance between years (by default set to 1)
#' @param depth is an optional parameter that can take values "0" (group's detail) or "1" (product's detail), by defaults its set to 1
#' @examples
#' # Run countries_list() to display the full list of countries
#' # For the example Chile is "chl" and China is "chn"
#'
#' # What does Chile export to China?
#' # Years 2010-2015 (HS92 4 characters)
#' # treemap_interval("chl", "chn", "exports", 2010, 2015)
#' # treemap_interval("chl", "chn", "exports", 2010, 2015, 1, 1, 1) # equivalent to last command
#' @keywords functions

treemap_interval = function(origin, dest, variable, initial_year, final_year, classification, interval, depth) {

  d3_folder = paste0(getwd(), "/d3plus-1.9.6")
  if(!file.exists(d3_folder)){
    print("D3plus is not installed. Installing...")
    install_d3plus()
  }

  if(missing(interval)) {interval = 1}

  if(missing(classification)) {classification = 1}

  if(missing(depth)) {depth = 1}

  getdata_interval(origin, dest, initial_year, final_year, classification, interval)

  if(classification == 1) {
    classification = "hs92"
    code_display = "HS92 code"
    characters = 4
  } else {
    if(classification == 2) {
      classification = "sitc"
      code_display = "SITC code"
      characters = 4
    } else {
      if(classification == 3) {
        classification = "hs92"
        code_display = "HS92 code"
        characters = 6
      } else {
        print('Classification can be "1" (HS92 4 characters since year 1995), "2" (SITC rev.3 4 characters since year 1962) or "3" (HS92 6 characters since year 1995.')
      }
    }
  }

  input = paste(origin, dest, initial_year, final_year, interval, classification, characters, sep = "_")

  grouped_val = 1
  replacement_variable = ifelse(variable == "imports", "import_val",
                        ifelse(variable == "exports", "export_val",
                               ifelse(variable == "exchange", "trade_exchange_val", message("input only allows \"imports\", \"exports\" and \"exchange\""))))
  replacement_name = ifelse(variable == "imports", "Import",
                         ifelse(variable == "exports", "Export",
                                ifelse(variable == "exchange", "Trade exchange", message('input only allows "imports", "exports" and "exchange" (trade exchange)'))))

  output = input
  html_file = paste0(output, "_treemap_", variable, ".html")
  if(!file.exists(html_file)){
    print("Creating treemap...")
    treemap_template = paste(readLines(system.file("extdata", "treemap_template.html", package = "oec"), warn = F), collapse = "\n") %>%
      gsub("json_file", paste0(output, ".json"), .) %>%
      gsub("replace_classification", classification, .) %>%
      gsub("replace_variable", replacement_variable, .) %>%
      gsub("replace_name", replacement_name, .) %>%
      gsub("code_display", code_display, .) %>%
      gsub("depth_val", depth, .) %>%
      gsub("replace_origin", countries_list[countries_list$country_code == origin, 1], .) %>%
      gsub("replace_dest", countries_list[countries_list$country_code == dest, 1], .) %>%
      gsub("replace_years", paste0(initial_year," - ",final_year), .) %>%
      gsub("replace_timeline", paste0('.time({"value": "year", "solo":', initial_year, '})'), .)

    treemap_template = ifelse(variable == "exports", gsub("replace_action", "export to", treemap_template),
                                       ifelse(variable == "imports", gsub("replace_action", "import from", treemap_template),
                                              "exchange with"))

    print("Writing HTML file...")
    writeLines(treemap_template, paste0(output, "_treemap_", variable, ".html"))
    print("Opening HTML files in the browser.")
    httw(pattern = NULL, daemon = TRUE)
  } else {
    print("HTML treemap file already exists. Skipping.")
    print("Opening HTML file in the browser.")
    httw(pattern = NULL, daemon = TRUE)
  }
}
