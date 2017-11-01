globalVariables(c("countries_list","."))

#' Creates a treemap for a given year
#' @export
#' @return Creates an \code{HTML} file with a treemap visualization for a given year.
#' @param origin is the country code of origin (e.g. "chl" for Chile)
#' @param dest is the country code of destination (e.g. "chn" for China)
#' @param variable is the variable to visualize and it can be "imports", "exports" or "exchange" (trade exchange)
#' @param classification Trade classification that can be "1" (HS92 4 charactersacters since year 1995), "2" (SITC rev.3 4 charactersacters since year 1962) or "3" (HS92 6 charactersacters since year 1995)
#' @param year is the year and the OEC's API ranges from 1962 to 2014
#' @param depth is an optional parameter that can take values "0" (group's detail) or "1" (product's detail)
#' @examples
#' # Run countries_list() to display the full list of countries
#' # For the example Chile is "chl" and China is "chn"
#'
#' # What does Chile export to China?
#' # Year 2015 (HS92 4 characters)
#' # treemap("chl", "chn", "exports", 2015)
#' # treemap("chl", "chn", "exports", 2015, 1) # equivalent to last command
#' @keywords functions

treemap = function(origin, dest, variable, year, classification, depth) {

  d3_folder = paste0(getwd(), "/d3plus-1.9.8")
  if(!file.exists(d3_folder)){
    print("D3plus was not found in your working directory. Copying files...")
    install_d3plus()
  }

  if(missing(classification)) {classification = 1}

  if(missing(depth)) {depth = 1}

  getdata(origin, dest, year, classification)

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
        print('Classification only admits "1" (HS92 4 charactersacters) or "3" (HS92 6 charactersacters) for the year 1995 and going or "3" (SITC rev.3 4 charactersacters) for the year 1962 and ongoing.')
      }
    }
  }

  input = paste(origin, dest, year, classification, characters, sep = "_")

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
        gsub("replace_years", year, .) %>%
        gsub("replace_timeline", " ", .)

      treemap_template = ifelse(variable == "exports", gsub("replace_action", "export to", treemap_template),
                                 ifelse(variable == "imports", gsub("replace_action", "import from", treemap_template),
                                        "exchange with"))

      print("Writing html file...")
      writeLines(treemap_template, paste0(output, "_treemap_", variable, ".html"))
      print("Opening html files in the browser.")
      httw(pattern = NULL, daemon = TRUE)
    } else {
      print("HTML treemap file already exists. Skipping.")
      print("Opening HTML file in the browser.")
      httw(pattern = NULL, daemon = TRUE)
    }
}
