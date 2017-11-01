#' Creates a network of exports for a given period of years
#' @export
#' @return Creates an \code{HTML} file with a network visualization for a given given period of years.
#' @param origin is the country code of origin (e.g. "chl" for Chile)
#' @param dest is the country code of destination (e.g. "chn" for China)
#' @param initial_year is the initial year and the OEC's API ranges from 1942 to 2014
#' @param final_year is the final year and the OEC's API ranges from 1942 to 2014
#' @param classification Trade classification that can be "1" (HS92 4 characters since year 1995) or "2" (SITC rev.2 4 characters since year 1962)
#' @param interval is an optional parameter to define the distance between years (by default set to 1)
#' #' @examples
#' # Run countries_list() to display the full list of countries
#' # For the example Chile is "chl" and China is "chn"
#'
#' # What are the export opportunities of Chile?
#' # Years 2010-2015, trade with China (HS92 4 characters)
#' # network_interval("chl", "chn", 2010, 2015)
#' # network_interval("chl", "chn", 2010, 2015, 1, 1) # equivalent to last command
#' @keywords functions

network_interval = function(origin, dest, initial_year, final_year, classification, interval) {

  d3_folder = paste0(getwd(), "/d3plus-1.9.8")
  if(!file.exists(d3_folder)){
    print("D3plus was not found in your working directory. Copying files...")
    install_d3plus()
  }

  if(missing(interval)) {interval = 1}

  if(missing(classification)) {classification = 1}

  # the OEC website only displays exports networks so "exports" will be a fixed parameter
  variable = "exports"

  getdata_interval(origin, dest, initial_year, final_year, classification, interval)

  if(classification == 1) {
    classification = "hs92"
    code_display = "HS92 code"
    characters = 4
    edges = "edges_hs92_4.json"
    nodes = "nodes_hs92_4.json"

    if(!file.exists(edges) & !file.exists(nodes)) {
      ### nodes ###
      print("Copying HS92 edges...")
      file.copy(from = system.file("extdata", edges, package = "oec"), to = getwd())
      ### edges ###
      print("Copying HS92 nodes...")
      file.copy(from = system.file("extdata", nodes, package = "oec"), to = getwd())
    }
  } else {
    if(classification == 2) {
      classification = "sitc"
      code_display = "SITC code"
      characters = 4
      edges = "edges_sitc_4.json"
      nodes = "nodes_sitc_4.json"

      if(!file.exists(edges) & !file.exists(nodes)) {
        ### nodes ###
        print("Creating SITC rev.2 edges...")
        file.copy(from=system.file("extdata", edges, package = "oec"), to=getwd())
        ### edges ###
        print("Creating SITC rev.2 nodes...")
        file.copy(from=system.file("extdata", nodes, package = "oec"), to=getwd())
      }
    } else {
      if(classification != 1 | classification != 2) {
        print('Error: network_interval() only admits 4 characters codes (HS92 or SITC rev.2).')
        stop()
      }
    }
  }

  input = paste(origin, dest, initial_year, final_year, interval, classification, characters, sep = "_")

  replacement_variable = "export_val"
  replacement_name = "Export"

  json_file = paste0(input, ".json")
  if(!file.exists(json_file)){
    print("JSON file not found. Run getdata() first.")
  } else {
    ### html ###
    output = input
    html_file = paste0(output, "_network_exports", ".html")
    if(!file.exists(html_file)){
      print("Creating network...")
      network_template = paste(readLines(system.file("extdata", "network_template.html", package = "oec"), warn = F), collapse = "\n") %>%
        gsub("json_file", paste0(output, ".json"), .) %>%
        gsub("edges_file", edges, .) %>%
        gsub("nodes_file", nodes, .) %>%
        gsub("replace_classification", classification, .) %>%
        gsub("replace_variable", replacement_variable, .) %>%
        gsub("replace_name", replacement_name, .) %>%
        gsub("code_display", code_display, .) %>%
        gsub("replace_origin", countries_list[countries_list$country_code == origin, 1], .) %>%
        gsub("replace_dest", countries_list[countries_list$country_code == dest, 1], .) %>%
        gsub("replace_years", paste0(initial_year," - ",final_year), .) %>%
        gsub("replace_timeline", paste0('.time({"value": "year", "solo":', initial_year, '})'), .)

      network_template = ifelse(variable == "exports", gsub("replace_action", "export to", network_template),
                                ifelse(variable == "imports", gsub("replace_action", "import from", network_template),
                                       "exchange with"))

      print("Writing HTML file...")
      writeLines(network_template, paste0(output, "_network_exports", ".html"))
      print("Opening html files in the browser.")
      httw(pattern = NULL, daemon = TRUE)
    } else {
      print("HTML network file already exists. Skipping.")
      print("Opening HTML file in the browser.")
      httw(pattern = NULL, daemon = TRUE)
    }
  }
}
