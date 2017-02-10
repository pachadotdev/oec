#' Creates a network of exports for a given year
#' @export
#' @return Creates an \code{HTML} file with a network visualization for a given year.
#' @param origin is the country code of origin (e.g. "chl" for Chile)
#' @param destination is the country code of origin (e.g. "chn" for China)
#' @param initial_year The OEC's API ranges from 1962 to 2014. This needs to be lower than `final_year`
#' @param final_year The OEC's API ranges from 1962 to 2014. This needs to be greater than `initial_year`
#' @param classification Trade classification that can be "1" (HS92 4 characters since year 1995) or "2" (SITC rev.2 4 characters since year 1962)
#' @param interval is an optional parameter to define the distance between years (by default set to 1)
#' @examples
#' # Run countries_list() to display the full list of countries
#' # Chile is "chl" and China is "chn"
#' # Visualize trade data from OEC's API (HS92 4 characters product list)
#' # for exports from Chile to China in the years 2011-2014
#' # network_interval("chl", "chn", 2011, 2014, 1, 1)
#' # is the same as
#' # network_interval("chl", "chn", 2011, 2014)
#' @keywords functions

network_interval <- function(origin, destination, initial_year, final_year, interval, classification) {

  countries_list <-  oec::countries_list

  d3_folder <- paste0(getwd(), "/d3plus-1.9.8")
  if(!file.exists(d3_folder)){
    print("D3plus not installed. Installing...")
    install_d3plus()
  }

  if(missing(interval)) {interval = 1}

  if(missing(classification)) {classification = 1}

  # the OEC website only displays exports networks so "exports" will be a fixed parameter
  variable <- "exports"

  getdata_interval(origin, destination, initial_year, final_year, classification, interval)

  if(classification == 1) {
    code_display <- "HS92 code"
    product_display <- "HS92 product"
    char <- "4char"
    edges <- "edges_hs92_4char.json"
    nodes <- "nodes_hs92_4char.json"
    input <- paste(origin, destination, initial_year, final_year, interval, char, "hs92", sep = "_")

    if(!file.exists(edges) & !file.exists(nodes)) {
      ### nodes ###
      print("Creating HS92 nodes...")
      file.copy(from = system.file("extdata", "nodes_hs92_4char.json", package = "oec"), to = getwd())
      ### edges ###
      print("Creating HS92 edges...")
      file.copy(from = system.file("extdata", "edges_hs92_4char.json", package = "oec"), to = getwd())
    }
  } else {
    if(classification == 2) {
      code_display <- "SITC code"
      product_display <- "SITC product"
      char <- "4char"
      edges <- "edges_sitc_rev2_4char.json"
      nodes <- "nodes_sitc_rev2_4char.json"
      input <- paste(origin, destination, initial_year, final_year, interval, char, "sitc_rev2", sep = "_")

      if(!file.exists(edges) & !file.exists(nodes)) {
        ### nodes ###
        print("Creating SITC rev. 3 nodes...")
        file.copy(from=system.file("extdata", "nodes_sitc_rev2_4char.json", package = "oec"), to=getwd())
        ### edges ###
        print("Creating SITC rev. 3 edges...")
        file.copy(from=system.file("extdata", "edges_sitc_rev2_4char.json", package = "oec"), to=getwd())
      }
    } else {
      if(classification != 1 | classification != 2) {
        print('Error. network() only admits 4 characters codes (HS92 or SITC rev.2).')
        stop()
      }
    }
  }

  replacement_variable <-"export_val"

  replacement_name <- "Export"

  json_file <- paste0(input, ".json")
  if(!file.exists(json_file)){
    print("JSON file not found. Run getdata_interval() first.")
  } else {
    ### html ###
    output <- input
    html_file <- paste0(output, "_network_exports", ".html")
    if(!file.exists(html_file)){
      print("Creating network...")
      network_interval_template <- paste(readLines(system.file("extdata", "network_template.html", package = "oec"), warn = F), collapse = "\n")
      network_interval_template <- gsub("json_file", paste0(output, ".json"), network_interval_template)
      network_interval_template <- gsub("edges_file", edges, network_interval_template)
      network_interval_template <- gsub("nodes_file", nodes, network_interval_template)
      network_interval_template <- gsub("replace_variable", replacement_variable, network_interval_template)
      network_interval_template <- gsub("replace_name", replacement_name, network_interval_template)
      network_interval_template <- ifelse(classification == 1 | classification == 3, gsub("replace_group_id", "hs92_group_id", network_interval_template), gsub("replace_group_id", "sitc_rev2_group_id", network_interval_template))
      network_interval_template <- ifelse(classification == 1 | classification == 3, gsub("replace_group_name", "hs92_group_name", network_interval_template), gsub("replace_group_name", "sitc_rev2_group_name", network_interval_template))
      network_interval_template <- ifelse(classification == 1 | classification == 3, gsub("replace_product_id", "hs92_product_id", network_interval_template), gsub("replace_product_id", "sitc_rev2_product_id", network_interval_template))
      network_interval_template <- ifelse(classification == 1 | classification == 3, gsub("replace_product_name", "hs92_product_name", network_interval_template), gsub("replace_product_name", "sitc_rev2_product_name", network_interval_template))
      network_interval_template <- ifelse(classification == 1 | classification == 3, gsub("replace_color", "hs92_color", network_interval_template), gsub("replace_color", "sitc_rev2_color", network_interval_template))
      network_interval_template <- ifelse(classification == 1 | classification == 3, gsub("replace_icon", "hs92_icon", network_interval_template), gsub("replace_icon", "sitc_rev2_icon", network_interval_template))
      network_interval_template <- gsub("code_display", code_display, network_interval_template)
      network_interval_template <- gsub("product_display", product_display, network_interval_template)
      network_interval_template <- ifelse(origin == "all", gsub("replace_origin", "the rest of the World", network_interval_template),
                                          gsub("replace_origin", countries_list[countries_list$country_code == origin, 1], network_interval_template))
      network_interval_template <- ifelse(origin == "all", gsub("replace_destination", "the rest of World", network_interval_template),
                                          gsub("replace_destination", countries_list[countries_list$country_code == destination, 1], network_interval_template))
      network_interval_template <- ifelse(variable == "exports", gsub("replace_action", "export to", network_interval_template),
                                          ifelse(variable == "imports", gsub("replace_action", "import from", network_interval_template),
                                                 "exchange with"))
      network_interval_template <- gsub("replace_years", paste0(initial_year," - ",final_year), network_interval_template)
      network_interval_template <- gsub("replace_timeline", paste0('.time({"value": "year", "solo":', initial_year, '})'), network_interval_template)
      print("Writing HTML file...")
      writeLines(network_interval_template, paste0(output, "_network_exports", ".html"))
      print("Opening HTML files in the browser.")
      httw(pattern = NULL, daemon = TRUE)
    } else {
      print("HTML network file already exists. Skipping.")
      print("Opening HTML files in the browser.")
      httw(pattern = NULL, daemon = TRUE)
    }
  }
}
