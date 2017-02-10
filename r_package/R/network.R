#' Creates a network of exports for a given year
#' @export
#' @return Creates an \code{HTML} file with a network visualization for a given year.
#' @param origin is the country code of origin (e.g. "chl" for Chile)
#' @param destination is the country code of origin (e.g. "chn" for China)
#' @param classification Trade classification that can be "1" (HS92 4 characters since year 1995) or "2" (SITC rev.2 4 characters since year 1962)
#' @param year is the year and the OEC's API ranges from 1962 to 2014
#' @examples
#' # Run countries_list() to display the full list of countries
#' # Chile is "chl" and China is "chn"
#' # Visualize trade data from OEC's API (HS92 4 characters product list)
#' # for exports from Chile to China in the year 2014
#' # network("chl", "chn", 2014, 1)
#' # is the same as
#' # network("chl", "chn", 2014)
#' @keywords functions

network <- function(origin, destination, year, classification) {

  countries_list <-  oec::countries_list

  d3_folder <- paste0(getwd(), "/d3plus-1.9.8")
  if(!file.exists(d3_folder)){
    print("D3plus not installed. Installing...")
    install_d3plus()
  }

  if(missing(classification)) {classification = 1}

  # the OEC website only displays exports networks so "exports" will be a fixed parameter
  variable <- "exports"

  getdata(origin, destination, year, classification)

  if(classification == 1) {
    code_display <- "HS92 code"
    product_display <- "HS92 product"
    char <- "4char"
    edges <- "edges_hs92_4char.json"
    nodes <- "nodes_hs92_4char.json"
    input <- paste(origin, destination, year, char, "hs92", sep = "_")

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
      input <- paste(origin, destination, year, char, "sitc_rev2", sep = "_")

      if(!file.exists(edges) & !file.exists(nodes)) {
        ### nodes ###
        print("Creating SITC rev.2 nodes...")
        file.copy(from=system.file("extdata", "nodes_sitc_rev2_4char.json", package = "oec"), to=getwd())
        ### edges ###
        print("Creating SITC rev.2 edges...")
        file.copy(from=system.file("extdata", "edges_sitc_rev2_4char.json", package = "oec"), to=getwd())
      }
    } else {
      if(classification != 1 | classification != 2) {
        print('Error. network() only admits 4 characters codes (HS92 or SITC rev.2).')
        stop()
      }
    }
  }

  replacement_variable <- "export_val"

  replacement_name <- "Export"

  json_file <- paste0(input, ".json")
  if(!file.exists(json_file)){
    print("JSON file not found. Run getdata() first.")
  } else {
    ### html ###
    output <- input
    html_file <- paste0(output, "_network_exports", ".html")
    if(!file.exists(html_file)){
      print("Creating network...")
      network_template <- paste(readLines(system.file("extdata", "network_template.html", package = "oec"), warn = F), collapse = "\n")
      network_template <- gsub("json_file", paste0(output, ".json"), network_template)
      network_template <- gsub("edges_file", edges, network_template)
      network_template <- gsub("nodes_file", nodes, network_template)
      network_template <- gsub("replace_variable", replacement_variable, network_template)
      network_template <- gsub("replace_name", replacement_name, network_template)
      network_template <- ifelse(classification == 1, gsub("replace_group_id", "hs92_group_id", network_template), gsub("replace_group_id", "sitc_rev2_group_id", network_template))
      network_template <- ifelse(classification == 1, gsub("replace_group_name", "hs92_group_name", network_template), gsub("replace_group_name", "sitc_rev2_group_name", network_template))
      network_template <- ifelse(classification == 1, gsub("replace_product_id", "hs92_product_id", network_template), gsub("replace_product_id", "sitc_rev2_product_id", network_template))
      network_template <- ifelse(classification == 1, gsub("replace_product_name", "hs92_product_name", network_template), gsub("replace_product_name", "sitc_rev2_product_name", network_template))
      network_template <- ifelse(classification == 1, gsub("replace_color", "hs92_color", network_template), gsub("replace_color", "sitc_rev2_color", network_template))
      network_template <- ifelse(classification == 1, gsub("replace_icon", "hs92_icon", network_template), gsub("replace_icon", "sitc_rev2_icon", network_template))
      network_template <- gsub("code_display", code_display, network_template)
      network_template <- gsub("product_display", product_display, network_template)
      network_template <- gsub("replace_origin", countries_list[countries_list$country_code == origin, 1], network_template)
      network_template <- gsub("replace_destination", countries_list[countries_list$country_code == destination, 1], network_template)
      network_template <- ifelse(variable == "exports", gsub("replace_action", "export to", network_template),
                                 ifelse(variable == "imports", gsub("replace_action", "import from", network_template),
                                        "exchange with"))
      network_template <- gsub("replace_years", year, network_template)
      network_template <- gsub("replace_timeline", " ", network_template)
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
