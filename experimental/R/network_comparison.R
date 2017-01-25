#' Creates a network to see if new exported products have acquired a comparative advantage within a period of year
#' @export
#' @return Creates an \code{HTML} file with a network visualization that compares two given years to see if more exported products have acquired a Revealed Comparative Advantage (RCA > 1) within the period.
#' @param origin is the country code of origin (e.g. "chl" for Chile)
#' @param destination is the country code of origin (e.g. "chn" for China)
#' @param classification refers to the trade classification that can be "6" (HS92 6 characters) or "8" (HS92 8 characters) for the year 1995 and going or "4" (SITC rev.2 4 characters) for the year 1962 and ongoing
#' @param initial_year is the initial year and the OEC's API ranges from 1962 to 2014
#' @param final_year is the final year and the OEC's API ranges from 1962 to 2014
#' @examples
#' # network_comparison("chl", "chn", 6, 2010, 2014)
#' @keywords functions

network_comparison <- function(origin, destination, classification, initial_year, final_year) {
  d3_folder <- paste0(getwd(), "/d3plus-1.9.8")
  if(!file.exists(d3_folder)){
    print("D3plus not installed... installing using install_d3plus()...")
    install_d3plus()
  }

  variable = "exports"

  input = paste(origin, destination, initial_year, final_year, classification, sep="_")
  input = paste0(input, "char")

  if(final_year > initial_year) {

    print(paste0("Processing files for the years ", initial_year, " and ", final_year))
    getdata_interval(origin, destination, classification, initial_year, final_year, final_year - initial_year)

    if(classification == 4 | classification == 6) {
      if(classification == 4) {
        origin_compare$group2 = ifelse(!(origin_final_year$sitc_rev2_id %in% origin_initial_year_non_null$sitc_rev2_id), final_year, initial_year)
        origin_compare$color2 = ifelse(origin_final_year$rca > origin_initial_year$rca, final_year, initial_year)
      }

      if(classification == 6) {
        origin_compare$group2 = ifelse(!(origin_final_year$hs92_id %in% origin_initial_year_non_null$hs92_id), final_year, initial_year)
        origin_compare$color2 = ifelse(origin_compare$group2 == final_year, "#4169e1", "#e1b941")
      }

      write(toJSON(origin_compare, pretty = TRUE), file=paste0(input, ".json"))
      envir = as.environment(1)
      assign(paste0(origin, "_", destination, "_", initial_year, "_", final_year, "_", classification, "char"), origin_compare, envir = envir)

    } else {
      print("network.compare() only admits 4 characters codes (SITC rev.2) or 6 characters codes (HS92)")
    }
  } else {
    print("final_year must be greater than initial_year")
  }

  if(classification == 4) {
    code_display = "SITC code"
    edges = "edges_sitc.json"
    nodes = "nodes_sitc.json"

    if(!file.exists(edges) & !file.exists(nodes)) {
      ### nodes ###
      print("creating SITC rev. 2 nodes...")
      file.copy(from=system.file("extdata", "nodes_sitc.json", package = "oec"), to=getwd())
      ### edges ###
      print("creating SITC rev. 2 edges...")
      file.copy(from=system.file("extdata", "edges_sitc.json", package = "oec"), to=getwd())
    }
  }
  if(classification == 6) {
    code_display = "HS92 code"
    edges = "edges_hs.json"
    nodes = "nodes_hs.json"

    if(!file.exists(edges) & !file.exists(nodes)) {
      ### nodes ###
      print("creating HS92 nodes...")
      file.copy(from=system.file("extdata", "nodes_hs.json", package = "oec"), to=getwd())
      ### edges ###
      print("creating HS92 edges...")
      file.copy(from=system.file("extdata", "edges_hs.json", package = "oec"), to=getwd())
    }
  }
  if(variable == "exports") {
    if(classification == 4 | classification == 6) {
      variablecol <- ifelse(variable == "imports", "import_val",
                            ifelse(variable == "exports", "export_val",
                                   ifelse(variable == "exchange", "trade_exchange_val", "error")))
      variablename <- ifelse(variable == "imports", "Import",
                             ifelse(variable == "exports", "Export",
                                    ifelse(variable == "exchange", "Trade exchange", "error")))

      json_file <- paste0(input, ".json")
      if(!file.exists(json_file)){
        print("json file not found. run getdata() first")
      } else {
        ### html ###
        output = input
        html_file <- paste0(output, "_network_exports", ".html")
        if(!file.exists(html_file)){
          print("creating network")
          network_compare_template <- paste(readLines(system.file("extdata", "network_compare_template.html", package = "oec"), warn = F), collapse = "\n")
          network_compare_template <- gsub("json_file", paste0(output, ".json"), network_compare_template)
          network_compare_template <- gsub("edges_file", edges, network_compare_template)
          network_compare_template <- gsub("nodes_file", nodes, network_compare_template)
          network_compare_template <- gsub("variablecol", variablecol, network_compare_template)
          network_compare_template <- gsub("variablename", variablename, network_compare_template)
          network_compare_template <- ifelse(classification == 6 | classification == 8, gsub("product_id", "hs92_id", network_compare_template), gsub("product_id", "sitc_rev2_id", network_compare_template))
          network_compare_template <- gsub("code_display", code_display, network_compare_template)
          network_compare_template <- ifelse(origin == "all", gsub("origin_id_replace", "the rest of the World", network_compare_template),
                                     gsub("origin_id_replace", countries_list[countries_list$country_code == origin, 1], network_compare_template))
          network_compare_template <- ifelse(origin == "all", gsub("destination_id_replace", "the rest of World", network_compare_template),
                                     gsub("destination_id_replace", countries_list[countries_list$country_code == destination, 1], network_compare_template))
          network_compare_template <- ifelse(variable == "exports", gsub("variable_replace", "export to", network_compare_template),
                                     ifelse(variable == "imports", gsub("variable_replace", "import from", network_compare_template),
                                            "exchange with"))
          network_compare_template <- gsub("initial_year_replace", initial_year, network_compare_template)
          network_compare_template <- gsub("final_year_replace", final_year, network_compare_template)
          print("writing html file...")
          writeLines(network_compare_template, paste0(output, "_network_exports", ".html"))
          print("opening html files in the browser.")
          httw(pattern = NULL, daemon = TRUE)
        } else {
          print("html network file already exists. skipping.")
          print("opening html files in the browser.")
          httw(pattern = NULL, daemon = TRUE)
        }
      }
    }
  } else {
    print('network.compare() only admits 4 characters codes (SITC rev.2) or 6 characters codes (HS92). This method only allows "exports".')
  }
}
