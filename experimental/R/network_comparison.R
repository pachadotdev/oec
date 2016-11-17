#' Creates a network to compare two years
#' @export
#' @return Creates an \code{HTML} file with a network visualization that compares two given years.
#' @param origin is the country code of origin (e.g. "chl" for Chile)
#' @param destination is the country code of origin (e.g. "chn" for China)
#' @param classification refers to the trade classification that can be "6" (HS92 6 characters) or "8" (HS92 8 characters) for the year 1995 and going or "4" (SITC rev.2 4 characters) for the year 1962 and ongoing
#' @param year1 is the initial year and the OEC's API ranges from 1962 to 2014
#' @param year2 is the final year and the OEC's API ranges from 1962 to 2014
#' @examples
#' network_comparison("chl", "chn", 6, 2010, 2014)
#' @keywords functions

network_comparison <- function(origin, destination, classification, year1, year2) {
  d3_folder <- paste0(getwd(), "/d3plus-2.0")
  if(!file.exists(d3_folder)){
    print("D3plus not installed... installing using install_d3plus()...")
    install_d3plus()
  }

  variable = "exports"

  input = paste(origin, destination, year1, year2, classification, sep="_")
  input = paste0(input, "char")

  if(year2 > year1) {

    print(paste0("Processing files for the year ", year1, "..."))
    getdata(origin, destination, classification, year1)

    print(paste0("Processing files for the year ", year2, "..."))
    getdata(origin, destination, classification, year2)

    year1_file = paste(origin, destination, year1, classification, sep="_")
    year2_file = paste(origin, destination, year2, classification, sep="_")
    year1_file = paste0(year1_file, "char.json")
    year2_file = paste0(year2_file, "char.json")

    origin_year1 <- as.data.frame(fromJSON(year1_file))
    origin_year2 <- as.data.frame(fromJSON(year2_file))

    origin_year1$rca = ifelse(is.na(origin_year1$rca), 0, origin_year1$rca)
    origin_year2$rca = ifelse(is.na(origin_year2$rca), 0, origin_year2$rca)

    origin_year1$export_val = ifelse(is.na(origin_year1$export_val), 0, origin_year1$export_val)
    origin_year1_non_null = origin_year1[origin_year1$export_val > 0, ]

    origin_compare = origin_year2
    origin_compare$rca = ifelse(origin_year2$rca > origin_year1$rca, origin_year2$rca, origin_year1$rca)
    origin_compare$group2 = ifelse(!(origin_year2$product_id %in% origin_year1_non_null$product_id), year2, year1)
    origin_compare$group2 = ifelse(origin_year2$rca > origin_year1$rca, year2, year1)
    origin_compare$color2 = ifelse(origin_compare$group2 == year2, "#4169e1", "#e1b941")
    origin_compare$rca = ifelse(origin_compare$rca == 0, NA, origin_compare$rca)
    origin_compare$export_val = ifelse(origin_compare$export_val == 0, NA, origin_compare$export_val)

    write(toJSON(origin_compare, pretty = TRUE), file=paste0(input, ".json"))
    envir = as.environment(1)
    assign(paste0(origin, "_", destination, "_", year1, "_", year2, "_", classification, "char"), origin_compare, envir = envir)
  } else {
    print("year2 must be greater than year1")
  }

  code_lenght = classification
  if(code_lenght == 4) {
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
  if(code_lenght == 6) {
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
    if(code_lenght == 4 | code_lenght == 6) {
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
          network_compare_template = gsub("json_file", paste0(output, ".json"), network_compare_template)
          network_compare_template = gsub("edges_file", edges, network_compare_template)
          network_compare_template = gsub("nodes_file", nodes, network_compare_template)
          network_compare_template = gsub("code_display", code_display, network_compare_template)
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
    print('network.compare() only admits 4 characters codes (SITC) or 6 characters codes (HS92). This method only allows "exports".')
  }
}
