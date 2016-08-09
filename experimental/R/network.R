#' Creates a network for a given year
#' @export
#' @return Creates an \code{HTML} file with a network visualization for a given year.
#' @param origin is the country code of origin (e.g. "chl" for Chile)
#' @param destination is the country code of origin (e.g. "chn" for China)
#' @param classification refers to the trade classification that can be "6" (HS92 6 characters) or "8" (HS92 8 characters) for the year 1995 and going or "4" (SITC rev.2 4 characters) for the year 1962 and ongoing
#' @param year is the year and the OEC's API ranges from 1962 to 2014
#' @examples
#' network("chl", "chn", 6, 2014)
#' @keywords functions

network <- function(origin, destination, classification, year) {
  d3_folder <- paste0(getwd(), "/d3plus")
  if(!file.exists(d3_folder)){
  print("D3plus not installed... installing using install_d3plus()...")
  install_d3plus()
  }

  variable <- "exports"

  input <- paste(origin, destination, year, classification, sep="_")
  input <- paste0(input, "char")

  getdata(origin, destination, classification, year)

  code_lenght <- classification
  if(code_lenght == 4) {
    code_display <- "SITC code"
    edges <- "edges_sitc.json"
    nodes <- "nodes_sitc.json"

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
    code_display <- "HS92 code"
    edges <- "edges_hs.json"
    nodes <- "nodes_hs.json"

    if(!file.exists(edges) & !file.exists(nodes)) {
    ### nodes ###
    print("creating HS92 nodes...")
    file.copy(from = system.file("extdata", "nodes_hs.json", package = "oec"), to = getwd())
    ### edges ###
    print("creating HS92 edges...")
    file.copy(from = system.file("extdata", "edges_hs.json", package = "oec"), to = getwd())
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
      output <- input
      html_file <- paste0(output, "_network_exports", ".html")
      if(!file.exists(html_file)){
      print("creating network")
      network_template <- paste(readLines(system.file("extdata", "network_template.html", package = "oec"), warn = F), collapse = "\n")
      network_template <- gsub("json_file", paste0(output, ".json"), network_template)
      network_template <- gsub("edges_file", edges, network_template)
      network_template <- gsub("nodes_file", nodes, network_template)
      network_template <- gsub("code_display", code_display, network_template)
      print("writing html file...")
      writeLines(network_template, paste0(output, "_network_exports", ".html"))
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
    print('network() only admits 4 characters codes (SITC) or 6 characters codes (HS92). This method only allows "exports".')
  }
}
