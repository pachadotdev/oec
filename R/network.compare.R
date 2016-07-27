#' Creates a network to compare two years
#' @export
#' @return Creates an \code{HTML} file with a network visualization that compares two given years.
#' @param ORIGIN is the country code of origin (e.g. "chl" for Chile)
#' @param DESTINATION is the country code of origin (e.g. "chn" for China)
#' @param CLASSIFICATION refers to the trade classification that can be "6" (HS92 6 characters) or "8" (HS92 8 characters) for the year 1995 and going or "4" (SITC rev.2 4 characters) for the year 1962 and ongoing
#' @param YEAR1 is the initial year and the OEC's API ranges from 1962 to 2014
#' @param YEAR2 is the final year and the OEC's API ranges from 1962 to 2014
#' @examples
#' network.compare("chl", "chn", 6, 2000, 2010)
#' @keywords functions

network.compare <- function(ORIGIN, DESTINATION, CLASSIFICATION, YEAR1, YEAR2) {
  d3_folder <- paste0(getwd(),"/d3plus")
  if(!file.exists(d3_folder)){
    print("d3plus not installed... installing using d3plus()...")
    d3plus()
  }

  #ORIGIN = deparse(substitute(ORIGIN))
  #DESTINATION = deparse(substitute(DESTINATION))
  VARIABLE = "exports"
  #GROUPED = deparse(substitute(GROUPED))

  INPUT = paste(ORIGIN,DESTINATION,YEAR1,YEAR2,CLASSIFICATION, sep="_")
  INPUT = paste0(INPUT,"char")

  if(YEAR2 > YEAR1) {
    print(paste0("Processing files for the year ",YEAR1,"..."))
    getdata(ORIGIN, DESTINATION, CLASSIFICATION, YEAR1)
    print(paste0("Processing files for the year ",YEAR2,"..."))
    getdata(ORIGIN, DESTINATION, CLASSIFICATION, YEAR2)

    YEAR1_file = paste(ORIGIN,DESTINATION,YEAR1,CLASSIFICATION, sep="_")
    YEAR2_file = paste(ORIGIN,DESTINATION,YEAR2,CLASSIFICATION, sep="_")
    YEAR1_file = paste0(YEAR1_file,"char.json")
    YEAR2_file = paste0(YEAR2_file,"char.json")

    ORIGIN_YEAR1 <- as.data.frame(fromJSON(YEAR1_file))
    ORIGIN_YEAR2 <- as.data.frame(fromJSON(YEAR2_file))

    ORIGIN_YEAR1$rca = ifelse(is.na(ORIGIN_YEAR1$rca), 0, ORIGIN_YEAR1$rca)
    ORIGIN_YEAR2$rca = ifelse(is.na(ORIGIN_YEAR2$rca), 0, ORIGIN_YEAR2$rca)

    ORIGIN_YEAR1$export_val = ifelse(is.na(ORIGIN_YEAR1$export_val), 0, ORIGIN_YEAR1$export_val)
    ORIGIN_YEAR1_non_null = ORIGIN_YEAR1[ORIGIN_YEAR1$export_val >0, ]

    ORIGIN_compare = ORIGIN_YEAR2
    ORIGIN_compare$rca = ifelse(ORIGIN_YEAR2$rca > ORIGIN_YEAR1$rca, ORIGIN_YEAR2$rca, ORIGIN_YEAR1$rca)
    ORIGIN_compare$group2 = ifelse(!(ORIGIN_YEAR2$product_id %in% ORIGIN_YEAR1_non_null$product_id), YEAR2, YEAR1)
    ORIGIN_compare$group2 = ifelse(ORIGIN_YEAR2$rca > ORIGIN_YEAR1$rca, YEAR2, YEAR1)
    ORIGIN_compare$color2 = ifelse(ORIGIN_compare$group2 == YEAR2, "#4169e1", "#e1b941")
    ORIGIN_compare$rca = ifelse(ORIGIN_compare$rca == 0, NA, ORIGIN_compare$rca)
    ORIGIN_compare$export_val = ifelse(ORIGIN_compare$export_val == 0, NA, ORIGIN_compare$export_val)

    write(toJSON(ORIGIN_compare, pretty = TRUE), file=paste0(INPUT,".json"))
    envir = as.environment(1)
    assign(paste0(ORIGIN,"_",DESTINATION,"_",YEAR1,"_",YEAR2,"_",CLASSIFICATION,"char"), ORIGIN_compare, envir = envir)
  } else {
    print("YEAR2 must be greater than YEAR1")
  }

  code_lenght = CLASSIFICATION
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
  if(VARIABLE == "exports") {
    if(code_lenght == 4 | code_lenght == 6) {
      variablecol <- ifelse(VARIABLE == "imports", "import_val",
                            ifelse(VARIABLE == "exports", "export_val",
                                   ifelse(VARIABLE == "exchange", "trade_exchange_val", "error")))
      variablename <- ifelse(VARIABLE == "imports", "Import",
                             ifelse(VARIABLE == "exports", "Export",
                                    ifelse(VARIABLE == "exchange", "Trade exchange", "error")))

      json_file <- paste0(INPUT,".json")
      if(!file.exists(json_file)){
        print("json file not found. run getdata() first")
      } else {
        ### html ###
        OUTPUT = INPUT
        html_file <- paste0(OUTPUT, "_network_exports", ".html")
        if(!file.exists(html_file)){
          print("creating network")
          network_compare_template <- paste(readLines(system.file("extdata", "network_compare_template.html", package = "oec"), warn = F), collapse = "\n")
          network_compare_template = gsub("json_file", paste0(OUTPUT, ".json"), network_compare_template)
          network_compare_template = gsub("edges_file", edges, network_compare_template)
          network_compare_template = gsub("nodes_file", nodes, network_compare_template)
          network_compare_template = gsub("code_display", code_display, network_compare_template)
          print("writing html file...")
          writeLines(network_compare_template, paste0(OUTPUT, "_network_exports", ".html"))
          print("opening html files in the browser.")
          httw(pattern = NULL, daemon = TRUE)
        } else {
          print("html treemap file already exists. skipping.")
          print("opening html files in the browser.")
          httw(pattern = NULL, daemon = TRUE)
        }
      }
    }
  } else {
    print('Network only admits 4 characters codes (SITC) or 6 characters codes (HS92). This method only allows "exports".')
  }
}
