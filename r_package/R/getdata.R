#' Downloads and processes the data from the API
#' @export
#' @param origin Country code of origin (e.g. "chl" for Chile)
#' @param destination Country code of destination (e.g. "chn" for China)
#' @param classification Trade classification that can be "1" (HS92 4 characters since year 1995), "2" (SITC rev.2 4 characters since year 1962) or "3" (HS92 6 characters since year 1995)
#' @param year The OEC's API ranges from 1962 to 2014
#' @import curl jsonlite plyr servr
#' @importFrom utils write.csv
#' @examples
#' # Run countries_list() to display the full list of countries
#' # Chile is "chl" and China is "chn"
#'
#' # Download trade data from OEC's API (HS92 4 characters product list)
#' # for Chile and China in the year 2014
#' # getdata("chl", "chn", 2014)
#' # is the same as
#' # getdata("chl", "chn", 2014, 1)
#'
#' # Download trade data from OEC's API (SITC rev.2 4 characters product list)
#' # for Chile and China in the year 2014
#' # getdata("chl", "chn", 2014, 2)
#'
#' # Download trade data from OEC's API (HS92 6 characters product list)
#' # for Chile and China in the year 2014
#' # getdata("chl", "chn", 2014, 3)
#' @keywords functions

getdata <- function(origin, destination, year, classification) {

  countries_list <-  oec::countries_list

  if(missing(classification)) {classification = 1}

  output <- paste(origin, destination, year, sep = "_")

  arrange_cols <- function(data, vars){
    stopifnot(is.data.frame(data))

    data.nms <- names(data)
    var.nr <- length(data.nms)
    var.nms <- names(vars)
    var.pos <- vars

    stopifnot(!any(duplicated(var.nms)),
              !any(duplicated(var.pos)))
    stopifnot(is.character(var.nms),
              is.numeric(var.pos))
    stopifnot(all(var.nms %in% data.nms))
    stopifnot(all(var.pos > 0),
              all(var.pos <= var.nr))

    out.vec <- character(var.nr)
    out.vec[var.pos] <- var.nms
    out.vec[-var.pos] <- data.nms[ !(data.nms %in% var.nms) ]
    stopifnot( length(out.vec)==var.nr )

    data <- data[ , out.vec]
    return(data)
  }

  if(origin %in% countries_list$country_code & destination %in% countries_list$country_code){
    print("Valid country codes. Proceeding...")
  } else {
    print("Error. Invalid country codes, see 'countries_list'.")
    stop()
  }

  if(year < 1961 | year > 2014) {
    print("The data is only available from 1962 to 2014.")
    stop()
  } else {
    if((classification == 1 | classification == 3) & year < 1995) {
      print("HS92 classification is only available from the year 1995 and ongoing.")
      stop()
    } else {
      if(classification == 1 | classification == 2 | classification == 3){
        if(classification == 1){
          classification <- "hs"
          characters <- 4
          print("Using HS92 classification (4 characters)...")
        }
        if(classification == 2){
          classification <- "sitc"
          characters <- 4
          print("Using SITC rev.2 classification (4 characters)...")
        }
        if(classification == 3) {
          classification <- "hs"
          characters <- 6
          print("Using HS92 classification (6 characters)")
        }
      }

      if(classification == "sitc" | classification == "hs") {
        if(classification == "sitc") {
          if(characters == 4) {
            csv_file_4char <- paste0(output,"_4char_sitc_rev2.csv")
            json_file_4char <- paste0(output,"_4char_sitc_rev2.json")
            if(!file.exists(csv_file_4char) | !file.exists(json_file_4char)) {
              print("Processing SITC rev.2 (4 characters) files...")

              origin_destination_year_4char <- as.data.frame(fromJSON(paste0("http://atlas.media.mit.edu/sitc/export/", year, "/", origin, "/", destination, "/show/")))
              if(destination == "all"){origin_destination_year_4char$data.dest_id <- "rest_of_the_world"}
              if(origin == "all"){origin_destination_year_4char$data.origin_id <- "rest_of_the_world"}
              keep <- names(origin_destination_year_4char) %in% c("data.year", "data.origin_id","data.dest_id","data.sitc_id", "data.export_val", "data.import_val")
              origin_destination_year_4char <- origin_destination_year_4char[keep]
              origin_destination_year_4char <- arrange_cols(origin_destination_year_4char, c("data.year" = 1, "data.origin_id" = 3, "data.dest_id" = 2, "data.sitc_id" = 4, "data.export_val" = 5, "data.import_val" = 6))
              names(origin_destination_year_4char)[names(origin_destination_year_4char)] <- c("year", "origin_id", "destination_id", "sitc_rev2_product_id", "export_val", "import_val")
              origin_destination_year_4char$trade_exchange_val <- rowSums(origin_destination_year_4char[, c("export_val", "import_val")], na.rm=T)
              origin_destination_year_4char$sitc_rev2_len <- nchar(origin_destination_year_4char$sitc_rev2_product_id)
              origin_destination_year_4char <- subset(origin_destination_year_4char, origin_destination_year_4char$sitc_rev2_len == "6")
              origin_destination_year_4char$sitc_rev2_product_id <- substr(origin_destination_year_4char$sitc_rev2_product_id, 3,6)
              drop <- names(origin_destination_year_4char) %in% c("sitc_rev2_len")
              origin_destination_year_4char <- origin_destination_year_4char[!drop]

              all_all_year_4char <- as.data.frame(fromJSON(paste0("http://atlas.media.mit.edu/sitc/export/", year, "/all/all/show/")))
              keep <- names(all_all_year_4char) %in% c("data.export_val","data.sitc_id","data.top_importer","data.top_exporter")
              all_all_year_4char <- all_all_year_4char[keep]
              all_all_year_4char <- arrange_cols(all_all_year_4char, c("data.sitc_id" = 1))
              names(all_all_year_4char)[names(all_all_year_4char)] <- c("sitc_rev2_product_id","world_total_export_val","top_exporter_code","top_importer_code")
              all_all_year_4char$sitc_rev2_len <- nchar(all_all_year_4char$sitc_rev2_product_id)
              all_all_year_4char <- subset(all_all_year_4char, all_all_year_4char$sitc_rev2_len == "6")
              all_all_year_4char$sitc_rev2_product_id <- substr(all_all_year_4char$sitc_rev2_product_id, 3,6)
              drop <- names(all_all_year_4char) %in% c("sitc_rev2_len")
              all_all_year_4char <- all_all_year_4char[!drop]

              origin_all_year_4char <- as.data.frame(fromJSON(paste0("http://atlas.media.mit.edu/sitc/export/", year, "/", origin, "/all/show/")))
              keep <- names(origin_all_year_4char) %in% c("data.export_val","data.sitc_id")
              origin_all_year_4char <- origin_all_year_4char[keep]
              origin_all_year_4char <- arrange_cols(origin_all_year_4char, c("data.sitc_id" = 1))
              names(origin_all_year_4char)[names(origin_all_year_4char)] <- c("sitc_rev2_product_id","origin_total_export_val")
              origin_all_year_4char$sitc_rev2_len <- nchar(origin_all_year_4char$sitc_rev2_product_id)
              origin_all_year_4char <- subset(origin_all_year_4char, origin_all_year_4char$sitc_rev2_len == "6")
              origin_all_year_4char$sitc_rev2_product_id <- substr(origin_all_year_4char$sitc_rev2_product_id, 3,6)
              drop <- names(origin_all_year_4char) %in% c("sitc_rev2_len")
              origin_all_year_4char <- origin_all_year_4char[!drop]

              origin_destination_year_4char <- join(origin_destination_year_4char, all_all_year_4char, by = "sitc_rev2_product_id")
              origin_destination_year_4char <- join(origin_destination_year_4char, origin_all_year_4char, by = "sitc_rev2_product_id")
              rm(all_all_year_4char,origin_all_year_4char)
              origin_destination_year_4char$rca <- (origin_destination_year_4char$origin_total_export_val/sum(origin_destination_year_4char$origin_total_export_val, na.rm=TRUE))/(origin_destination_year_4char$world_total_export_val/sum(origin_destination_year_4char$world_total_export_val, na.rm=TRUE))
              origin_destination_year_4char$rca <- round(origin_destination_year_4char$rca, digits = 3)

              sitc_rev2_4char <- sitc_rev2_4char
              sitc_rev2_colors <- sitc_rev2_colors
              sitc_rev2_4char <- join(sitc_rev2_4char,sitc_rev2_colors, by="sitc_rev2_group_name")
              origin_destination_year_4char <- join(sitc_rev2_4char, origin_destination_year_4char, by = "sitc_rev2_product_id")
              rm(sitc_rev2_4char,sitc_rev2_colors)

              origin_destination_year_4char$sitc_rev2_icon <- paste0("d3plus-1.9.8/icons/sitc_rev2/sitc_rev2_", origin_destination_year_4char$sitc_rev2_group_id, ".png")
              origin_destination_year_4char <- arrange_cols(origin_destination_year_4char, c("year" = 1, "origin_id" = 3, "destination_id" = 2, "origin_total_export_val" = 12, "rca" = 15))
              origin_destination_year_4char$year <- ifelse(is.na(origin_destination_year_4char$year),year,origin_destination_year_4char$year)

              drop <- names(origin_destination_year_4char) %in% c("sitc_rev2_product_code")
              origin_destination_year_4char <- origin_destination_year_4char[!drop]

              names(countries_list)[names(countries_list)] <- c("top_importer","top_importer_code")
              origin_destination_year_4char$top_importer_code <- substr(origin_destination_year_4char$top_importer_code, 3, 5)
              origin_destination_year_4char <- join(origin_destination_year_4char, countries_list, by="top_importer_code")

              names(countries_list)[names(countries_list)] <- c("top_exporter","top_exporter_code")
              origin_destination_year_4char$top_exporter_code <- substr(origin_destination_year_4char$top_exporter_code, 3, 5)
              origin_destination_year_4char <- join(origin_destination_year_4char, countries_list, by="top_exporter_code")

              drop <- names(origin_destination_year_4char) %in% c("top_exporter_code","top_importer_code")
              origin_destination_year_4char <- origin_destination_year_4char[!drop]

              names(countries_list)[names(countries_list)] <- c("country","country_code")

              envir = as.environment(1)
              assign(paste0(origin, "_", destination, "_", year, "_4char_sitc_rev2"), origin_destination_year_4char, envir = envir)

              print("Writing SITC rev.2 (4 characters) JSON and CSV files...")
              write.csv(origin_destination_year_4char, paste0(output,"_4char_sitc_rev2.csv"))
              jsonOut_4char <- toJSON(origin_destination_year_4char, pretty = TRUE)
              write(jsonOut_4char, file=paste0(output,"_4char_sitc_rev2.json"))
            } else {
              envir = as.environment(1)
              print("The file you want to download is in the working folder. Reading JSON...")
              assign(paste(origin, destination, year, paste0(characters,"char_sitc_rev2"), sep = "_"), fromJSON(json_file_4char), envir = envir)
            }
          }
        }

        if(classification == "hs") {
          if(characters == 4 | characters == 6) {
            if(characters == 4) {
              csv_file_4char <- paste0(output,"_4char_hs92.csv")
              json_file_4char <- paste0(output,"_4char_hs92.json")
              if(!file.exists(csv_file_4char) | !file.exists(json_file_4char)) {
                print("Processing HS92 (4 characters) files...")

                origin_destination_year_4char <- as.data.frame(fromJSON(paste0("http://atlas.media.mit.edu/hs92/export/", year, "/", origin, "/", destination, "/show/")))
                if(destination == "all"){origin_destination_year_4char$data.dest_id <- "rest_of_the_world"}
                if(origin == "all"){origin_destination_year_4char$data.origin_id <- "rest_of_the_world"}
                keep <- names(origin_destination_year_4char) %in% c("data.year", "data.origin_id","data.dest_id","data.hs92_id", "data.export_val", "data.import_val")
                origin_destination_year_4char <- origin_destination_year_4char[keep]
                origin_destination_year_4char <- arrange_cols(origin_destination_year_4char, c("data.year" = 1, "data.origin_id" = 2, "data.dest_id" = 3, "data.hs92_id" = 4, "data.export_val" = 5, "data.import_val" = 6))
                names(origin_destination_year_4char)[names(origin_destination_year_4char)] <- c("year", "origin_id", "destination_id", "hs92_product_id", "export_val", "import_val")
                origin_destination_year_4char$trade_exchange_val <- rowSums(origin_destination_year_4char[, c("export_val", "import_val")], na.rm=T)
                origin_destination_year_4char$hs92_len <- nchar(origin_destination_year_4char$hs92_product_id)
                origin_destination_year_4char <- subset(origin_destination_year_4char, origin_destination_year_4char$hs92_len == "6")
                origin_destination_year_4char$hs92_product_id <- substr(origin_destination_year_4char$hs92_product_id, 3,6)
                drop <- names(origin_destination_year_4char) %in% c("hs92_len")
                origin_destination_year_4char <- origin_destination_year_4char[!drop]

                all_all_year_4char <- as.data.frame(fromJSON(paste0("http://atlas.media.mit.edu/hs92/export/", year, "/all/all/show/")))
                keep <- names(all_all_year_4char) %in% c("data.export_val","data.hs92_id","data.pci","data.pci_rank","data.top_importer","data.top_exporter")
                all_all_year_4char <- all_all_year_4char[keep]
                all_all_year_4char <- arrange_cols(all_all_year_4char, c("data.hs92_id" = 1))
                names(all_all_year_4char)[names(all_all_year_4char)] <- c("hs92_product_id","world_total_export_val","pci","pci_rank","top_exporter_code","top_importer_code")
                all_all_year_4char$hs92_len <- nchar(all_all_year_4char$hs92_product_id)
                all_all_year_4char <- subset(all_all_year_4char, all_all_year_4char$hs92_len == "6")
                all_all_year_4char$hs92_product_id <- substr(all_all_year_4char$hs92_product_id, 3,6)
                drop <- names(all_all_year_4char) %in% c("hs92_len")
                all_all_year_4char <- all_all_year_4char[!drop]

                origin_all_year_4char <- as.data.frame(fromJSON(paste0("http://atlas.media.mit.edu/hs92/export/", year, "/", origin, "/all/show/")))
                keep <- names(origin_all_year_4char) %in% c("data.export_val","data.hs92_id")
                origin_all_year_4char <- origin_all_year_4char[keep]
                origin_all_year_4char <- arrange_cols(origin_all_year_4char, c("data.hs92_id" = 1))
                names(origin_all_year_4char)[names(origin_all_year_4char)] <- c("hs92_product_id","origin_total_export_val")
                origin_all_year_4char$hs92_len <- nchar(origin_all_year_4char$hs92_product_id)
                origin_all_year_4char <- subset(origin_all_year_4char, origin_all_year_4char$hs92_len == "6")
                origin_all_year_4char$hs92_product_id <- substr(origin_all_year_4char$hs92_product_id, 3,6)
                drop <- names(origin_all_year_4char) %in% c("hs92_len")
                origin_all_year_4char <- origin_all_year_4char[!drop]

                origin_destination_year_4char <- join(origin_destination_year_4char, all_all_year_4char, by = "hs92_product_id")
                origin_destination_year_4char <- join(origin_destination_year_4char, origin_all_year_4char, by = "hs92_product_id")
                rm(all_all_year_4char,origin_all_year_4char)
                origin_destination_year_4char$rca <- (origin_destination_year_4char$export_val/origin_destination_year_4char$world_total_export_val)/(sum(origin_destination_year_4char$origin_total_export_val, na.rm=TRUE)/sum(origin_destination_year_4char$world_total_export_val, na.rm=TRUE))
                origin_destination_year_4char$rca <- round(origin_destination_year_4char$rca, digits = 3)

                hs92_4char <- hs92_4char
                hs92_colors <- hs92_colors
                hs92_4char <- join(hs92_4char,hs92_colors, by="hs92_group_name")
                origin_destination_year_4char <- join(hs92_4char, origin_destination_year_4char, by = "hs92_product_id")
                rm(hs92_4char,hs92_colors)

                origin_destination_year_4char$hs92_icon <- paste0("d3plus-1.9.8/icons/hs92/hs92_", origin_destination_year_4char$hs92_group_id, ".png")
                origin_destination_year_4char <- arrange_cols(origin_destination_year_4char, c("year" = 1, "origin_id" = 2, "destination_id" = 3, "origin_total_export_val" = 12, "rca" = 15))
                origin_destination_year_4char$year <- ifelse(is.na(origin_destination_year_4char$year),year,origin_destination_year_4char$year)

                names(countries_list)[names(countries_list)] <- c("top_importer","top_importer_code")
                origin_destination_year_4char$top_importer_code <- substr(origin_destination_year_4char$top_importer_code, 3, 5)
                origin_destination_year_4char <- join(origin_destination_year_4char, countries_list, by="top_importer_code")

                names(countries_list)[names(countries_list)] <- c("top_exporter","top_exporter_code")
                origin_destination_year_4char$top_exporter_code <- substr(origin_destination_year_4char$top_exporter_code, 3, 5)
                origin_destination_year_4char <- join(origin_destination_year_4char, countries_list, by="top_exporter_code")

                drop <- names(origin_destination_year_4char) %in% c("top_exporter_code","top_importer_code")
                origin_destination_year_4char <- origin_destination_year_4char[!drop]

                names(countries_list)[names(countries_list)] <- c("country","country_code")

                envir = as.environment(1)
                assign(paste0(origin, "_", destination, "_", year, "_4char_hs92"), origin_destination_year_4char, envir = envir)

                print("writing HS92 (4 characters) json and csv files...")
                write.csv(origin_destination_year_4char, paste0(output,"_4char_hs92.csv"))
                jsonOut_4char <- toJSON(origin_destination_year_4char, pretty = TRUE)
                write(jsonOut_4char, file=paste0(output,"_4char_hs92.json"))
              } else {
                envir = as.environment(1)
                print("The file you want to download is in the working folder. Reading JSON...")
                assign(paste(origin, destination, year, paste0(characters,"char_hs92"), sep = "_"), fromJSON(json_file_4char), envir = envir)
              }
            }
            if(characters == 6) {
              csv_file_6char <- paste0(output,"_6char_hs92.csv")
              json_file_6char <- paste0(output,"_6char_hs92.json")
              if(!file.exists(csv_file_6char) | !file.exists(json_file_6char)) {
                print("Processing HS92 (6 characters) files...")

                origin_destination_year_6char <- as.data.frame(fromJSON(paste0("http://atlas.media.mit.edu/hs92/export/", year, "/", origin, "/", destination, "/show/")))
                if(destination == "all"){origin_destination_year_6char$data.dest_id <- "rest_of_the_world"}
                if(origin == "all"){origin_destination_year_6char$data.origin_id <- "rest_of_the_world"}
                keep <- names(origin_destination_year_6char) %in% c("data.year", "data.origin_id","data.dest_id","data.hs92_id", "data.export_val", "data.import_val")
                origin_destination_year_6char <- origin_destination_year_6char[keep]
                origin_destination_year_6char <- arrange_cols(origin_destination_year_6char, c("data.year" = 1, "data.origin_id" = 2, "data.dest_id" = 3, "data.hs92_id" = 4, "data.export_val" = 5, "data.import_val" = 6))
                names(origin_destination_year_6char)[names(origin_destination_year_6char)] <- c("year", "origin_id", "destination_id", "hs92_product_id", "export_val", "import_val")
                origin_destination_year_6char$trade_exchange_val <- rowSums(origin_destination_year_6char[, c("export_val", "import_val")], na.rm=T)
                origin_destination_year_6char$hs92_len <- nchar(origin_destination_year_6char$hs92_product_id)
                origin_destination_year_6char <- subset(origin_destination_year_6char, origin_destination_year_6char$hs92_len == "8")
                origin_destination_year_6char$hs92_product_id <- substr(origin_destination_year_6char$hs92_product_id, 3,8)
                drop <- names(origin_destination_year_6char) %in% c("hs92_len")
                origin_destination_year_6char <- origin_destination_year_6char[!drop]

                all_all_year_6char <- as.data.frame(fromJSON(paste0("http://atlas.media.mit.edu/hs92/export/", year, "/all/all/show/")))
                keep <- names(all_all_year_6char) %in% c("data.export_val","data.hs92_id","data.pci","data.pci_rank","data.top_importer","data.top_exporter")
                all_all_year_6char <- all_all_year_6char[keep]
                all_all_year_6char <- arrange_cols(all_all_year_6char, c("data.hs92_id" = 1))
                names(all_all_year_6char)[names(all_all_year_6char)] <- c("hs92_product_id","world_total_export_val","pci","pci_rank","top_exporter_code","top_importer_code")
                all_all_year_6char$hs92_len <- nchar(all_all_year_6char$hs92_product_id)
                all_all_year_6char <- subset(all_all_year_6char, all_all_year_6char$hs92_len == "8")
                all_all_year_6char$hs92_product_id <- substr(all_all_year_6char$hs92_product_id, 3,8)
                drop <- names(all_all_year_6char) %in% c("hs92_len")
                all_all_year_6char <- all_all_year_6char[!drop]

                origin_all_year_6char <- as.data.frame(fromJSON(paste0("http://atlas.media.mit.edu/hs92/export/", year, "/", origin, "/all/show/")))
                keep <- names(origin_all_year_6char) %in% c("data.export_val","data.hs92_id")
                origin_all_year_6char <- origin_all_year_6char[keep]
                origin_all_year_6char <- arrange_cols(origin_all_year_6char, c("data.hs92_id" = 1))
                names(origin_all_year_6char)[names(origin_all_year_6char)] <- c("hs92_product_id","origin_total_export_val")
                origin_all_year_6char$hs92_len <- nchar(origin_all_year_6char$hs92_product_id)
                origin_all_year_6char <- subset(origin_all_year_6char, origin_all_year_6char$hs92_len == "8")
                origin_all_year_6char$hs92_product_id <- substr(origin_all_year_6char$hs92_product_id, 3,8)
                drop <- names(origin_all_year_6char) %in% c("hs92_len")
                origin_all_year_6char <- origin_all_year_6char[!drop]

                origin_destination_year_6char <- join(origin_destination_year_6char, all_all_year_6char, by = "hs92_product_id")
                origin_destination_year_6char <- join(origin_destination_year_6char, origin_all_year_6char, by = "hs92_product_id")
                rm(all_all_year_6char,origin_all_year_6char)
                origin_destination_year_6char$rca <- (origin_destination_year_6char$export_val/origin_destination_year_6char$world_total_export_val)/(sum(origin_destination_year_6char$origin_total_export_val, na.rm=TRUE)/sum(origin_destination_year_6char$world_total_export_val, na.rm=TRUE))
                origin_destination_year_6char$rca <- round(origin_destination_year_6char$rca, digits = 3)

                hs92_6char <- hs92_6char
                hs92_colors <- hs92_colors
                hs92_6char <- join(hs92_6char,hs92_colors, by="hs92_group_name")
                origin_destination_year_6char <- join(hs92_6char, origin_destination_year_6char, by = "hs92_product_id")
                rm(hs92_6char,hs92_colors)

                origin_destination_year_6char$hs92_icon <- paste0("d3plus-1.9.8/icons/hs92/hs92_", origin_destination_year_6char$hs92_group_id, ".png")
                origin_destination_year_6char <- arrange_cols(origin_destination_year_6char, c("year" = 1, "origin_id" = 3, "destination_id" = 2, "origin_total_export_val" = 12, "rca" = 15))
                origin_destination_year_6char$year <- ifelse(is.na(origin_destination_year_6char$year),year,origin_destination_year_6char$year)

                names(countries_list)[names(countries_list)] <- c("top_importer","top_importer_code")
                origin_destination_year_6char$top_importer_code <- substr(origin_destination_year_6char$top_importer_code, 3, 5)
                origin_destination_year_6char <- join(origin_destination_year_6char, countries_list, by="top_importer_code")

                names(countries_list)[names(countries_list)] <- c("top_exporter","top_exporter_code")
                origin_destination_year_6char$top_exporter_code <- substr(origin_destination_year_6char$top_exporter_code, 3, 5)
                origin_destination_year_6char <- join(origin_destination_year_6char, countries_list, by="top_exporter_code")

                drop <- names(origin_destination_year_6char) %in% c("top_exporter_code","top_importer_code")
                origin_destination_year_6char <- origin_destination_year_6char[!drop]

                names(countries_list)[names(countries_list)] <- c("country","country_code")

                envir = as.environment(1)
                assign(paste0(origin, "_", destination, "_", year, "_6char_hs92"), origin_destination_year_6char, envir = envir)

                print("Writing HS92 (6 characters) json and csv files...")
                write.csv(origin_destination_year_6char, paste0(output,"_6char_hs92.csv"))
                jsonOut_6char <- toJSON(origin_destination_year_6char, pretty = TRUE)
                write(jsonOut_6char, file=paste0(output,"_6char_hs92.json"))
              } else {
                envir = as.environment(1)
                print("The file you want to download is in the working folder. Reading JSON...")
                assign(paste(origin, destination, year, paste0(characters,"char_hs92"), sep = "_"), fromJSON(json_file_6char), envir = envir)
              }
            }
          }
        }
      } else {
        print('Error. The allowed classifications can be "1" (HS92 4 characters) or "3" (HS92 6 characters) for the year 1995 and going or "2" (SITC rev.2 4 characters) for the year 1962 and ongoing.')
        stop()
      }
    }
  }
}
