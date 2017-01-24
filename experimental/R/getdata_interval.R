#' Downloads and processes the data from the API
#' @export
#' @param origin is the country code of origin (e.g. "chl" for Chile)
#' @param destination is the country code of origin (e.g. "chn" for China)
#' @param classification refers to the trade classification that can be "6" (HS92 6 characters) or "8" (HS92 8 characters) for the year 1995 and going or "4" (SITC rev.2 4 characters) for the year 1962 and ongoing
#' @param first_year is the first year and the OEC's API ranges from 1962 to 2014
#' @param last_year is the last year and the OEC's API ranges from 1962 to 2014
#' @param interval is an optional parameter to define the distance between years (by default set to 1)
#' @import curl data.table jsonlite plyr servr
#' @importFrom utils write.csv
#' @examples
#' # Run countries_list() to display the full list of countries
#' # Chile is "chl" and China is "chn"
#'
#' # Download trade data from OEC's API (HS92 6 characters product lists)
#' # for the years 2010, 2012 and 2014
#' # getdata_interval("chl", "chn", 6, 2010, 2014, 2)
#'
#' # Download trade data from OEC's API (HS92 8 characters product lists)
#' # for the years 2010, 2012 and 2014
#' # getdata_interval("chl", "chn", 8, 2010, 2014, 2)
#'
#' # Download trade data from OEC's API (SITC rev.2 4 characters product lists)
#' # for the years 2010, 2012 and 2014
#' # getdata_interval("chl", "chn", 4, 2010, 2014, 2)
#' @keywords functions

getdata_interval <- function(origin, destination, classification, first_year, last_year, interval) {

  if(missing(interval)) {
    interval = 1
  }

  output <- paste(origin, destination, first_year, last_year, interval, sep = "_")

  arrange.vars <- function(data, vars){
    stopifnot(is.data.frame(data))

    data.nms <- names(data)
    var.nr <- length(data.nms)
    var.nms <- names(vars)
    var.pos <- vars

    stopifnot( !any(duplicated(var.nms)),
               !any(duplicated(var.pos)) )
    stopifnot( is.character(var.nms),
               is.numeric(var.pos) )
    stopifnot( all(var.nms %in% data.nms) )
    stopifnot( all(var.pos > 0),
               all(var.pos <= var.nr) )

    out.vec <- character(var.nr)
    out.vec[var.pos] <- var.nms
    out.vec[-var.pos] <- data.nms[ !(data.nms %in% var.nms) ]
    stopifnot( length(out.vec)==var.nr )

    data <- data[ , out.vec]
    return(data)
  }

  if(first_year > last_year) {
    print("the first year needs to be lower than the last year.")
  } else {
      if(first_year < 1961 | first_year > 2015 | last_year < 1961 | last_year > 2015) {
        print("data is only available from 1962 to 2014.")
      } else {
        if(classification > 4 & first_year < 1995) {
          print("HS92 classification is only available from year 1995 and ongoing.")
        } else {
          if(classification == 4 | classification == 6 | classification == 8){
            if(classification == 4){
              classification <- "sitc"
              characters <- 4
            }
            if(classification == 6){
              classification <- "hs"
              characters <- 6
            }
            if(classification == 8){
              classification <- "hs"
              characters <- 8
            }
          } else {
            print("getdata_interval() error: allowed classifications are hs6, hs8 and sitc.")
          }

          if(classification == "sitc" | classification == "hs") {
            if(classification == "sitc") {
              if(characters == 4) {
                csv_file_4char <- paste0(output,"_4char.csv")
                json_file_4char <- paste0(output,"_4char.json")
                if(!file.exists(csv_file_4char) | !file.exists(json_file_4char)) {
                  print("processing SITC rev.2 (4 characters) files...")

                  or_de_fy_ly_in_4char <- as.data.frame(fromJSON(paste0("http://atlas.media.mit.edu/sitc/export/", first_year, ".", last_year, ".", interval, "/", origin, "/", destination, "/show/")))
                  keep <- names(or_de_fy_ly_in_4char) %in% c("data.year", "data.origin_id","data.dest_id","data.sitc_id", "data.export_val", "data.import_val")
                  or_de_fy_ly_in_4char <- or_de_fy_ly_in_4char[keep]
                  or_de_fy_ly_in_4char <- arrange.vars(or_de_fy_ly_in_4char, c("data.year" = 1, "data.origin_id" = 2, "data.dest_id" = 3, "data.sitc_id" = 4, "data.export_val" = 5))
                  setnames(or_de_fy_ly_in_4char, names(or_de_fy_ly_in_4char), c("year", "origin_id", "destination_id", "sitc_rev2_id", "export_val", "import_val"))
                  or_de_fy_ly_in_4char$trade_exchange_val <- rowSums(or_de_fy_ly_in_4char[, c("export_val", "import_val")], na.rm=T)

                  all_all_fyear_lyear_in_4char <- as.data.frame(fromJSON(paste0("http://atlas.media.mit.edu/sitc/export/", first_year, ".", last_year, ".", interval, "/all/all/show/")))
                  keep <- names(all_all_fyear_lyear_in_4char) %in% c("data.export_val","data.sitc_id","data.pci","data.pci_rank","data.top_importer","data.top_exporter")
                  all_all_fyear_lyear_in_4char <- all_all_fyear_lyear_in_4char[keep]
                  all_all_fyear_lyear_in_4char <- arrange.vars(all_all_fyear_lyear_in_4char, c("data.sitc_id" = 1))
                  setnames(all_all_fyear_lyear_in_4char, names(all_all_fyear_lyear_in_4char), c("sitc_rev2_id","world_total_export_val","pci","pci_rank","top_exporter","top_importer"))

                  origin_all_fyear_lyear_in_4char <- as.data.frame(fromJSON(paste0("http://atlas.media.mit.edu/sitc/export/", first_year, ".", last_year, ".", interval, "/", origin, "/all/show/")))
                  keep <- names(origin_all_fyear_lyear_in_4char) %in% c("data.export_val","data.sitc_id")
                  origin_all_fyear_lyear_in_4char <- origin_all_fyear_lyear_in_4char[keep]
                  origin_all_fyear_lyear_in_4char <- arrange.vars(origin_all_fyear_lyear_in_4char, c("data.sitc_id" = 1))
                  setnames(origin_all_fyear_lyear_in_4char, names(origin_all_fyear_lyear_in_4char), c("sitc_rev2_id","origin_total_export_val"))

                  or_de_fy_ly_in_4char <- join(or_de_fy_ly_in_4char, all_all_fyear_lyear_in_4char, by = "sitc_rev2_id")
                  or_de_fy_ly_in_4char <- join(or_de_fy_ly_in_4char, origin_all_fyear_lyear_in_4char, by = "sitc_rev2_id")
                  rm(all_all_fyear_lyear_in_4char,origin_all_fyear_lyear_in_4char)
                  or_de_fy_ly_in_4char$rca <- (or_de_fy_ly_in_4char$export_val/or_de_fy_ly_in_4char$world_total_export_val)/(sum(or_de_fy_ly_in_4char$origin_total_export_val, na.rm=TRUE)/sum(or_de_fy_ly_in_4char$world_total_export_val, na.rm=TRUE))
                  or_de_fy_ly_in_4char$rca <- round(or_de_fy_ly_in_4char$rca, digits = 2)

                  sitc_rev2_4char <- sitc_rev2_4char
                  or_de_fy_ly_in_4char <- join(sitc_rev2_4char, or_de_fy_ly_in_4char, by = "sitc_rev2_id")
                  or_de_fy_ly_in_4char <- subset(or_de_fy_ly_in_4char, !is.na(or_de_fy_ly_in_4char$year))

                  or_de_fy_ly_in_4char$icon <- paste0("d3plus-1.9.8/icons/sitc_rev2/sitc_rev2_", or_de_fy_ly_in_4char$sitc_rev2_group, ".png")
                  sitc_rev2_colors <- sitc_rev2_colors
                  or_de_fy_ly_in_4char <- merge(or_de_fy_ly_in_4char,sitc_rev2_colors)
                  or_de_fy_ly_in_4char <- subset(or_de_fy_ly_in_4char, !is.na(or_de_fy_ly_in_4char$year))
                  or_de_fy_ly_in_4char <- arrange.vars(or_de_fy_ly_in_4char, c("year" = 1, "origin_id" = 2, "destination_id" = 3, "origin_total_export_val" = 12, "rca" = 15))
                  or_de_fy_ly_in_4char <- or_de_fy_ly_in_4char[order(or_de_fy_ly_in_4char$year), ]

                  envir = as.environment(1)
                  assign(paste0(origin, "_", destination, "_", first_year, "_", last_year, "_", interval, "_4char"), or_de_fy_ly_in_4char, envir = envir)

                  print("writing SITC rev.2 (4 characters) json and csv files...")
                  write.csv(or_de_fy_ly_in_4char, paste0(output,"_4char.csv"))
                  jsonOut_4char <- toJSON(or_de_fy_ly_in_4char, pretty = TRUE)
                  write(jsonOut_4char, file=paste0(output,"_4char.json"))
                } else {
                  print("SITC rev.2 (4 characters) files already exists. skipping.")
                }
              } else {
                print("SITC rev.2 list only allows 4 characters.")
              }
            }

            if(classification == "hs") {
              if(characters == 6 | characters == 8) {
                if(characters == 6) {
                  csv_file_6char <- paste0(output,"_6char.csv")
                  json_file_6char <- paste0(output,"_6char.json")
                  if(!file.exists(csv_file_6char) | !file.exists(json_file_6char)) {
                    print("processing HS92 (6 characters) files...")

                    or_de_fy_ly_in_6char <- as.data.frame(fromJSON(paste0("http://atlas.media.mit.edu/hs92/export/", first_year, ".", last_year, ".", interval, "/", origin, "/", destination, "/show/")))
                    keep <- names(or_de_fy_ly_in_6char) %in% c("data.year", "data.origin_id","data.dest_id","data.hs92_id", "data.export_val", "data.import_val")
                    or_de_fy_ly_in_6char <- or_de_fy_ly_in_6char[keep]
                    or_de_fy_ly_in_6char <- arrange.vars(or_de_fy_ly_in_6char, c("data.year" = 1, "data.origin_id" = 2, "data.dest_id" = 3, "data.hs92_id" = 4, "data.export_val" = 5))
                    setnames(or_de_fy_ly_in_6char, names(or_de_fy_ly_in_6char), c("year", "origin_id", "destination_id", "hs92_id", "export_val", "import_val"))
                    or_de_fy_ly_in_6char$trade_exchange_val <- rowSums(or_de_fy_ly_in_6char[, c("export_val", "import_val")], na.rm=T)
                    or_de_fy_ly_in_6char$hs92_len <- nchar(or_de_fy_ly_in_6char$hs92_id)
                    or_de_fy_ly_in_6char <- subset(or_de_fy_ly_in_6char, or_de_fy_ly_in_6char$hs92_len == "6")
                    drop <- names(or_de_fy_ly_in_6char) %in% c("hs92_len")
                    or_de_fy_ly_in_6char <- or_de_fy_ly_in_6char[!drop]

                    all_all_fyear_lyear_in_6char <- as.data.frame(fromJSON(paste0("http://atlas.media.mit.edu/hs92/export/", first_year, ".", last_year, ".", interval, "/all/all/show/")))
                    keep <- names(all_all_fyear_lyear_in_6char) %in% c("data.export_val","data.hs92_id","data.pci","data.pci_rank","data.top_importer","data.top_exporter")
                    all_all_fyear_lyear_in_6char <- all_all_fyear_lyear_in_6char[keep]
                    all_all_fyear_lyear_in_6char <- arrange.vars(all_all_fyear_lyear_in_6char, c("data.hs92_id" = 1))
                    setnames(all_all_fyear_lyear_in_6char, names(all_all_fyear_lyear_in_6char), c("hs92_id","world_total_export_val","pci","pci_rank","top_exporter","top_importer"))

                    origin_all_fyear_lyear_in_6char <- as.data.frame(fromJSON(paste0("http://atlas.media.mit.edu/hs92/export/", first_year, ".", last_year, ".", interval, "/", origin, "/all/show/")))
                    keep <- names(origin_all_fyear_lyear_in_6char) %in% c("data.export_val","data.hs92_id")
                    origin_all_fyear_lyear_in_6char <- origin_all_fyear_lyear_in_6char[keep]
                    origin_all_fyear_lyear_in_6char <- arrange.vars(origin_all_fyear_lyear_in_6char, c("data.hs92_id" = 1))
                    setnames(origin_all_fyear_lyear_in_6char, names(origin_all_fyear_lyear_in_6char), c("hs92_id","origin_total_export_val"))

                    or_de_fy_ly_in_6char <- join(or_de_fy_ly_in_6char, all_all_fyear_lyear_in_6char, by = "hs92_id")
                    or_de_fy_ly_in_6char <- join(or_de_fy_ly_in_6char, origin_all_fyear_lyear_in_6char, by = "hs92_id")
                    rm(all_all_fyear_lyear_in_6char,origin_all_fyear_lyear_in_6char)
                    or_de_fy_ly_in_6char$rca <- (or_de_fy_ly_in_6char$export_val/or_de_fy_ly_in_6char$world_total_export_val)/(sum(or_de_fy_ly_in_6char$origin_total_export_val, na.rm=TRUE)/sum(or_de_fy_ly_in_6char$world_total_export_val, na.rm=TRUE))
                    or_de_fy_ly_in_6char$rca <- round(or_de_fy_ly_in_6char$rca, digits = 2)

                    hs92_6char <- hs92_6char
                    or_de_fy_ly_in_6char <- join(hs92_6char, or_de_fy_ly_in_6char, by = "hs92_id")
                    or_de_fy_ly_in_6char <- subset(or_de_fy_ly_in_6char, !is.na(or_de_fy_ly_in_6char$year))

                    or_de_fy_ly_in_6char$icon <- paste0("d3plus-1.9.8/icons/hs92/hs92_", or_de_fy_ly_in_6char$hs92_group, ".png")
                    hs92_colors <- hs92_colors
                    or_de_fy_ly_in_6char <- merge(or_de_fy_ly_in_6char,hs92_colors)
                    or_de_fy_ly_in_6char <- subset(or_de_fy_ly_in_6char, !is.na(or_de_fy_ly_in_6char$year))
                    or_de_fy_ly_in_6char <- arrange.vars(or_de_fy_ly_in_6char, c("year" = 1, "origin_id" = 2, "destination_id" = 3, "origin_total_export_val" = 12, "rca" = 15))
                    or_de_fy_ly_in_6char <- or_de_fy_ly_in_6char[order(or_de_fy_ly_in_6char$year), ]

                    envir = as.environment(1)
                    assign(paste0(origin, "_", destination, "_", first_year, "_", last_year, "_", interval, "_6char"), or_de_fy_ly_in_6char, envir = envir)

                    print("writing HS92 (6 characters) json and csv files...")
                    write.csv(or_de_fy_ly_in_6char, paste0(output,"_6char.csv"))
                    jsonOut_6char <- toJSON(or_de_fy_ly_in_6char, pretty = TRUE)
                    write(jsonOut_6char, file=paste0(output,"_6char.json"))
                  } else {
                    print("HS92 (6 characters) files already exists. skipping.")
                  }
                }
                if(characters == 8) {
                  csv_file_8char <- paste0(output,"_8char.csv")
                  json_file_8char <- paste0(output,"_8char.json")
                  if(!file.exists(csv_file_8char) | !file.exists(json_file_8char)) {
                    print("processing HS92 (8 characters) files...")

                    or_de_fy_ly_in_8char <- as.data.frame(fromJSON(paste0("http://atlas.media.mit.edu/hs92/export/", first_year, ".", last_year, ".", interval, "/", origin, "/", destination, "/show/")))
                    keep <- names(or_de_fy_ly_in_8char) %in% c("data.year", "data.origin_id","data.dest_id","data.hs92_id", "data.export_val", "data.import_val")
                    or_de_fy_ly_in_8char <- or_de_fy_ly_in_8char[keep]
                    or_de_fy_ly_in_8char <- arrange.vars(or_de_fy_ly_in_8char, c("data.year" = 1, "data.origin_id" = 2, "data.dest_id" = 3, "data.hs92_id" = 4, "data.export_val" = 5))
                    setnames(or_de_fy_ly_in_8char, names(or_de_fy_ly_in_8char), c("year", "origin_id", "destination_id", "hs92_id", "export_val", "import_val"))
                    or_de_fy_ly_in_8char$trade_exchange_val <- rowSums(or_de_fy_ly_in_8char[, c("export_val", "import_val")], na.rm=T)
                    or_de_fy_ly_in_8char$hs92_len <- nchar(or_de_fy_ly_in_8char$hs92_id)
                    or_de_fy_ly_in_8char <- subset(or_de_fy_ly_in_8char, or_de_fy_ly_in_8char$hs92_len == "8")
                    drop <- names(or_de_fy_ly_in_8char) %in% c("hs92_len")
                    or_de_fy_ly_in_8char <- or_de_fy_ly_in_8char[!drop]

                    all_all_fyear_lyear_in_8char <- as.data.frame(fromJSON(paste0("http://atlas.media.mit.edu/hs92/export/", first_year, ".", last_year, ".", interval, "/all/all/show/")))
                    keep <- names(all_all_fyear_lyear_in_8char) %in% c("data.export_val","data.hs92_id","data.pci","data.pci_rank","data.top_importer","data.top_exporter")
                    all_all_fyear_lyear_in_8char <- all_all_fyear_lyear_in_8char[keep]
                    all_all_fyear_lyear_in_8char <- arrange.vars(all_all_fyear_lyear_in_8char, c("data.hs92_id" = 1))
                    setnames(all_all_fyear_lyear_in_8char, names(all_all_fyear_lyear_in_8char), c("hs92_id","world_total_export_val","pci","pci_rank","top_exporter","top_importer"))

                    origin_all_fyear_lyear_in_8char <- as.data.frame(fromJSON(paste0("http://atlas.media.mit.edu/hs92/export/", first_year, ".", last_year, ".", interval, "/", origin, "/all/show/")))
                    keep <- names(origin_all_fyear_lyear_in_8char) %in% c("data.export_val","data.hs92_id")
                    origin_all_fyear_lyear_in_8char <- origin_all_fyear_lyear_in_8char[keep]
                    origin_all_fyear_lyear_in_8char <- arrange.vars(origin_all_fyear_lyear_in_8char, c("data.hs92_id" = 1))
                    setnames(origin_all_fyear_lyear_in_8char, names(origin_all_fyear_lyear_in_8char), c("hs92_id","origin_total_export_val"))

                    or_de_fy_ly_in_8char <- join(or_de_fy_ly_in_8char, all_all_fyear_lyear_in_8char, by = "hs92_id")
                    or_de_fy_ly_in_8char <- join(or_de_fy_ly_in_8char, origin_all_fyear_lyear_in_8char, by = "hs92_id")
                    rm(all_all_fyear_lyear_in_8char,origin_all_fyear_lyear_in_8char)
                    or_de_fy_ly_in_8char$rca <- (or_de_fy_ly_in_8char$export_val/or_de_fy_ly_in_8char$world_total_export_val)/(sum(or_de_fy_ly_in_8char$origin_total_export_val, na.rm=TRUE)/sum(or_de_fy_ly_in_8char$world_total_export_val, na.rm=TRUE))
                    or_de_fy_ly_in_8char$rca <- round(or_de_fy_ly_in_8char$rca, digits = 2)

                    hs92_8char <- hs92_8char
                    or_de_fy_ly_in_8char <- join(hs92_8char, or_de_fy_ly_in_8char, by = "hs92_id")
                    or_de_fy_ly_in_8char <- subset(or_de_fy_ly_in_8char, !is.na(or_de_fy_ly_in_8char$year))

                    or_de_fy_ly_in_8char$icon <- paste0("d3plus-1.9.8/icons/hs92/hs92_", or_de_fy_ly_in_8char$hs92_group, ".png")
                    hs92_colors <- hs92_colors
                    or_de_fy_ly_in_8char <- merge(or_de_fy_ly_in_8char,hs92_colors)
                    or_de_fy_ly_in_8char <- subset(or_de_fy_ly_in_8char, !is.na(or_de_fy_ly_in_8char$year))
                    or_de_fy_ly_in_8char <- arrange.vars(or_de_fy_ly_in_8char, c("year" = 1, "origin_id" = 2, "destination_id" = 3, "origin_total_export_val" = 12, "rca" = 15))
                    or_de_fy_ly_in_8char <- or_de_fy_ly_in_8char[order(or_de_fy_ly_in_8char$year), ]

                    envir = as.environment(1)
                    assign(paste0(origin, "_", destination, "_", first_year, "_", last_year, "_", interval, "_8char"), or_de_fy_ly_in_8char, envir = envir)

                    print("writing HS92 (8 characters) json and csv files...")
                    write.csv(or_de_fy_ly_in_8char, paste0(output,"_8char.csv"))
                    jsonOut_8char <- toJSON(or_de_fy_ly_in_8char, pretty = TRUE)
                    write(jsonOut_8char, file=paste0(output,"_8char.json"))
                  } else {
                    print("HS92 (8 characters) files already exists. skipping.")
                  }
                }
              } else {
                print("HS92 list only allows 6 or 8 characters.")
              }
            }
          } else {
            print('getdata_interval() error: classification only admits sitc for SITC rev.2 and hs6/hs8 for H292.')
          }
        }
      }
  }
}
