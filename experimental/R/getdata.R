#' Downloads and processes the data from the API
#' @export
#' @param origin Country code of origin (e.g. "chl" for Chile)
#' @param destination Country code of destination (e.g. "chn" for China)
#' @param classification Trade classification that can be "6" (HS92 6 characters) or "8" (HS92 8 characters) for the year 1995 and going or "4" (SITC rev.2 4 characters) for the year 1962 and ongoing. The default is set to "6".
#' @param year The OEC's API ranges from 1962 to 2014
#' @import curl data.table jsonlite plyr servr
#' @importFrom utils write.csv
#' @examples
#' # Run countries_list() to display the full list of countries
#' # Chile is "chl" and China is "chn"
#'
#' # Download trade data from OEC's API (HS92 6 characters product list)
#' # for Chile and China in the year 2014
#' # getdata("chl", "chn", 2014)
#' # is the same as
#' # getdata("chl", "chn", 2014, 6)
#'
#' # Download trade data from OEC's API (HS92 8 characters product list)
#' # for Chile and China in the year 2014
#' # getdata("chl", "chn", 2014, 8)
#'
#' # Download trade data from OEC's API (SITC rev.2 4 characters product list)
#' # for Chile and China in the year 2014
#' # getdata("chl", "chn", 2014, 4)
#' @keywords functions

getdata <- function(origin, destination, year, classification) {

  if(missing(classification)) {
    classification = 6
  }

  output <- paste(origin, destination, year, sep = "_")

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

  if(year < 1961 | year > 2014) {
    print("data is only available from 1962 to 2014.")
    stop()
  } else {
    if(classification > 4 & year < 1995) {
      print("HS92 classification is only available from year 1995 and ongoing.")
      stop()
    } else {
      if(classification == 4 | classification == 6 | classification == 8){
        if(classification == 4){
          classification <- "sitc"
          characters <- 4
          print("using SITC rev.2 classification (4 characters)")
        }
        if(classification == 6){
          classification <- "hs"
          characters <- 6
          print("using HS92 classification (6 characters)")
        }
        if(classification == 8){
          classification <- "hs"
          characters <- 8
          print("using HS92 classification (8 characters)")
        }
      }

      if(classification == "sitc" | classification == "hs") {
        if(classification == "sitc") {
          if(characters == 4) {
            csv_file_4char <- paste0(output,"_4char.csv")
            json_file_4char <- paste0(output,"_4char.json")
            if(!file.exists(csv_file_4char) | !file.exists(json_file_4char)) {
              print("processing SITC rev.2 (4 characters) files...")

              origin_destination_year_4char <- as.data.frame(fromJSON(paste0("http://atlas.media.mit.edu/sitc/export/", year, "/", origin, "/", destination, "/show/")))
              if(destination == "all"){origin_destination_year_4char$data.dest_id <- "rest_of_the_world"}
              if(origin == "all"){origin_destination_year_4char$data.origin_id <- "rest_of_the_world"}
              keep <- names(origin_destination_year_4char) %in% c("data.year", "data.origin_id","data.dest_id","data.sitc_id", "data.export_val", "data.import_val")
              origin_destination_year_4char <- origin_destination_year_4char[keep]
              origin_destination_year_4char <- arrange.vars(origin_destination_year_4char, c("data.year" = 1, "data.origin_id" = 2, "data.dest_id" = 3, "data.sitc_id" = 4, "data.export_val" = 5, "data.import_val" = 6))
              setnames(origin_destination_year_4char, names(origin_destination_year_4char), c("year", "origin_id", "destination_id", "sitc_rev2_id", "export_val", "import_val"))
              origin_destination_year_4char$trade_exchange_val <- rowSums(origin_destination_year_4char[, c("export_val", "import_val")], na.rm=T)

              all_all_year_4char <- as.data.frame(fromJSON(paste0("http://atlas.media.mit.edu/sitc/export/", year, "/all/all/show/")))
              keep <- names(all_all_year_4char) %in% c("data.export_val","data.sitc_id","data.pci","data.pci_rank","data.top_importer","data.top_exporter")
              all_all_year_4char <- all_all_year_4char[keep]
              all_all_year_4char <- arrange.vars(all_all_year_4char, c("data.sitc_id" = 1))
              setnames(all_all_year_4char, names(all_all_year_4char), c("sitc_rev2_id","world_total_export_val","pci","pci_rank","top_exporter","top_importer"))

              origin_all_year_4char <- as.data.frame(fromJSON(paste0("http://atlas.media.mit.edu/sitc/export/", year, "/", origin, "/all/show/")))
              keep <- names(origin_all_year_4char) %in% c("data.export_val","data.sitc_id")
              origin_all_year_4char <- origin_all_year_4char[keep]
              origin_all_year_4char <- arrange.vars(origin_all_year_4char, c("data.sitc_id" = 1))
              setnames(origin_all_year_4char, names(origin_all_year_4char), c("sitc_rev2_id","origin_total_export_val"))

              origin_destination_year_4char <- join(origin_destination_year_4char, all_all_year_4char, by = "sitc_rev2_id")
              origin_destination_year_4char <- join(origin_destination_year_4char, origin_all_year_4char, by = "sitc_rev2_id")
              rm(all_all_year_4char,origin_all_year_4char)
              origin_destination_year_4char$rca <- (origin_destination_year_4char$export_val/origin_destination_year_4char$world_total_export_val)/(sum(origin_destination_year_4char$origin_total_export_val, na.rm=TRUE)/sum(origin_destination_year_4char$world_total_export_val, na.rm=TRUE))
              origin_destination_year_4char$rca <- round(origin_destination_year_4char$rca, digits = 2)

              sitc_rev2_4char <- sitc_rev2_4char
              sitc_rev2_colors <- sitc_rev2_colors
              sitc_rev2_4char <- join(sitc_rev2_4char,sitc_rev2_colors, by="group")
              origin_destination_year_4char <- join(sitc_rev2_4char, origin_destination_year_4char, by = "sitc_rev2_id")
              rm(sitc_rev2_4char,sitc_rev2_colors)

              origin_destination_year_4char$icon <- paste0("d3plus-1.9.8/icons/sitc_rev2/sitc_rev2_", origin_destination_year_4char$sitc_rev2_group, ".png")
              origin_destination_year_4char <- arrange.vars(origin_destination_year_4char, c("year" = 1, "origin_id" = 2, "destination_id" = 3, "origin_total_export_val" = 12, "rca" = 15))
              origin_destination_year_4char$year <- ifelse(is.na(origin_destination_year_4char$year), year, origin_destination_year_4char$year)

              envir = as.environment(1)
              assign(paste0(origin, "_", destination, "_", year, "_4char"), origin_destination_year_4char, envir = envir)

              print("writing SITC rev.2 (4 characters) json and csv files...")
              write.csv(origin_destination_year_4char, paste0(output,"_4char.csv"))
              jsonOut_4char <- toJSON(origin_destination_year_4char, pretty = TRUE)
              write(jsonOut_4char, file=paste0(output,"_4char.json"))
            } else {
              envir = as.environment(1)
              print("the file you want to download is in the working folder. reading JSON...")
              assign(paste(origin, destination, year, paste0(classification,"char"), sep = "_"), fromJSON(json_file_4char), envir = envir)
            }
          } else {
            print("SITC rev.2 list only allows 4 characters.")
            stop()
          }
        }

        if(classification == "hs") {
          if(characters == 6 | characters == 8) {
            if(characters == 6) {
              csv_file_6char <- paste0(output,"_6char.csv")
              json_file_6char <- paste0(output,"_6char.json")
              if(!file.exists(csv_file_6char) | !file.exists(json_file_6char)) {
                print("processing HS92 (6 characters) files...")

                origin_destination_year_6char <- as.data.frame(fromJSON(paste0("http://atlas.media.mit.edu/hs92/export/", year, "/", origin, "/", destination, "/show/")))
                if(destination == "all"){origin_destination_year_6char$data.dest_id <- "rest_of_the_world"}
                if(origin == "all"){origin_destination_year_6char$data.origin_id <- "rest_of_the_world"}
                keep <- names(origin_destination_year_6char) %in% c("data.year", "data.origin_id","data.dest_id","data.hs92_id", "data.export_val", "data.import_val")
                origin_destination_year_6char <- origin_destination_year_6char[keep]
                origin_destination_year_6char <- arrange.vars(origin_destination_year_6char, c("data.year" = 1, "data.origin_id" = 2, "data.dest_id" = 3, "data.hs92_id" = 4, "data.export_val" = 5, "data.import_val" = 6))
                setnames(origin_destination_year_6char, names(origin_destination_year_6char), c("year", "origin_id", "destination_id", "hs92_id", "export_val", "import_val"))
                origin_destination_year_6char$trade_exchange_val <- rowSums(origin_destination_year_6char[, c("export_val", "import_val")], na.rm=T)
                origin_destination_year_6char$hs92_len <- nchar(origin_destination_year_6char$hs92_id)
                origin_destination_year_6char <- subset(origin_destination_year_6char, origin_destination_year_6char$hs92_len == "6")
                drop <- names(origin_destination_year_6char) %in% c("hs92_len")
                origin_destination_year_6char <- origin_destination_year_6char[!drop]

                all_all_year_6char <- as.data.frame(fromJSON(paste0("http://atlas.media.mit.edu/hs92/export/", year, "/all/all/show/")))
                keep <- names(all_all_year_6char) %in% c("data.export_val","data.hs92_id","data.pci","data.pci_rank","data.top_importer","data.top_exporter")
                all_all_year_6char <- all_all_year_6char[keep]
                all_all_year_6char <- arrange.vars(all_all_year_6char, c("data.hs92_id" = 1))
                setnames(all_all_year_6char, names(all_all_year_6char), c("hs92_id","world_total_export_val","pci","pci_rank","top_exporter","top_importer"))

                origin_all_year_6char <- as.data.frame(fromJSON(paste0("http://atlas.media.mit.edu/hs92/export/", year, "/", origin, "/all/show/")))
                keep <- names(origin_all_year_6char) %in% c("data.export_val","data.hs92_id")
                origin_all_year_6char <- origin_all_year_6char[keep]
                origin_all_year_6char <- arrange.vars(origin_all_year_6char, c("data.hs92_id" = 1))
                setnames(origin_all_year_6char, names(origin_all_year_6char), c("hs92_id","origin_total_export_val"))

                origin_destination_year_6char <- join(origin_destination_year_6char, all_all_year_6char, by = "hs92_id")
                origin_destination_year_6char <- join(origin_destination_year_6char, origin_all_year_6char, by = "hs92_id")
                rm(all_all_year_6char,origin_all_year_6char)
                origin_destination_year_6char$rca <- (origin_destination_year_6char$export_val/origin_destination_year_6char$world_total_export_val)/(sum(origin_destination_year_6char$origin_total_export_val, na.rm=TRUE)/sum(origin_destination_year_6char$world_total_export_val, na.rm=TRUE))
                origin_destination_year_6char$rca <- round(origin_destination_year_6char$rca, digits = 2)

                hs92_6char <- hs92_6char
                hs92_colors <- hs92_colors
                hs92_6char <- join(hs92_6char,hs92_colors, by="group")
                origin_destination_year_6char <- join(hs92_6char, origin_destination_year_6char, by = "hs92_id")
                rm(hs92_6char,hs92_colors)

                origin_destination_year_6char$icon <- paste0("d3plus-1.9.8/icons/hs92/hs92_", origin_destination_year_6char$hs92_group, ".png")
                origin_destination_year_6char <- arrange.vars(origin_destination_year_6char, c("year" = 1, "origin_id" = 2, "destination_id" = 3, "origin_total_export_val" = 12, "rca" = 15))
                origin_destination_year_6char$year <- ifelse(is.na(origin_destination_year_6char$year), year, origin_destination_year_6char$year)

                envir = as.environment(1)
                assign(paste0(origin, "_", destination, "_", year, "_6char"), origin_destination_year_6char, envir = envir)

                print("writing HS92 (6 characters) json and csv files...")
                write.csv(origin_destination_year_6char, paste0(output,"_6char.csv"))
                jsonOut_6char <- toJSON(origin_destination_year_6char, pretty = TRUE)
                write(jsonOut_6char, file=paste0(output,"_6char.json"))
              } else {
                envir = as.environment(1)
                print("the file you want to download is in the working folder. reading JSON...")
                assign(paste(origin, destination, year, paste0(classification,"char"), sep = "_"), fromJSON(json_file_6char), envir = envir)
              }
            }
            if(characters == 8) {
              csv_file_8char <- paste0(output,"_8char.csv")
              json_file_8char <- paste0(output,"_8char.json")
              if(!file.exists(csv_file_8char) | !file.exists(json_file_8char)) {
                print("processing HS92 (8 characters) files...")

                origin_destination_year_8char <- as.data.frame(fromJSON(paste0("http://atlas.media.mit.edu/hs92/export/", year, "/", origin, "/", destination, "/show/")))
                if(destination == "all"){origin_destination_year_8char$data.dest_id <- "rest_of_the_world"}
                if(origin == "all"){origin_destination_year_8char$data.origin_id <- "rest_of_the_world"}
                keep <- names(origin_destination_year_8char) %in% c("data.year", "data.origin_id","data.dest_id","data.hs92_id", "data.export_val", "data.import_val")
                origin_destination_year_8char <- origin_destination_year_8char[keep]
                origin_destination_year_8char <- arrange.vars(origin_destination_year_8char, c("data.year" = 1, "data.origin_id" = 2, "data.dest_id" = 3, "data.hs92_id" = 4, "data.export_val" = 5, "data.import_val" = 6))
                setnames(origin_destination_year_8char, names(origin_destination_year_8char), c("year", "origin_id", "destination_id", "hs92_id", "export_val", "import_val"))
                origin_destination_year_8char$trade_exchange_val <- rowSums(origin_destination_year_8char[, c("export_val", "import_val")], na.rm=T)
                origin_destination_year_8char$hs92_len <- nchar(origin_destination_year_8char$hs92_id)
                origin_destination_year_8char <- subset(origin_destination_year_8char, origin_destination_year_8char$hs92_len == "8")
                drop <- names(origin_destination_year_8char) %in% c("hs92_len")
                origin_destination_year_8char <- origin_destination_year_8char[!drop]

                all_all_year_8char <- as.data.frame(fromJSON(paste0("http://atlas.media.mit.edu/hs92/export/", year, "/all/all/show/")))
                keep <- names(all_all_year_8char) %in% c("data.export_val","data.hs92_id","data.pci","data.pci_rank","data.top_importer","data.top_exporter")
                all_all_year_8char <- all_all_year_8char[keep]
                all_all_year_8char <- arrange.vars(all_all_year_8char, c("data.hs92_id" = 1))
                setnames(all_all_year_8char, names(all_all_year_8char), c("hs92_id","world_total_export_val","pci","pci_rank","top_exporter","top_importer"))

                origin_all_year_8char <- as.data.frame(fromJSON(paste0("http://atlas.media.mit.edu/hs92/export/", year, "/", origin, "/all/show/")))
                keep <- names(origin_all_year_8char) %in% c("data.export_val","data.hs92_id")
                origin_all_year_8char <- origin_all_year_8char[keep]
                origin_all_year_8char <- arrange.vars(origin_all_year_8char, c("data.hs92_id" = 1))
                setnames(origin_all_year_8char, names(origin_all_year_8char), c("hs92_id","origin_total_export_val"))

                origin_destination_year_8char <- join(origin_destination_year_8char, all_all_year_8char, by = "hs92_id")
                origin_destination_year_8char <- join(origin_destination_year_8char, origin_all_year_8char, by = "hs92_id")
                rm(all_all_year_8char,origin_all_year_8char)
                origin_destination_year_8char$rca <- (origin_destination_year_8char$export_val/origin_destination_year_8char$world_total_export_val)/(sum(origin_destination_year_8char$origin_total_export_val, na.rm=TRUE)/sum(origin_destination_year_8char$world_total_export_val, na.rm=TRUE))
                origin_destination_year_8char$rca <- round(origin_destination_year_8char$rca, digits = 2)

                hs92_8char <- hs92_8char
                hs92_colors <- hs92_colors
                hs92_8char <- join(hs92_8char,hs92_colors, by="group")
                origin_destination_year_8char <- join(hs92_8char, origin_destination_year_8char, by = "hs92_id")
                rm(hs92_8char,hs92_colors)

                origin_destination_year_8char$icon <- paste0("d3plus-1.9.8/icons/hs92/hs92_", origin_destination_year_8char$hs92_group, ".png")
                origin_destination_year_8char <- arrange.vars(origin_destination_year_8char, c("year" = 1, "origin_id" = 2, "destination_id" = 3, "origin_total_export_val" = 12, "rca" = 15))
                origin_destination_year_8char$year <- ifelse(is.na(origin_destination_year_8char$year), year, origin_destination_year_8char$year)

                envir = as.environment(1)
                assign(paste0(origin, "_", destination, "_", year, "_8char"), origin_destination_year_8char, envir = envir)

                print("writing HS92 (8 characters) json and csv files...")
                write.csv(origin_destination_year_8char, paste0(output,"_8char.csv"))
                jsonOut_8char <- toJSON(origin_destination_year_8char, pretty = TRUE)
                write(jsonOut_8char, file=paste0(output,"_8char.json"))
              } else {
                envir = as.environment(1)
                print("the file you want to download is in the working folder. reading JSON...")
                assign(paste(origin, destination, year, paste0(classification,"char"), sep = "_"), fromJSON(json_file_8char), envir = envir)
              }
            }
          } else {
            print("HS92 list only allows 6 or 8 characters.")
            stop()
          }
        }
      } else {
        print("getdata() error: allowed classifications are HS92 (6 and 8 characters) and SITC rev.2 (4 characters)")
        stop()
      }
    }
  }
}
