#' Downloads and processes the data from the API
#' @export
#' @param origin is the country code of origin (e.g. "chl" for Chile)
#' @param destination is the country code of origin (e.g. "chn" for China)
#' @param classification refers to the trade classification that can be "6" (HS92 6 characters) or "8" (HS92 8 characters) for the year 1995 and going or "4" (SITC rev.2 4 characters) for the year 1962 and ongoing
#' @param year is the year and the OEC's API ranges from 1962 to 2014
#' @import curl data.table jsonlite plyr servr
#' @importFrom stats complete.cases
#' @importFrom utils write.table
#' @importFrom utils write.csv
#' @examples
#' # Run countries_list() to display the full list of countries
#' # Chile is "chl" and China is "chn"
#'
#' # Download Chile (chl) and China (chn) trade data (imports, export and trade balance)
#' getdata("chl", "chn", 6, 2010)
#'
#' # Download trade data from OEC's API (HS92 6 characters product lists)
#' getdata("chl", "chn", 6, 2010)
#'
#' # Download trade data from OEC's API (SITC rev.2 4 characters product lists)
#' getdata("chl", "chn", 4, 2010)
#' @keywords functions

getdata <- function(origin, destination, classification, year) {

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

  if(year < 1961 | year > 2015) {
    print("data is only available from 1962 to 2014.")
  } else {
    if(classification > 4 & year < 1995) {
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
        print("getdata() error: allowed classifications are hs6, hs8 and sitc.")
      }


      if(classification == "sitc" | classification == "hs") {
        if(classification == "sitc") {
          if(characters == 4) {
            csv_file_4char <- paste0(output,"_4char.csv")
            json_file_4char <- paste0(output,"_4char.json")
            if(!file.exists(csv_file_4char) | !file.exists(json_file_4char)) {
              print("processing SITC rev.2 (4 characters) files...")

              origin_all_year_4char <- as.data.frame(fromJSON(paste0("http://atlas.media.mit.edu/sitc/export/", year, "/", origin, "/all/show/")))
              drop <- names(origin_all_year_4char) %in% c("data.dest_id", "data.sitc_id_len", "data.origin_id", "data.year")
              origin_all_year_4char <- origin_all_year_4char[!drop]
              setnames(origin_all_year_4char, c("data.export_val", "data.sitc_id", "data.import_val"), c("export_val", "id", "import_val"))
              origin_all_year_4char$group_id = substr(origin_all_year_4char$id, 1, 2)

              sitc_rev2_4char = sitc_rev2_4char
              origin_all_year_4char = join(sitc_rev2_4char, origin_all_year_4char, by = "id")
              origin_all_year_4char = origin_all_year_4char[, c("product", "group", "product_id", "group_id", "import_val", "export_val")]
              origin_all_year_4char$export_val <- as.numeric(origin_all_year_4char$export_val)
              origin_all_year_4char$import_val <- as.numeric(origin_all_year_4char$import_val)
              origin_all_year_4char$trade_exchange_val <- as.numeric(rowSums(origin_all_year_4char[, c("export_val", "import_val")], na.rm=T))
              origin_all_year_4char$product_id = as.character(origin_all_year_4char$product_id)
              origin_all_year_4char$image = paste0("d3plus-2.0/icons/sitc/sitc_", origin_all_year_4char$group_id, ".png")
              sitc_colors = sitc_colors
              origin_all_year_4char <- merge(origin_all_year_4char,sitc_colors)

              all_all_year_4char <- as.data.frame(fromJSON(paste0("http://atlas.media.mit.edu/sitc/export/", year, "/all/all/show/")))
              drop <- names(all_all_year_4char) %in% c("data.dest_id", "data.sitc_id_len", "data.origin_id", "data.year")
              all_all_year_4char <- all_all_year_4char[!drop]
              setnames(all_all_year_4char, c("data.export_val", "data.sitc_id", "data.import_val"), c("export_val", "id", "import_val"))
              all_all_year_4char$group_id = substr(all_all_year_4char$id, 1, 2)

              all_all_year_4char = join(sitc_rev2_4char, all_all_year_4char, by = "id")
              all_all_year_4char = all_all_year_4char[, c("product", "group", "product_id", "group_id", "import_val", "export_val")]
              all_all_year_4char$export_val <- as.numeric(all_all_year_4char$export_val)
              all_all_year_4char$import_val <- as.numeric(all_all_year_4char$import_val)
              all_all_year_4char$trade_exchange_val <- as.numeric(rowSums(all_all_year_4char[, c("export_val", "import_val")], na.rm=T))
              all_all_year_4char$product_id = as.character(all_all_year_4char$product_id)
              all_all_year_4char$image = paste0("d3plus-2.0/icons/sitc/sitc_", all_all_year_4char$group_id, ".png")
              all_all_year_4char <- merge(all_all_year_4char,sitc_colors)

              keep <- names(origin_all_year_4char) %in% c("product_id","export_val")
              origin_all_year_4char_exp <- origin_all_year_4char[keep]
              setnames(origin_all_year_4char_exp, "export_val","export_val_origin_all")

              keep <- names(all_all_year_4char) %in% c("product_id","export_val")
              all_all_year_4char_exp <- all_all_year_4char[keep]
              setnames(all_all_year_4char_exp, "export_val","export_val_all_all")

              origin_destination_year_4char <- as.data.frame(fromJSON(paste0("http://atlas.media.mit.edu/sitc/export/", year, "/", origin, "/", destination, "/show/")))
              drop <- names(origin_destination_year_4char) %in% c("data.dest_id", "data.sitc_id_len", "data.origin_id", "data.year")
              origin_destination_year_4char <- origin_destination_year_4char[!drop]
              setnames(origin_destination_year_4char, c("data.export_val", "data.sitc_id", "data.import_val"), c("export_val", "id", "import_val"))
              origin_destination_year_4char$group_id = substr(origin_destination_year_4char$id, 1, 2)

              origin_destination_year_4char = join(sitc_rev2_4char, origin_destination_year_4char, by = "id")
              origin_destination_year_4char = origin_destination_year_4char[, c("product", "group", "product_id", "group_id", "import_val", "export_val")]
              origin_destination_year_4char$export_val <- as.numeric(origin_destination_year_4char$export_val)
              origin_destination_year_4char$import_val <- as.numeric(origin_destination_year_4char$import_val)
              origin_destination_year_4char$trade_exchange_val <- as.numeric(rowSums(origin_destination_year_4char[, c("export_val", "import_val")], na.rm=T))
              origin_destination_year_4char$product_id = as.character(origin_destination_year_4char$product_id)
              origin_destination_year_4char <- merge(origin_destination_year_4char,origin_all_year_4char_exp)
              origin_destination_year_4char <- merge(origin_destination_year_4char,all_all_year_4char_exp)
              origin_destination_year_4char$rca = (origin_destination_year_4char$export_val/origin_destination_year_4char$export_val_all_all)/(sum(origin_destination_year_4char$export_val_origin_all, na.rm=TRUE)/sum(origin_destination_year_4char$export_val_all_all, na.rm=TRUE))
              origin_destination_year_4char$rca = round(origin_destination_year_4char$rca, digits = 2)
              origin_destination_year_4char <- merge(origin_destination_year_4char,sitc_colors)
              origin_destination_year_4char$rca = as.character(origin_destination_year_4char$rca)
              origin_destination_year_4char$image = paste0("d3plus-2.0/icons/sitc/sitc_", origin_destination_year_4char$group_id, ".png")
              setnames(origin_destination_year_4char, c("export_val_all_all"), c("world_trade_val"))
              drop <- names(origin_destination_year_4char) %in% c("export_val_origin_all")
              origin_destination_year_4char <- origin_destination_year_4char[!drop]
              origin_destination_year_4char <- arrange.vars(origin_destination_year_4char, c("group"=1, "product"=2, "group_id"=3, "product_id"=4))
              envir = as.environment(1)
              assign(paste0(origin, "_", destination, "_", year, "_4char"), origin_destination_year_4char, envir = envir)

              print("writing SITC rev.2 (4 characters) json and csv files...")
              save_as_csv <- write.csv(origin_destination_year_4char, paste0(output,"_4char.csv"))
              save_as_csv
              jsonOut_4char <- toJSON(origin_destination_year_4char, pretty = TRUE)
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

                origin_all_year <- as.data.frame(fromJSON(paste0("http://atlas.media.mit.edu/hs92/export/", year, "/", origin, "/all/show/")))
                drop <- names(origin_all_year) %in% c("data.dest_id", "data.hs92_id_len", "data.origin_id ", "data.year", "data.export_val_growth_pct", "data.export_val_growth_pct_5", "data.export_val_growth_val", "data.export_val_growth_val_5", "data.import_val_growth_pct", "data.import_val_growth_pct_5", "data.import_val_growth_val", "data.import_val_growth_val_5")
                origin_all_year <- origin_all_year[!drop]
                setnames(origin_all_year, c("data.export_val", "data.hs92_id", "data.import_val"), c("export_val", "product_id", "import_val"))
                origin_all_year$group_id = substr(origin_all_year$product_id, 1, 2)
                origin_all_year$product_id_len = nchar(origin_all_year$product_id)

                hs92_6char = hs92_6char
                origin_all_year_6char = subset(origin_all_year, origin_all_year$product_id_len == "6")
                origin_all_year_6char = join(hs92_6char, origin_all_year_6char, by = "product_id")
                origin_all_year_6char = origin_all_year_6char[, c("product", "group", "product_id", "group_id", "import_val", "export_val")]
                origin_all_year_6char$export_val <- as.numeric(origin_all_year_6char$export_val)
                origin_all_year_6char$import_val <- as.numeric(origin_all_year_6char$import_val)
                origin_all_year_6char$trade_exchange_val <- as.numeric(rowSums(origin_all_year_6char[, c("export_val", "import_val")], na.rm=T))
                origin_all_year_6char$product_id = as.character(origin_all_year_6char$product_id)
                origin_all_year_6char$image = paste0("d3plus-2.0/icons/hs/hs_", origin_all_year_6char$group_id, ".png")
                hs_colors = hs_colors
                origin_all_year_6char <- merge(origin_all_year_6char,hs_colors)

                all_all_year <- as.data.frame(fromJSON(paste0("http://atlas.media.mit.edu/hs92/export/", year, "/all/all/show/")))
                drop <- names(all_all_year) %in% c("data.dest_id", "data.hs92_id_len", "data.origin_id ", "data.year", "data.export_val_growth_pct", "data.export_val_growth_pct_5", "data.export_val_growth_val", "data.export_val_growth_val_5", "data.import_val_growth_pct", "data.import_val_growth_pct_5", "data.import_val_growth_val", "data.import_val_growth_val_5")
                all_all_year <- all_all_year[!drop]
                setnames(all_all_year, c("data.export_val", "data.hs92_id", "data.import_val"), c("export_val", "product_id", "import_val"))
                all_all_year$group_id = substr(all_all_year$product_id, 1, 2)
                all_all_year$product_id_len = nchar(all_all_year$product_id)

                all_all_year_6char = subset(all_all_year, all_all_year$product_id_len == "6")
                all_all_year_6char = join(hs92_6char, all_all_year_6char, by = "product_id")
                all_all_year_6char = all_all_year_6char[, c("product", "group", "product_id", "group_id", "import_val", "export_val")]
                all_all_year_6char$export_val <- as.numeric(all_all_year_6char$export_val)
                all_all_year_6char$import_val <- as.numeric(all_all_year_6char$import_val)
                all_all_year_6char$trade_exchange_val <- as.numeric(rowSums(all_all_year_6char[, c("export_val", "import_val")], na.rm=T))
                all_all_year_6char$product_id = as.character(all_all_year_6char$product_id)
                all_all_year_6char$image = paste0("d3plus-2.0/icons/hs/hs_", all_all_year_6char$group_id, ".png")
                all_all_year_6char <- merge(all_all_year_6char,hs_colors)

                keep <- names(origin_all_year_6char) %in% c("product_id","export_val")
                origin_all_year_6char_exp <- origin_all_year_6char[keep]
                setnames(origin_all_year_6char_exp, "export_val","export_val_origin_all")

                keep <- names(all_all_year_6char) %in% c("product_id","export_val")
                all_all_year_6char_exp <- all_all_year_6char[keep]
                setnames(all_all_year_6char_exp, "export_val","export_val_all_all")

                origin_destination_year <- as.data.frame(fromJSON(paste0("http://atlas.media.mit.edu/hs92/export/", year, "/", origin, "/", destination, "/show/")))
                drop <- names(origin_destination_year) %in% c("data.dest_id", "data.hs92_id_len", "data.origin_id ", "data.year", "data.export_val_growth_pct", "data.export_val_growth_pct_5", "data.export_val_growth_val", "data.export_val_growth_val_5", "data.import_val_growth_pct", "data.import_val_growth_pct_5", "data.import_val_growth_val", "data.import_val_growth_val_5")
                origin_destination_year <- origin_destination_year[!drop]
                setnames(origin_destination_year, c("data.export_val", "data.hs92_id", "data.import_val"), c("export_val", "product_id", "import_val"))
                origin_destination_year$group_id = substr(origin_destination_year$product_id, 1, 2)
                origin_destination_year$product_id_len = nchar(origin_destination_year$product_id)

                origin_destination_year_6char = subset(origin_destination_year, origin_destination_year$product_id_len == "6")
                origin_destination_year_6char = join(hs92_6char, origin_destination_year_6char, by = "product_id")
                origin_destination_year_6char = origin_destination_year_6char[, c("product", "group", "product_id", "group_id", "import_val", "export_val")]
                origin_destination_year_6char$export_val <- as.numeric(origin_destination_year_6char$export_val)
                origin_destination_year_6char$import_val <- as.numeric(origin_destination_year_6char$import_val)
                origin_destination_year_6char$trade_exchange_val <- as.numeric(rowSums(origin_destination_year_6char[, c("export_val", "import_val")], na.rm=T))
                origin_destination_year_6char$product_id = as.character(origin_destination_year_6char$product_id)
                origin_destination_year_6char <- merge(origin_destination_year_6char,origin_all_year_6char_exp)
                origin_destination_year_6char <- merge(origin_destination_year_6char,all_all_year_6char_exp)
                origin_destination_year_6char$rca = (origin_destination_year_6char$export_val/origin_destination_year_6char$export_val_all_all)/(sum(origin_destination_year_6char$export_val_origin_all, na.rm=TRUE)/sum(origin_destination_year_6char$export_val_all_all, na.rm=TRUE))
                origin_destination_year_6char$rca = round(origin_destination_year_6char$rca, digits = 2)
                origin_destination_year_6char <- merge(origin_destination_year_6char,hs_colors)
                origin_destination_year_6char$rca = as.character(origin_destination_year_6char$rca)
                origin_destination_year_6char$image = paste0("d3plus-2.0/icons/hs/hs_", origin_destination_year_6char$group_id, ".png")
                setnames(origin_destination_year_6char, c("export_val_all_all"), c("world_trade_val"))
                drop <- names(origin_destination_year_6char) %in% c("export_val_origin_all")
                origin_destination_year_6char <- origin_destination_year_6char[!drop]
                origin_destination_year_6char <- arrange.vars(origin_destination_year_6char, c("group"=1, "product"=2, "group_id"=3, "product_id"=4))
                envir = as.environment(1)
                assign(paste0(origin, "_", destination, "_", year, "_6char"), origin_destination_year_6char, envir = envir)

                print("writing HS92 (6 characters) json and csv files...")
                save_as_csv <- write.csv(origin_destination_year_6char, paste0(output,"_6char.csv"))
                save_as_csv
                jsonOut_6char <- toJSON(origin_destination_year_6char, pretty = TRUE)
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

                origin_all_year <- as.data.frame(fromJSON(paste0("http://atlas.media.mit.edu/hs92/export/", year, "/", origin, "/all/show/")))
                drop <- names(origin_all_year) %in% c("data.dest_id", "data.hs92_id_len", "data.origin_id ", "data.year", "data.export_val_growth_pct", "data.export_val_growth_pct_5", "data.export_val_growth_val", "data.export_val_growth_val_5", "data.import_val_growth_pct", "data.import_val_growth_pct_5", "data.import_val_growth_val", "data.import_val_growth_val_5")
                origin_all_year <- origin_all_year[!drop]
                setnames(origin_all_year, c("data.export_val", "data.hs92_id", "data.import_val"), c("export_val", "product_id", "import_val"))
                origin_all_year$group_id = substr(origin_all_year$product_id, 1, 2)
                origin_all_year$product_id_len = nchar(origin_all_year$product_id)

                hs92_8char = hs92_8char
                origin_all_year_8char = subset(origin_all_year, origin_all_year$product_id_len == "8")
                origin_all_year_8char = join(hs92_8char, origin_all_year_8char, by = "product_id")
                origin_all_year_8char = origin_all_year_8char[, c("product", "group", "product_id", "group_id", "import_val", "export_val")]
                origin_all_year_8char$export_val <- as.numeric(origin_all_year_8char$export_val)
                origin_all_year_8char$import_val <- as.numeric(origin_all_year_8char$import_val)
                origin_all_year_8char$trade_exchange_val <- as.numeric(rowSums(origin_all_year_8char[, c("export_val", "import_val")], na.rm=T))
                origin_all_year_8char$product_id = as.character(origin_all_year_8char$product_id)
                origin_all_year_8char$image = paste0("d3plus-2.0/icons/hs/hs_", origin_all_year_8char$group_id, ".png")
                origin_all_year_8char <- merge(origin_all_year_8char,hs_colors)

                all_all_year <- as.data.frame(fromJSON(paste0("http://atlas.media.mit.edu/hs92/export/", year, "/all/all/show/")))
                drop <- names(all_all_year) %in% c("data.dest_id", "data.hs92_id_len", "data.origin_id ", "data.year", "data.export_val_growth_pct", "data.export_val_growth_pct_5", "data.export_val_growth_val", "data.export_val_growth_val_5", "data.import_val_growth_pct", "data.import_val_growth_pct_5", "data.import_val_growth_val", "data.import_val_growth_val_5")
                all_all_year <- all_all_year[!drop]
                setnames(all_all_year, c("data.export_val", "data.hs92_id", "data.import_val"), c("export_val", "product_id", "import_val"))
                all_all_year$group_id = substr(all_all_year$product_id, 1, 2)
                all_all_year$product_id_len = nchar(all_all_year$product_id)

                all_all_year_8char = subset(all_all_year, all_all_year$product_id_len == "8")
                all_all_year_8char = join(hs92_8char, all_all_year_8char, by = "product_id")
                all_all_year_8char = all_all_year_8char[, c("product", "group", "product_id", "group_id", "import_val", "export_val")]
                all_all_year_8char$export_val <- as.numeric(all_all_year_8char$export_val)
                all_all_year_8char$import_val <- as.numeric(all_all_year_8char$import_val)
                all_all_year_8char$trade_exchange_val <- as.numeric(rowSums(all_all_year_8char[, c("export_val", "import_val")], na.rm=T))
                all_all_year_8char$product_id = as.character(all_all_year_8char$product_id)
                all_all_year_8char$image = paste0("d3plus-2.0/icons/hs/hs_", all_all_year_8char$group_id, ".png")
                all_all_year_8char <- merge(all_all_year_8char,hs_colors)

                keep <- names(origin_all_year_8char) %in% c("product_id","export_val")
                origin_all_year_8char_exp <- origin_all_year_8char[keep]
                setnames(origin_all_year_8char_exp, "export_val","export_val_origin_all")

                keep <- names(all_all_year_8char) %in% c("product_id","export_val")
                all_all_year_8char_exp <- all_all_year_8char[keep]
                setnames(all_all_year_8char_exp, "export_val","export_val_all_all")

                origin_destination_year <- as.data.frame(fromJSON(paste0("http://atlas.media.mit.edu/hs92/export/", year, "/", origin, "/", destination, "/show/")))
                drop <- names(origin_destination_year) %in% c("data.dest_id", "data.hs92_id_len", "data.origin_id ", "data.year", "data.export_val_growth_pct", "data.export_val_growth_pct_5", "data.export_val_growth_val", "data.export_val_growth_val_5", "data.import_val_growth_pct", "data.import_val_growth_pct_5", "data.import_val_growth_val", "data.import_val_growth_val_5")
                origin_destination_year <- origin_destination_year[!drop]
                setnames(origin_destination_year, c("data.export_val", "data.hs92_id", "data.import_val"), c("export_val", "product_id", "import_val"))
                origin_destination_year$group_id = substr(origin_destination_year$product_id, 1, 2)
                origin_destination_year$product_id_len = nchar(origin_destination_year$product_id)

                origin_destination_year_8char = subset(origin_destination_year, origin_destination_year$product_id_len == "8")
                origin_destination_year_8char = join(hs92_8char, origin_destination_year_8char, by = "product_id")
                origin_destination_year_8char = origin_destination_year_8char[, c("product", "group", "product_id", "group_id", "import_val", "export_val")]
                origin_destination_year_8char$export_val <- as.numeric(origin_destination_year_8char$export_val)
                origin_destination_year_8char$import_val <- as.numeric(origin_destination_year_8char$import_val)
                origin_destination_year_8char$trade_exchange_val <- as.numeric(rowSums(origin_destination_year_8char[, c("export_val", "import_val")], na.rm=T))
                origin_destination_year_8char$product_id = as.character(origin_destination_year_8char$product_id)
                origin_destination_year_8char <- merge(origin_destination_year_8char,origin_all_year_8char_exp)
                origin_destination_year_8char <- merge(origin_destination_year_8char,all_all_year_8char_exp)
                origin_destination_year_8char$rca = (origin_destination_year_8char$export_val/origin_destination_year_8char$export_val_all_all)/(sum(origin_destination_year_8char$export_val_origin_all, na.rm=TRUE)/sum(origin_destination_year_8char$export_val_all_all, na.rm=TRUE))
                origin_destination_year_8char$rca = round(origin_destination_year_8char$rca, digits = 2)
                origin_destination_year_8char <- merge(origin_destination_year_8char,hs_colors)
                origin_destination_year_8char$rca = as.character(origin_destination_year_8char$rca)
                origin_destination_year_8char$image = paste0("d3plus-2.0/icons/hs/hs_", origin_destination_year_8char$group_id, ".png")
                setnames(origin_destination_year_8char, c("export_val_all_all"), c("world_trade_val"))
                drop <- names(origin_destination_year_8char) %in% c("export_val_origin_all")
                origin_destination_year_8char <- origin_destination_year_8char[!drop]
                origin_destination_year_8char <- arrange.vars(origin_destination_year_8char, c("group"=1, "product"=2, "group_id"=3, "product_id"=4))
                envir = as.environment(1)
                assign(paste0(origin, "_", destination, "_", year, "_8char"), origin_destination_year_8char, envir = envir)

                print("writing HS92 (8 characters) json and csv files...")
                save_as_csv <- write.csv(origin_destination_year_8char, paste0(output,"_8char.csv"))
                save_as_csv
                jsonOut_8char <- toJSON(origin_destination_year_8char, pretty = TRUE)
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
        print('getdata() error: classification only admits sitc for SITC rev.2 and hs6/hs8 for H292.')
      }
    }
  }
}
