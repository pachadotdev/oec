#' Download trade data from OEC's API.
#' @export
#' @return Creates the data files in \code{json} and \code{csv} formats that are needed to create the visualizations.
#' @param ORIGIN is the country code of origin (e.g. "chl" for Chile)
#' @param DESTINATION is the country code of origin (e.g. "chn" for China)
#' @param CLASSIFICATION refers to the trade classification that can be "6" (HS92 6 characters) or "8" (HS92 8 characters) for the year 1995 and going or "4" (SITC rev.2 4 characters) for the year 1962 and ongoing
#' @param YEAR is the year and the OEC's API ranges from 1962 to 2014
#' @import curl data.table jsonlite plyr servr
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

getdata <- function(ORIGIN, DESTINATION, CLASSIFICATION, YEAR) {

  #ORIGIN = deparse(substitute(ORIGIN))
  #DESTINATION = deparse(substitute(DESTINATION))

  OUTPUT <- paste(ORIGIN, DESTINATION, YEAR, sep = "_")

  if(YEAR < 1961 | YEAR > 2015) {
    print("data is only available from 1962 to 2014.")
  } else {
    if(CLASSIFICATION > 4 & YEAR < 1995) {
      print("HS92 classification is only available from year 1995 and ongoing.")
    } else {
      if(CLASSIFICATION == 4 | CLASSIFICATION == 6 | CLASSIFICATION == 8){
        if(CLASSIFICATION == 4){
          CLASSIFICATION <- "sitc"
          CHARACTERS <- 4
        }
        if(CLASSIFICATION == 6){
          CLASSIFICATION <- "hs"
          CHARACTERS <- 6
        }
        if(CLASSIFICATION == 8){
          CLASSIFICATION <- "hs"
          CHARACTERS <- 8
        }
      } else {
        print("getdata() error: allowed classifications are hs6, hs8 and sitc.")
      }


      if(CLASSIFICATION == "sitc" | CLASSIFICATION == "hs") {
        if(CLASSIFICATION == "sitc") {
          if(CHARACTERS == 4) {
            csv_file_4char <- paste0(OUTPUT,"_4char.csv")
            json_file_4char <- paste0(OUTPUT,"_4char.json")
            if(!file.exists(csv_file_4char) | !file.exists(json_file_4char)) {
              print("processing SITC rev.2 (4 characters) files...")

              ORIGIN_all_YEAR_4char <- as.data.frame(fromJSON(paste0("http://atlas.media.mit.edu/sitc/export/", YEAR, "/", ORIGIN, "/all/show/")))
              drop <- names(ORIGIN_all_YEAR_4char) %in% c("data.dest_id", "data.sitc_id_len", "data.origin_id", "data.year")
              ORIGIN_all_YEAR_4char <- ORIGIN_all_YEAR_4char[!drop]
              setnames(ORIGIN_all_YEAR_4char, c("data.export_val", "data.sitc_id", "data.import_val"), c("export_val", "id", "import_val"))
              ORIGIN_all_YEAR_4char$group_id = substr(ORIGIN_all_YEAR_4char$id, 1, 2)

              sitc_rev2_4char = sitc_rev2_4char
              ORIGIN_all_YEAR_4char = join(sitc_rev2_4char, ORIGIN_all_YEAR_4char, by = "id")
              ORIGIN_all_YEAR_4char = ORIGIN_all_YEAR_4char[, c("product", "group", "product_id", "group_id", "import_val", "export_val")]
              ORIGIN_all_YEAR_4char$export_val <- as.numeric(ORIGIN_all_YEAR_4char$export_val)
              ORIGIN_all_YEAR_4char$import_val <- as.numeric(ORIGIN_all_YEAR_4char$import_val)
              ORIGIN_all_YEAR_4char$trade_exchange_val <- as.numeric(rowSums(ORIGIN_all_YEAR_4char[, c("export_val", "import_val")], na.rm=T))
              ORIGIN_all_YEAR_4char$product_id = as.character(ORIGIN_all_YEAR_4char$product_id)
              ORIGIN_all_YEAR_4char$image = paste0("d3plus/icons/sitc/sitc_", ORIGIN_all_YEAR_4char$group_id, ".png")
              sitc_colors = sitc_colors
              ORIGIN_all_YEAR_4char <- merge(ORIGIN_all_YEAR_4char,sitc_colors)

              all_all_YEAR_4char <- as.data.frame(fromJSON(paste0("http://atlas.media.mit.edu/sitc/export/", YEAR, "/all/all/show/")))
              drop <- names(all_all_YEAR_4char) %in% c("data.dest_id", "data.sitc_id_len", "data.origin_id", "data.year")
              all_all_YEAR_4char <- all_all_YEAR_4char[!drop]
              setnames(all_all_YEAR_4char, c("data.export_val", "data.sitc_id", "data.import_val"), c("export_val", "id", "import_val"))
              all_all_YEAR_4char$group_id = substr(all_all_YEAR_4char$id, 1, 2)

              all_all_YEAR_4char = join(sitc_rev2_4char, all_all_YEAR_4char, by = "id")
              all_all_YEAR_4char = all_all_YEAR_4char[, c("product", "group", "product_id", "group_id", "import_val", "export_val")]
              all_all_YEAR_4char$export_val <- as.numeric(all_all_YEAR_4char$export_val)
              all_all_YEAR_4char$import_val <- as.numeric(all_all_YEAR_4char$import_val)
              all_all_YEAR_4char$trade_exchange_val <- as.numeric(rowSums(all_all_YEAR_4char[, c("export_val", "import_val")], na.rm=T))
              all_all_YEAR_4char$product_id = as.character(all_all_YEAR_4char$product_id)
              all_all_YEAR_4char$image = paste0("d3plus/icons/sitc/sitc_", all_all_YEAR_4char$group_id, ".png")
              all_all_YEAR_4char <- merge(all_all_YEAR_4char,sitc_colors)

              keep <- names(ORIGIN_all_YEAR_4char) %in% c("product_id","export_val")
              ORIGIN_all_YEAR_4char_exp <- ORIGIN_all_YEAR_4char[keep]
              setnames(ORIGIN_all_YEAR_4char_exp, "export_val","export_val_origin_all")

              keep <- names(all_all_YEAR_4char) %in% c("product_id","export_val")
              all_all_YEAR_4char_exp <- all_all_YEAR_4char[keep]
              setnames(all_all_YEAR_4char_exp, "export_val","export_val_all_all")

              ORIGIN_DESTINATION_YEAR_4char <- as.data.frame(fromJSON(paste0("http://atlas.media.mit.edu/sitc/export/", YEAR, "/", ORIGIN, "/", DESTINATION, "/show/")))
              drop <- names(ORIGIN_DESTINATION_YEAR_4char) %in% c("data.dest_id", "data.sitc_id_len", "data.origin_id", "data.year")
              ORIGIN_DESTINATION_YEAR_4char <- ORIGIN_DESTINATION_YEAR_4char[!drop]
              setnames(ORIGIN_DESTINATION_YEAR_4char, c("data.export_val", "data.sitc_id", "data.import_val"), c("export_val", "id", "import_val"))
              ORIGIN_DESTINATION_YEAR_4char$group_id = substr(ORIGIN_DESTINATION_YEAR_4char$id, 1, 2)

              ORIGIN_DESTINATION_YEAR_4char = join(sitc_rev2_4char, ORIGIN_DESTINATION_YEAR_4char, by = "id")
              ORIGIN_DESTINATION_YEAR_4char = ORIGIN_DESTINATION_YEAR_4char[, c("product", "group", "product_id", "group_id", "import_val", "export_val")]
              ORIGIN_DESTINATION_YEAR_4char$export_val <- as.numeric(ORIGIN_DESTINATION_YEAR_4char$export_val)
              ORIGIN_DESTINATION_YEAR_4char$import_val <- as.numeric(ORIGIN_DESTINATION_YEAR_4char$import_val)
              ORIGIN_DESTINATION_YEAR_4char$trade_exchange_val <- as.numeric(rowSums(ORIGIN_DESTINATION_YEAR_4char[, c("export_val", "import_val")], na.rm=T))
              ORIGIN_DESTINATION_YEAR_4char$product_id = as.character(ORIGIN_DESTINATION_YEAR_4char$product_id)
              ORIGIN_DESTINATION_YEAR_4char <- merge(ORIGIN_DESTINATION_YEAR_4char,ORIGIN_all_YEAR_4char_exp)
              ORIGIN_DESTINATION_YEAR_4char <- merge(ORIGIN_DESTINATION_YEAR_4char,all_all_YEAR_4char_exp)
              ORIGIN_DESTINATION_YEAR_4char$rca = (ORIGIN_DESTINATION_YEAR_4char$export_val/ORIGIN_DESTINATION_YEAR_4char$export_val_all_all)/(sum(ORIGIN_DESTINATION_YEAR_4char$export_val_origin_all, na.rm=TRUE)/sum(ORIGIN_DESTINATION_YEAR_4char$export_val_all_all, na.rm=TRUE))
              ORIGIN_DESTINATION_YEAR_4char$rca = round(ORIGIN_DESTINATION_YEAR_4char$rca, digits = 2)
              ORIGIN_DESTINATION_YEAR_4char <- merge(ORIGIN_DESTINATION_YEAR_4char,sitc_colors)
              ORIGIN_DESTINATION_YEAR_4char$rca = as.character(ORIGIN_DESTINATION_YEAR_4char$rca)
              ORIGIN_DESTINATION_YEAR_4char$image = paste0("d3plus/icons/sitc/sitc_", ORIGIN_DESTINATION_YEAR_4char$group_id, ".png")
              setnames(ORIGIN_DESTINATION_YEAR_4char, c("export_val_all_all"), c("world_trade_val"))
              drop <- names(ORIGIN_DESTINATION_YEAR_4char) %in% c("export_val_origin_all")
              ORIGIN_DESTINATION_YEAR_4char <- ORIGIN_DESTINATION_YEAR_4char[!drop]
              envir = as.environment(1)
              assign(paste0(ORIGIN, "_", DESTINATION, "_", YEAR, "_4char"), ORIGIN_DESTINATION_YEAR_4char, envir = envir)

              print("writing SITC rev.2 (4 characters) json and csv files...")
              save_as_csv <- write.csv(ORIGIN_DESTINATION_YEAR_4char, paste0(OUTPUT,"_4char.csv"))
              save_as_csv
              jsonOut_4char <- toJSON(ORIGIN_DESTINATION_YEAR_4char, pretty = TRUE)
              write(jsonOut_4char, file=paste0(OUTPUT,"_4char.json"))
            } else {
              print("SITC rev.2 (4 characters) files already exists. skipping.")
            }
          } else {
            print("SITC rev.2 list only allows 4 characters.")
          }
        }

        if(CLASSIFICATION == "hs") {
            if(CHARACTERS == 6 | CHARACTERS == 8) {
                if(CHARACTERS == 6) {
                  csv_file_6char <- paste0(OUTPUT,"_6char.csv")
                  json_file_6char <- paste0(OUTPUT,"_6char.json")
                  if(!file.exists(csv_file_6char) | !file.exists(json_file_6char)) {
                    print("processing HS92 (6 characters) files...")

                    ORIGIN_all_YEAR <- as.data.frame(fromJSON(paste0("http://atlas.media.mit.edu/hs92/export/", YEAR, "/", ORIGIN, "/all/show/")))
                    drop <- names(ORIGIN_all_YEAR) %in% c("data.dest_id", "data.hs92_id_len", "data.origin_id ", "data.year", "data.export_val_growth_pct", "data.export_val_growth_pct_5", "data.export_val_growth_val", "data.export_val_growth_val_5", "data.import_val_growth_pct", "data.import_val_growth_pct_5", "data.import_val_growth_val", "data.import_val_growth_val_5")
                    ORIGIN_all_YEAR <- ORIGIN_all_YEAR[!drop]
                    setnames(ORIGIN_all_YEAR, c("data.export_val", "data.hs92_id", "data.import_val"), c("export_val", "product_id", "import_val"))
                    ORIGIN_all_YEAR$group_id = substr(ORIGIN_all_YEAR$product_id, 1, 2)
                    ORIGIN_all_YEAR$product_id_len = nchar(ORIGIN_all_YEAR$product_id)

                    hs92_6char = hs92_6char
                    ORIGIN_all_YEAR_6char = subset(ORIGIN_all_YEAR, ORIGIN_all_YEAR$product_id_len == "6")
                    ORIGIN_all_YEAR_6char = join(hs92_6char, ORIGIN_all_YEAR_6char, by = "product_id")
                    ORIGIN_all_YEAR_6char = ORIGIN_all_YEAR_6char[, c("product", "group", "product_id", "group_id", "import_val", "export_val")]
                    ORIGIN_all_YEAR_6char$export_val <- as.numeric(ORIGIN_all_YEAR_6char$export_val)
                    ORIGIN_all_YEAR_6char$import_val <- as.numeric(ORIGIN_all_YEAR_6char$import_val)
                    ORIGIN_all_YEAR_6char$trade_exchange_val <- as.numeric(rowSums(ORIGIN_all_YEAR_6char[, c("export_val", "import_val")], na.rm=T))
                    ORIGIN_all_YEAR_6char$product_id = as.character(ORIGIN_all_YEAR_6char$product_id)
                    ORIGIN_all_YEAR_6char$image = paste0("d3plus/icons/hs/hs_", ORIGIN_all_YEAR_6char$group_id, ".png")
                    hs_colors = hs_colors
                    ORIGIN_all_YEAR_6char <- merge(ORIGIN_all_YEAR_6char,hs_colors)

                    all_all_YEAR <- as.data.frame(fromJSON(paste0("http://atlas.media.mit.edu/hs92/export/", YEAR, "/all/all/show/")))
                    drop <- names(all_all_YEAR) %in% c("data.dest_id", "data.hs92_id_len", "data.origin_id ", "data.year", "data.export_val_growth_pct", "data.export_val_growth_pct_5", "data.export_val_growth_val", "data.export_val_growth_val_5", "data.import_val_growth_pct", "data.import_val_growth_pct_5", "data.import_val_growth_val", "data.import_val_growth_val_5")
                    all_all_YEAR <- all_all_YEAR[!drop]
                    setnames(all_all_YEAR, c("data.export_val", "data.hs92_id", "data.import_val"), c("export_val", "product_id", "import_val"))
                    all_all_YEAR$group_id = substr(all_all_YEAR$product_id, 1, 2)
                    all_all_YEAR$product_id_len = nchar(all_all_YEAR$product_id)

                    all_all_YEAR_6char = subset(all_all_YEAR, all_all_YEAR$product_id_len == "6")
                    all_all_YEAR_6char = join(hs92_6char, all_all_YEAR_6char, by = "product_id")
                    all_all_YEAR_6char = all_all_YEAR_6char[, c("product", "group", "product_id", "group_id", "import_val", "export_val")]
                    all_all_YEAR_6char$export_val <- as.numeric(all_all_YEAR_6char$export_val)
                    all_all_YEAR_6char$import_val <- as.numeric(all_all_YEAR_6char$import_val)
                    all_all_YEAR_6char$trade_exchange_val <- as.numeric(rowSums(all_all_YEAR_6char[, c("export_val", "import_val")], na.rm=T))
                    all_all_YEAR_6char$product_id = as.character(all_all_YEAR_6char$product_id)
                    all_all_YEAR_6char$image = paste0("d3plus/icons/hs/hs_", all_all_YEAR_6char$group_id, ".png")
                    all_all_YEAR_6char <- merge(all_all_YEAR_6char,hs_colors)

                    keep <- names(ORIGIN_all_YEAR_6char) %in% c("product_id","export_val")
                    ORIGIN_all_YEAR_6char_exp <- ORIGIN_all_YEAR_6char[keep]
                    setnames(ORIGIN_all_YEAR_6char_exp, "export_val","export_val_origin_all")

                    keep <- names(all_all_YEAR_6char) %in% c("product_id","export_val")
                    all_all_YEAR_6char_exp <- all_all_YEAR_6char[keep]
                    setnames(all_all_YEAR_6char_exp, "export_val","export_val_all_all")

                    ORIGIN_DESTINATION_YEAR <- as.data.frame(fromJSON(paste0("http://atlas.media.mit.edu/hs92/export/", YEAR, "/", ORIGIN, "/", DESTINATION, "/show/")))
                    drop <- names(ORIGIN_DESTINATION_YEAR) %in% c("data.dest_id", "data.hs92_id_len", "data.origin_id ", "data.year", "data.export_val_growth_pct", "data.export_val_growth_pct_5", "data.export_val_growth_val", "data.export_val_growth_val_5", "data.import_val_growth_pct", "data.import_val_growth_pct_5", "data.import_val_growth_val", "data.import_val_growth_val_5")
                    ORIGIN_DESTINATION_YEAR <- ORIGIN_DESTINATION_YEAR[!drop]
                    setnames(ORIGIN_DESTINATION_YEAR, c("data.export_val", "data.hs92_id", "data.import_val"), c("export_val", "product_id", "import_val"))
                    ORIGIN_DESTINATION_YEAR$group_id = substr(ORIGIN_DESTINATION_YEAR$product_id, 1, 2)
                    ORIGIN_DESTINATION_YEAR$product_id_len = nchar(ORIGIN_DESTINATION_YEAR$product_id)

                    ORIGIN_DESTINATION_YEAR_6char = subset(ORIGIN_DESTINATION_YEAR, ORIGIN_DESTINATION_YEAR$product_id_len == "6")
                    ORIGIN_DESTINATION_YEAR_6char = join(hs92_6char, ORIGIN_DESTINATION_YEAR_6char, by = "product_id")
                    ORIGIN_DESTINATION_YEAR_6char = ORIGIN_DESTINATION_YEAR_6char[, c("product", "group", "product_id", "group_id", "import_val", "export_val")]
                    ORIGIN_DESTINATION_YEAR_6char$export_val <- as.numeric(ORIGIN_DESTINATION_YEAR_6char$export_val)
                    ORIGIN_DESTINATION_YEAR_6char$import_val <- as.numeric(ORIGIN_DESTINATION_YEAR_6char$import_val)
                    ORIGIN_DESTINATION_YEAR_6char$trade_exchange_val <- as.numeric(rowSums(ORIGIN_DESTINATION_YEAR_6char[, c("export_val", "import_val")], na.rm=T))
                    ORIGIN_DESTINATION_YEAR_6char$product_id = as.character(ORIGIN_DESTINATION_YEAR_6char$product_id)
                    ORIGIN_DESTINATION_YEAR_6char <- merge(ORIGIN_DESTINATION_YEAR_6char,ORIGIN_all_YEAR_6char_exp)
                    ORIGIN_DESTINATION_YEAR_6char <- merge(ORIGIN_DESTINATION_YEAR_6char,all_all_YEAR_6char_exp)
                    ORIGIN_DESTINATION_YEAR_6char$rca = (ORIGIN_DESTINATION_YEAR_6char$export_val/ORIGIN_DESTINATION_YEAR_6char$export_val_all_all)/(sum(ORIGIN_DESTINATION_YEAR_6char$export_val_origin_all, na.rm=TRUE)/sum(ORIGIN_DESTINATION_YEAR_6char$export_val_all_all, na.rm=TRUE))
                    ORIGIN_DESTINATION_YEAR_6char$rca = round(ORIGIN_DESTINATION_YEAR_6char$rca, digits = 2)
                    ORIGIN_DESTINATION_YEAR_6char <- merge(ORIGIN_DESTINATION_YEAR_6char,hs_colors)
                    ORIGIN_DESTINATION_YEAR_6char$rca = as.character(ORIGIN_DESTINATION_YEAR_6char$rca)
                    ORIGIN_DESTINATION_YEAR_6char$image = paste0("d3plus/icons/hs/hs_", ORIGIN_DESTINATION_YEAR_6char$group_id, ".png")
                    setnames(ORIGIN_DESTINATION_YEAR_6char, c("export_val_all_all"), c("world_trade_val"))
                    drop <- names(ORIGIN_DESTINATION_YEAR_6char) %in% c("export_val_origin_all")
                    ORIGIN_DESTINATION_YEAR_6char <- ORIGIN_DESTINATION_YEAR_6char[!drop]
                    envir = as.environment(1)
                    assign(paste0(ORIGIN, "_", DESTINATION, "_", YEAR, "_6char"), ORIGIN_DESTINATION_YEAR_6char, envir = envir)

                    print("writing HS92 (6 characters) json and csv files...")
                    save_as_csv <- write.csv(ORIGIN_DESTINATION_YEAR_6char, paste0(OUTPUT,"_6char.csv"))
                    save_as_csv
                    jsonOut_6char <- toJSON(ORIGIN_DESTINATION_YEAR_6char, pretty = TRUE)
                    write(jsonOut_6char, file=paste0(OUTPUT,"_6char.json"))
                    } else {
                      print("HS92 (6 characters) files already exists. skipping.")
                    }
                }
                if(CHARACTERS == 8) {
                  csv_file_8char <- paste0(OUTPUT,"_8char.csv")
                  json_file_8char <- paste0(OUTPUT,"_8char.json")
                  if(!file.exists(csv_file_8char) | !file.exists(json_file_8char)) {
                    print("processing HS92 (8 characters) files...")

                    ORIGIN_all_YEAR <- as.data.frame(fromJSON(paste0("http://atlas.media.mit.edu/hs92/export/", YEAR, "/", ORIGIN, "/all/show/")))
                    drop <- names(ORIGIN_all_YEAR) %in% c("data.dest_id", "data.hs92_id_len", "data.origin_id ", "data.year", "data.export_val_growth_pct", "data.export_val_growth_pct_5", "data.export_val_growth_val", "data.export_val_growth_val_5", "data.import_val_growth_pct", "data.import_val_growth_pct_5", "data.import_val_growth_val", "data.import_val_growth_val_5")
                    ORIGIN_all_YEAR <- ORIGIN_all_YEAR[!drop]
                    setnames(ORIGIN_all_YEAR, c("data.export_val", "data.hs92_id", "data.import_val"), c("export_val", "product_id", "import_val"))
                    ORIGIN_all_YEAR$group_id = substr(ORIGIN_all_YEAR$product_id, 1, 2)
                    ORIGIN_all_YEAR$product_id_len = nchar(ORIGIN_all_YEAR$product_id)

                    hs92_8char = hs92_8char
                    ORIGIN_all_YEAR_8char = subset(ORIGIN_all_YEAR, ORIGIN_all_YEAR$product_id_len == "8")
                    ORIGIN_all_YEAR_8char = join(hs92_8char, ORIGIN_all_YEAR_8char, by = "product_id")
                    ORIGIN_all_YEAR_8char = ORIGIN_all_YEAR_8char[, c("product", "group", "product_id", "group_id", "import_val", "export_val")]
                    ORIGIN_all_YEAR_8char$export_val <- as.numeric(ORIGIN_all_YEAR_8char$export_val)
                    ORIGIN_all_YEAR_8char$import_val <- as.numeric(ORIGIN_all_YEAR_8char$import_val)
                    ORIGIN_all_YEAR_8char$trade_exchange_val <- as.numeric(rowSums(ORIGIN_all_YEAR_8char[, c("export_val", "import_val")], na.rm=T))
                    ORIGIN_all_YEAR_8char$product_id = as.character(ORIGIN_all_YEAR_8char$product_id)
                    ORIGIN_all_YEAR_8char$image = paste0("d3plus/icons/hs/hs_", ORIGIN_all_YEAR_8char$group_id, ".png")
                    ORIGIN_all_YEAR_8char <- merge(ORIGIN_all_YEAR_8char,hs_colors)

                    all_all_YEAR <- as.data.frame(fromJSON(paste0("http://atlas.media.mit.edu/hs92/export/", YEAR, "/all/all/show/")))
                    drop <- names(all_all_YEAR) %in% c("data.dest_id", "data.hs92_id_len", "data.origin_id ", "data.year", "data.export_val_growth_pct", "data.export_val_growth_pct_5", "data.export_val_growth_val", "data.export_val_growth_val_5", "data.import_val_growth_pct", "data.import_val_growth_pct_5", "data.import_val_growth_val", "data.import_val_growth_val_5")
                    all_all_YEAR <- all_all_YEAR[!drop]
                    setnames(all_all_YEAR, c("data.export_val", "data.hs92_id", "data.import_val"), c("export_val", "product_id", "import_val"))
                    all_all_YEAR$group_id = substr(all_all_YEAR$product_id, 1, 2)
                    all_all_YEAR$product_id_len = nchar(all_all_YEAR$product_id)

                    all_all_YEAR_8char = subset(all_all_YEAR, all_all_YEAR$product_id_len == "8")
                    all_all_YEAR_8char = join(hs92_8char, all_all_YEAR_8char, by = "product_id")
                    all_all_YEAR_8char = all_all_YEAR_8char[, c("product", "group", "product_id", "group_id", "import_val", "export_val")]
                    all_all_YEAR_8char$export_val <- as.numeric(all_all_YEAR_8char$export_val)
                    all_all_YEAR_8char$import_val <- as.numeric(all_all_YEAR_8char$import_val)
                    all_all_YEAR_8char$trade_exchange_val <- as.numeric(rowSums(all_all_YEAR_8char[, c("export_val", "import_val")], na.rm=T))
                    all_all_YEAR_8char$product_id = as.character(all_all_YEAR_8char$product_id)
                    all_all_YEAR_8char$image = paste0("d3plus/icons/hs/hs_", all_all_YEAR_8char$group_id, ".png")
                    all_all_YEAR_8char <- merge(all_all_YEAR_8char,hs_colors)

                    keep <- names(ORIGIN_all_YEAR_8char) %in% c("product_id","export_val")
                    ORIGIN_all_YEAR_8char_exp <- ORIGIN_all_YEAR_8char[keep]
                    setnames(ORIGIN_all_YEAR_8char_exp, "export_val","export_val_origin_all")

                    keep <- names(all_all_YEAR_8char) %in% c("product_id","export_val")
                    all_all_YEAR_8char_exp <- all_all_YEAR_8char[keep]
                    setnames(all_all_YEAR_8char_exp, "export_val","export_val_all_all")

                    ORIGIN_DESTINATION_YEAR <- as.data.frame(fromJSON(paste0("http://atlas.media.mit.edu/hs92/export/", YEAR, "/", ORIGIN, "/", DESTINATION, "/show/")))
                    drop <- names(ORIGIN_DESTINATION_YEAR) %in% c("data.dest_id", "data.hs92_id_len", "data.origin_id ", "data.year", "data.export_val_growth_pct", "data.export_val_growth_pct_5", "data.export_val_growth_val", "data.export_val_growth_val_5", "data.import_val_growth_pct", "data.import_val_growth_pct_5", "data.import_val_growth_val", "data.import_val_growth_val_5")
                    ORIGIN_DESTINATION_YEAR <- ORIGIN_DESTINATION_YEAR[!drop]
                    setnames(ORIGIN_DESTINATION_YEAR, c("data.export_val", "data.hs92_id", "data.import_val"), c("export_val", "product_id", "import_val"))
                    ORIGIN_DESTINATION_YEAR$group_id = substr(ORIGIN_DESTINATION_YEAR$product_id, 1, 2)
                    ORIGIN_DESTINATION_YEAR$product_id_len = nchar(ORIGIN_DESTINATION_YEAR$product_id)

                    ORIGIN_DESTINATION_YEAR_8char = subset(ORIGIN_DESTINATION_YEAR, ORIGIN_DESTINATION_YEAR$product_id_len == "8")
                    ORIGIN_DESTINATION_YEAR_8char = join(hs92_8char, ORIGIN_DESTINATION_YEAR_8char, by = "product_id")
                    ORIGIN_DESTINATION_YEAR_8char = ORIGIN_DESTINATION_YEAR_8char[, c("product", "group", "product_id", "group_id", "import_val", "export_val")]
                    ORIGIN_DESTINATION_YEAR_8char$export_val <- as.numeric(ORIGIN_DESTINATION_YEAR_8char$export_val)
                    ORIGIN_DESTINATION_YEAR_8char$import_val <- as.numeric(ORIGIN_DESTINATION_YEAR_8char$import_val)
                    ORIGIN_DESTINATION_YEAR_8char$trade_exchange_val <- as.numeric(rowSums(ORIGIN_DESTINATION_YEAR_8char[, c("export_val", "import_val")], na.rm=T))
                    ORIGIN_DESTINATION_YEAR_8char$product_id = as.character(ORIGIN_DESTINATION_YEAR_8char$product_id)
                    ORIGIN_DESTINATION_YEAR_8char <- merge(ORIGIN_DESTINATION_YEAR_8char,ORIGIN_all_YEAR_8char_exp)
                    ORIGIN_DESTINATION_YEAR_8char <- merge(ORIGIN_DESTINATION_YEAR_8char,all_all_YEAR_8char_exp)
                    ORIGIN_DESTINATION_YEAR_8char$rca = (ORIGIN_DESTINATION_YEAR_8char$export_val/ORIGIN_DESTINATION_YEAR_8char$export_val_all_all)/(sum(ORIGIN_DESTINATION_YEAR_8char$export_val_origin_all, na.rm=TRUE)/sum(ORIGIN_DESTINATION_YEAR_8char$export_val_all_all, na.rm=TRUE))
                    ORIGIN_DESTINATION_YEAR_8char$rca = round(ORIGIN_DESTINATION_YEAR_8char$rca, digits = 2)
                    ORIGIN_DESTINATION_YEAR_8char <- merge(ORIGIN_DESTINATION_YEAR_8char,hs_colors)
                    ORIGIN_DESTINATION_YEAR_8char$rca = as.character(ORIGIN_DESTINATION_YEAR_8char$rca)
                    ORIGIN_DESTINATION_YEAR_8char$image = paste0("d3plus/icons/hs/hs_", ORIGIN_DESTINATION_YEAR_8char$group_id, ".png")
                    setnames(ORIGIN_DESTINATION_YEAR_8char, c("export_val_all_all"), c("world_trade_val"))
                    drop <- names(ORIGIN_DESTINATION_YEAR_8char) %in% c("export_val_origin_all")
                    ORIGIN_DESTINATION_YEAR_8char <- ORIGIN_DESTINATION_YEAR_8char[!drop]
                    envir = as.environment(1)
                    assign(paste0(ORIGIN, "_", DESTINATION, "_", YEAR, "_8char"), ORIGIN_DESTINATION_YEAR_8char, envir = envir)

                    print("writing HS92 (8 characters) json and csv files...")
                    save_as_csv <- write.csv(ORIGIN_DESTINATION_YEAR_8char, paste0(OUTPUT,"_8char.csv"))
                    save_as_csv
                    jsonOut_8char <- toJSON(ORIGIN_DESTINATION_YEAR_8char, pretty = TRUE)
                    write(jsonOut_8char, file=paste0(OUTPUT,"_8char.json"))
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



