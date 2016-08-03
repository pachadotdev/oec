#' Creates a stacked area plot to compare two years
#' @export
#' @return Creates an \code{HTML} file with a bar chart visualization that compares two given years.
#' @param ORIGIN is the country code of origin (e.g. "chl" for Chile)
#' @param DESTINATION is the country code of origin (e.g. "chn" for China)
#' @param VARIABLE is the variable to visualize and it can be "imports", "exports" or "exchange" (trade exchange)
#' @param CLASSIFICATION refers to the trade classification that can be "6" (HS92 6 characters) or "8" (HS92 8 characters) for the year 1995 and going or "4" (SITC rev.2 4 characters) for the year 1962 and ongoing
#' @param YEAR is the initial year and the OEC's API ranges from 1962 to 2014
#' @examples
#' stackedareaplot.comparison(chl, chn, exports, 6, 2010)
#' @keywords functions

stackedareaplot.comparison <- function(ORIGIN, DESTINATION, VARIABLE, CLASSIFICATION, YEAR) {
  d3_folder <- paste0(getwd(), "/d3plus")
  if(!file.exists(d3_folder)){
    print("d3plus not installed... installing using install_d3plus()...")
    install_d3plus()
  }

  ORIGIN <- deparse(substitute(ORIGIN))
  DESTINATION <- deparse(substitute(DESTINATION))
  VARIABLE <- deparse(substitute(VARIABLE))
  STEP = 1

  INPUT <- paste(ORIGIN, DESTINATION, YEAR, "comparison", CLASSIFICATION, sep="_")
  INPUT <- paste0(INPUT, "char")

  YEARminus1step <- YEAR - STEP
  YEARminus2step <- YEAR - 2*STEP
  YEARplus1step <- YEAR + STEP
  YEARplus2step <- YEAR + 2*STEP

  if((YEARminus2step > 1961) & (YEARminus2step < 2015)) {
    print(paste0("Processing files for the year ", YEARminus2step, "..."))
    getdata(ORIGIN, DESTINATION, CLASSIFICATION, YEARminus2step)
    YEARminus2step_file <- paste(ORIGIN, DESTINATION, YEARminus2step, CLASSIFICATION, sep="_")
    YEARminus2step_file <- paste0(YEARminus2step_file, "char.json")
    ORIGIN_YEARminus2step <- as.data.frame(fromJSON(YEARminus2step_file))
  } else {
    YEARminus2step <- NA
  }

  if((YEARminus1step > 1961) & (YEARminus1step < 2015)) {
    print(paste0("Processing files for the year ", YEARminus1step, "..."))
    getdata(ORIGIN, DESTINATION, CLASSIFICATION, YEARminus1step)
    YEARminus1step_file <- paste(ORIGIN, DESTINATION, YEARminus1step, CLASSIFICATION, sep="_")
    YEARminus1step_file <- paste0(YEARminus1step_file, "char.json")
    ORIGIN_YEARminus1step <- as.data.frame(fromJSON(YEARminus1step_file))
  } else {
    YEARminus1step <- NA
  }

  if((YEAR > 1961) & (YEAR < 2015)) {
    print(paste0("Processing files for the year ", YEAR, "..."))
    getdata(ORIGIN, DESTINATION, CLASSIFICATION, YEAR)
    YEAR_file <- paste(ORIGIN, DESTINATION, YEAR, CLASSIFICATION, sep="_")
    YEAR_file <- paste0(YEAR_file, "char.json")
    ORIGIN_YEARcentered <- as.data.frame(fromJSON(YEAR_file))
  } else {
    YEAR <- NA
  }

  if((YEARplus1step > 1961) & (YEARplus1step < 2015)) {
    print(paste0("Processing files for the year ", YEARplus1step, "..."))
    getdata(ORIGIN, DESTINATION, CLASSIFICATION, YEARplus1step)
    YEARplus1step_file <- paste(ORIGIN, DESTINATION, YEARplus1step, CLASSIFICATION, sep="_")
    YEARplus1step_file <- paste0(YEARplus1step_file, "char.json")
    ORIGIN_YEARplus1step <- as.data.frame(fromJSON(YEARplus1step_file))
  } else {
    YEARplus1step <- NA
  }

  if((YEARplus2step > 1961) & (YEARplus2step < 2015)) {
    print(paste0("Processing files for the year ", YEARplus2step, "..."))
    getdata(ORIGIN, DESTINATION, CLASSIFICATION, YEARplus2step)
    YEARplus2step_file <- paste(ORIGIN, DESTINATION, YEARplus2step, CLASSIFICATION, sep="_")
    YEARplus2step_file <- paste0(YEARplus2step_file, "char.json")
    ORIGIN_YEARplus2step <- as.data.frame(fromJSON(YEARplus2step_file))
  } else {
    YEARplus2step <- NA
  }

  if(!is.na(YEARminus2step)) {
    ORIGIN_compare <- rbind(ORIGIN_YEARcentered, ORIGIN_YEARminus2step)
    if(!is.na(YEARminus1step)) {
      ORIGIN_compare <- rbind(ORIGIN_compare, ORIGIN_YEARminus1step)
    }
    if(!is.na(YEARplus1step)) {
      ORIGIN_compare <- rbind(ORIGIN_compare, ORIGIN_YEARplus1step)
    }
    if(!is.na(YEARplus2step)) {
      ORIGIN_compare <- rbind(ORIGIN_compare, ORIGIN_YEARplus2step)
    }
  } else {
    ORIGIN_compare <- ORIGIN_YEARcentered
  }

  ORIGIN_compare$export_val <- ifelse(ORIGIN_compare$export_val == 0, NA, ORIGIN_compare$export_val)

  write(toJSON(ORIGIN_compare, pretty = TRUE), file=paste0(INPUT, ".json"))
  write.csv(ORIGIN_compare, file=paste0(INPUT, ".csv"))

  envir = as.environment(1)
  assign(paste0(ORIGIN, "_", DESTINATION, "_", YEAR, "_comparison_", CLASSIFICATION, "char"), ORIGIN_compare, envir = envir)

  code_lenght = CLASSIFICATION
  if(code_lenght == 4) {
    code_display = "SITC code"
  }
  if(code_lenght == 6) {
    code_display = "HS92 code"
  }

  if(code_lenght == 4 | code_lenght == 6) {
    variablecol <- ifelse(VARIABLE == "imports", "import_val",
                          ifelse(VARIABLE == "exports", "export_val",
                                 ifelse(VARIABLE == "exchange", "trade_exchange_val", "error")))
    variablename <- ifelse(VARIABLE == "imports", "Import",
                           ifelse(VARIABLE == "exports", "Export",
                                  ifelse(VARIABLE == "exchange", "Trade exchange", "error")))

    json_file <- paste0(INPUT, ".json")
    if(!file.exists(json_file)){
      print("json file not found. run getdata() first")
    } else {
      ### html ###
      OUTPUT = INPUT
      html_file <- paste0(OUTPUT, "_stackedareaplot_", VARIABLE, ".html")
      if(!file.exists(html_file)){
        print("creating bar chart")
        stackedareaplot_compare_template <- paste(readLines(system.file("extdata", "stackedareaplot_compare_template.html", package = "oec"), warn = F), collapse = "\n")
        stackedareaplot_compare_template = gsub("json_file", paste0(OUTPUT, ".json"), stackedareaplot_compare_template)
        stackedareaplot_compare_template = gsub("code_display", code_display, stackedareaplot_compare_template)
        stackedareaplot_compare_template = gsub("variablecol", variablecol, stackedareaplot_compare_template)
        stackedareaplot_compare_template = gsub("variablename", variablename, stackedareaplot_compare_template)
        print("writing html file...")
        writeLines(stackedareaplot_compare_template, paste0(OUTPUT, "_stackedareaplot_exports", ".html"))
        print("opening html files in the browser.")
        httw(pattern = NULL, daemon = TRUE)
      } else {
        print("html treemap file already exists. skipping.")
        print("opening html files in the browser.")
        httw(pattern = NULL, daemon = TRUE)
      }
    }
  } else {
    print('Stackedareaplot.comparison only admits 4 characters codes (SITC) or 6 characters codes (HS92).')
  }
}
