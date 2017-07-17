# generic functions

messageline = function() {
  message(rep("-", 60))
}

fread2 = function(x) {
  messageline()
  message("function fread2")
  message("x: ", x)
  suppressWarnings(tbl_df(fread(x, colClasses = list(character = c("commodity_code")))))
}

fread3 = function(x) {
  messageline()
  message("function fread3")
  message("x: ", x)
  suppressWarnings(tbl_dt(fread(x, colClasses = list(character = c("prod_id")))))
}

countries_codes = function(dout) {
  messageline()
  message("function: countries_codes")
  
  dout = dout %>% 
    left_join(countries, by = c("reporter_iso" = "id_3char")) %>% 
    left_join(countries, by = c("partner_iso" = "id_3char")) %>% 
    left_join(products, by = c("commodity_code" = "id")) %>% 
    select(-c(reporter_iso,partner_iso,commodity_code)) %>% 
    rename(origin_id = id.x, dest_id = id.y)
}

join_by = function(group_fun_name = "get_yd") {
  switch(group_fun_name,
         yd = "dest_id",
         ydp = c("dest_id", "prod_id"),
         yo = "origin_id",
         yod = c("origin_id", "dest_id"),
         yodp = c("origin_id", "dest_id", "prod_id"),
         yop = c("origin_id", "prod_id"),
         yp = "prod_id")
}

fix_na64_1 = function(dout) {
  messageline()
  message("function: fix_na64_1")
  
  dout = dout %>% 
    mutate(export_val1 = if_else(as.character(export_val1) == "9218868437227407266", as.integer64(NA), export_val1)) %>% 
    mutate(import_val1 = if_else(as.character(import_val1) == "9218868437227407266", as.integer64(NA), import_val1))
}

fix_na64_5 = function(dout) {
  messageline()
  message("function: fix_na64_5")
  
  dout = dout %>% 
    mutate(export_val5 = if_else(as.character(export_val5) == "9218868437227407266", as.integer64(NA), export_val1)) %>% 
    mutate(import_val5 = if_else(as.character(import_val5) == "9218868437227407266", as.integer64(NA), import_val1))
}

obtain_growth = function(dout) {
  messageline()
  message("function: obtain_growth")
  
  dout = dout %>% 
    mutate(export_val_growth_pct   = suppressWarnings((export_val - export_val1) / export_val1),
           export_val_growth_pct_5 = suppressWarnings((export_val - export_val5) / export_val5),
           export_val_growth_val   = export_val - export_val1,
           export_val_growth_val_5 = export_val - export_val5,
           import_val_growth_pct   = suppressWarnings((import_val - import_val1) / import_val1),
           import_val_growth_pct_5 = suppressWarnings((import_val - import_val5) / import_val5),
           import_val_growth_val   = import_val - import_val1,
           import_val_growth_val_5 = import_val - import_val5) %>%
    select(-c(export_val1, export_val5, import_val1, import_val5))
}
