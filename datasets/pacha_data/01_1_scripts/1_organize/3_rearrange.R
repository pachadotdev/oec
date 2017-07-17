# Open the project datasets.Rproj two levels up to run this script

#1: List all files in the raw csv folder

raw_csv_list = list.files(path = temporal_raw_csv_folder, pattern = "\\.csv")

#2: Rearrange csv files by trade flow

if(file.exists(temporal_raw_csv_folder_2)) {
  print("The rearranged files exist. Skipping.") 
} else {
  print("Rearranging files. Please wait...") 
  try(dir.create(temporal_raw_csv_folder_2))
  
  for(t in 1:length(raw_csv_list)) {
    raw_csv = fread(paste0(temporal_raw_csv_folder,raw_csv_list[[t]]), colClasses = list(character = c("Commodity Code")))
    
    setnames(raw_csv, colnames(raw_csv), c("classification","year","period","period_desc","aggregate_level",
                                           "is_leaf_code","trade_flow_code","trade_flow","reporter_code","reporter",
                                           "reporter_iso","partner_code","partner","partner_iso","commodity_code",
                                           "commodity","qty_unit_code","qty_unit","qty","netweight_kg",
                                           "trade_value_usd","flag"))
    
    raw_csv = raw_csv[, c("trade_flow","reporter_iso","partner_iso","aggregate_level","commodity_code","trade_value_usd")]
    
    raw_csv = tbl_dt(raw_csv) %>% 
      filter(trade_flow == "Export" | trade_flow == "Import") %>% 
      filter(!is.na(reporter_iso),!is.na(partner_iso)) %>% 
      filter(reporter_iso != "",partner_iso != "") %>% 
      filter(reporter_iso != " ",partner_iso != " ") %>% 
      mutate(reporter_iso = tolower(reporter_iso), partner_iso = tolower(partner_iso))
    
    if(classification == "sitc") {J = 4}
    if(classification == "hs") {J = c(4,6)}
    
    for(j in J) {
      if(!file.exists(paste0(temporal_raw_csv_folder_2,classification,"_rev",rev,"_",years[[t]],"_",j,".csv"))) {
        print(paste("Dividing year",years[[t]]))
        
        raw_csv %>%
          filter(aggregate_level == j) %>% 
          select(-aggregate_level) %>% 
          dcast(reporter_iso + partner_iso + commodity_code ~ trade_flow, value.var = "trade_value_usd") %>% 
          rename(export_val = Export, import_val = Import) %>% 
          mutate(year = years[[t]]) %>% 
          select(year,everything()) %>% 
          fwrite(paste0(temporal_raw_csv_folder_2,classification,"_rev",rev,"_",years[[t]],"_",j,".csv"))
      }
    }
    
    rm(raw_csv)
    gc()
  }
}
