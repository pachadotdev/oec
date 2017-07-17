# Open the project datasets.Rproj two levels up to run this script

#1: Replace empty imports/exports with counter reported values

if(classification == "sitc") {J = 4}
if(classification == "hs") {J = c(4,6)}

for(j in J) {
  raw_csv_list_2 = list.files(path = temporal_raw_csv_folder_2, pattern = paste0("_",j,".csv"))
  verified_section_csv_folder = paste0(verified_csv_folder,"trade_",j,"/")
  
  if(file.exists(verified_section_csv_folder)) {
    print("The verified section files exist. Skipping.")
  } else {
    print("Verifying files. Please wait...") 
    try(dir.create(verified_section_csv_folder))
    for(k in 1:length(raw_csv_list_2)) {
      print(paste("Verifying file",raw_csv_list_2[[k]]))
      verified_csv = fread(paste0(temporal_raw_csv_folder_2,raw_csv_list_2[[k]]), colClasses = list(character = c("commodity_code")))
      
      verified_csv = tbl_dt(verified_csv) %>% 
        filter(reporter_iso != "sxm", partner_iso != "sxm") %>% # San Marino is "smr" and not "sxm"
        filter(reporter_iso != "wld", partner_iso != "wld") %>% # the World (wld) is not needed as the OEC aggregates and computes total trade
        unite(pairs, reporter_iso, partner_iso, commodity_code, remove = FALSE) 
      
      replacements_exports = verified_csv %>% 
        select(reporter_iso, partner_iso, export_val, commodity_code) %>% 
        unite(pairs, reporter_iso, partner_iso, commodity_code) %>% 
        rename(import_val_rep = export_val)
      
      replacements_imports = verified_csv %>% 
        select(reporter_iso, partner_iso, import_val, commodity_code) %>% 
        unite(pairs, reporter_iso, partner_iso, commodity_code) %>% 
        rename(export_val_rep = import_val)
      
      verified_csv = verified_csv %>% 
        left_join(replacements_exports, by = "pairs") %>% 
        left_join(replacements_imports, by = "pairs")
      
      verified_csv = verified_csv[export_val == as.integer64(0) | is.na.integer64(export_val), export_val := export_val_rep]
      verified_csv = verified_csv[import_val == as.integer64(0) | is.na.integer64(import_val), import_val := import_val_rep]
      
      # Fix int64 NAs (9218868437227407266 = NA)
      verified_csv = verified_csv[as.character(export_val) == "9218868437227407266", export_val := as.integer64(NA)]
      verified_csv = verified_csv[as.character(import_val) == "9218868437227407266", import_val := as.integer64(NA)]
      
      verified_csv %>% 
        mutate(marker = ifelse(export_val == export_val_rep & import_val == import_val_rep, 3,
                               ifelse(export_val == export_val_rep & import_val != import_val_rep, 2,
                                      ifelse(export_val != export_val_rep & import_val == import_val_rep, 1,
                                             NA)))) %>% 
        select(-c(export_val_rep,import_val_rep,pairs)) %>% 
        fwrite(paste0(verified_section_csv_folder,raw_csv_list_2[[k]]))
      
      rm(verified_csv,replacements_exports,replacements_imports)
      gc()
    }
  }
}
