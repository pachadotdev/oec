# Open the project datasets.Rproj three levels up to run this script

#1: read files for the year t

trade_t0 = fread2(paste0(temporal_csv_folder,classification,"_rev",rev,"_",t,"_",digits,".csv")) %>% 
  countries_codes()

#2: create YODP table

yodp_t0 = trade_t0 %>% 
  mutate(export_val1 = NA, import_val1 = NA) %>% 
  mutate(export_val5 = NA, import_val5 = NA) %>% 
  obtain_growth() %>%
  mutate(prod_id_len = nchar(prod_id)) %>% 
  select(year, origin_id, dest_id, prod_id, prod_id_len, export_val, import_val,
         matches("export_val_"),matches("import_val_"),marker)

rm(trade_t0)

fwrite(yodp_t0, paste0(yodp_folder,classification,"_rev",rev,"_yodp_",digits,"_",t,".csv"))

rm(yodp_t0)

gc()
