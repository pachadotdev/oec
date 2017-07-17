# Open the project datasets.Rproj three levels up to run this script

#1: read files for the year t-1

trade_t1 = fread2(paste0(temporal_csv_folder,classification,"_rev",rev,"_",t-1,"_",digits,".csv")) %>% 
  countries_codes() %>% 
  rename(export_val1 = export_val, import_val1 = import_val) %>% 
  select(-c(year,marker))

#2: read files for the year t

trade_t0 = fread2(paste0(temporal_csv_folder,classification,"_rev",rev,"_",t,"_",digits,".csv")) %>% 
  countries_codes()

#3: create YODP table

yodp_t0 = trade_t0 %>% 
  left_join(trade_t1, by = join_by("yodp")) %>% 
  mutate(export_val5 = NA, import_val5 = NA) %>% 
  mutate(prod_id_len = nchar(prod_id)) %>% 
  fix_na64_1() %>% 
  obtain_growth() %>%
  mutate(prod_id_len = nchar(prod_id)) %>% 
  select(year, origin_id, dest_id, prod_id, prod_id_len, export_val, import_val,
         matches("export_val_"), matches("import_val_"), marker)

rm(trade_t0, trade_t1)

fwrite(yodp_t0, paste0(yodp_folder,classification,"_rev",rev,"_yodp_",digits,"_",t,".csv"))

rm(yodp_t0)

gc()
