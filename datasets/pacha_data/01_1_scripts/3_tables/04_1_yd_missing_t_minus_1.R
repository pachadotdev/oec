# Open the project datasets.Rproj three levels up to run this script

#1: read YODP

yodp_t0 = fread3(paste0(yodp_folder,classification,"_rev",rev,"_yodp_",digits,"_",t,".csv"))

#2: create YD 
yd_t0 = yodp_t0 %>%
  select(dest_id, export_val, import_val) %>% 
  group_by(dest_id) %>% 
  summarise(export_val = sum(export_val, na.rm = TRUE), import_val = sum(import_val, na.rm = TRUE)) %>% 
  mutate(export_val1 = NA, import_val1 = NA,
         export_val5 = NA, import_val5 = NA) %>% 
  obtain_growth() %>%
  mutate(year = t) %>% 
  select(year, dest_id, everything())

rm(yodp_t0)

fwrite(yd_t0, paste0(yd_folder,classification,"_rev",rev,"_yd_",digits,"_",t,".csv"))

rm(yd_t0)

gc()
