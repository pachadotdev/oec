# Open the project datasets.Rproj three levels up to run this script

#1: read YODP

yodp_t1 = fread3(paste0(yodp_folder,classification,"_rev",rev,"_yodp_",digits,"_",t-1,".csv"))
yodp_t0 = fread3(paste0(yodp_folder,classification,"_rev",rev,"_yodp_",digits,"_",t,".csv"))

#2: create YD 

yd_t1 = yodp_t1 %>%
  select(dest_id, export_val, import_val) %>% 
  group_by(dest_id) %>% 
  summarise(export_val = sum(export_val, na.rm = TRUE), import_val = sum(import_val, na.rm = TRUE)) %>% 
  rename(export_val1 = export_val, import_val1 = import_val)

rm(yodp_t1)

yd_t0 = yodp_t0 %>%
  select(dest_id, export_val, import_val) %>% 
  group_by(dest_id) %>% 
  summarise(export_val = sum(export_val, na.rm = TRUE), import_val = sum(import_val, na.rm = TRUE)) %>% 
  left_join(yd_t1, by = join_by("yd")) %>% 
  mutate(export_val5 = NA, import_val5 = NA) %>% 
  fix_na64_1() %>% 
  obtain_growth() %>%
  mutate(year = t) %>% 
  select(year, dest_id, everything())

rm(yodp_t0, yd_t1)

fwrite(yd_t0, paste0(yd_folder,classification,"_rev",rev,"_yd_",digits,"_",t,".csv"))

rm(yd_t0)

gc()
