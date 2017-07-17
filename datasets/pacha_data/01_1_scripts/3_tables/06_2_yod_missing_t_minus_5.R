# Open the project datasets.Rproj three levels up to run this script

#1: read YODP

yodp_t1 = fread3(paste0(yodp_folder,classification,"_rev",rev,"_yodp_",digits,"_",t-1,".csv"))
yodp_t0 = fread3(paste0(yodp_folder,classification,"_rev",rev,"_yodp_",digits,"_",t,".csv"))

#2: create YOD 

yod_t1 = yodp_t1 %>%
  select(origin_id, dest_id, export_val, import_val) %>% 
  group_by(origin_id, dest_id) %>% 
  summarise(export_val = sum(export_val, na.rm = TRUE), import_val = sum(import_val, na.rm = TRUE)) %>% 
  rename(export_val1 = export_val, import_val1 = import_val)

rm(yodp_t1)

yod_t0 = yodp_t0 %>%
  select(origin_id, dest_id, export_val, import_val) %>% 
  group_by(origin_id, dest_id) %>% 
  summarise(export_val = sum(export_val, na.rm = TRUE), import_val = sum(import_val, na.rm = TRUE)) %>% 
  left_join(yod_t1, by = join_by("yod")) %>% 
  mutate(export_val5 = NA, import_val5 = NA) %>% 
  fix_na64_1() %>% 
  obtain_growth() %>%
  mutate(year = t) %>% 
  select(year, origin_id, dest_id, everything())

rm(yodp_t0, yod_t1)

fwrite(yod_t0, paste0(yod_folder,classification,"_rev",rev,"_yod_",digits,"_",t,".csv"))

rm(yod_t0)

gc()
