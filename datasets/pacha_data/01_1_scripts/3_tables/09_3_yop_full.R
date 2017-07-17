# Open the project datasets.Rproj three levels up to run this script

#1: read YODP

yodp_t5 = fread3(paste0(yodp_folder,classification,"_rev",rev,"_yodp_",digits,"_",t-5,".csv"))
yodp_t1 = fread3(paste0(yodp_folder,classification,"_rev",rev,"_yodp_",digits,"_",t-1,".csv"))
yodp_t0 = fread3(paste0(yodp_folder,classification,"_rev",rev,"_yodp_",digits,"_",t,".csv"))

#2: extract RCA data

rca_exports_data = tbl_dt(fread(paste0(rca_exp_folder,classification,"_rev",rev,"_",digits,"_rca_exports_smooth_",t,".csv"), colClasses = list(character = c("commodity_code")))) %>% 
  select(id,rca_export_smooth,commodity_code) %>% 
  rename(origin_id = id, export_rca = rca_export_smooth, id = commodity_code) %>% 
  left_join(products, by = "id") %>% 
  select(-id)

rca_imports_data = tbl_dt(fread(paste0(rca_imp_folder,classification,"_rev",rev,"_",digits,"_rca_imports_smooth_",t,".csv"), colClasses = list(character = c("commodity_code")))) %>% 
  select(id,rca_import_smooth,commodity_code) %>% 
  rename(origin_id = id, import_rca = rca_import_smooth, id = commodity_code) %>% 
  left_join(products, by = "id") %>% 
  select(-id)

#3: create YOP 

yop_t5 = yodp_t5 %>%
  select(origin_id, prod_id, export_val, import_val) %>% 
  group_by(origin_id,prod_id) %>% 
  summarise(export_val = sum(export_val, na.rm = TRUE), import_val = sum(import_val, na.rm = TRUE)) %>% 
  rename(export_val5 = export_val, import_val5 = import_val)

yop_t1 = yodp_t1 %>%
  select(origin_id, prod_id, export_val, import_val) %>% 
  group_by(origin_id,prod_id) %>% 
  summarise(export_val = sum(export_val, na.rm = TRUE), import_val = sum(import_val, na.rm = TRUE)) %>% 
  rename(export_val1 = export_val, import_val1 = import_val)

yop_t0 = yodp_t0 %>%
  select(origin_id, prod_id, export_val, import_val) %>% 
  group_by(origin_id,prod_id) %>% 
  summarise(export_val = sum(export_val, na.rm = TRUE), import_val = sum(import_val, na.rm = TRUE)) %>% 
  left_join(yop_t1, by = join_by("yop")) %>% 
  left_join(yop_t5, by = join_by("yop")) %>% 
  fix_na64_1() %>% 
  fix_na64_5() %>% 
  left_join(rca_exports_data, by = join_by("yop")) %>% 
  left_join(rca_imports_data, by = join_by("yop")) %>% 
  obtain_growth() %>%
  mutate(year = t) %>% 
  mutate(prod_id_len = nchar(prod_id)) %>% 
  select(year, origin_id, prod_id, prod_id_len, everything())

rm(yodp_t0,yodp_t1,yodp_t5,yop_t1,yop_t5,rca_exports_data,rca_imports_data)

fwrite(yop_t0, paste0(yop_folder,classification,"_rev",rev,"_yop_",digits,"_",t,".csv"))

rm(yop_t0)

gc()
