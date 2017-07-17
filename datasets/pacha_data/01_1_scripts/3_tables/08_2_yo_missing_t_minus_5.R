# Open the project datasets.Rproj three levels up to run this script

#1: read YODP

yodp_t1 = fread3(paste0(yodp_folder,classification,"_rev",rev,"_yodp_",digits,"_",t-1,".csv"))
yodp_t0 = fread3(paste0(yodp_folder,classification,"_rev",rev,"_yodp_",digits,"_",t,".csv"))

#2: create max product table

if(classification == "sitc" & digits == 4) {
  max_prod_imp = yodp_t0 %>% 
    select(prod_id,origin_id,import_val) %>% 
    group_by(origin_id) %>% 
    slice(which.max(import_val)) %>% 
    select(-import_val) %>% 
    rename(top_import = prod_id)
  
  max_prod_exp = yodp_t0 %>% 
    select(prod_id,origin_id,export_val) %>% 
    group_by(origin_id) %>% 
    slice(which.max(export_val)) %>% 
    select(-export_val) %>% 
    rename(top_export = prod_id)
}

if(classification == "hs" & digits == 4) {
  max_prod_imp = yodp_t0 %>% 
    select(prod_id,origin_id,import_val) %>% 
    group_by(origin_id) %>% 
    slice(which.max(import_val)) %>% 
    select(-import_val) %>% 
    rename(top_import_hs4 = prod_id)
  
  max_prod_exp = yodp_t0 %>% 
    select(prod_id,origin_id,export_val) %>% 
    group_by(origin_id) %>% 
    slice(which.max(export_val)) %>% 
    select(-export_val) %>% 
    rename(top_export_hs4 = prod_id)
}

if(classification == "hs" & digits == 6) {
  max_prod_imp = yodp_t0 %>% 
    select(prod_id,origin_id,import_val) %>% 
    group_by(origin_id) %>% 
    slice(which.max(import_val)) %>% 
    select(-import_val) %>% 
    rename(top_import_hs6 = prod_id)
  
  max_prod_exp = yodp_t0 %>% 
    select(prod_id,origin_id,export_val) %>% 
    group_by(origin_id) %>% 
    slice(which.max(export_val)) %>% 
    select(-export_val) %>% 
    rename(top_export_hs6 = prod_id)
}

#3: create YO 

yo_t1 = yodp_t1 %>%
  select(origin_id, export_val, import_val) %>% 
  group_by(origin_id) %>% 
  summarise(export_val = sum(export_val, na.rm = TRUE), import_val = sum(import_val, na.rm = TRUE)) %>% 
  rename(export_val1 = export_val, import_val1 = import_val)

yo_t0 = yodp_t0 %>%
  filter(dest_id != "xxwld") %>% 
  select(origin_id, export_val, import_val) %>% 
  group_by(origin_id) %>% 
  summarise(export_val = sum(export_val, na.rm = TRUE), import_val = sum(import_val, na.rm = TRUE)) %>% 
  left_join(yo_t1, join_by("yo")) %>%
  mutate(export_val5 = NA, import_val5 = NA) %>% 
  fix_na64_1() %>% 
  left_join(max_prod_imp, by = "origin_id") %>% 
  left_join(max_prod_exp, by = "origin_id") %>% 
  obtain_growth() %>%
  mutate(year = t) %>% 
  select(year, origin_id, everything())

rm(yodp_t0,yodp_t1,max_prod_imp,max_prod_exp,yo_t1)

fwrite(yo_t0, paste0(yo_folder,classification,"_rev",rev,"_yo_",digits,"_",t,".csv"))

rm(yo_t0)

gc()
