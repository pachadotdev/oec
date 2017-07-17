# Open the project datasets.Rproj three levels up to run this script

#1: read YODP

yodp_t0 = fread3(paste0(yodp_folder,classification,"_rev",rev,"_yodp_",digits,"_",t,".csv"))

#2: read PCI table

pci_table = tbl_dt(fread(paste0(pci_folder,classification,"_rev",rev,"_",digits,"_joined_pci_rankings.csv"), colClasses = list(character = c("id")))) %>% 
  left_join(products, by = "id") %>% 
  select(-id)

pci_table_t0 = pci_table %>%
  filter(year == t) %>% 
  select(-year) %>% 
  mutate(pci_rank_delta = NA) %>% 
  select(prod_id,everything())

rm(pci_table)

#3: create max exporter/importer table

max_imp = yodp_t0 %>% 
  select(prod_id,dest_id,import_val) %>% 
  group_by(prod_id) %>% 
  slice(which.max(import_val)) %>% 
  select(-import_val) %>% 
  rename(top_importer = dest_id)

max_exp = yodp_t0 %>% 
  select(prod_id,origin_id,export_val) %>% 
  group_by(prod_id) %>% 
  slice(which.max(export_val)) %>% 
  select(-export_val) %>% 
  rename(top_exporter = origin_id)

#4: create YP 

yp_t0 = yodp_t0 %>%
  select(prod_id, export_val, import_val) %>% 
  group_by(prod_id) %>% 
  summarise(export_val = sum(export_val, na.rm = TRUE), import_val = sum(import_val, na.rm = TRUE)) %>% 
  mutate(export_val1 = NA, import_val1 = NA,
         export_val5 = NA, import_val5 = NA) %>% 
  left_join(pci_table_t0, by = "prod_id") %>% 
  left_join(max_imp, by = "prod_id") %>% 
  left_join(max_exp, by = "prod_id") %>% 
  obtain_growth() %>%
  mutate(year = t) %>% 
  mutate(prod_id_len = nchar(prod_id)) %>% 
  select(year, prod_id, prod_id_len, everything())

rm(yodp_t0,max_exp,max_imp)

fwrite(yp_t0, paste0(yp_folder,classification,"_rev",rev,"_yp_",digits,"_",t,".csv"))

rm(yp_t0)

gc()
