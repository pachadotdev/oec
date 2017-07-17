# Open the project datasets.Rproj three levels up to run this script

#1: List countries with a population greater or equal to 1.25 million

country_codes = read_excel("comtrade_data/country_codes/country_codes.xls") %>% 
  rename(country_code = `Country Code`, country_iso = `ISO3-digit Alpha`) %>% 
  select(country_code, country_iso) %>%
  mutate(country_iso = tolower(country_iso)) %>% 
  filter(country_iso != "null", 
         country_iso != "wld") 

population_estimates = read_excel("united_nations_data/un_population_2015.xls", sheet = "ESTIMATES", skip = 16) %>% 
  rename(country_code = `Country code`, pop_estimate_2015 = `2015`) %>% 
  select(country_code, pop_estimate_2015)

## India (356) shows no data because the Excel file says "India = IND = 356" BUT the API has data for "India (excl. Sikkim) = IND = 699"
## Also some countries have mismatching codes between trade and demography data
population_estimates =  population_estimates %>%
  mutate(country_code = ifelse(country_code == 840, 842, country_code), # United States
         country_code = ifelse(country_code == 756, 757, country_code), # Switzerland
         country_code = ifelse(country_code == 729, 736, country_code), # Sudan
         country_code = ifelse(country_code == 578, 579, country_code), # Norway
         country_code = ifelse(country_code == 380, 381, country_code), # Italy
         country_code = ifelse(country_code == 356, 699, country_code), # India
         country_code = ifelse(country_code == 250, 251, country_code)) # France

country_codes = left_join(x= country_codes, y = population_estimates) %>%
  filter(pop_estimate_2015 >= 1250) %>%
  arrange(country_code)

## Finally I create a list of countries
country_codes_list = as.list(country_codes$country_iso)
rm(population_estimates)

# 2: Import OEC codes and replace ISO codes by OEC codes

oec_country_names = tbl_dt(fread("oec_data/country_names.tsv","\t")) %>% 
  rename(country_iso = id_3char)

country_codes = tbl_dt(left_join(x= country_codes, y = oec_country_names)) %>%
  select(-c(country_code,pop_estimate_2015)) %>%
  rename(reporter_iso = country_iso)

#3: For each year compute RCA exports

try(dir.create(rca_exports_folder))

for(t in 1:length(years)){
  if(file.exists(paste0(rca_exports_folder,"rca_exports_",years[[t]],".csv"))) {
    print(paste0("Skipping year ",years[[t]],". The clean file already exist."))
  } else {
    if(!file.exists(paste0(temporal_csv_folder,classification,"_rev",rev,"_",years[[t]],"_",digits,".csv"))) {
      print(paste0("Skipping year ",years[[t]],". The verified file for the year ",years[[t]]," was not found."))
    } else {
      print(paste0("Creating RCA file for the year ",years[[t]],". Be patient..."))
      exports = fread(paste0(temporal_csv_folder,classification,"_rev",rev,"_",years[[t]],"_",digits,".csv"), colClasses = list(character = c("commodity_code","marker")), verbose = TRUE)
      
      exports = tbl_dt(exports) %>% 
        select(-c(import_val,partner_iso,marker)) %>% 
        unite(pairs, reporter_iso, commodity_code, remove = FALSE) %>% 
        filter(reporter_iso %in% country_codes_list) #Keep countries with population greater or equal to 1.25 million
        
      sum_by_product = exports %>% 
        group_by(commodity_code) %>% # Sum by product
        summarise(sum_p_xcp = sum(export_val, na.rm = TRUE))
      
      sum_by_country = exports %>% 
        group_by(reporter_iso) %>% # Sum by country
        summarise(sum_c_xcp = sum(export_val, na.rm = TRUE))
      
      sum_by_country_and_product = exports %>% 
        group_by(pairs) %>% # Sum country exports by product
        summarise(xcp = sum(export_val, na.rm = TRUE))

      exports = exports %>% 
        left_join(sum_by_product) %>% 
        left_join(sum_by_country) %>% 
        left_join(sum_by_country_and_product) %>% 
        filter(sum_c_xcp >= 1000000000) %>% # Remove countries whose traded value is less than 1 billion to avoid distortions
        filter(sum_p_xcp >= 10000000) %>% # Remove products whose traded value is less than 10 million to avoid distortions
        distinct(pairs, .keep_all = TRUE) %>% 
        select(-pairs)
      
      exports = exports %>% 
        mutate(sum_c_p_xcp = sum(xcp, na.rm = TRUE)) %>%  # World's total exported value
        mutate(rca_export = (xcp/sum_c_xcp) / (sum_p_xcp/sum_c_p_xcp)) %>%  # Compute RCA
        mutate(rca_export = ifelse(is.nan(rca_export), NA, rca_export)) %>% 
        left_join(country_codes) %>% 
        select(year,id,commodity_code,rca_export)
  
      fwrite(exports, paste0(rca_exports_folder,classification,"_rev",rev,"_",digits,"_rca_exports_",years[[t]],".csv"), verbose = TRUE)
      rm(exports,sum_by_product,sum_by_country,sum_by_country_and_product)
    }
  }
}

rm(country_codes,oec_country_names)
gc()
