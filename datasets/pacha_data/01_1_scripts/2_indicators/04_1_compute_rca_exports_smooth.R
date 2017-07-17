# Open the project datasets.Rproj three levels up to run this script

#1: For each year compute smooth RCA exports

try(dir.create(rca_exports_smooth_folder))

years_inv = rev(years)

for(t in 1:(length(years_inv)-2)) {
  if(file.exists(paste0(rca_exports_smooth_folder,classification,"_rev",rev,"_",digits,"_rca_exports_smooth_",years_inv[[t]],".csv"))) {
    print(paste0("Skipping year ",years_inv[[t]],". The clean file already exist."))
  } else {
    if(!file.exists(paste0(rca_exports_folder,classification,"_rev",rev,"_",digits,"_rca_exports_",years_inv[[t]],".csv"))   |
       !file.exists(paste0(rca_exports_folder,classification,"_rev",rev,"_",digits,"_rca_exports_",years_inv[[t+1]],".csv")) |
       !file.exists(paste0(rca_exports_folder,classification,"_rev",rev,"_",digits,"_rca_exports_",years_inv[[t+2]],".csv")) ) {
      print(paste0("Skipping year ",years_inv[[t]],". The source files for the years_inv ",years_inv[[t]],years_inv[[t+1]],years_inv[[t+2]]," were not found."))
    } else {
      print(paste0("Creating RCA smooth file for the year ",years_inv[[t]],". Be patient..."))
      exports_t3 = fread(paste0(rca_exports_folder,classification,"_rev",rev,"_",digits,"_rca_exports_",years_inv[[t]],".csv"), colClasses = list(character = c("commodity_code")), verbose = TRUE)
      exports_t2 = fread(paste0(rca_exports_folder,classification,"_rev",rev,"_",digits,"_rca_exports_",years_inv[[t+1]],".csv"), colClasses = list(character = c("commodity_code")), verbose = TRUE)
      exports_t1 = fread(paste0(rca_exports_folder,classification,"_rev",rev,"_",digits,"_rca_exports_",years_inv[[t+2]],".csv"), colClasses = list(character = c("commodity_code")), verbose = TRUE)
      
      exports_t1 = tbl_dt(exports_t1) %>% 
        unite(pairs, id, commodity_code, remove = FALSE) %>% 
        rename(rca_export_t1 = rca_export) %>% 
        select(pairs, rca_export_t1)
      
      exports_t2 = tbl_dt(exports_t2) %>% 
        unite(pairs, id, commodity_code, remove = FALSE) %>% 
        rename(rca_export_t2 = rca_export) %>% 
        select(pairs, rca_export_t2)
      
      exports_t3 = tbl_dt(exports_t3) %>% 
        unite(pairs, id, commodity_code, remove = FALSE) %>% 
        rename(rca_export_t3 = rca_export) %>% 
        left_join(exports_t2, by = "pairs") %>% 
        left_join(exports_t1, by = "pairs") %>% 
        select(-pairs)
      
      exports_t3 = exports_t3 %>% 
        rowwise() %>% # To apply a weighted mean by rows with 1 weight = 1 column
        mutate(rca_export_smooth = weighted.mean(x = c(rca_export_t3,rca_export_t2,rca_export_t1), w = c(2,1,1), na.rm = TRUE)) %>% 
        mutate(rca_export_smooth = ifelse(is.nan(rca_export_smooth), NA, rca_export_smooth))

      fwrite(exports_t3, paste0(rca_exports_smooth_folder,classification,"_rev",rev,"_",digits,"_rca_exports_smooth_",years_inv[[t]],".csv"), verbose = TRUE)
      rm(exports_t1,exports_t2,exports_t3)
    }
  }
}

for(t in (length(years_inv)-1)) {
  if(file.exists(paste0(rca_exports_smooth_folder,classification,"_rev",rev,"_",digits,"_rca_exports_smooth_",years_inv[[t]],".csv"))) {
    print(paste0("Skipping year ",years_inv[[t]],". The clean file already exist."))
  } else {
    if(!file.exists(paste0(rca_exports_folder,classification,"_rev",rev,"_",digits,"_rca_exports_",years_inv[[t]],".csv"))   |
       !file.exists(paste0(rca_exports_folder,classification,"_rev",rev,"_",digits,"_rca_exports_",years_inv[[t+1]],".csv")) ) {
      print(paste0("Skipping year ",years_inv[[t]],". The source files for the years_inv ",years_inv[[t]],years_inv[[t+1]],years_inv[[t+2]]," were not found."))
    } else {
      print(paste0("Creating RCA smooth file for the year ",years_inv[[t]],". Be patient..."))
      exports_t3 = fread(paste0(rca_exports_folder,classification,"_rev",rev,"_",digits,"_rca_exports_",years_inv[[t]],".csv"), colClasses = list(character = c("commodity_code")), verbose = TRUE)
      exports_t2 = fread(paste0(rca_exports_folder,classification,"_rev",rev,"_",digits,"_rca_exports_",years_inv[[t+1]],".csv"), colClasses = list(character = c("commodity_code")), verbose = TRUE)
      
      exports_t2 = tbl_dt(exports_t2) %>% 
        unite(pairs, id, commodity_code, remove = FALSE) %>% 
        rename(rca_export_t2 = rca_export) %>% 
        select(pairs, rca_export_t2)
      
      exports_t3 = tbl_dt(exports_t3) %>% 
        unite(pairs, id, commodity_code, remove = FALSE) %>% 
        rename(rca_export_t3 = rca_export) %>% 
        left_join(exports_t2, by = "pairs") %>% 
        mutate(rca_export_t1 = NA) %>% 
        select(-pairs)
      
      exports_t3 = exports_t3 %>% 
        rowwise() %>% # To apply a weighted mean by rows with 1 weight = 1 column
        mutate(rca_export_smooth = weighted.mean(x = c(rca_export_t3,rca_export_t2,rca_export_t1), w = c(2,1,1), na.rm = TRUE)) %>% 
        mutate(rca_export_smooth = ifelse(is.nan(rca_export_smooth), NA, rca_export_smooth))
      
      fwrite(exports_t3, paste0(rca_exports_smooth_folder,classification,"_rev",rev,"_",digits,"_rca_exports_smooth_",years_inv[[t]],".csv"), verbose = TRUE)
      rm(exports_t2,exports_t3)
    }
  }
}

for(t in length(years_inv)) {
  if(file.exists(paste0(rca_exports_smooth_folder,classification,"_rev",rev,"_",digits,"_rca_exports_smooth_",years_inv[[t]],".csv"))) {
    print(paste0("Skipping year ",years_inv[[t]],". The clean file already exist."))
  } else {
    if(!file.exists(paste0(rca_exports_folder,classification,"_rev",rev,"_",digits,"_rca_exports_",years_inv[[t]],".csv"))) {
      print(paste0("Skipping year ",years_inv[[t]],". The source files for the years_inv ",years_inv[[t]],years_inv[[t+1]],years_inv[[t+2]]," were not found."))
    } else {
      print(paste0("Creating RCA smooth file for the year ",years_inv[[t]],". Be patient..."))
      exports_t3 = fread(paste0(rca_exports_folder,classification,"_rev",rev,"_",digits,"_rca_exports_",years_inv[[t]],".csv"), colClasses = list(character = c("commodity_code")), verbose = TRUE)
      
      exports_t3 = tbl_dt(exports_t3) %>% 
        unite(pairs, id, commodity_code, remove = FALSE) %>% 
        rename(rca_export_t3 = rca_export) %>% 
        mutate(rca_export_t2 = NA) %>% 
        mutate(rca_export_t1 = NA) %>% 
        select(-pairs)
      
      exports_t3 = exports_t3 %>% 
        rowwise() %>% # To apply a weighted mean by rows with 1 weight = 1 column
        mutate(rca_export_smooth = weighted.mean(x = c(rca_export_t3,rca_export_t2,rca_export_t1), w = c(2,1,1), na.rm = TRUE)) %>% 
        mutate(rca_export_smooth = ifelse(is.nan(rca_export_smooth), NA, rca_export_smooth))
      
      fwrite(exports_t3, paste0(rca_exports_smooth_folder,classification,"_rev",rev,"_",digits,"_rca_exports_smooth_",years_inv[[t]],".csv"), verbose = TRUE)
      rm(exports_t3)
    }
  }
}

gc()
