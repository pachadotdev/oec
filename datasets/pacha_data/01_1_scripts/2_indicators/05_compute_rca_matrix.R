# Open the project datasets.Rproj three levels up to run this script

#1: for each year compute the RCA matrix

try(dir.create(rca_matrices_folder))

for(t in 1:(length(years_inv))) {
  if(file.exists(paste0(rca_matrices_folder,classification,"_rev",rev,"_",digits,"_rca_matrix_",years_inv[[t]],".csv"))) {
    print(paste0("Skipping year ",years_inv[[t]],". The RCA matrix already exist."))
  } else {
    if(!file.exists(paste0(rca_exports_smooth_folder,classification,"_rev",rev,"_",digits,"_rca_exports_smooth_",years_inv[[t]],".csv"))) {
      print(paste0("Skipping year ",years_inv[[t]],". The source file for the year ",years_inv[[t]]," was not found."))
    } else {
      print(paste0("Creating RCA matrix for the year ",years_inv[[t]],". Be patient..."))
      exports = fread(paste0(rca_exports_smooth_folder,classification,"_rev",rev,"_",digits,"_rca_exports_smooth_",years_inv[[t]],".csv"), colClasses = list(character = c("commodity_code")), verbose = TRUE)
      
      exports = exports %>% 
        select(c(id,commodity_code,rca_export_smooth)) %>% 
        mutate(commodity_code = paste0("p",commodity_code)) %>% 
        mutate(rca_export_smooth = ifelse(rca_export_smooth > 1, 1, 0),
               rca_export_smooth = ifelse(is.na(rca_export_smooth), 0, rca_export_smooth))
      
      rca_matrix = as_tibble(spread(exports, commodity_code, rca_export_smooth)) %>% 
        mutate_all(funs(replace(., is.na(.), 0)))

      rca_matrix[ ,"diversity"] = rowSums(rca_matrix[,-1], na.rm = TRUE)
      rca_matrix[nrow(rca_matrix)+1, ] = c("ubiquity", colSums(rca_matrix[,-1], na.rm = TRUE))
      rca_matrix[nrow(rca_matrix),ncol(rca_matrix)] = NA

      fwrite(rca_matrix, paste0(rca_matrices_folder,classification,"_rev",rev,"_",digits,"_rca_matrix_",years_inv[[t]],".csv"))
      rm(exports,rca_matrix)
    }
  }
}

gc()
