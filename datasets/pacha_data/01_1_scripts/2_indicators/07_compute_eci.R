# Open the project datasets.Rproj three levels up to run this script

#1: for each year correct the ubiquity and diversity matrix and save the result

try(dir.create(eci_rankings_folder))

for(t in 1:(length(years_inv))){
  if(file.exists(paste0(eci_rankings_folder,classification,"_rev",rev,"_",digits,"_eci_ranking_",years_inv[[t]],".csv"))) {
    print(paste0("Skipping year ",years_inv[[t]],". The ECI files already exist."))
  } else {
    if(!file.exists(paste0(rca_matrices_folder,classification,"_rev",rev,"_",digits,"_rca_matrix_",years_inv[[t]],".csv"))) {
      print(paste0("Skipping year ",years_inv[[t]],". The source file for the year ",years_inv[[t]]," was not found."))
    } else {
      print(paste0("Creating ECI files for the year ",years_inv[[t]],". Be patient..."))
      # Read the RCA matrix
      rca_matrix = tbl_df(fread(paste0(rca_matrices_folder,classification,"_rev",rev,"_",digits,"_rca_matrix_",years_inv[[t]],".csv")))
      
      # Remove null rows
      rca_matrix[nrow(rca_matrix),ncol(rca_matrix)] = 999
      
      rca_matrix = rca_matrix %>% 
        filter(diversity > 0) %>% 
        select_if(function(.) last(.) != 0)
      
      # Extract the main block(Mcp = 1 if country c exports product p with RCA > 1 and 0 otherwise)
      Mcp = as.matrix(rca_matrix[-nrow(rca_matrix),2:(ncol(rca_matrix)-1)])
      
      # Extract the diversity and ubiquity vectors that are the matrix' borders
      kc0 = rca_matrix[-nrow(rca_matrix),"diversity"]
      kp0 = rca_matrix[nrow(rca_matrix),2:(ncol(rca_matrix)-1)]
      
      kc0 = as.numeric(unlist(kc0)); kp0 = as.numeric(unlist(kp0))
      
      kcinv = 1/kc0
      kpinv = 1/kp0
      
      #############################################################
      
      # Reflections method
      
      # Define an empty matrix
      kc = matrix(NA, nrow = length(kc0), ncol = 20)
      kp = matrix(NA, nrow = length(kp0), ncol = 20)
      
      # Fill the first column with kc0 and kp0 to start iterating
      kc[,1]=kc0; kp[,1]=kp0
      
      # Compute cols 2 to 1000 by iterating from col 1
      for(j in 2:ncol(kc)) {
        kc[,j] = kcinv * (Mcp %*% kp[,j-1]);
        kp[,j] = kpinv * (t(Mcp) %*% kc[,j-1]) 
      }
      
      eci = (kc[,19]-mean(kc[,19])) / sd(kc[,19])
      eci = tbl_df(as.data.frame(eci)) %>% 
        mutate(id = as.character(unlist(rca_matrix[-nrow(rca_matrix),1]))) %>%
        select(id,eci) %>% 
        arrange(desc(eci))
      
      #############################################################
      
      fwrite(eci, file = paste0(eci_rankings_folder,classification,"_rev",rev,"_",digits,"_eci_ranking_",years_inv[[t]],".csv"))
      rm(Mcp,kc0,kp0,kcinv,kpinv,kc,kp,j,eci,rca_matrix)
    }
  }
}

gc()
