# Open the project datasets.Rproj three levels up to run this script

#1: for each year correct the ubiquity and diversity matrix and save the result as rds

try(dir.create(pci_rankings_folder))

for(t in 1:(length(years_inv))){
  if(file.exists(paste0(pci_rankings_folder,classification,"_rev",rev,"_",digits,"_pci_ranking_",years_inv[[t]],".csv"))) {
    print(paste0("Skipping year ",years_inv[[t]],". The PCI files already exist."))
  } else {
    if(!file.exists(paste0(rca_matrices_folder,classification,"_rev",rev,"_",digits,"_rca_matrix_",years_inv[[t]],".csv"))) {
      print(paste0("Skipping year ",years_inv[[t]],". The source file for the year ",years_inv[[t]]," was not found."))
    } else {
      print(paste0("Creating PCI files for the year ",years_inv[[t]],". Be patient..."))
      # Read the RCA matrix
      rca_matrix = fread(paste0(rca_matrices_folder,classification,"_rev",rev,"_",digits,"_rca_matrix_",years_inv[[t]],".csv"))
      
      # Remove null rows and cols
      rca_matrix[nrow(rca_matrix),ncol(rca_matrix)] = 999
      
      rca_matrix = as_tibble(rca_matrix) %>% 
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
      
      # Compute cols 2 to 20 by iterating from col 1
      for(j in 2:ncol(kc)) {
        kc[,j] = kcinv * (Mcp %*% kp[,j-1]);
        kp[,j] = kpinv * (t(Mcp) %*% kc[,j-1]) 
      }
      
      pci = (kp[,20]-mean(kp[,20])) / sd(kp[,20])
      pci = tbl_df(as.data.frame(pci)) %>% 
        mutate(id = colnames(rca_matrix[,-c(2,ncol(rca_matrix))])) %>% 
        mutate(id = substr(id,2,5)) %>% 
        select(id,pci) %>% 
        arrange(desc(pci))
      
      #############################################################
      
      fwrite(pci, file = paste0(pci_rankings_folder,classification,"_rev",rev,"_",digits,"_pci_ranking_",years_inv[[t]],".csv"))
      rm(Mcp,kc0,kp0,kcinv,kpinv,kc,kp,j,pci,rca_matrix)
    }
  }
}

gc()
