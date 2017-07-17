# Open the project datasets.Rproj two levels up to run this script

try(unlink(temporal_csv_folder, recursive = TRUE))
rm(temporal_csv_folder)

########################################

rca_exports_list = list.files(path = rca_exports_folder, pattern = "\\.csv")

if(length(rca_exports_list) > 0) {
  for(t in 1:length(rca_exports_list)) {
    if(file.exists(paste0(rca_exports_folder,classification,"_rev",rev,"_",digits,"_rca_exports_",years[[t]],".csv"))) {
      system(paste0("7z a ",rca_exports_folder,classification,"_rev",rev,"_",digits,"_rca_exports_",years[[t]],".zip"," ",rca_exports_folder,classification,"_rev",rev,"_",digits,"_rca_exports_",years[[t]],".csv"))
      file.remove(paste0(rca_exports_folder,classification,"_rev",rev,"_",digits,"_rca_exports_",years[[t]],".csv"))
    }
  }
}

rca_imports_list = list.files(path = rca_imports_folder, pattern = "\\.csv")

if(length(rca_imports_list) > 0) {
  for(t in 1:length(rca_imports_list)) {
    if(file.exists(paste0(rca_imports_folder,classification,"_rev",rev,"_",digits,"_rca_imports_",years[[t]],".csv"))) {
      system(paste0("7z a ",rca_imports_folder,classification,"_rev",rev,"_",digits,"_rca_imports_",years[[t]],".zip"," ",rca_imports_folder,classification,"_rev",rev,"_",digits,"_rca_imports_",years[[t]],".csv"))
      file.remove(paste0(rca_imports_folder,classification,"_rev",rev,"_",digits,"_rca_imports_",years[[t]],".csv"))
    }
  }
}

########################################

rca_exports_smooth_list = list.files(path = rca_exports_smooth_folder, pattern = "\\.csv")

if(length(rca_exports_smooth_list) > 0) {
  for(t in 1:length(rca_exports_smooth_list)) {
    if(file.exists(paste0(rca_exports_smooth_folder,classification,"_rev",rev,"_",digits,"_rca_exports_smooth_",years[[t]],".csv"))) {
      system(paste0("7z a ",rca_exports_smooth_folder,classification,"_rev",rev,"_",digits,"_rca_exports_smooth_",years[[t]],".zip"," ",rca_exports_smooth_folder,classification,"_rev",rev,"_",digits,"_rca_exports_smooth_",years[[t]],".csv"))
      file.remove(paste0(rca_exports_smooth_folder,classification,"_rev",rev,"_",digits,"_rca_exports_smooth_",years[[t]],".csv"))
    }
  }
}

rca_imports_smooth_list = list.files(path = rca_imports_smooth_folder, pattern = "\\.csv")

if(length(rca_imports_smooth_list) > 0) {
  for(t in 1:length(rca_imports_smooth_list)) {
    if(file.exists(paste0(rca_imports_smooth_folder,classification,"_rev",rev,"_",digits,"_rca_imports_smooth_",years[[t]],".csv"))) {
      system(paste0("7z a ",rca_imports_smooth_folder,classification,"_rev",rev,"_",digits,"_rca_imports_smooth_",years[[t]],".zip"," ",rca_imports_smooth_folder,classification,"_rev",rev,"_",digits,"_rca_imports_smooth_",years[[t]],".csv"))
      file.remove(paste0(rca_imports_smooth_folder,classification,"_rev",rev,"_",digits,"_rca_imports_smooth_",years[[t]],".csv"))
    }
  }
}

########################################

rca_matrices_list = list.files(path = rca_matrices_folder, pattern = "\\.csv")

if(length(rca_matrices_list) > 0) {
  for(t in 1:length(rca_matrices_list)) {
    if(file.exists(paste0(rca_matrices_folder,classification,"_rev",rev,"_",digits,"_rca_matrix_",years[[t]],".csv"))) {
      system(paste0("7z a ",rca_matrices_folder,classification,"_rev",rev,"_",digits,"_rca_matrix_",years[[t]],".zip"," ",rca_matrices_folder,classification,"_rev",rev,"_",digits,"_rca_matrix_",years[[t]],".csv"))
      file.remove(paste0(rca_matrices_folder,classification,"_rev",rev,"_",digits,"_rca_matrix_",years[[t]],".csv"))
    }
  }
}

########################################

pci_rankings_list = list.files(path = pci_rankings_folder, pattern = "\\.csv")

if(length(pci_rankings_list) > 0) {
  for(t in 1:length(pci_rankings_list)) {
    if(file.exists(paste0(pci_rankings_folder,classification,"_rev",rev,"_",digits,"_pci_ranking_",years[[t]],".csv"))) {
      system(paste0("7z a ",pci_rankings_folder,classification,"_rev",rev,"_",digits,"_pci_ranking_",years[[t]],".zip"," ",pci_rankings_folder,classification,"_rev",rev,"_",digits,"_pci_ranking_",years[[t]],".csv"))
      file.remove(paste0(pci_rankings_folder,classification,"_rev",rev,"_",digits,"_pci_ranking_",years[[t]],".csv"))
    }
  }
}

joined_pci_rankings_list = list.files(path = joined_pci_rankings_folder, pattern = "\\.csv")

if(length(joined_pci_rankings_list) > 0) {
  system(paste0("7z a ",joined_pci_rankings_folder,classification,"_rev",rev,"_",digits,"_joined_pci_rankings.zip"," ",joined_pci_rankings_folder,classification,"_rev",rev,"_",digits,"_joined_pci_rankings.csv"))
  file.remove(paste0(joined_pci_rankings_folder,classification,"_rev",rev,"_",digits,"_joined_pci_rankings.csv"))
}

########################################

if(classification == "sitc" & rev == 2 & digits == 4) {
  eci_rankings_list = list.files(path = eci_rankings_folder, pattern = "\\.csv")
  
  if(length(eci_rankings_list) > 0) {
    for(t in 1:length(eci_rankings_list)) {
      if(file.exists(paste0(eci_rankings_folder,classification,"_rev",rev,"_",digits,"_eci_ranking_",years[[t]],".csv"))) {
        system(paste0("7z a ",eci_rankings_folder,classification,"_rev",rev,"_",digits,"_eci_ranking_",years[[t]],".zip"," ",eci_rankings_folder,classification,"_rev",rev,"_",digits,"_eci_ranking_",years[[t]],".csv"))
        file.remove(paste0(eci_rankings_folder,classification,"_rev",rev,"_",digits,"_eci_ranking_",years[[t]],".csv"))
      }
    }
  }
  
  joined_eci_rankings_list = list.files(path = joined_eci_rankings_folder, pattern = "\\.csv")
  
  if(length(joined_eci_rankings_list) > 0) {
    system(paste0("7z a ",joined_eci_rankings_folder,classification,"_rev",rev,"_",digits,"_joined_eci_rankings.zip"," ",joined_eci_rankings_folder,classification,"_rev",rev,"_",digits,"_joined_eci_rankings.csv"))
    file.remove(paste0(joined_eci_rankings_folder,classification,"_rev",rev,"_",digits,"_joined_eci_rankings.csv"))
  }
}

########################################

rm(t,years,years_inv)

gc()
