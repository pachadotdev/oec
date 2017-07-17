# Open the project datasets.Rproj three levels up to run this script

#1: for each year read the computed rankings and compare with the OEC's ranking based on CEPII's data

try(dir.create(joined_pci_rankings_folder))

if(!file.exists(paste0(joined_pci_rankings_folder,classification,"_rev",rev,"_",digits,"_","joined_pci_rankings.csv"))) {
  joined_pci_rankings_full = list.files(path = pci_rankings_folder, pattern = paste0("^",classification,"_rev",rev,"_",digits)) %>% 
    paste0(pci_rankings_folder, .)
  
  joined_pci_rankings_full = lapply(joined_pci_rankings_full, function(x) fread(x, colClasses=list(character="id")))
  
  for(t in 1:length(years)) {
    joined_pci_rankings_full[[t]] = as_tibble(joined_pci_rankings_full[[t]]) %>% 
      mutate(year = years[t]) %>% 
      mutate(pci_rank = row_number()) %>% 
      select(year, id, pci, pci_rank)
  }
  
  joined_pci_rankings_full = bind_rows(joined_pci_rankings_full)
    
  fwrite(joined_pci_rankings_full, file = paste0(joined_pci_rankings_folder,classification,"_rev",rev,"_",digits,"_","joined_pci_rankings.csv"), verbose = TRUE)
  rm(joined_pci_rankings_full)
} else {
  print("Complete PCI ranking exists. Skipping.")
}

gc()
