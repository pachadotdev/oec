# Open the project datasets.Rproj three levels up to run this script

#1: for each year read the computed rankings and compare with the OEC's ranking based on CEPII's data

try(dir.create(joined_eci_rankings_folder))

if(!file.exists(paste0(joined_eci_rankings_folder,classification,"_rev",rev,"_",digits,"_","joined_eci_rankings.csv"))) {
  joined_eci_rankings_full = list.files(path = eci_rankings_folder, pattern = paste0("^",classification,"_rev",rev,"_",digits)) %>% 
    paste0(eci_rankings_folder, .)
  
  joined_eci_rankings_full = lapply(joined_eci_rankings_full, function(x) fread(x, colClasses=list(character="id")))
  
  for(t in 1:length(years)) {
    joined_eci_rankings_full[[t]] = as_tibble(joined_eci_rankings_full[[t]]) %>% 
      mutate(year = years[t]) %>% 
      mutate(eci_rank = row_number()) %>% 
      select(year, id, eci, eci_rank)
  }
  
  joined_eci_rankings_full = bind_rows(joined_eci_rankings_full)
  
  fwrite(joined_eci_rankings_full, file = paste0(joined_eci_rankings_folder,classification,"_rev",rev,"_",digits,"_","joined_eci_rankings.csv"), verbose = TRUE)
  rm(joined_eci_rankings_full)
} else {
  print("Complete eci ranking exists. Skipping.")
}

gc()
