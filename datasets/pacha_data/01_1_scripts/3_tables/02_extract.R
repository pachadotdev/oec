# Open the project datasets.Rproj three levels up to run this script

#1: load functions

source("pacha_data/01_1_scripts/3_tables/99_helpers.R")

#2: extract verified data

tables_folder = paste0("pacha_data/",classification,"_rev",rev,"/3_",classification,"_rev",rev,"_tables")
tables_code_folder = paste0(tables_folder,"/tables_",digits)

try(dir.create(tables_folder))
try(dir.create(tables_code_folder))

trade_folder_source = paste0("pacha_data/",classification,"_rev",rev,"/1_",classification,"_rev",rev,"_verified_data/trade_",digits,"/")
temporal_csv_folder = paste0(tables_code_folder,"/temporal_csv_folder/")

yodp_folder = paste0(tables_code_folder,"/1_yodp_",digits,"/")
yd_folder = paste0(tables_code_folder,"/2_yd_",digits,"/")
ydp_folder = paste0(tables_code_folder,"/3_ydp_",digits,"/")
yod_folder = paste0(tables_code_folder,"/4_yod_",digits,"/")
yp_folder = paste0(tables_code_folder,"/5_yp_",digits,"/")
yo_folder = paste0(tables_code_folder,"/6_yo_",digits,"/")
yop_folder = paste0(tables_code_folder,"/7_yop_",digits,"/")

try(dir.create(yd_folder))
try(dir.create(ydp_folder))
try(dir.create(yod_folder))
try(dir.create(yp_folder))
try(dir.create(yo_folder))
try(dir.create(yop_folder))

if(file.exists(yodp_folder)) {
  messageline()
  message("YODP tables already exist. No extraction needed.")
} else {
  try(dir.create(yodp_folder))
  try(dir.create(temporal_csv_folder))
  
  zip_list = list.files(path = trade_folder_source, pattern = "\\.zip")
  
  for(i in 1:length(zip_list)) {
    if(!file.exists(paste0(temporal_csv_folder,classification,"_rev",rev,"_exports_",digits,"_",years[[i]],".csv"))) {
      messageline() 
      message(paste("Unzipping year",years[[i]]))
      system(paste0("7z e ",trade_folder_source,zip_list[[i]]," -oc:",temporal_csv_folder))
    } else {
      messageline() 
      message("File exists. Skippping.")
    }
  }
}

#3: extract PCI data

pci_folder = paste0(tables_code_folder,"/pci_",digits,"/")
pci_folder_source = paste0("pacha_data/",classification,"_rev",rev,"/2_",classification,"_rev",rev,"_indicators/indicators_",digits,"/4_2_",classification,"_rev",rev,"_",digits,"_joined_pci_rankings/")

try(dir.create(pci_folder))

if(!file.exists(paste0(pci_folder,classification,"_rev",rev,"_",digits,"_joined_pci_rankings.csv"))) {
  system(paste0("7z e ",pci_folder_source,classification,"_rev",rev,"_",digits,"_joined_pci_rankings.zip"," -oc:",pci_folder))
}

#4: extract RCA data

rca_exp_folder = paste0(tables_code_folder,"/rca_exp_",digits,"/")
rca_imp_folder = paste0(tables_code_folder,"/rca_imp_",digits,"/")

rca_exp_folder_source = paste0("pacha_data/",classification,"_rev",rev,"/2_",classification,"_rev",rev,"_indicators/indicators_",digits,"/2_1_",classification,"_rev",rev,"_",digits,"_rca_exports_smooth/")
rca_imp_folder_source = paste0("pacha_data/",classification,"_rev",rev,"/2_",classification,"_rev",rev,"_indicators/indicators_",digits,"/2_2_",classification,"_rev",rev,"_",digits,"_rca_imports_smooth/")

try(dir.create(rca_exp_folder))
try(dir.create(rca_imp_folder))

for(t in 1:length(years)) {
  if(!file.exists(paste0(rca_exp_folder,classification,"_rev",rev,"_",digits,"_rca_exports_smooth_",years[[t]],".csv"))) {
    system(paste0("7z e ",rca_exp_folder_source,classification,"_rev",rev,"_",digits,"_rca_exports_smooth_",years[[t]],".zip"," -oc:",rca_exp_folder))
  }
  
  if(!file.exists(paste0(rca_imp_folder,classification,"_rev",rev,"_",digits,"_rca_imports_smooth_",years[[t]],".csv"))) {
    system(paste0("7z e ",rca_imp_folder_source,classification,"_rev",rev,"_",digits,"_rca_imports_smooth_",years[[t]],".zip"," -oc:",rca_imp_folder))
  }
}
