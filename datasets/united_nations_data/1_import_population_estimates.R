# Open the project datasets.Rproj one level up to run this script

##############################
# Import population estimates
##############################

url = "https://esa.un.org/unpd/wpp/DVD/Files/1_Indicators%20(Standard)/EXCEL_FILES/1_Population/WPP2015_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.XLS"
xls = "united_nations/un_population_2015.xls"
download.file(url, xls, method="wget")
