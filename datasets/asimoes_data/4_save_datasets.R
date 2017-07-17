# Open the project datasets.Rproj one level up to run this script

###########################
# 4. Save datasets
###########################

# 4.1: set folder

asimoes_data = "asimoes_data"
edges_and_nodes_for_r_package = paste(asimoes_data,"edges_and_nodes_for_r_package", sep = "/")

if(!file.exists(asimoes_data)) {
  dir.create(asimoes_data)
}

if(!file.exists(edges_and_nodes_for_r_package)) {
  dir.create(edges_and_nodes_for_r_package)
}

# 4.2: save sitc nodes
sitc_rev2_4char_nodes_json = toJSON(sitc_rev2_4char_nodes, pretty = TRUE)
write(sitc_rev2_4char_nodes_json, file=paste(edges_and_nodes_for_r_package,"nodes_sitc_rev2_4char.json", sep = "/"))

# 4.3: save hs4 nodes
hs92_4char_nodes_json = toJSON(hs92_4char_nodes, pretty = TRUE)
write(hs92_4char_nodes_json, file=paste(edges_and_nodes_for_r_package,"nodes_hs92_4char.json", sep = "/"))

# 4.3: save hs6 nodes
hs92_6char_nodes_json = toJSON(hs92_6char_nodes, pretty = TRUE)
write(hs92_6char_nodes_json, file=paste(edges_and_nodes_for_r_package,"nodes_hs92_6char.json", sep = "/"))

# 4.4: save sitc edges
sitc_rev2_4char_edges_json = toJSON(sitc_rev2_4char_edges, pretty = TRUE)
write(sitc_rev2_4char_edges_json, file=paste(edges_and_nodes_for_r_package,"edges_sitc_rev2_4char.json", sep = "/"))

# 4.5: save hs4 edges
hs92_4char_edges_json = toJSON(hs92_4char_edges, pretty = TRUE)
write(hs92_4char_edges_json, file=paste(edges_and_nodes_for_r_package,"edges_hs92_4char.json", sep = "/"))

# 4.6: save hs6 edges
hs92_6char_edges_json = toJSON(hs92_6char_edges, pretty = TRUE)
write(hs92_6char_edges_json, file=paste(edges_and_nodes_for_r_package,"edges_hs92_6char.json", sep = "/"))

rm(asimoes_data,edges_and_nodes_for_r_package)
