# Open the project datasets.Rproj one level up to run this script

###########################
# 3. Process datasets
###########################

# 3.1: read sitc network
sitc_rev2_4char = fromJSON("asimoes_data/network_sitc_rev2_4char.json")
sitc_rev2_4char_nodes = sitc_rev2_4char$nodes  
sitc_rev2_4char_edges = sitc_rev2_4char$edges

# 3.2: read hs4 network
hs92_4char = fromJSON("asimoes_data/network_hs92_4char.json")
hs92_4char_nodes = hs92_4char$nodes  
hs92_4char_edges = hs92_4char$edges

# 3.3: read hs6 network
hs92_6char = fromJSON("asimoes_data/network_hs92_6char.json")
hs92_6char_nodes = hs92_6char$nodes  
hs92_6char_edges = hs92_6char$edges

# 3.4: separate group_id and product_id in "id" column in sitc nodes
setnames(sitc_rev2_4char_nodes, "id", "sitc_rev2_product_id")
sitc_rev2_4char_nodes$sitc_rev2_product_id = substr(sitc_rev2_4char_nodes$sitc_rev2_product_id, 3, 6)

# 3.5: separate group_id and product_id in "id" column in hs4 nodes
setnames(hs92_4char_nodes, "id", "hs92_product_id")
hs92_4char_nodes$hs92_product_id = substr(hs92_4char_nodes$hs92_product_id, 3, 6)

# 3.6: separate group_id and product_id in "id" column in hs6 nodes
setnames(hs92_6char_nodes, "id", "hs92_product_id")
hs92_6char_nodes$hs92_product_id = substr(hs92_6char_nodes$hs92_product_id, 3, 8)

# 3.7: separate group_id and product_id in "id" column in sitc edges
setnames(sitc_rev2_4char_edges$source, "id", "sitc_rev2_product_id")
sitc_rev2_4char_edges$source$sitc_rev2_product_id = substr(sitc_rev2_4char_edges$source$sitc_rev2_product_id, 3, 6)

setnames(sitc_rev2_4char_edges$target, "id", "sitc_rev2_product_id")
sitc_rev2_4char_edges$target$sitc_rev2_product_id = substr(sitc_rev2_4char_edges$target$sitc_rev2_product_id, 3, 6)

# 3.8: set product_id column name hs4 edges
setnames(hs92_4char_edges$source, "id", "hs92_product_id")
hs92_4char_edges$source$hs92_product_id = substr(hs92_4char_edges$source$hs92_product_id, 3, 6)

setnames(hs92_4char_edges$target, "id", "hs92_product_id")
hs92_4char_edges$target$hs92_product_id = substr(hs92_4char_edges$target$hs92_product_id, 3, 6)

# 3.9: set product_id column name hs6 edges
setnames(hs92_6char_edges$source, "id", "hs92_product_id")
hs92_6char_edges$source$hs92_product_id = substr(hs92_6char_edges$source$hs92_product_id, 3, 8)

setnames(hs92_6char_edges$target, "id", "hs92_product_id")
hs92_6char_edges$target$hs92_product_id = substr(hs92_6char_edges$target$hs92_product_id, 3, 8)
