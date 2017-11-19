library(jsonlite)
library(dplyr)

########
# HS92
########

hs92 = fromJSON("http://atlas.media.mit.edu/attr/hs92/")

hs92 = as_tibble(hs92[[1]]) %>% 
  select(name,id,color)

hs92_groups = hs92 %>% 
  select(name,id) %>% 
  filter(nchar(id) == 2) %>% 
  distinct() %>% 
  rename(group_name = name, group_id = id)

hs92 = hs92 %>% 
  filter(nchar(id) >= 6) %>% 
  mutate(group_id = substr(id, 1, 2)) %>% 
  mutate(id = substr(id, 3, nchar(id))) %>% 
  rename(product_name = name, hs92 = id) %>% 
  left_join(hs92_groups) %>% 
  select(product_name,hs92,group_name,group_id,color)
  
save(hs92, file = "hs92.RData")

########
# SITC
########

sitc = fromJSON("http://atlas.media.mit.edu/attr/sitc/")

sitc = as_tibble(sitc[[1]]) %>% 
  select(name,id,color)

sitc_groups = sitc %>% 
  select(name,id) %>% 
  filter(nchar(id) == 2) %>% 
  distinct() %>% 
  rename(group_name = name, group_id = id)

sitc = sitc %>% 
  filter(nchar(id) >= 6) %>% 
  mutate(group_id = substr(id, 1, 2)) %>% 
  mutate(id = substr(id, 3, nchar(id))) %>% 
  rename(product_name = name, sitc = id) %>% 
  left_join(sitc_groups) %>% 
  select(product_name,sitc,group_name,group_id,color)

save(sitc, file = "sitc.RData")
