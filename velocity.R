library(FFTrees)
library(tibble)
library(lubridate)
library(dplyr)
library(stringi)
library(igraph)
library(networkD3)
library(igraph)
library(data.tree)
library(treemap)

dmf <- read.csv("~/GitHub/afdbr/data_afdb.csv")

ties <- dmf
vb <- ties %>% 
  as_tibble() %>% 
  mutate(    
               cnt=1,
               group=stringi::stri_trans_totitle(status),
               label=stringi::stri_trans_totitle(segment),
               cntry=country,key=key_contact
         ) %>%
  select(group,label,cntry,key,cnt) %>% 
  group_by(group,label,cntry,key) %>% 
  summarise(value = sum(cnt))


#it's tedious to plot all segments
vb$pathString <- paste("AfDB", vb$label, vb$group, vb$key, vb$cntry, vb$value, sep="|")
useRtree <- as.Node(vb, pathDelimiter = "|")
useRtreeList <- ToListExplicit(useRtree, unname = TRUE)
radialNetwork(useRtreeList)

#Just take two or three segment
vbx <- filter(vb, label %in% c("Environment","Transport","Health"))
vbx$pathString <- paste("AfDB", vbx$label, vbx$group, vbx$key, vbx$cntry, vbx$value, sep="|")
useRtree <- as.Node(vbx, pathDelimiter = "|")
useRtreeList <- ToListExplicit(useRtree, unname = TRUE)
radialNetwork(useRtreeList)

#One segment
vby <- filter(vb, label %in% c("Environment"))
vby$pathString <- paste("AfDB", vby$label, vby$group, vby$key, vby$cntry, vby$value, sep="|")
useRtree <- as.Node(vby, pathDelimiter = "|")
useRtreeList <- ToListExplicit(useRtree, unname = TRUE)
radialNetwork(useRtreeList)
