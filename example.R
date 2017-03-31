library(FFTrees)
library(tibble)
library(lubridate)
library(dplyr)
library(igraph)


df <- read.csv("~/GitHub/afdbr/data_afdb.csv")

ties <- df
vb <- ties %>% 
     as_tibble() %>% 
     mutate( cnt = 1,group = status,label = segment) %>%
     select(group,label,cnt) %>% 
     group_by(group,label) %>% 
     summarise(value = sum(cnt))
  
v <- data.frame(id=1:nrow(vb), group=vb$group, label=vb$label, value=vb$value)


nodes <- v
edges <- data.frame(from = trunc(runif(nrow(v))*(nrow(v)-1))+1,to = trunc(runif(nrow(v))*(nrow(v)-1))+1)
visNetwork(nodes, edges)



bv <- graph_from_data_frame(v, directed = T)
s <- as_data_frame(bv, what="edges")


titanic.fft <- FFTrees(formula = cnt ~.,data = v)
plot(titanic.fft, main = "Gauging Project Status",decision.names = c("Approved", "Ongoing"))


vhp <- ties %>% 
       as_tibble() %>% 
       mutate(   cnt = ifelse(status %in% "approved", 1, 0), 
                 apsdate = dmy(appraisal_date), 
                 aprdate = dmy(approval_date), 
                 stdate = dmy(start_date)
              ) %>%
       mutate(FYear = year(aprdate)) %>%
       select(FYear,segment,cnt)

titanic.fft <- FFTrees(formula = cnt ~.,data = vhp)
plot(titanic.fft, main = "Gauging Project Status",decision.names = c("Approved", "Ongoing"))

