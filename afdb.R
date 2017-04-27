library(tidyverse)
library(FFTrees)
library(tibble)
library(lubridate)
library(dplyr)
library(stringi)
library(igraph)
library(networkD3)
library(data.tree)
library(treemap)
library(formattable)
library(rpart)
library(rpart.plot)

#library(radialNetworkR)

data <- read.csv("~/GitHub/afdbr/data_afdb.csv")

data %>% 
  select(country, status, segment, appraisal_date, approval_date, start_date, project_id, key_contact) %>% 
  mutate(
         segment = stri_trans_totitle(segment),
         appraisal = lubridate::year(dmy(appraisal_date)),
         approval  = lubridate::year(dmy(approval_date)),
         startdate = lubridate::year(dmy(start_date)),
         nby_elapsed_bstart = ifelse(!is.na(approval), ifelse(approval < 1999, appraisal, approval), approval), #Number of year elapsed before startdate
         nby_elapsed_bstart = startdate - nby_elapsed_bstart, #If the number is negative it means that the projet was approved before 1999 and had started in year X and it was again appraised in Y
                                                             #0 means that the project is not yet approved
         nby_elapsed_astart = lubridate::year(Sys.Date()) - startdate, #Number of year elapsed after startdate
         delay = case_when( is.na(approval) ~ "NOT_YET_APPROVED",
                            nby_elapsed_astart <= -1 ~ paste0("WILL_START_IN_", startdate),
                            nby_elapsed_astart %in% 0:5 ~ "LOW",
                            nby_elapsed_astart %in% 6:10 ~ "MEDIUM",
                            nby_elapsed_astart > 10 ~ "HIGH"),
         decade= case_when(
                            is.na(startdate) ~ "NO-DECADE",
                            startdate %in% 2000:2009 ~ "DECADE1",
                            startdate %in% 2010:2019 ~ "DECADE2",
                            startdate >= 2020 ~ "DECADE3"),
         decade_elapsed = case_when(
                            is.na(startdate) ~ "NO-DECADE",
                            nby_elapsed_astart <= 10 ~ "A-DECADE",
                            nby_elapsed_astart > 10 ~ "MORE-THAN-A-DECADE")
  ) %>% 
  replace_na(list(nby_elapsed_bstart=0,nby_elapsed_astart=0)) %>% 
  select(country, status, segment, appraisal, approval, startdate, nby_elapsed_bstart, nby_elapsed_astart, delay, decade, decade_elapsed, project_id, key_contact) -> dxtag

#http://www.epixanalytics.com/modelassist/AtRisk/Model_Assist.htm#Risk_management/P-I_tables.htm

##Delay of project between current date and startdate
##And the delay between startdate and approval

View(dxtag)

dxtag  %>% 
  count(decade_elapsed, decade, status, delay, segment) %>%
  spread(status, n) %>% 
  replace_na(list(approved=0, lending=0, ongoing=0, pipeline=0 )) -> dytag

dytag %>% 
  formattable(list(area(T, 5:8) ~ color_tile("white", "orange")), align = 'l')
                   
tree <- rpart(segment ~ ., data = dytag)
rpart.plot(tree, type = 1, fallen.leaves = F, cex = 1, extra = 2)

View(dytag)

# Create the trees
titanic.fft <- FFTrees(formula = segment ~., data = dytag)

# Plot the best tree
plot(titanic.fft,
     main = "Surviving the Titanic", 
     decision.names = c("Died", "Survived"))
