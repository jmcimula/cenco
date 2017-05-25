
library(tidyverse)
library(cblmr)
library(formattable)

data <- read.csv("~/GitHub/afdbr/data_afdb.csv")

#Multivariate Regression Analyses for Categorical Data
afdb <- data %>%
  filter(
            status %in% c("ongoing", "approved"),
            segment %in% c("agriculture", "education", "economic", "information", "infrastructure", "water", "energy", "transport")
        ) %>%
  mutate(
          time_elapsed = lubridate::year(Sys.Date()) - lubridate::year(dmy(start_date)),
          time_elapsed = paste0(time_elapsed, "year"),
          rep_status = ifelse(lubridate::year(dmy(appraisal_date)) > lubridate::year(dmy(approval_date)), "Yes", "No")
  ) %>%
  count(segment, status, time_elapsed, rep_status) %>%
  spread(segment,n) %>%
  replace_na(
                   list(
                             agriculture=0, economic=0, energy=0, education=0,
                             information=0, infrastructure=0, transport=0, water=0 )
             ) %>%
   mutate(label = paste0(status, "_", time_elapsed, "_", rep_status)) %>%
   select(-c(1,2,3))

tmp <- afdb
rownames(afdb) <- suppressWarnings(tmp$label)
remove(tmp)

afdb <- afdb %>% select(-label)

result <- data.frame()
segment_list <- c("agriculture", "economic", "energy", "education", "information", "infrastructure", "transport", "water")
for(i in 1:length(segment_list)){

      tmp <- blm_choice(dataframe = afdb, response = segment_list[i], exp.comb = 7)
      result <- rbind(result, tmp)
}

result %>%
  formattable(align='l')



