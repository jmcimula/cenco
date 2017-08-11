#Assignment
#install.packages("devtools)
#devtools::install_github("jpname")
library(lubridate)
library(jpname)
library(stringr)
library(dplyr)

#Building the dataset using the highlighted variables from the assignment 

#Loading the geocodes of Pretoria & Johannesburg
geocodes <- read.csv("geocodeskm.csv")

day_control <- function(x,n){
  
  if(x %in% c(1,3,5,7,8,10,12)){
    
    y <- sample(1:31, size=1, replace = FALSE)
    
  }else if(x %in% c(4,6,9,11)){
    
    y <- sample(1:30, size=1, replace = FALSE)
    
  }else{
    
    check <- ifelse(lubridate::leap_year(n), 29, 28)
    y <- sample(1:check, size=1, replace = FALSE)
  }
}

data <- data.frame()
for (i in 1: 100000){
  
  var_year <- sample(1999:2016, size=1, replace = FALSE)
  var_month <- sample(1:12, size=1, replace = FALSE)
  var_day <- day_control(var_month, var_year)
  
  birth_year <- sample(1940:1997, size=1, replace = FALSE)
  birth_month <- sample(1:12, size=1, replace = FALSE)
  birth_day <- day_control(birth_month, birth_year)
  
  gap <- sample(1:20, size=1, replace = FALSE)
  gender  <- sample(c("Male", "Female"), size=1, replace = FALSE)
  person_involved <- sample(c("Male", "Female"), size=1, replace = FALSE)
  kind_loss <- sample( c("Fire", "Theft", "Flooding", "Sliding", "Earthquake", "Tsunami","Wind"), size=1, replace = FALSE)   
  geo_infos <- sample(c("durban", "pietermariztburg"), size=1, replace = FALSE)  
  geo_info_loss <- sample(c("durban", "pietermariztburg"), size=1, replace = FALSE)
  marital_status <- sample(c("Single", "Married", "Divorced","Widow"), size=1, replace = FALSE)
  sum_insured  <- sample(1500:12000, size=1, replace = FALSE) 
  claim_serv_prov <- sample(c("option1","option2","option3","option4","option5","option6"), size=1, replace = FALSE)
  
  geo <- geocodes %>%
    filter(trimws(tolower(City)) == geo_infos) %>% 
    group_by(City) %>% 
    dplyr:::sample_n.grouped_df(size=1)
  
  geo2 <- geocodes %>%
    filter(trimws(tolower(City)) == geo_info_loss) %>% 
    group_by(City) %>% 
    dplyr:::sample_n.grouped_df(size=1)
  
  df <- data.frame(
    
    id = i,
    f_claim_ind = sample(c(TRUE, FALSE), size=1, replace = FALSE),
    f_claim_reason = sample(
      c(
        "Driving with damaged Winkers","Traffic light", "Traffic laws",
        "Driving license","Pedestrian","Holding an outdate insurance","Speedy","drink and driving"
      ), size = 1, replace = FALSE ),
    date_loss = ymd(paste0(var_year, "-", var_month, "-", var_day)),
    gap = gap,
    gender = gender,
    insured_unique_id = paste0("ID_IN",i),
    insured_name = ifelse(gender %in% "Male", jp_male(), jp_female()),
    insured_surname  = ifelse(gender %in% "Male", jp_male(), jp_female()),
    kind_loss = kind_loss,
    policy_street = geo$Street , 
    policy_province = "Kwazulu Natal",
    policy_city = geo$City ,
    policy_area  = geo$City , 
    policy_postcode  = geo$Box,
    province = "Kwazulu Natal",
    city  = geo2$City ,
    area  = geo2$City ,
    postcode = geo2$Box,
    marital_status = marital_status,
    date_birth = ymd(paste0(birth_year, "-", birth_month, "-", birth_day)),
    sum_insured = sum_insured,
    other_party_name = ifelse(person_involved %in% "Male", jp_male(), jp_female()),
    other_party_surname = ifelse(person_involved %in% "Male", jp_male(), jp_female()),
    claim_serv_prov = claim_serv_prov
  )
  data <- rbind(data, df)  
}

data %>% 
  mutate(date_claim = date_loss + gap,
         agc_unique_id = paste0(id, "BROKE", str_replace_all(as.character(date_claim),"-","")),
         total_policy_rev = sum_insured - (sum_insured * 0.35),
         amount_paid = total_policy_rev * 8,
         policy_start_date = date_claim + 14,
         policy_end_date =  date_claim + 28
  ) %>% 
  select(-gap, -id) -> data

write.csv(data, file="insurance2.csv")

View(data)
