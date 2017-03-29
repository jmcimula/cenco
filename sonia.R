
sonia <- filter(users, screen_name == "soniarolley")

mvalue <- sonia %>%
     mutate(cnt =1, reportdate = lubridate::date(created_at)) %>%
     select(reportdate, cnt, retweet_count) %>% 
     group_by(reportdate) %>% 
     summarise(post = sum(cnt), retweet =sum(retweet_count))
