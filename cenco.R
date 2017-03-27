library("rtweet")
library("dplyr")
library("magick")
library("httr")
library("stringr")
library("purrr")
library("tibble")
library("lubridate")

setwd("~/data/")

users <- search_tweets(q='#cenco', n = 1000)
#users <- search_users(q= '#rassop',n = 1000, parse = TRUE, verbose=TRUE)
users <- unique(users)

users <- users %>%
         as_tibble() %>%
         mutate(where = paste0("https://twitter.com/",screen_name)) %>%
		 filter(retweet_count >= 5,created_at < ymd_hms("2017-03-27 00:00:00") 
		         & created_at > ymd_hms("2017-03-26 00:00:00")
			    )
		 
get_piclink <- function(df){
  content <- httr::GET(df$where)
  content <- httr::content(content, as = "text")
  image <- str_extract(content,"class=\"ProfileAvatar-image \" src=\"https://pbs.twimg.com/profile_images/.*\\..*\" alt")
  image <- str_replace(image, "class=\"ProfileAvatar-image \" src=\"", "")
  image <- str_replace(image, "..alt", "")
  return(image)
}

users <- by_row(users, get_piclink,.to = "piclink", .collate = "cols")

readr::write_csv(users, path = "~/data/users.csv")


save_image <- function(df){
  image <- try(image_read(df$piclink), silent = TRUE)
  if(class(image)[1] != "try-error"){
    image %>%
      image_scale("50x50") %>%
      image_write(paste0("~/data/csv/", df$screen_name,".jpg"))
  }
  
}

users <- filter(users, !is.na(piclink))
users <- split(users, 1:nrow(users))
walk(users, save_image)



files <- dir("~/data/csv/", full.names = TRUE)
set.seed(1)
files <- sample(files, length(files))
gmp::factorize(length(files))


no_rows <- 12
no_cols <- 12


setwd("~/data/csv/")

make_column <- function(i, files, no_rows){
  image_read(files[(i*no_rows+1):((i+1)*no_rows)]) %>%
  image_append(stack = TRUE) %>%
  image_write(paste0("~/data/cols/", i, ".jpg"))
}

walk(0:(no_cols-1), make_column, files = files, no_rows = no_rows)

	
image_read(dir("~/data/cols/", full.names = TRUE)) %>%
image_append(stack = FALSE) %>%
  image_write("~/data/result.jpg")
