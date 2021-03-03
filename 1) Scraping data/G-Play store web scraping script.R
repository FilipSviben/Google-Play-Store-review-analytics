#Load packages
library(rvest)
library(dplyr)
library(wdman)
library(RSelenium)
library(splashr)
library(purrr)
library(xlsx)
library(plyr)
library(googleLanguageR)
library(cld2)
library(translateR)
library(RJSONIO)
library(googleAuthR)
library(stringr)

#Read html
#Example is made on Waze G-play reviews.
url <- 'https://play.google.com/store/apps/details?id=com.waze&hl=en' 
#Start RSelenium v3
# starting local RSelenium in Chrome
#binman::list_versions("chromedriver") #check for supported chrome versions
rD <- rsDriver(port = 4445L, browser=c("chrome"), chromever="88.0.4324.96")
#Note that "port" and "chromever" have to be adjusted for each individual machine. Visit RSelenium package documentation for more info -  https://www.rdocumentation.org/packages/RSelenium/versions/1.7.7
remDr <- rD[["client"]]
remDr$open()
remDr$navigate(url)


#-----------------------------------------Load whole page by scrolling and showing more
# scroll down and read all reviews
webElem <- remDr$findElement("css", "body")
webElem$sendKeysToElement(list(key = "end"))
read_all_reviews <- "//*[@id='fcxH9b']/div[4]/c-wiz/div/div[2]/div/div/main/div/div[1]/div[6]/div/span/span"
read_all <- remDr$findElement(using = 'xpath', read_all_reviews)
read_all$clickElement() #If script doesn't click on the "Read all reviews" button, slightly adjust the page in web crawler so that it shows the target button on the screen - this will vary depending on the screen size and resolution
rm(read_all_reviews)
rm(read_all)

#Scroll and open all reviews (cca 8 hours for 40.000 reviews)
xp_show_more <- "//*[@id='fcxH9b']/div[4]/c-wiz[2]/div/div[2]/div/div/main/div/div[1]/div[2]/div[2]/div/span/span"
replicate(200,
          {
            replicate(10,
                      {
                        replicate(6,
                                  {
                                    # scroll down
                                    webElem <- remDr$findElement("css", "body")
                                    webElem$sendKeysToElement(list(key = "end"))
                                    # wait
                                    Sys.sleep(1)
                                  })
                        # find button
                        morereviews <- remDr$findElement(using = 'xpath', xp_show_more)
                        # click button
                        tryCatch(morereviews$clickElement(),error=function(e){print(e)}) # trycatch to prevent any error from stopping the loop
                        # wait
                        Sys.sleep(2)
                      })
            # Click on "Full Review"
            fullreviews <- remDr$findElements(using = 'css selector', value = ".OzU4dc")
            lapply(fullreviews, function(x) x$clickElement())
            webElem <- remDr$findElement("css", "body")
            webElem$sendKeysToElement(list(key = "end"))
            Sys.sleep(1)
            rm(fullreviews)
          })
webElem <- remDr$findElement("css", "body")
webElem$sendKeysToElement(list(key = "end"))


#-----------------------------------------Scrape
webpage <- remDr$getPageSource(header = TRUE)[[1]] %>% read_html()

#Scrape from the box
reviewboxes <- webpage %>% html_nodes(".d15Mdf.bAhLNe")
review2 <- map_chr(reviewboxes, ~html_node(., ".UD7Dzf") %>% html_text())
name2 <- map_chr(reviewboxes, ~html_node(., ".X43Kjb") %>% html_text())
stars2 <- map_chr(reviewboxes, ~html_node(., ".kx8XBd .nt2C1d [role='img']") %>% html_attr("aria-label"))
date2 <- map_chr(reviewboxes, ~html_node(., ".p2TkOb") %>% html_text())
thumb <- map_chr(reviewboxes, ~html_node(., ".jUL89d") %>% html_text())
reply_t <- map_chr(reviewboxes, ~html_node(., ".LVQB0b") %>% html_text())
no_date <- gsub("^[^,]*,", "", reply_t) #Remove date from reply
reply2 <- str_sub(no_date, 6) #Remove date from reply
language <- cld2::detect_language(review2)
#Make dataframe
review_data <- data.frame(name = name2, date = date2, stars = stars2, review = review2, lang = language, thumb = thumb, reply = reply2, 
                          stringsAsFactors = F)
rbr <- rep(1:nrow(review_data))
review_data <- data.frame(rbr = rbr, name = name2, date = date2, stars = stars2, review = review2, lang = language, thumb = thumb, reply = reply2, 
                          stringsAsFactors = F)

review_data$date <- lubridate::mdy(review_data$date)
review_data$thumb = as.numeric(review_data$thumb)
review_data$stars <- revalue(review_data$stars, 
                                c("Rated 1 stars out of five stars"="1"))
review_data$stars <- revalue(review_data$stars, 
                                c("Rated 2 stars out of five stars"="2"))
review_data$stars <- revalue(review_data$stars, 
                                c("Rated 3 stars out of five stars"="3"))
review_data$stars <- revalue(review_data$stars, 
                                c("Rated 4 stars out of five stars"="4"))
review_data$stars <- revalue(review_data$stars, 
                                c("Rated 5 stars out of five stars"="5"))
review_data$stars = as.numeric(review_data$stars)
#Write file
write.csv(review_data, file = 'review_Waze_fin.csv', row.names = FALSE)
write.xlsx(review_data, "review_Waze_fin.xlsx", row.names = FALSE, showNA = FALSE)

rm(reviewboxes)
rm(webpage)
rm(date2)
rm(language)
rm(name2)
rm(no_date)
rm(rbr)
rm(reply2)
rm(review2)
rm(stars2)
rm(thumb)


#####################################################################################
#Initial data cleaning
WZ_review_data = read.csv("review_Waze_fin.csv")
WZ_review_data$name = as.character(WZ_review_data$name)
WZ_review_data$date = as.Date(WZ_review_data$date)
WZ_review_data$stars = as.numeric(WZ_review_data$stars)
WZ_review_data$review = as.character(WZ_review_data$review)
WZ_review_data$thumb = as.numeric(WZ_review_data$thumb)
WZ_review_data$reply = as.character(WZ_review_data$reply)
WZ_review_data$review = str_replace(WZ_review_data$review, "<.*>", "")
count(WZ_review_data$lang != "en") #Check non english comments
WZ_review_data_final = WZ_review_data[WZ_review_data$lang == "en", ]

count(is.na(WZ_review_data_final$review)) #Check
count(is.na(WZ_review_data$review)) #Check

write.csv(WZ_review_data_final, file = 'review_Waze_fin.csv', row.names = FALSE)
write.xlsx(WZ_review_data_final, "review_Waze_fin.xlsx", row.names = FALSE, showNA = FALSE)

