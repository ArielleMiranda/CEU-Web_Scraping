########################################################################################################################

## Arielle Miranda
## Coding 2: Web Scraping

## Scraping 'vaccine' related news from CNN website (https://edition.cnn.com/search?q=vaccine)

# INSTRUCTIONS:
# Download news data from any websites for a given keyword:
#  The outcome should be a dataframe
#  Apply your function to at least 3 page
#  Write function, use lapply and rbindlist

########################################################################################################################

### LOAD LIBRARIES
library(jsonlite)
library(data.table)
library(dplyr)

### CREATE FUNCTION TO SCRAPE DATA FROM CNN NEWS WEBSITE
CNN_news <- function(url) {
  t <- fromJSON(url,flatten = T)
  news <- t[[1]] %>% 
    select(headline, type, body, url, firstPublishDate, lastPublishDate, lastModifiedDate, 
           source, byLine, section, mappedSection, thumbnail)
  return(news)
}

## GENERATE URLs FOR THE FIRST 5 PAGES 
## I modified the page size to contain 20 records (size=20) (https://search.api.cnn.io/content?q=vaccine&size=20&from=0&page=1)
## the value after "from=" dhould be dynamic 

news_urls <- paste0("https://search.api.cnn.io/content?q=vaccine&size=20&from=",20*(c(1:5)-1),"&page=",c(1:5))

## SCRAPE THE DATA FOR ALL PAGES, COMBINE INTO 1 DATAFRAME
CNN_news_df <- rbind_list(lapply(news_urls, CNN_news))

## SAVE DATAFRAME AS CSV
write.csv(CNN_news_df, "CNN_news_vaccine.csv")
