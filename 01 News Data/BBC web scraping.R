########################################################################################################################

## Arielle Miranda
## Coding 2: Web Scraping

## Scraping 'vaccine' related news from BBC website (https://www.bbc.co.uk/search?q=vaccine)
## ADDITIONAL: Create a function where we can specify the keyword to search for and the pages to look at

# INSTRUCTIONS:
# Download news data from any websites for a given keyword:
#  The outcome should be a dataframe
#  Apply your function to at least 3 page
#  Write function, use lapply and rbindlist

########################################################################################################################

### LOAD LIBRARIES
library(rvest)
library(data.table)
library(dplyr)

### CREATE FUNCTION TO SCRAPE DATA FROM BBC NEWS WEBSITE
BBC_news <- function(url) {
t <- read_html(url)

news_title <- t %>% 
  html_nodes('.results a') %>% 
  html_text() %>%
  trimws() %>% 
  na_if("") %>%   # remove blank rows 
  na.omit()

news_link <- t %>% 
  html_nodes('.results a') %>% 
  html_attr('href')
news_link<- news_link[c(1:10)*3]   # values are duplicated thrice, only get one occurence per entry

news_preview <- t %>% 
  html_nodes('.long') %>% 
  html_text() %>%
  trimws()
news_preview <- gsub("…","", news_preview)  # texts start and end with a special character "…"

news_date <- t %>% 
  html_nodes('.display-date') %>% 
  html_text() %>%
  trimws()%>% 
  as.Date(format = '%d %b %Y')  # convert string to date format
news_date <- news_date[c(1:10)*3]  # values are duplicated thrice, only get one occurence per entry

news_category <- t %>% 
  html_nodes('.signpost-site') %>% 
  html_text() %>%
  trimws()

news_section <- t %>% 
  html_nodes('.signpost-section') %>% 
  html_text() %>%
  trimws()

news <- data.frame(Title = news_title, 
                   Date = news_date, 
                   Link = news_link, 
                   Category = news_category, 
                   Section = news_section, 
                   Preview =news_preview)
return(news)
}

## GENERATE URLs FOR THE FIRST 5 PAGES 

news_urls <- paste0("https://www.bbc.co.uk/search/more?page=",c(1:5),"&q=vaccine&sa_f=search-product&scope=")

## SCRAPE THE DATA FOR ALL PAGES, COMBINE INTO 1 DATAFRAME
BBC_news_df <- rbind_list(lapply(news_urls, BBC_news))

## SAVE DATAFRAME AS CSV
write.csv(BBC_news_df, "BBC_news_vaccine.csv")


## ADDITIONAL: Originally, I wanted to get data for news articles containing 'vaccine'.
# I wanted to make a function where the keyword to search and the pages to use can be modified as well.
# I noticed that the URLs have the same syntax, but keywords containing spaces are replaced with "+" 
# (note that I didn't test numeric and special characers anymore)

# For this, I created a new function to generate the URLs

generate_url <- function(keyword, first_page, last_page) {
  keyword <- gsub("\\s","+", keyword)
  url <- paste0("https://www.bbc.co.uk/search/more?page=",c(first_page:last_page),"&q=",keyword,"#page=","&sa_f=search-product&scope=" )
  return(url)
}

# GENERATE URLs for "hong kong" PAGES 2 TO 4 ONLY
news_urls_hk <- generate_url("hong kong", 2, 4)

## SCRAPE THE DATA FOR ALL PAGES, COMBINE INTO 1 DATAFRAME
BBC_news_hk_df <- rbind_list(lapply(news_urls_hk, BBC_news))

## SAVE DATAFRAME AS CSV
write.csv(BBC_news_hk_df, "BBC_news_hong_kong.csv")

### It works :)
