#' webs_scrape/scrape.R
#' Amanda Dobbyn
#' Last updated: `r Sys.time()`
 
#' Using knitr::spin 

#+ Set working directory
setwd(getwd())

#+ Load packages
library(pacman)
p_load(knitr,  # for weaving this into pretty format
       XML,  # for web scraping 
       rvest,  # for web scraping
       tibble,  # for an easier way to work with data.frames
       dplyr,  # for data manipulation
       stringr,  # for string manipulation
       lubridate,  # for dates
       tidyr,  # for gather() and spread()
       data.table,  # for 
       DT,  # for kable()
       ggplot2,  # for plots
       ggrepel  # for spreading point labels
)

#' Set page we want to scrape
#+
wiki_url <- "https://en.wikipedia.org/wiki/Ireland"
wiki_page <- read_html(wiki_url)

#' Scrape both tables
#' A "view page source" and command+F shows that this table is actually 
#' `<table class="wikitable sortable">` and the smaller table is just 
#' `<table class="wikitable">` but using `html_nodes(".wikitable sortable")`
#' returns an empy list
#+
wiki_table <- 
  wiki_page %>%
  html_nodes(".wikitable") %>%
  html_table()

#' Check out what we've scraped. Looks like two tables.
#+
wiki_table
length(wiki_table) # a list of 2

#' Select the table we want
#+
gdp <- wiki_table[[2]]

#' ***
  
#' Scrape all text (excluding citations) from the Wikipedia page
#+
wiki_text <-
  wiki_page %>% 
  html_nodes("p") %>% 
  html_text

#' Check out our text
#+
head(wiki_text)

#' Take out [citation_number]
#+
txt <- wiki_text %>% 
  str_replace_all(
    "[:digit:]", ""
  )
head(txt)

# txt <- str_replace_all(txt, "[^a-zA-Z ]","") #only keep letters
# 
# 
# str_replace_all(myText,"[:digit:]","")




