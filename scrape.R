#' webs_scrape/scrape.R
#' Amanda Dobbyn
#' Last updated: `r Sys.time()`
 

#' Inspiration from https://github.com/daattali/UBC-STAT545/blob/master/hw/hw12_web-scraping-api/hw12_web-scraping-api.R
#' and https://quantmacro.wordpress.com/2016/04/30/web-scraping-for-text-mining-in-r/

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
       tm,  # for text mining
       stringr,  # for string manipulation
       stringi,  # for string manipulation
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

#' We actually have a list of paragraphs because we used the "p" tag in html_nodes()
is.list(wiki_text)
length(wiki_text)  # so we have 156 paragraphs
wiki_text[[3]]

#' Combine our lists to one vector
#' Note that just doing unlist(wiki_text) doesn't work
ireland <- NULL
for (i in 2:(length(wiki_text))) {   # omit first paragraph
  ireland <- paste(ireland, as.character(wiki_text[i]), sep = ' ')
}
head(ireland)
length(ireland)  # good, our 156 paragraphs are now one vector

#' Get all text to lowercase
ireland <- tolower(ireland)

#' Create a corpus
i.corp <- Corpus(VectorSource(ireland))

#' Wrap strings into paragraphs so we can see what we have better
#' Not assigning this to i.corp object, i.e., not i.corp <- str_wrap(i.corp[[1]])
#' Note: this is the base::strwrap not stringr::str_wrap
str_wrap(i.corp[[1]]) # [[1]] because this corpus contains one document

i.corp <- 





txt <- paste(readLines(txt[[3]]))



#' #' Take out [citation_number]
#' #+
#' txt <- wiki_text %>% 
#'   str_replace_all(
#'     "[:digit:]", ""
#'   )
#' head(txt)
#' 
#' txt <- wiki_text %>% 
#'   gsub(
#'     "[1]", "", .
#'   )
#' head(txt)



# txt <- str_replace_all(txt, "[^a-zA-Z ]","") #only keep letters
# 
# 
# str_replace_all(myText,"[:digit:]","")




