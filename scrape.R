#' scrape.R
#' Amanda Dobbyn
#' Last updated: `r Sys.time()`

#+ Set working directory
setwd(getwd())

#+ Load packages
library(pacman)
p_load(knitr,  # for weaving this into pretty format
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

wiki_url <-
wiki_page <- rvest::html(wiki_url)
wiki_table <- 
  wiki_page %>%
  html_node("talbe") %>%
  html_table


podcasts <- wiki_table %>% 
  select(-grep("")
           )


-grep("(Rank)|(Source)|(Expenditure.*per capita)|(Year)",
      colnames(.),
      ignore.case = TRUE))
  

wikiRnDPageUrl <- "http://en.wikipedia.org/wiki/List_of_countries_by_research_and_development_spending"
wikiRnDPage <- rvest::html(wikiRnDPageUrl)
wikiRnDTable <-
  wikiRnDPage %>%
  html_node("table") %>%
  html_table



