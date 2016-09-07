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
       quanteda,  # for term document matrices
       wordcloud,  # for making wordclouds
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

#' Rename columns with Euro symbols
gdp <- gdp %>% 
  rename(
    GDP = `GDP €`,
    GDP_percap = `GDP per person €`
  )
gdp

#' #' Take out last row with totals
gdp <- gdp[1:(nrow(gdp) - 1), ]


#' Check out table structure
str(gdp)

#' Make tibble
gdp <- as_tibble(gdp)


#' Take out Euro symbols in rows and "bn" for billion in GDP column
gdp$GDP <- str_replace_all(gdp$GDP, "€", "")
gdp$GDP <- str_replace_all(gdp$GDP, "bn", "")
gdp$GDP_percap <- str_replace_all(gdp$GDP_percap, "€", "")


gdp2 <- gdp
#' Replace "m" (in Population) with scientific notation
gdp2$Population <- gdp$Population %>% 
  gsub(" m", "e+06", .) 
  # as.numeric(.)
#   format(scientific=FALSE)
gdp2$Population

# gdp2$population <- if("e" %in% gdp2$population) {as.numeric(gdp2$population)}

gdp2$Population[1] <- as.numeric(gdp2$Population[1])
format(gdp2$Population[1], scientific = TRUE)


gdp2

#' Set variable data types

gdp3 <- gdp2
gdp3$Area <- factor(gdp$Area)

gdp3$GDP_percap <- parse_number(gdp$GDP_percap)
gdp3$GDP <- parse_number(gdp$GDP)
gdp3$Population <- parse_number(gdp2$Population)
gdp3$City <- factor(gdp$City)
gdp3$Country <- factor(gdp$Country)

str(gdp3)




# repl.m <- function(pop) {
#   for (r in pop) {
#     if (contains("m")) {
#       str_replace(., "m", "000000")
#     } else {
#       next
#     }
#   }
# }
# gdp2$population <- repl.m(gdp$Population)







#' ***
  
#' Scrape all text (excluding citations) from the Wikipedia page
#+
wiki_text <-
  wiki_page %>% 
  html_nodes("p") %>% 
  html_text

#' Check out our text
#+ eval=FALSE
head(wiki_text)

#' We actually have a list of paragraphs because we used the "p" tag in html_nodes()
is.list(wiki_text)  # why does this return `FALSE`?
length(wiki_text)  # so we have 156 paragraphs
wiki_text[[3]]

#' Combine our lists to one vector
#' Note that just doing unlist(wiki_text) doesn't work
ireland <- NULL
for (i in 2:(length(wiki_text))) {   # omit first paragraph
  ireland <- paste(ireland, as.character(wiki_text[i]), sep = ' ')
}
#+ eval=FALSE
head(ireland)
#+
length(ireland)  # good, our 156 paragraphs are now one vector

#' Get all text to lowercase
ireland <- tolower(ireland)

#' Take out all numbers
ireland <- str_replace_all(ireland,"[0-9]+","")

#' Remove `\n` newlines
# ireland <- gsub("\r?\n|\r", "", ireland)
ireland <- str_replace_all(ireland, "[\r\n]", "")

#' ***

#' Create a corpus
i.corp <- Corpus(VectorSource(ireland))

#' Wrap strings into paragraphs so we can see what we have better
#' Not assigning this to i.corp object, i.e., not i.corp <- str_wrap(i.corp[[1]])
#' Note: this is the base::strwrap not stringr::str_wrap
#+ eval=FALSE
strwrap(i.corp[[1]]) # [[1]] because this corpus contains one document


#' Take out punctuation and white space
i.corp <- tm_map(i.corp, removePunctuation)
i.corp <- tm_map(i.corp, stripWhitespace)

#' Make corpus a plain text doc
i.corp <- tm_map(i.corp, PlainTextDocument)

#' View what we've got
#+ eval=FALSE
strwrap(i.corp[[1]])


#' ***


#' Make a document feature or document term matrix
i.dfm <- tm::TermDocumentMatrix(i.corp)

#' Check out rows 1000 to 1010
inspect(i.dfm[1000:1010, ] )



#' Make a wordcloud
#' First convert dfm to matrix
i.matrix <- as.matrix(i.dfm)

#' Label the frequency column
colnames(i.matrix) <- 'frequency'

#' Sort terms by frequency
i.sorted <- sort(rowSums(i.matrix), decreasing = TRUE)

#' Ten most frequent words
i.sorted[1:10]

#' Make into a data.frame
i.dat <- data.frame(word = names(i.sorted), freq = i.sorted)

#' Remove "the" and "and"
i.dat.trim <- i.dat %>% 
  filter(
    !(word %in% c("the", "and")))

head(i.dat.trim)

#' Set RColorBrewer palate
pal <- brewer.pal(20,"Dark2")
#' Set background to black
par(bg = 'dark green')

#' Make the wordcloud
wordcloud(i.dat.trim$word, i.dat.trim$freq, random.order = FALSE,
          max.word = 100, color = pal)
















