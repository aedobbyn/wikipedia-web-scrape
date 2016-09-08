#' ---
#' title: "Wikipedia Ireland Web Scrape"
#' author: 
#'  name: "Amanda Dobbyn"
#'  email: aedobbyn@uchicago.edu
#' output:
#'  html_document:
#'    keep_md: true
#' ---


#' Last updated: `r Sys.time()`  

#' Inspiration from https://github.com/daattali/UBC-STAT545/blob/master/hw/hw12_web-scraping-api/hw12_web-scraping-api.R  
#' and https://quantmacro.wordpress.com/2016/04/30/web-scraping-for-text-mining-in-r/  

#' This doc compiled using knitr::spin   


#+ set.wd, eval=FALSE
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
       readr,  # for parse_number()
       stringr,  # for string manipulation
       stringi,  # for string manipulation
       lubridate,  # for dates
       tidyr,  # for gather() and spread()
       data.table,  # for 
       DT,  # for kable()
       ggplot2,  # for plots
       ggvis,  # for plots
       ggrepel  # for spreading point labels
)

#' Set page we want to scrape
#+
wiki_url <- "https://en.wikipedia.org/wiki/Ireland"
wiki_page <- read_html(wiki_url)

#' # Adventures in table scraping  

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
emerald_table <- wiki_table[[2]]

#' Rename columns with Euro symbols
em_tab <- emerald_table %>% 
  rename(
    GDP = `GDP €`,
    GDP_percap = `GDP per person €`
  )
em_tab

#' Take out last row with totals
em_tab <- em_tab[1:(nrow(em_tab) - 1), ]

#' Check out table structure
str(em_tab)

#' Make tibble
em_tab <- as_tibble(em_tab)

#' Take out Euro symbols in rows and "bn" for billion in GDP column
em_tab$GDP <- str_replace_all(em_tab$GDP, "€", "")
em_tab$GDP <- str_replace_all(gdem_tabp$GDP, "bn", "")
em_tab$GDP_percap <- str_replace_all(em_tab$GDP_percap, "€", "")


# Copy our dataframe
em_tab <- em_tab

#' Replace `m` (in Population) with scientific notation
em_tab$Population <- gdp$Population %>% 
  gsub(" m", "e+06", .) 
em_tab$Population

em_tab$Population[1] <- as.numeric(em_tab$Population[1])
format(em_tab$Population[1], scientific = TRUE)


#+ eval=FALSE
em_tab

#' Set variable data types
# em_tab <- em_tab

#' Numerize
to.numerize <- c("Population", "GDP", "GDP_percap")
em_tab[, to.numerize] <- data.frame(apply
                                    (em_tab[, to.numerize], 2, 
                                    parse_number)) # use readr::parse_numer

#' Factorize
em_tab <- em_tab %>%
  rsalad::dfFactorize(
    ignore = c("Population", "GDP", "GDP_percap")
  )

em_tab


# Multiply GDP by 1 bil
em_tab <- em_tab
em_tab$GDP <- (em_tab$GDP)*(1e+09)

#+ eval=FALSE
em_tab


#' Graph population and GDP per capita, coloring points by country
em_tab %>% 
  ggvis(~Population, ~GDP_percap, fill = ~Country) %>% 
  layer_points()


#' For countries in the ROI (Republic of Ireland and also our region of interest, lol), 
#' plot GDP vs. per capita GDP and fill by Area
em_tab %>%
  filter(Country == "ROI") %>%
  droplevels() %>%    # drop unused Areas (e.g., Greater Belfast) from legend
  ggvis(~GDP, ~GDP_percap, fill=~Area) %>%
  scale_numeric("x") %>%   # reorder levels by GDP
  layer_points()











#'
#' *** 
#' .  
#' .  
#' .  
#' # Adventures in text munging and wordclouding  
#' .  
#' .  
#' .  
#'
#' ***
#'
#' Scrape all text (excluding citations) from the Wikipedia page
#+
wiki_text <-
  wiki_page %>% 
  html_nodes("p") %>% 
  html_text

#' Check out our text
#+ eval=FALSE
head(wiki_text)

#' We actually have a list of paragraphs because we used the `<p>` tag in `html_nodes()`
is.list(wiki_text)  # why does this return `FALSE`?
length(wiki_text)  # so we have 156 paragraphs

#' The third paragraph
wiki_text[[3]]

#' Combine our lists to one vector 
#' Note that just doing `unlist(wiki_text)` doesn't work
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
















