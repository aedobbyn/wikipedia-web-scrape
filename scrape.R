#' ---
#' title: "Wikipedia Ireland Web Scrape"
#' author: 
#'  name: "Amanda Dobbyn"
#'  email: aedobbyn@uchicago.edu
#' output:
#'  html_document:
#'    fig_caption: yes
#'    keep_md: true
#' ---

#' ### Quick overview  
#' * Use `rvest` package to scrape the [Wikipedia page on Ireland](https://en.wikipedia.org/wiki/Ireland)  
#' + Tame the larger of the two tables on the page and create some visualizations  
#' + Munge the text of the entire page and create word frequency wordclouds to see
#' what words are mentioned most often on the page  


#' * This doc was compiled using knitr::spin, 
#' thanks to a [post by Dean Attali](http://deanattali.com/2015/03/24/knitrs-best-hidden-gem-spin/)  
#'   
#'   
#'   
#'   
#'   

#' ***

#' #### Set things up

#+ set.wd, eval=FALSE, echo=FALSE
setwd(getwd())

#' Load packages
#+ package_load
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
       DT,  # for kable()
       ggplot2,  # for plots
       ggvis  # for plots
)

#' Set page we want to scrape
#+
wiki_url <- "https://en.wikipedia.org/wiki/Ireland"

#' Parse the HTML and save as an object
#+
wiki_page <- read_html(wiki_url)

#' ***

#' # Adventures in Table Taming  

#' There are two tables on the Ireland Wikipedia page. I'm interested in
#' the larger of the two.  
#' The idea is to use the extension [selectorgadget](http://selectorgadget.com/) to 
#' identify CSS selectors that wrap the bits of HTML we're interested in
#' so that we can select just the HTML associated with that selector.  
#' Scrape both tables using a more general selector that a wraps both of them.  
#' (In the browser, a "view page source" and command+F shows that the table 
#' we want is actually tagged with
#' `<table class="wikitable sortable">` and the smaller table is tagged with 
#' `<table class="wikitable">`, but using `html_nodes(".wikitable sortable")`
#' returns an empy list.)

#+ scrape_talbes
wiki_table <- 
  wiki_page %>%
  html_nodes(".wikitable") %>%
  html_table()

#' Check out what we've scraped
#+
wiki_table

#' We've got two tables
#+
length(wiki_table) # a list of 2

#' Select the table we want (the second one)
#+
emerald_table <- wiki_table[[2]]

#' #### Munge strings

#' Rename columns that contain `€` symbols
#+
em_tab <- emerald_table %>% 
  rename(
    GDP = `GDP €`,
    GDP_percap = `GDP per person €`
  )
#+ eval=FALSE, echo=FALSE
em_tab

#' Take out the last row of the table that just contains totals
#+
em_tab <- em_tab[1:(nrow(em_tab) - 1), ]

#' Check out table structure. All varaibles are characters.
#+
str(em_tab)

#' Make the table into a `tibble` so we can more easily see variable types
#' when we print the table
#+
em_tab <- as_tibble(em_tab)

#' Take out `€` symbols in rows and "bn" for billion in GDP column
#+
em_tab$GDP <- str_replace_all(em_tab$GDP, "€", "")
em_tab$GDP <- str_replace_all(em_tab$GDP, "bn", "")
em_tab$GDP_percap <- str_replace_all(em_tab$GDP_percap, "€", "")


#' There's an "m" for million in the first row of the `Population` 
#' column. We need to make the m into something that can be interpreted as
#' a number.
#' Since the column is still of type charager and not numeric, we
#' can just replace it with scientific notation characters with a `gsub`.
#+
em_tab$Population <- em_tab$Population %>% 
  gsub(" m", "e+06", .) 

#' Take a look at the `Population` vector to make sure our `gsub` worked.
em_tab$Population  # good

#' Make that first element of `Population` into standard notation
#+
em_tab$Population[1] <- as.numeric(em_tab$Population[1])
format(em_tab$Population[1], scientific = TRUE)

#' Check out our table
#+ 
em_tab

#' #### Set variable data types  

#' Numerize variables that should be numeric
#+
to.numerize <- c("Population", "GDP", "GDP_percap")
em_tab[, to.numerize] <- data.frame(apply
                                    (em_tab[, to.numerize], 2, 
                                    parse_number)) # use readr::parse_numer

#' Factorize variables that should be factors
#+
em_tab <- em_tab %>%
  rsalad::dfFactorize(
    ignore = c("Population", "GDP", "GDP_percap")  # in other words, select Area, City, and Country
  )


#' Multiply `GDP` by 1 bil (because we took out the trailing "b" earlier)
#+
em_tab <- em_tab
em_tab$GDP <- (em_tab$GDP)*(1e+09)

#+ eval=FALSE, echo=FALSE
em_tab


#' Graph population and GDP per capita, coloring points by country

#+ echo=FALSE, eval=FALSE
# The ggvis version (does not render in GitHub)
# em_tab %>% 
#   ggvis(~Population, ~GDP_percap, fill = ~Country) %>% 
#   layer_points() %>% 
#   add_axis("x", title = "Population", ticks = 5) %>%
#   add_axis("y", title = "GDP per capita", ticks = 5, title_offset = 60) %>% 
#   add_axis("x", orient = "top", ticks = 0,  # hack to add a title since ggvis doesn't have equivalent of ggtitle() yet
#            title = "Ireland and Northern Ireland: Population and GDP per capita",
#            properties = axis_props(
#              axis = list(stroke = "white"),
#              labels = list(fontSize = 0))) 

#+ country_gdp_ggvis
gdp_by_country_plot <- ggplot(em_tab, aes(x=Population, y=GDP_percap, colour=Country)) + 
  geom_point() +
  ggtitle("Ireland and Northern Ireland: \n Population and GDP per capita") +
  ylab("GDP per capita") +
  theme_classic() +
  theme(axis.line.x = element_line(color="black", size = 0.3), # theme_classic() removes axes so draw them back in
        axis.line.y = element_line(color="black", size = 0.3)) 

print(gdp_by_country_plot)

#' So it looks like the ROI is generally more populous and wealthier than Northern Ireland.  
#' What about `Area`s within the ROI?  




#+ echo=FALSE, eval=FALSE
# The ggvis version (does not render in GitHub)
# em_tab %>%
#   filter(Country == "ROI") %>%
#   droplevels() %>%    # drop unused Areas (e.g., Greater Belfast) from legend
#   ggvis(~GDP, ~GDP_percap, fill=~Area) %>%
#   scale_numeric("x") %>%   # reorder levels by GDP
#   layer_points() %>%
#   add_axis("x", title = "GDP", ticks = 3) %>%
#   add_axis("y", title = "GDP per capita", ticks = 5, title_offset = 60) %>%
#   add_axis("x", orient = "top", ticks = 0,
#            title = "Regions in Ireland: Population and GDP per capita",
#            properties = axis_props(
#              axis = list(stroke = "white"),
#              labels = list(fontSize = 0)))

#' Filter down to just areas in the ROI (Republic of Ireland 
#' and also our region of interest, lol)

#' For Areas in the ROI, 
#' plot GDP vs. per capita GDP
#+ 
em_ROI <- em_tab %>%
  filter(Country == "ROI") %>% 
  droplevels()  # drop unused Areas (e.g., Greater Belfast) from legend

#+ ROI_gdp_ggvis
ROI_plot <- ggplot(em_ROI, aes(x=GDP, y=GDP_percap, colour=Area)) + 
  geom_point() +
  ggtitle("Regions in Ireland: \n Population and GDP per capita") +
  ylab("GDP per capita") +
  theme_classic() +
  theme(axis.line.x = element_line(color="black", size = 0.3), 
        axis.line.y = element_line(color="black", size = 0.3)) 

print(ROI_plot)


#' ***

#' # Adventures in Text Munging and Wordclouding

#' ***

#' Scrape all text (excluding citations) from the same Wikipedia page. Note that 
#' we're using the selector `p` in `html_nodes()`. If we use `#bodyContent` instead,
#' the citations at the bottom of the page get included, which we don't want.
#+
wiki_text <-
  wiki_page %>% 
  html_nodes("p") %>%  
  html_text

#' Check out our text
#+ eval=FALSE
head(wiki_text)

#+ eval=FALSE, echo=FALSE
is.list(wiki_text)  # why does this return `FALSE`?

#' We have a list of paragraphs.
#+
length(wiki_text)  # 156 paragraphs

#' For example, we can get the third paragraph of the page with
#+
wiki_text[[3]]

#' Combine our lists to one vector.  
#' Note that we can't just do `unlist(wiki_text)`.
#+
ireland <- NULL
for (i in 2:(length(wiki_text))) {   # omit first paragraph because it just says "in Europe  (green & dark grey)"
  ireland <- paste(ireland, as.character(wiki_text[i]), sep = ' ')
}

#+ eval=FALSE, echo=FALSE
head(ireland)

#+
length(ireland)  # good, our 156 paragraphs are now one vector

#' Get all text to lowercase
#+
ireland <- tolower(ireland)

#' Take out all numbers
#+
ireland <- str_replace_all(ireland,"[0-9]+","")

#' Remove `\n` newlines
#+
ireland <- str_replace_all(ireland, "[\r\n]", "")  # same as # ireland <- gsub("\r?\n|\r", "", ireland)

#' ***

#' #### Begin wordclouding

#' Much of the wordclouding inspiration was adapted 
#' from [this blog](https://quantmacro.wordpress.com/2016/04/30/web-scraping-for-text-mining-in-r/)  

#' Create a corpus
#+
i.corp <- Corpus(VectorSource(ireland))

#' Wrap strings into paragraphs so we can see what we have better  
#' Note that this is the `base::strwrap` not `stringr::str_wrap`
#+ eval=FALSE
strwrap(i.corp[[1]]) # [[1]] because this corpus contains one document


#' Take out punctuation and white space
#+
i.corp <- tm_map(i.corp, removePunctuation)
i.corp <- tm_map(i.corp, stripWhitespace)

#' Make corpus a plain text doc
#+
i.corp <- tm_map(i.corp, PlainTextDocument)

#' View what we've got
#+ eval=FALSE
strwrap(i.corp[[1]])


#' ***


#' Make a document feature or document term matrix
#+
i.dfm <- tm::TermDocumentMatrix(i.corp)

#' Check out rows 1000 to 1010
inspect(i.dfm[1000:1010, ] )



#' Convert dfm to matrix
#+
i.matrix <- as.matrix(i.dfm)

#' Label the frequency column
#+
colnames(i.matrix) <- 'frequency'

#' Sort terms by frequency
#+
i.sorted <- sort(rowSums(i.matrix), decreasing = TRUE)

#' Check out the ten most frequent words in the doc
#+
i.sorted[1:10]

#' Make our matrix into a data.frame, taking it from wide to long format
#+
i.dat <- data.frame(word = names(i.sorted), freq = i.sorted)

#' Check out the most frequent words
#+
head(i.dat)

#' Remove "the" and "and" because they're not interesting
#+
i.dat.trim <- i.dat %>% 
  filter(
    !(word %in% c("the", "and")))


#' Set RColorBrewer palate
#+
palette <- brewer.pal(12,"Paired")
#' Set background to black
par(bg = 'black')

#' Generate the wordcloud!
#+ ireland_wordcloud, dev=c('png')
wordcloud(i.dat.trim$word, i.dat.trim$freq, random.order = FALSE,
          max.word = 200, color = palette,
          vfont = c("serif", "plain"))



















