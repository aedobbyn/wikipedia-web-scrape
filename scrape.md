# Wikipedia Ireland Web Scrape
### Quick overview  
* Use `rvest` package to scrape the Wikipedia page on Ireland  
+ Tame the larger of the two tables on the page and create some visualizations  
+ Munge the text of the entire page and create word frequency wordclouds to see
what words are mentioned most often on the page  
This doc was compiled using knitr::spin, 
thanks to a [post by Dean Attali](http://deanattali.com/2015/03/24/knitrs-best-hidden-gem-spin/)  
  
  
  
  
  
***
#### Set things up
Set working directory



Load packages


```r
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
```

Set page we want to scrape


```r
wiki_url <- "https://en.wikipedia.org/wiki/Ireland"
wiki_page <- read_html(wiki_url)
```

***
# Adventures in Table Taming  
There are two tables on the Ireland Wikipedia page  
Scrape both tables  
In the browser, a "view page source" and command+F shows that the table 
we want is actually tagged with
`<table class="wikitable sortable">` and the smaller table is tagged with 
`<table class="wikitable">`, but using `html_nodes(".wikitable sortable")`
returns an empy list


```r
wiki_table <- 
  wiki_page %>%
  html_nodes(".wikitable") %>%
  html_table()
```

Check out what we've scraped. 


```r
wiki_table
```

```
## [[1]]
##                                            X1
## 1 Republic of Ireland: Border Midlands & West
## 2                                  €30 bn[92]
## 3                      €23,700 per person[93]
##                                        X2                               X3
## 1 Republic of Ireland: Southern & Eastern United Kingdom: Northern Ireland
## 2            €142 bn (Dublin €72.4bn)[92]  €43.4 bn (Belfast €20.9 bn)[93]
## 3                  €39,900 per person[93]           €21,000 per person[93]
## 
## [[2]]
##                                  Area Population Country       City
## 1                       Dublin Region      1.3 m     ROI     Dublin
## 2                   South-West Region    670,000     ROI       Cork
## 3                     Greater Belfast    720,000      NI    Belfast
## 4                         West Region    380,000     ROI     Galway
## 5                     Mid-West Region    340,000     ROI   Limerick
## 6                   South-East Region    460,000     ROI  Waterford
## 7                     Mid-East Region    475,000     ROI       Bray
## 8                       Border Region    430,000     ROI   Drogheda
## 9            East of Northern Ireland    430,000      NI Ballymeena
## 10                    Midlands Region    280,000     ROI    Athlone
## 11 West and South of Northern Ireland    400,000      NI      Newry
## 12          North of Northern Ireland    280,000      NI      Derry
## 13                              Total      6.4 m                   
##        GDP € GDP per person €
## 1   €72.4 bn          €57,200
## 2   €32.3 bn          €48,500
## 3   €20.9 bn          €33,550
## 4   €13.8 bn          €31,500
## 5   €11.4 bn          €30,300
## 6   €12.8 bn          €25,600
## 7   €13.3 bn          €24,700
## 8   €10.7 bn          €21,100
## 9    €9.5 bn          €20,300
## 10   €5.7 bn          €20,100
## 11   €8.4 bn          €19,300
## 12   €5.5 bn          €18,400
## 13 €216.7 bn
```

We've got two tables.


```r
length(wiki_table) # a list of 2
```

```
## [1] 2
```

Select the table we want


```r
emerald_table <- wiki_table[[2]]
```

Rename columns that contain `€` symbols


```r
em_tab <- emerald_table %>% 
  rename(
    GDP = `GDP €`,
    GDP_percap = `GDP per person €`
  )
```

```r
em_tab
```

Take out last row that contains totals


```r
em_tab <- em_tab[1:(nrow(em_tab) - 1), ]
```

Check out table structure. All varaibles are characters.


```r
str(em_tab)
```

```
## 'data.frame':	12 obs. of  6 variables:
##  $ Area      : chr  "Dublin Region" "South-West Region" "Greater Belfast" "West Region" ...
##  $ Population: chr  "1.3 m" "670,000" "720,000" "380,000" ...
##  $ Country   : chr  "ROI" "ROI" "NI" "ROI" ...
##  $ City      : chr  "Dublin" "Cork" "Belfast" "Galway" ...
##  $ GDP       : chr  "€72.4 bn" "€32.3 bn" "€20.9 bn" "€13.8 bn" ...
##  $ GDP_percap: chr  "€57,200" "€48,500" "€33,550" "€31,500" ...
```

Make the table into a `tibble` so we can more easily see variable types


```r
em_tab <- as_tibble(em_tab)
```

Take out `€` symbols in rows and "bn" for billion in GDP column


```r
em_tab$GDP <- str_replace_all(em_tab$GDP, "€", "")
em_tab$GDP <- str_replace_all(em_tab$GDP, "bn", "")
em_tab$GDP_percap <- str_replace_all(em_tab$GDP_percap, "€", "")
```

Replace "m" for million (in the `Population` column) with scientific notation characters


```r
em_tab$Population <- em_tab$Population %>% 
  gsub(" m", "e+06", .) 
em_tab$Population
```

```
##  [1] "1.3e+06" "670,000" "720,000" "380,000" "340,000" "460,000" "475,000"
##  [8] "430,000" "430,000" "280,000" "400,000" "280,000"
```

Take that element of `Population` into standard notation


```r
em_tab$Population[1] <- as.numeric(em_tab$Population[1])
format(em_tab$Population[1], scientific = TRUE)
```

```
## [1] "1300000"
```

```r
em_tab
```

Set variable data types  
Numerize variables that should be numeric


```r
to.numerize <- c("Population", "GDP", "GDP_percap")
em_tab[, to.numerize] <- data.frame(apply
                                    (em_tab[, to.numerize], 2, 
                                    parse_number)) # use readr::parse_numer
```

Factorize variables that should be factors


```r
em_tab <- em_tab %>%
  rsalad::dfFactorize(
    ignore = c("Population", "GDP", "GDP_percap")  # in other words, select Area, City, and Country
  )
```

Multiply GDP by 1 bil  
(We took out the trailing "b" earlier)


```r
em_tab <- em_tab
em_tab$GDP <- (em_tab$GDP)*(1e+09)
```


Graph population and GDP per capita, coloring points by country


```r
em_tab %>% 
  ggvis(~Population, ~GDP_percap, fill = ~Country) %>% 
  layer_points() %>% 
  add_axis("x", title = "Population", ticks = 5) %>%
  add_axis("y", title = "GDP per capita", ticks = 5, title_offset = 60) %>% 
  add_axis("x", orient = "top", ticks = 0,  # hack to add a title since ggvis doesn't have equivalent of ggtitle() yet
           title = "Ireland and Northern Ireland: Population and GDP per capita",
           properties = axis_props(
             axis = list(stroke = "white"),
             labels = list(fontSize = 0))) 
```

<!--html_preserve--><div id="plot_id493053761-container" class="ggvis-output-container">
<div id="plot_id493053761" class="ggvis-output"></div>
<div class="plot-gear-icon">
<nav class="ggvis-control">
<a class="ggvis-dropdown-toggle" title="Controls" onclick="return false;"></a>
<ul class="ggvis-dropdown">
<li>
Renderer: 
<a id="plot_id493053761_renderer_svg" class="ggvis-renderer-button" onclick="return false;" data-plot-id="plot_id493053761" data-renderer="svg">SVG</a>
 | 
<a id="plot_id493053761_renderer_canvas" class="ggvis-renderer-button" onclick="return false;" data-plot-id="plot_id493053761" data-renderer="canvas">Canvas</a>
</li>
<li>
<a id="plot_id493053761_download" class="ggvis-download" data-plot-id="plot_id493053761">Download</a>
</li>
</ul>
</nav>
</div>
</div>
<script type="text/javascript">
var plot_id493053761_spec = {
  "data": [
    {
      "name": ".0",
      "format": {
        "type": "csv",
        "parse": {
          "Population": "number",
          "GDP_percap": "number"
        }
      },
      "values": "\"Country\",\"Population\",\"GDP_percap\"\n\"ROI\",1300000,57200\n\"ROI\",670000,48500\n\"NI\",720000,33550\n\"ROI\",380000,31500\n\"ROI\",340000,30300\n\"ROI\",460000,25600\n\"ROI\",475000,24700\n\"ROI\",430000,21100\n\"NI\",430000,20300\n\"ROI\",280000,20100\n\"NI\",4e+05,19300\n\"NI\",280000,18400"
    },
    {
      "name": "scale/fill",
      "format": {
        "type": "csv",
        "parse": {}
      },
      "values": "\"domain\"\n\"NI\"\n\"ROI\""
    },
    {
      "name": "scale/x",
      "format": {
        "type": "csv",
        "parse": {
          "domain": "number"
        }
      },
      "values": "\"domain\"\n229000\n1351000"
    },
    {
      "name": "scale/y",
      "format": {
        "type": "csv",
        "parse": {
          "domain": "number"
        }
      },
      "values": "\"domain\"\n16460\n59140"
    }
  ],
  "scales": [
    {
      "name": "fill",
      "type": "ordinal",
      "domain": {
        "data": "scale/fill",
        "field": "data.domain"
      },
      "points": true,
      "sort": false,
      "range": "category10"
    },
    {
      "name": "x",
      "domain": {
        "data": "scale/x",
        "field": "data.domain"
      },
      "zero": false,
      "nice": false,
      "clamp": false,
      "range": "width"
    },
    {
      "name": "y",
      "domain": {
        "data": "scale/y",
        "field": "data.domain"
      },
      "zero": false,
      "nice": false,
      "clamp": false,
      "range": "height"
    }
  ],
  "marks": [
    {
      "type": "symbol",
      "properties": {
        "update": {
          "size": {
            "value": 50
          },
          "fill": {
            "scale": "fill",
            "field": "data.Country"
          },
          "x": {
            "scale": "x",
            "field": "data.Population"
          },
          "y": {
            "scale": "y",
            "field": "data.GDP_percap"
          }
        },
        "ggvis": {
          "data": {
            "value": ".0"
          }
        }
      },
      "from": {
        "data": ".0"
      }
    }
  ],
  "legends": [
    {
      "orient": "right",
      "fill": "fill",
      "title": "Country"
    }
  ],
  "axes": [
    {
      "type": "x",
      "scale": "x",
      "orient": "bottom",
      "title": "Population",
      "ticks": 5,
      "layer": "back",
      "grid": true
    },
    {
      "type": "y",
      "scale": "y",
      "orient": "left",
      "title": "GDP per capita",
      "titleOffset": 60,
      "ticks": 5,
      "layer": "back",
      "grid": true
    },
    {
      "type": "x",
      "scale": "x",
      "orient": "top",
      "title": "Ireland and Northern Ireland: Population and GDP per capita",
      "ticks": 0,
      "layer": "back",
      "grid": true,
      "properties": {
        "labels": {
          "fontSize": {
            "value": 0
          }
        },
        "axis": {
          "stroke": {
            "value": "white"
          }
        }
      }
    }
  ],
  "padding": null,
  "ggvis_opts": {
    "keep_aspect": false,
    "resizable": true,
    "padding": {},
    "duration": 250,
    "renderer": "svg",
    "hover_duration": 0,
    "width": 672,
    "height": 480
  },
  "handlers": null
};
ggvis.getPlot("plot_id493053761").parseSpec(plot_id493053761_spec);
</script><!--/html_preserve-->

Looks like the ROI is generally more populous and wealthier than Northern Ireland  
What about `Area`s within the ROI?
For Areas in the ROI (Republic of Ireland and also our region of interest, lol), 
plot GDP vs. per capita GDP


```r
em_tab %>%
  filter(Country == "ROI") %>%
  droplevels() %>%    # drop unused Areas (e.g., Greater Belfast) from legend
  ggvis(~GDP, ~GDP_percap, fill=~Area) %>%
  scale_numeric("x") %>%   # reorder levels by GDP
  layer_points() %>% 
  add_axis("x", title = "GDP", ticks = 3) %>%
  add_axis("y", title = "GDP per capita", ticks = 5, title_offset = 60) %>% 
  add_axis("x", orient = "top", ticks = 0,  
           title = "Regions in Ireland: Population and GDP per capita",
           properties = axis_props(
             axis = list(stroke = "white"),
             labels = list(fontSize = 0))) 
```

<!--html_preserve--><div id="plot_id920301897-container" class="ggvis-output-container">
<div id="plot_id920301897" class="ggvis-output"></div>
<div class="plot-gear-icon">
<nav class="ggvis-control">
<a class="ggvis-dropdown-toggle" title="Controls" onclick="return false;"></a>
<ul class="ggvis-dropdown">
<li>
Renderer: 
<a id="plot_id920301897_renderer_svg" class="ggvis-renderer-button" onclick="return false;" data-plot-id="plot_id920301897" data-renderer="svg">SVG</a>
 | 
<a id="plot_id920301897_renderer_canvas" class="ggvis-renderer-button" onclick="return false;" data-plot-id="plot_id920301897" data-renderer="canvas">Canvas</a>
</li>
<li>
<a id="plot_id920301897_download" class="ggvis-download" data-plot-id="plot_id920301897">Download</a>
</li>
</ul>
</nav>
</div>
</div>
<script type="text/javascript">
var plot_id920301897_spec = {
  "data": [
    {
      "name": ".0",
      "format": {
        "type": "csv",
        "parse": {
          "GDP": "number",
          "GDP_percap": "number"
        }
      },
      "values": "\"Area\",\"GDP\",\"GDP_percap\"\n\"Dublin Region\",7.24e+10,57200\n\"South-West Region\",3.23e+10,48500\n\"West Region\",1.38e+10,31500\n\"Mid-West Region\",1.14e+10,30300\n\"South-East Region\",1.28e+10,25600\n\"Mid-East Region\",1.33e+10,24700\n\"Border Region\",1.07e+10,21100\n\"Midlands Region\",5.7e+09,20100"
    },
    {
      "name": "scale/fill",
      "format": {
        "type": "csv",
        "parse": {}
      },
      "values": "\"domain\"\n\"Border Region\"\n\"Dublin Region\"\n\"Mid-East Region\"\n\"Mid-West Region\"\n\"Midlands Region\"\n\"South-East Region\"\n\"South-West Region\"\n\"West Region\""
    },
    {
      "name": "scale/x",
      "format": {
        "type": "csv",
        "parse": {
          "domain": "number"
        }
      },
      "values": "\"domain\"\n2.365e+09\n7.5735e+10"
    },
    {
      "name": "scale/y",
      "format": {
        "type": "csv",
        "parse": {
          "domain": "number"
        }
      },
      "values": "\"domain\"\n18245\n59055"
    }
  ],
  "scales": [
    {
      "name": "fill",
      "type": "ordinal",
      "domain": {
        "data": "scale/fill",
        "field": "data.domain"
      },
      "points": true,
      "sort": false,
      "range": "category10"
    },
    {
      "name": "x",
      "domain": {
        "data": "scale/x",
        "field": "data.domain"
      },
      "zero": false,
      "nice": false,
      "clamp": false,
      "range": "width"
    },
    {
      "name": "y",
      "domain": {
        "data": "scale/y",
        "field": "data.domain"
      },
      "zero": false,
      "nice": false,
      "clamp": false,
      "range": "height"
    }
  ],
  "marks": [
    {
      "type": "symbol",
      "properties": {
        "update": {
          "size": {
            "value": 50
          },
          "fill": {
            "scale": "fill",
            "field": "data.Area"
          },
          "x": {
            "scale": "x",
            "field": "data.GDP"
          },
          "y": {
            "scale": "y",
            "field": "data.GDP_percap"
          }
        },
        "ggvis": {
          "data": {
            "value": ".0"
          }
        }
      },
      "from": {
        "data": ".0"
      }
    }
  ],
  "legends": [
    {
      "orient": "right",
      "fill": "fill",
      "title": "Area"
    }
  ],
  "axes": [
    {
      "type": "x",
      "scale": "x",
      "orient": "bottom",
      "title": "GDP",
      "ticks": 3,
      "layer": "back",
      "grid": true
    },
    {
      "type": "y",
      "scale": "y",
      "orient": "left",
      "title": "GDP per capita",
      "titleOffset": 60,
      "ticks": 5,
      "layer": "back",
      "grid": true
    },
    {
      "type": "x",
      "scale": "x",
      "orient": "top",
      "title": "Regions in Ireland: Population and GDP per capita",
      "ticks": 0,
      "layer": "back",
      "grid": true,
      "properties": {
        "labels": {
          "fontSize": {
            "value": 0
          }
        },
        "axis": {
          "stroke": {
            "value": "white"
          }
        }
      }
    }
  ],
  "padding": null,
  "ggvis_opts": {
    "keep_aspect": false,
    "resizable": true,
    "padding": {},
    "duration": 250,
    "renderer": "svg",
    "hover_duration": 0,
    "width": 672,
    "height": 480
  },
  "handlers": null
};
ggvis.getPlot("plot_id920301897").parseSpec(plot_id920301897_spec);
</script><!--/html_preserve-->

*** 
# Adventures in Text Munging and Wordclouding  
***
Scrape all text (excluding citations) from the Wikipedia page


```r
wiki_text <-
  wiki_page %>% 
  html_nodes("p") %>%  # if use the selector #bodyContent, citations get included as text which we don't want
  html_text
```

Check out our text


```r
head(wiki_text)
```


We actually have a list of paragraphs because we used the `<p>` tag in `html_nodes()`


```r
length(wiki_text)  # so we have 156 paragraphs
```

```
## [1] 156
```

For example, we can get the third paragraph of the page with


```r
wiki_text[[3]]
```

```
## [1] "Politically, Ireland is divided between the Republic of Ireland (officially named Ireland), which covers five-sixths of the island, and Northern Ireland, which is part of the United Kingdom, in the northeast of the island. In 2011 the population of Ireland was about 6.4 million, ranking it the second-most populous island in Europe after Great Britain. Just under 4.6 million live in the Republic of Ireland and just over 1.8 million live in Northern Ireland.[7]"
```

Combine our lists to one vector  
Note that just doing `unlist(wiki_text)` doesn't work


```r
ireland <- NULL
for (i in 2:(length(wiki_text))) {   # omit first paragraph
  ireland <- paste(ireland, as.character(wiki_text[i]), sep = ' ')
}
```


```r
length(ireland)  # good, our 156 paragraphs are now one vector
```

```
## [1] 1
```

Get all text to lowercase


```r
ireland <- tolower(ireland)
```

Take out all numbers


```r
ireland <- str_replace_all(ireland,"[0-9]+","")
```

Remove `\n` newlines


```r
ireland <- str_replace_all(ireland, "[\r\n]", "")  # same as # ireland <- gsub("\r?\n|\r", "", ireland)
```

***
Much of the wordclouding inspiration was adapted 
from [this blog](https://quantmacro.wordpress.com/2016/04/30/web-scraping-for-text-mining-in-r/)
Create a corpus


```r
i.corp <- Corpus(VectorSource(ireland))
```

Wrap strings into paragraphs so we can see what we have better  
Not assigning this to i.corp object, i.e., not i.corp <- str_wrap(i.corp[[1]])  
Note: this is the base::strwrap not stringr::str_wrap


```r
strwrap(i.corp[[1]]) # [[1]] because this corpus contains one document
```

Take out punctuation and white space


```r
i.corp <- tm_map(i.corp, removePunctuation)
i.corp <- tm_map(i.corp, stripWhitespace)
```

Make corpus a plain text doc


```r
i.corp <- tm_map(i.corp, PlainTextDocument)
```

View what we've got


```r
strwrap(i.corp[[1]])
```

***
Make a document feature or document term matrix


```r
i.dfm <- tm::TermDocumentMatrix(i.corp)
```

Check out rows 1000 to 1010


```r
inspect(i.dfm[1000:1010, ] )
```

```
## <<TermDocumentMatrix (terms: 11, documents: 1)>>
## Non-/sparse entries: 11/0
## Sparsity           : 0%
## Maximal term length: 9
## Weighting          : term frequency (tf)
## 
##            Docs
## Terms       character(0)
##   explorer             1
##   export               1
##   exporter             1
##   exposed              1
##   expressed            1
##   extant               2
##   extend               1
##   extended             2
##   extending            2
##   extensive            1
##   extent               1
```

Make a wordcloud  
First convert dfm to matrix


```r
i.matrix <- as.matrix(i.dfm)
```

Label the frequency column


```r
colnames(i.matrix) <- 'frequency'
```

Sort terms by frequency


```r
i.sorted <- sort(rowSums(i.matrix), decreasing = TRUE)
```

Ten most frequent words


```r
i.sorted[1:10]
```

```
##     the     and ireland   irish     was    with     for    from     are 
##    1088     459     252     125     118     104      79      77      73 
##    that 
##      68
```

Make into a data.frame


```r
i.dat <- data.frame(word = names(i.sorted), freq = i.sorted)
```

Remove "the" and "and"


```r
i.dat.trim <- i.dat %>% 
  filter(
    !(word %in% c("the", "and")))
```

```r
head(i.dat.trim)
```

```
##      word freq
## 1 ireland  252
## 2   irish  125
## 3     was  118
## 4    with  104
## 5     for   79
## 6    from   77
```

Set RColorBrewer palate


```r
palette <- brewer.pal(12,"Paired")
```

Set background to black


```r
par(bg = 'black')
```

Make the wordcloud


```r
wordcloud(i.dat.trim$word, i.dat.trim$freq, random.order = FALSE,
          max.word = 200, color = palette,
          vfont = c("serif", "plain"))
```

![](scrape_files/figure-html/ireland_wordcloud-1.png)<!-- -->


---
title: "scrape.R"
author: "amanda"
date: "Thu Sep  8 17:37:18 2016"
---
