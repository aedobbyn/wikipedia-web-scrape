# Ireland Wikipedia Web Scrape

A web scraping project using the [Wikipedia page on Ireland](https://en.wikipedia.org/wiki/Ireland). Report in `scrape.md`.

## Workflow
* Scrape certain parts of the page using CSS selectors
  * Scrape a table and use it to create some visualizations of GDP data 
  * Scrape all of the text on the page and create word frequency wordclouds to see what words are mentioned most often on the page  

## Files
* `scrape.R` contains the code for scraping and analyzing
* `scrape.md` provides the same content compiled to markdown using `knitr::spin`