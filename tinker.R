#' Thoughts on better ways to replace the "1.3 m" population
# gdp2$population <- if("e" %in% gdp2$population) {as.numeric(gdp2$population)}

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



# How we have it
# Write function that bundles/generalizes this


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

#' Take out last row with totals
gdp <- gdp[1:(nrow(gdp) - 1), ]

#' Check out table structure
str(gdp)

#' Make tibble
gdp <- as_tibble(gdp)

#' Take out Euro symbols in rows and "bn" for billion in GDP column
gdp$GDP <- str_replace_all(gdp$GDP, "€", "")
gdp$GDP <- str_replace_all(gdp$GDP, "bn", "")
gdp$GDP_percap <- str_replace_all(gdp$GDP_percap, "€", "")


# Copy our dataframe
gdp2 <- gdp

#' Replace `m` (in Population) with scientific notation
gdp2$Population <- gdp$Population %>% 
  gsub(" m", "e+06", .) 
gdp2$Population

gdp2$Population[1] <- as.numeric(gdp2$Population[1])
format(gdp2$Population[1], scientific = TRUE)


#+ eval=FALSE
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

gdp3

# Multiply GDP by 1 bil
gdp4 <- gdp3
gdp4$GDP <- (gdp3$GDP)*(1e+09)

#+ eval=FALSE
gdp4


#' Graph population and GDP per capita, coloring points by country
gdp4 %>% 
  ggvis(~Population, ~GDP_percap, fill = ~Country) %>% 
  layer_points()


#' For countries in the ROI (Republic of Ireland and also our region of interest, lol), 
#' plot GDP vs. per capita GDP and fill by Area
gdp4 %>%
  filter(Country == "ROI") %>%
  droplevels() %>%    # drop unused Areas (e.g., Greater Belfast) from legend
  ggvis(~GDP, ~GDP_percap, fill=~Area) %>%
  scale_numeric("x") %>%   # reorder levels by GDP
  layer_points()



