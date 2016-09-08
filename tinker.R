## TODO
#---------
# update gitignore to ignore this file
# add #+ around chunks
# make sure that graphs are visible in .md doc on github
# put tables in kable()




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



# Write function that bundles/generalizes this


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




# How we had it pre-em_tab
#' 
#' #' Select the table we want
#' #+
#' gdp <- wiki_table[[2]]
#' 
#' #' Rename columns with Euro symbols
#' gdp <- gdp %>% 
#'   rename(
#'     GDP = `GDP €`,
#'     GDP_percap = `GDP per person €`
#'   )
#' gdp
#' 
#' #' Take out last row with totals
#' gdp <- gdp[1:(nrow(gdp) - 1), ]
#' 
#' #' Check out table structure
#' str(gdp)
#' 
#' #' Make tibble
#' gdp <- as_tibble(gdp)
#' 
#' #' Take out Euro symbols in rows and "bn" for billion in GDP column
#' gdp$GDP <- str_replace_all(gdp$GDP, "€", "")
#' gdp$GDP <- str_replace_all(gdp$GDP, "bn", "")
#' gdp$GDP_percap <- str_replace_all(gdp$GDP_percap, "€", "")
#' 
#' 
#' # Copy our dataframe
#' gdp2 <- gdp
#' 
#' #' Replace `m` (in Population) with scientific notation
#' gdp2$Population <- gdp$Population %>% 
#'   gsub(" m", "e+06", .) 
#' gdp2$Population
#' 
#' gdp2$Population[1] <- as.numeric(gdp2$Population[1])
#' format(gdp2$Population[1], scientific = TRUE)
#' 
#' 
#' #+ eval=FALSE
#' gdp2
#' 
#' #' Set variable data types
#' gdp3 <- gdp2
#' 
#' #' Turn Area, City, and Country into factors
#' gdp3 <- gdp3 %>% 
#'   rsalad::dfFactorize(
#'     ignore = c("Population", "GDP", "GDP_percap")
#'   )
#' 
#' # gdp3$Area <- factor(gdp$Area)
#' gdp3$GDP_percap <- parse_number(gdp$GDP_percap)
#' gdp3$GDP <- parse_number(gdp$GDP)
#' gdp3$Population <- parse_number(gdp2$Population)
#' # gdp3$City <- factor(gdp$City)
#' # gdp3$Country <- factor(gdp$Country)
#' 
#' str(gdp3)
#' 
#' gdp3
#' 
#' # Multiply GDP by 1 bil
#' gdp4 <- gdp3
#' gdp4$GDP <- (gdp3$GDP)*(1e+09)
#' 
#' #+ eval=FALSE
#' gdp4









# gdp5[, to.numerize] <- gdp5 %>% 
#   data.frame(apply(.[, to.numerize], 2, parse_number)) %>% 
#   as_tibble(.)
# gdp5

# bh[, want.as.numeric] <- data.frame(apply(bh[, want.as.numeric], 2, as.numeric))


#' Factorize
# gdp5 <- gdp5 %>% 
#   select(
#     "Area", "Country", "City"
#   ) %>% 
#   data.frame(apply(., 2, factor))

