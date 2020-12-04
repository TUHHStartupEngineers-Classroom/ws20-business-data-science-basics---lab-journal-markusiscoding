# add library
library(tidyverse)
library(httr)
library(jsonlite)

# finds the most popular cat facts         
resp <- GET("https://cat-fact.herokuapp.com/facts")

cat_facts <- resp %>% 
  .$content %>% 
  rawToChar() %>% 
  fromJSON()
cat_facts_tbl <-cat_facts[["all"]] %>%
  as_tibble()

cat_facts_tbl %>% head(n=5)
