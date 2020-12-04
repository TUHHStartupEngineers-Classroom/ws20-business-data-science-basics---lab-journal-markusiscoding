# WEBSCRAPING ----
# Get the prices and model names for at least one category

# LIBRARIES ----

library(tidyverse) # Main Package - Loads dplyr, purrr, etc.
library(rvest)     # HTML Hacking & Web Scraping
library(xopen)     # Quickly opening URLs
library(jsonlite)  # converts JSON files to R objects
library(glue)      # concatenate strings
library(stringi)   # character string/text processing

# get the url for each bike in the product category "gravel"

gravel_groups_url_tbl <- read_html("https://www.rosebikes.de/fahrräder/gravel") %>%
  html_nodes(css = ".catalog-category-bikes__button") %>%
  html_attr("href") %>%
  enframe(name = "position", value = "subdirectory") %>%
  mutate(url = glue("https://www.rosebikes.de{subdirectory}")
  ) 

prices_tbl_1 <- read_html(gravel_groups_url_tbl$url[1]) %>%    
  html_nodes(css = (".catalog-category-model__price-current-value")) %>%
  html_text() %>%
  as_tibble()
model_names_tbl_1 <- read_html(gravel_groups_url_tbl$url[1]) %>%
  html_nodes(css = (".catalog-category-model__title")) %>%
  html_text() %>%
  as_tibble()

prices_tbl_2 <- read_html(gravel_groups_url_tbl$url[2]) %>%    
  html_nodes(css = (".catalog-category-model__price-current-value")) %>%
  html_text() %>%
  as_tibble()
model_names_tbl_2 <- read_html(gravel_groups_url_tbl$url[2]) %>%
  html_nodes(css = (".catalog-category-model__title")) %>%
  html_text() %>%
  as_tibble()

prices_tbl_3 <- read_html(gravel_groups_url_tbl$url[3]) %>%    
  html_nodes(css = (".catalog-category-model__price-current-value")) %>%
  html_text() %>%
  as_tibble()
model_names_tbl_3 <- read_html(gravel_groups_url_tbl$url[3]) %>%
  html_nodes(css = (".catalog-category-model__title")) %>%
  html_text() %>%
  as_tibble()

prices_tbl <- prices_tbl_1 %>%
  full_join(prices_tbl_2) %>%
  full_join(prices_tbl_3)
model_names_tbl <- model_names_tbl_1 %>%
  full_join(model_names_tbl_2) %>%
  full_join(model_names_tbl_3)

bikes_tbl <- bind_cols(model_names_tbl,prices_tbl) %>%
  rename(model = value...1, price = value...2)

head(bikes_tbl, n=10)
