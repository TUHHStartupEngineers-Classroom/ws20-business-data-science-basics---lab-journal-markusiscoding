library(tidyverse)
library(lubridate)
library(scales)
library(maps)
require(graphics)



# Data Manipulation
covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv") %>%
  as_tibble()

# calculate mortality rate for each country
covid_data_manipulated <- covid_data_tbl %>% 
  select(cases, deaths, countriesAndTerritories ) %>%
  group_by(countriesAndTerritories) %>%
  summarise(total_deaths = sum(deaths),total_cases = sum(cases)) %>%
  ungroup() %>%
  mutate(mortality_rate = total_deaths/total_cases) %>% 
  mutate(across(countriesAndTerritories, str_replace_all, "_", " ")) %>%
  rename(region = countriesAndTerritories) %>%
  mutate(region = case_when(
    
    region == "United Kingdom" ~ "UK",
    region == "United States of America" ~ "USA",
    region == "Czechia" ~ "Czech Republic",
    TRUE ~ region
    
  ))
  

# Data Visualization  
world <- map_data("world")

mortality_map <- left_join(covid_data_manipulated, world, by = "region")


ggplot(mortality_map, aes(map_id = region, fill = mortality_rate))+
  geom_map(map = mortality_map,  color = "white")+
  expand_limits(x = mortality_map$long, y = mortality_map$lat)+
  theme_void()+
  scale_fill_continuous(low="thistle3", high="darkred", 
                         guide="colorbar",na.value="grey")+
  # remove axis labels
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  labs(
    title = "Confirmed COVID-19 deaths relative to the size of the population",
    subtitle = "More than 1.2 Million confirmed COVID-19 deaths worldwide",
    caption = "",
    x = "",
    y = "",
    color = "Mortality Rate" # Legend text
  )
