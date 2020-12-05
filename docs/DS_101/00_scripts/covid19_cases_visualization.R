library(tidyverse)
library(lubridate)
library(scales)

# Data Manipulation
covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")


covid_data_manipulated <- covid_data_tbl %>%
  select(dateRep, cases, countriesAndTerritories) %>%
  rename(countries = countriesAndTerritories) %>%
  filter(countries == "Germany" | countries == "Spain" | countries == "France" | 
           countries == "United_Kingdom" | countries == "United_States_of_America") %>%
  pivot_wider(names_from  = countries, 
            values_from =  cases) %>%
  set_names("date", "GermanyNew", "SpainNew" , "FranceNew",
            "United_KingdomNew", "United_States_of_AmericaNew") %>%
  mutate(date = dmy(date)) %>%
  arrange(date) %>%
  mutate(Germany = cumsum(GermanyNew), Spain = cumsum(SpainNew), 
         France = cumsum(FranceNew), United_Kingdom = cumsum(United_KingdomNew), 
         United_States_of_America = cumsum(United_States_of_AmericaNew)) %>%
  select(date, Germany, Spain, France, United_Kingdom, United_States_of_America) %>%
  pivot_longer(cols = c(2:6), names_to = "country", values_to = "cumulative_cases") %>%
  mutate(Month = month(date))

colMax <- function(data) sapply(data, max, na.rm = TRUE)

max_data <-colMax(covid_data_manipulated) 

  
# Data Visualization
min <- as.Date("2020-1-1")
max <- NA


covid_data_manipulated %>%
  ggplot((aes(x = date, y = cumulative_cases, color = country))) +
    geom_line(size = 1)+
    geom_smooth(method = "loess", span = 0.1)+
    geom_label(label =  "14.371.633",
               vjust = 1, 
               size  = 3,
               fill  = "#1f78b4",
               color = "white",
               fontface = "bold",
               data = covid_data_manipulated %>%
                 filter(date == "2020-12-05" & country == "United_States_of_America")
               ) +
    theme_dark()+
    scale_x_date(breaks = date_breaks("months"),
               labels = date_format("%B"),limits = c(min, max))+
    theme(axis.text.x = element_text(angle=45))+
    scale_y_continuous(labels = label_number(accuracy = NULL, scale = 1e-6,
                        prefix = "", suffix = "M",
                        big.mark = "", decimal.mark = "."))+
    labs(
      title = "COVID-19 confirmed cases worldwide",
      subtitle = "As of 12/05/2020",
      caption = "",
      x = "Year 2020",
      y = "Cumulative Cases",
      color = "Country" # Legend text
    )
    
  

  
         
