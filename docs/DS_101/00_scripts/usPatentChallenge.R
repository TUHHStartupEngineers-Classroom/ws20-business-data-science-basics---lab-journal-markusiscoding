# libraries ----
library(tidyverse)
library(vroom)
library(data.table)
library(lubridate)
# select columns and define type ----
assignee_col_types <- list(
  id = col_character(), # connects 'patent_assignee' and 'assignee'
  type = col_double(), # Q1 & Q2: only type 2(US-Comp), Q3: type 2 and type 3(Foreign Comp) 
  name_first = col_skip(),
  name_last = col_skip(),
  organization = col_character() # name of comp
)

patent_col_types <- list(
  id = col_character(), # connects 'patent_assignee' and 'patent' and 'uspc'
  type = col_skip(),
  number = col_skip(),
  country = col_skip(), 
  date = col_date("%Y-%m-%d"), # in year 2019
  abstract = col_skip(),
  title = col_skip(),
  kind = col_skip(),
  num_claims = col_skip(),
  filename = col_skip(),
  withdrawn = col_skip()
)
patent_assignee_col_types <- list(
  patent_id = col_character(), # connects 'patent_assignee' and 'patent'
  assignee_id = col_character(), # connects 'patent_assignee' and 'assignee'
  location_id = col_skip()
)
uspc_col_types <- list(
  uuid = col_skip(),
  patent_id = col_character(), # connects 'patent_assignee' and 'uspc'
  mainclass_id = col_character(), # to select USPTO tech main class
  subclass_id = col_skip(),
  sequence = col_skip()
  
)
# import data ----
patent_tbl <- vroom(
  file       = "./DS_101/00_data/patent/patent.tsv", 
  delim      = "\t", 
  col_types  = patent_col_types,
  na         = c("", "NA", "NULL")
)
uspc_tbl <- vroom(
  file       = "./DS_101/00_data/uspc/uspc.tsv", 
  delim      = "\t", 
  col_types  = uspc_col_types,
  na         = c("", "NA", "NULL")
)
patent_assignee_tbl <- vroom(
  file       = "./DS_101/00_data/patent_assignee/patent_assignee.tsv", 
  delim      = "\t", 
  col_types  = patent_assignee_col_types,
  na         = c("", "NA", "NULL")
)
assignee_tbl <- vroom(
  file       = "./DS_101/00_data/assignee/assignee.tsv", 
  delim      = "\t", 
  col_types  = assignee_col_types,
  na         = c("", "NA", "NULL")
)

patent_tbl <- patent_tbl %>%
  setDT()
patent_assignee_tbl <- patent_assignee_tbl %>%
  setDT()
assignee_tbl <- assignee_tbl %>%
  setDT()
uspc_tbl <- uspc_tbl %>%
  setDT()

# rename columns
setnames(patent_tbl, "id", "patent_id")
setnames(assignee_tbl, "id", "assignee_id")

### Patent Dominance: 
### What US company / corporation has the most patents? 
### List the 10 US companies with the most assigned/granted patents.

# combine assignee_tbl with patent_assignee_tbl, 
combined_data_1 <- merge(x = assignee_tbl, y = patent_assignee_tbl, 
                       by    = "assignee_id", 
                       all.x = TRUE, 
                       all.y = FALSE)

setkey(combined_data_1, "assignee_id")
key(combined_data_1)
setorderv(combined_data_1, c("assignee_id", "type", "organization", "patent_id"))

# filter by type to get US companies only, group by organization
filtered_data_1 <- combined_data_1[type == 2, .(numberOfPatents = .N), by = organization]%>%
  filter(!is.na(organization))

# reorder in descending number of entries and plot top 10
filtered_data_1[order(-numberOfPatents)] %>%
  head(n=10)


### Recent patent acitivity: 
### What US company had the most patents granted in 2019? 
### List the top 10 companies with the most new granted patents for 2019.

combined_data_2 <- merge(x = combined_data_1, y = patent_tbl,
                         by = "patent_id",
                         all.x = TRUE,
                         all.y = TRUE)
# add year column
combined_data_2[, year := year(date)]
# filter for patents granted in 2019, group by organization
filtered_data_2 <- combined_data_2[year == "2019", .(numOfGrantedPatents = .N), by = organization] %>%
  filter(!is.na(date), !is.na(organization), !is.na(patent_id), type == 2) 

filtered_data_2[order(-numOfGrantedPatents)] %>%
  head(n=10)
  

### Innovation in Tech: 
### What is the most innovative tech sector?
### For the top 10 companies (worldwide) with the most patents, what are the top 5 USPTO tech main classes?

combined_data_3 <- merge(x = combined_data_2, y = uspc_tbl,
                         by = "patent_id",
                         all.x = TRUE,
                         all.y = FALSE) %>%
  filter(!is.na(mainclass_id), type == 2 | type ==3)
grouped_data_3 <- combined_data_3[, .(numOfPatents = .N), by = organization] 
top_worldwide <- grouped_data_3[order(-numOfPatents)]
top_10_worldwide <- top_worldwide[1:10]
final_table <- merge(x = combined_data_3, y = top_10_worldwide,
      by = "organization",
      all.x = FALSE,
      all.y = TRUE) 
grouped_final_table = final_table[!is.na(organization), .(mainclass_count = .N), by = mainclass_id]

grouped_final_table[order(-mainclass_count)] %>%
  head(n=5)
