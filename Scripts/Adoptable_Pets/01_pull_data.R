# AUTHOR:   G. Sarfaty
# PURPOSE:  Pull adoptable pets data from API
# LICENSE:  MIT
# DATE:     2025-02-26
# UPDATED: 


# LOCALS & SETUP ===============================================================

#Libraries

library(tidyverse)
library(httr)
library(janitor)
library(keyring)
library(jsonlite)

# key_set("datadotgov")

# LOAD DATA ==================================================================== 

# plug in your API key
datadotgov_api <- key_get("datadotgov")

# the url path to the service
URL <- "https://data.montgomerycountymd.gov/api/views/e54u-qx42/rows.json?accessType=DOWNLOAD"

# GET(): download all available data
get.data <- GET(URL, query=list(api_key=datadotgov_api))


# content(): extract the content from the query
data<- content(get.data, as="text")

data_parsed <- fromJSON(data)



# PULL AND MUNGE ===============================================================

column_names <- data_parsed %>% 
  pluck("meta", "view", "columns") %>%
  as_tibble() %>% 
  filter(!dataTypeName=="meta_data") %>% 
  select(id,name)


data <- data_parsed %>%
  pluck("data") %>%
  map(~ discard(.x, is.null)) %>%  # Remove NULL values
  # map(~ flatten(.x)) %>%  # Flatten any nested structures
  map_dfr(~ as_tibble(.x, .name_repair = "unique")) %>%   # Convert to tibble
  distinct(...1,.keep_all = TRUE) %>%  #deduplicate
  select(7:23)
  

df_data<-data %>% 
  set_names(column_names %>%  pull(name))

# EXPLORE ======================================================================

summary(df_data)

table(df_data$`Animal Type`)

table(df_data$`Intake Type`)


# EXPORT =======================================================================

write_csv(df_data, file = "Dataout/2025-03-02_AdoptablePets.csv")