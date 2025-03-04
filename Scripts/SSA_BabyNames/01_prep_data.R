# AUTHOR:   G. Sarfaty
# PURPOSE:  Munge baby names data to prep for analysis 
# LICENSE:  MIT
# DATE:     2025-03-04
# UPDATED: 


# LOCALS & SETUP ===============================================================

#Libraries

library(tidyverse)
library(janitor)


# LOAD DATA ==================================================================== 

# read in yearly files, extract year from file name, bind all rows

data_in<-"Data_public/ssa_babynames"

col_names<- c("name", "sex", "value")

# Read and combine all files, extracting the year from the filename and storing as column
df_data <- list.files(data_in, full.names = TRUE, pattern = "\\.txt$") %>%
  set_names() %>%
  map_dfr(~ read_csv(.x, col_names = col_names, col_types = cols(.default = "c"), skip = 0) %>%
            mutate(year = as.integer(str_extract(basename(.x), "\\d{4}"))))  



# EXPORT =======================================================================

write_csv(df_data, file = "Dataout/2025-03-04_SSA_BabyNames_1880to2023.csv")