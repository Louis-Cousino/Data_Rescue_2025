#
#
#
#
#
#
# Agency: Department of Health and Human Services, Centers for Disease Control and Prevention
# 
# Source Website: https://gis.cdc.gov/grasp/nchhstpatlas/tables.html
# 
# Description: File for cleaning the metadata generated from the AtlasPlus scraping.

# Loading Libraries

library(tidyverse)

# Loading in metadata

# Listing files with metadata

file_location <- "F:/temp3/00_Initial Run Metadata" # Or Wherever your files are stored.

files_df <- list.files(file_location, full.names = T) |> 
  as.data.frame()

colnames(files_df) <- "file_name"

files_df <- files_df |> 
  filter(str_detect(file_name, ".RDS"))

metadata_list <- list()
iteration_list <- list()

for (i in 1:nrow(files_df)) {
  
  metadata_list[[i]] <- readRDS(files_df$file_name[i])
  
  iteration_list[[i]] <- metadata_list[[i]][[1]]
  
}

full_list <- iteration_list |> 
  bind_rows()

large_tables <- full_list |> 
  filter(str_detect(location, "Error: Table was too large")) |> 
  left_join(scraping_tibble, by = "run")

# Need a plan to scrape large tables and scrape each individual year.

# Finding the runs that got missed by having two (2) commas in the numbers (more than 1 million rows)

error_list <- full_list |> 
  filter(str_detect(location, "Error"),
         !str_detect(location, "Table was too large"))

# Binding scraping tibble with metadata list

scraping_tibble <- scraping_tibble |> 
  mutate(run = 1:nrow(scraping_tibble))

error_list <- error_list |> 
  left_join(scraping_tibble, by = "run") |> 
  filter(!str_detect(location, "Error: No data found matching query criteria."),
         run == 4862)

large_tables <- large_tables |> 
  bind_rows(error_list)

large_tables2 <- large_tables |> 
  filter(run > 2882)