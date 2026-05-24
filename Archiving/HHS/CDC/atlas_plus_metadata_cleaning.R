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

base_dir <- "F:/final" # Or Wherever your files are stored.

# Listing files with metadata

file_location <- str_c(base_dir, "/metadata/Initial Run Metadata")

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

scraping_tibble <- readRDS(str_c(base_dir, "/metadata/scraping_list.RDS"))

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
         run == 4862) # This run threw an error but it was actually just too large

large_tables <- large_tables |> 
  bind_rows(error_list)

if (file.exists(str_c(base_dir, "/metadata/large_runs_metadata.RDS")) == FALSE) {
  
  large_tables |> 
    saveRDS(str_c(base_dir, "/metadata/large_runs_metadata.RDS"))
  
} else {
  
  large_tables <- readRDS(str_c(base_dir, "/metadata/large_runs_metadata.RDS"))
  
}

# Listing all of the large runs

file_location <- str_c(base_dir, "/metadata/Large Run Metadata")

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

large_list <- iteration_list |> 
  bind_rows()

# Binding large runs with the full list.

full_metadata_list <- full_list |> 
  left_join(large_list, by = "run") |> 
  mutate(
    location = coalesce(location.y, location.x),
    time_at_download = coalesce(time_at_download.y, time_at_download.x)
) |> 
  select(-ends_with(".x"), -ends_with(".y"))

if (file.exists(str_c(base_dir, "/metadata/final_metadata.RDS")) == FALSE) {
  
  full_metadata_list |> 
    saveRDS(str_c(base_dir, "/metadata/final_metadata.RDS"))
  
} else {
  
  full_metadata_list <- readRDS(str_c(base_dir, "/metadata/final_metadata.RDS"))
  
}

to_move <- full_metadata_list |> 
  filter(str_detect(location, "F:/temp/") == TRUE) |> 
  mutate(indicator = str_extract(str_replace(location, "F:/temp/", ""), "^[^/]+"),
         file = map(location, ~ list.files(.x)),
         old_location = str_c(location, "/", file),
         new_location = str_c("F:/final/", indicator, "/", file)) |> 
  distinct(new_location, .keep_all = TRUE)

# Some of the files in the Chlamydia set must have gotten downloaded twice.
# I have no reason to suspect that files were skipped over or corrupted.

folders <- to_move |> 
  select(indicator) |> 
  distinct() |> 
  unlist()

map(folders, ~ directory_creator("F:/final/datafiles", .x))

map2(to_move$old_location, to_move$new_location, ~ file.copy(.x, .y))

# Generating final metadata list

skipped_runs <- full_metadata_list |> 
  filter(str_detect(location, base_dir) == FALSE)

to_join <- to_move |> 
  select(run, time_at_download, new_location) |> 
  rename("location" = new_location) |> 
  bind_rows(skipped_runs)
  

final_metadata <- scraping_tibble |> 
  mutate(indicator = str_replace_all(indicator, "\\*|\\^|\\‡", ""),
         indicator = str_trim(indicator)) |> 
  left_join(to_join, by = join_by(run)) |> 
  relocate(run, time_at_download, location, .before = indicator) |> 
  mutate(location = case_when(str_detect(location, base_dir) == TRUE ~ str_replace(location, str_c(base_dir, "/"), "~"),
                              TRUE ~ location))

final_metadata |> 
  writexl::write_xlsx(path = str_c(base_dir, "/file_directory.xlsx"))
