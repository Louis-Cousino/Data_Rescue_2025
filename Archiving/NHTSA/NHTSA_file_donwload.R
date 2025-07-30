# Author: Louis Cousino
# Date: 07-27-2025
# Description: A file for downloading files hosted at https://www.nhtsa.gov/file-downloads?p=nhtsa/downloads/
# The method for gathering download links can be found in []

# Loading libraries
library(tidyverse)

# Loading helper function
# source(here::here("helper_functions", "directory_creator.R"))

directory_creator <- function(base_directory, ...) {
  
  path <- list(...)
  
  # Removing entries that have at least one dot (indicating a file)
  
  path <- path[!stringr::str_detect(path, "\\.")]
  
  current_path <- base_directory
  
  for (i in 1:length(path)) {
    
    current_path <- stringr::str_c(current_path, "/", path[[i]])
    
    # Removing excess slashes
    
    current_path <- stringr::str_replace_all(current_path, "/+", "/")
    
    if (dir.exists(current_path) == FALSE) {
      
      dir.create(current_path)
      
    }
    
  }
  
  return(current_path)
  
  
}



# Reading links in

# If here::here() does not work for you, enter your desired path below. The same applies for the base_path.
# In the case below, please make sure it ends with "NHTSA_full_links.RDS", as this is where the download links are stored.

base_path <- here::here("Archiving", "NHTSA", "files")

full_links <- readRDS(here::here("Archiving", "NHTSA", "NHTSA_full_links.RDS"))

# Removing duplicates and folders

full_links <- full_links |> 
  filter(size != "") |> 
  distinct()

# Parsing download links

links_download <- full_links |> 
  mutate(path = str_extract(parent_link, "(?<=https://static.nhtsa.gov/nhtsa/downloads/).*"),,
         path = str_replace(path, "/[^/]*$", ""),
         path = str_replace_all(path, " ", "_"),
         steps = str_split(path, "/")) |> 
  unnest_wider(steps, names_sep = "_step") |> 
  filter(index == 1,
         iteration == 20)

col_names <- links_download |> 
  select(starts_with("steps_")) |> 
  names()

# Setting longer timeout

options(timeout = 10000)

# Downloading files

for (i in 1:nrow(links_download)) {
  
  # Forming the list to be passed to the directory creator
  # This asks R to for a string, which corresponds to a column name, and then evaluate the expression to get the actual path.
  
  path_list <- map(col_names, ~ eval(rlang::parse_expr(str_c("links_download$", .x, "[", i, "]")))) 
  
  
  path_list <- base_path |> 
    append(path_list)
  # Creating download locations
  
  download_path <- do.call(directory_creator, path_list)

  # Downloading files
  
  file_name <- str_replace_all(links_download$parent_text[i], " ", "_")
  
  file_destination <- str_c(download_path, "/", file_name)
  
  if (file.exists(file_destination) == FALSE) {
    
    download.file(URLencode(links_download$parent_link[i]), file_destination, mode = "wb")
    
  }
  
  
}
