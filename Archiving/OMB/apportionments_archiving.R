#
#
#
#
#
#
# Agency: Office of Management and Budget
# 
# Source Website: https://apportionment-public.max.gov/
# 
# Description: Apportionment documents released by OMB.

# Loading Libraries

library(tidyverse)
library(rvest)
library(httr2)

# Loading helper functions

source(here::here("helper_functions", "directory_creator.R"))
source(here::here("helper_functions", "download_check.R"))

# Defining download directory

download_dir <- "F:/appr_temp"

# Loading page into R

page_html <- read_html("https://apportionment-public.max.gov/")

# xml2::write_html(page_html, here::here("Archiving", "OMB", "page_html.html"))

# page_html <- xml2::read_html(here::here("Archiving", "OMB", "page_html.html"))

link_elements <- page_html |> 
  html_elements("a")

# This was done in parallel with a second R session extracting information from the first half.

timer <- timeR::createTimer()

timer$start("2nd Half Brute Force")

links <- link_elements[((55752/2)+1):55752] |> 
  map_df(~{
    
    tibble(
      link = html_attr(.x, "href") |> 
        url_absolute("https://apportionment-public.max.gov/"),
      file_type = html_element(.x, xpath = "../..//text()[not(ancestor::a)]") |>
        html_text(trim = TRUE),
      agency = html_element(.x, xpath = "../../..") |>
        html_text(trim = TRUE) |> 
        str_extract(str_c(".*(?=",file_type, ")")),
      year = html_element(.x, xpath = "../../../..") |>
        html_text(trim = TRUE)|>  
        str_extract("^Fiscal Year \\d+")
    )
    
  })

saveRDS(links,here::here("Archiving", "OMB", "2nd_half_links.RDS"))


run_time <- timer$stop("2nd Half Brute Force")

run_time <- run_time$eventTable

saveRDS(run_time, here::here("Archiving", "OMB", "run_time_2.RDS"))

###

first_links <- readRDS(here::here("Archiving", "OMB", "1st_half_links.RDS"))

second_links <- readRDS(here::here("Archiving", "OMB", "2nd_half_links.RDS"))

combined_links <- first_links |> 
  bind_rows(second_links) |> 
  mutate(agency = case_when(is.na(year) == TRUE ~ "General",
                            TRUE ~ agency),
         file_name = str_extract(link, "(?<=.)[^/]*$"),
         file_type = str_extract(link, "(?<=\\.)[^.]*$"),
         file_path = case_when(is.na(year) == FALSE ~ str_c(download_dir, "/", year, "/", agency, "/", file_type, "/", file_name),
                               TRUE ~ str_c(download_dir, "/", agency, "/", file_type, "/", file_name)),
         file_path = URLdecode(file_path),
         file_name = URLdecode(file_name)) |> 
  filter(file_type %in% c("xlsx", "json", "pdf")) |>
  slice_tail(n = -1)  # Removing the first instance because it leads to a broken link.

directory_locations <- combined_links |> 
  distinct(year, agency, file_type)

for (i in 1:nrow(directory_locations)) {
  directory_creator(download_dir, directory_locations$year[i], directory_locations$agency[i], directory_locations$file_type[i])
}

download_function <- function (combined_frame, from, to) {
  
  on.exit({
    
    return_list[[2]] <- index
    
    return(return_list)
    
  }, add = TRUE)
  
  return_list <- list(list(), list())
  
  index <- from - 1
  
  for (i in from:to){
    
    index <- index + 1
    
    download.file(url = combined_frame$link[i], destfile = combined_frame$file_path[i], mode = "wb")
    
    return_list[[1]][[i - (from - 1)]] <- tibble(
      
      download_link = combined_frame$link[i],
      file = combined_frame$file_name[i],
      year = combined_frame$year[i],
      agency = combined_frame$agency[i],
      file_path = str_replace(combined_frame$file_path[i], download_dir, "."),
      time_of_download = Sys.time()
      
    )
    
  }
  
}

# Downloading links

last_number <- list(list(1))
final_list <- list()

# Repeat loop continues downloading files even in the event of an error (such as a timeout)

repeat ({
  
  returned_list <- download_function(combined_links, last_number[[1]][[length(last_number[[1]])]], nrow(combined_links))
  
  # Saving the returned_list data
  final_list[[length(final_list) + 1]] <- returned_list[[1]]
  
  # Placing returned data in its proper place
  last_number[[1]][[length(last_number[[1]])+1]] <- as.numeric(returned_list[[2]])
  
  if (last_number[[1]][[length(last_number[[1]])]] == nrow(combined_links)) { # Checking if the previous run is the last one.
    
    saveRDS(final_list, here::here("Archiving", "OMB", "returned_list.RDS"))
    break
    
  } else if (length(last_number[[1]])  >= 3 && last_number[[1]][[length(last_number[[1]])]] == last_number[[1]][[length(last_number[[1]])-1]] && last_number[[1]][[length(last_number[[1]])-1]] == last_number[[1]][[length(last_number[[1]])-2]]) {
    
    last_number[[2]] <- str_c("Please check run #", last_number[[1]][[length(last_number[[1]])]], " as an error occured three times in a row.")
    
    break
  }
  
})

final_list <- readRDS(here::here("Archiving", "OMB", "returned_list.RDS"))

final_list <- final_list |> 
  bind_rows()

# Checking for missing files.

file_checking <- map(combined_links$file_path, file.exists) |> 
  tibble() |> 
  rename_with(~ "file_exists") |> 
  mutate(index = row_number())

check_links <- final_list |> 
  mutate(index = row_number()) |> 
  left_join(file_checking, by = join_by(index)) |> 
  relocate(index, .before = download_link)

missing_files <- check_links |> 
  filter(file_exists == FALSE)

# No missing files!

# Separating metadata and writing to excel files.

file_split <- function(year_vector) {
  
  year <- str_c("Fiscal Year ", as.character(year_vector))
  
  specific_year <- check_links |> 
    mutate(file_exists = as.character(file_exists)) |> 
    filter(year == year)
  
  writexl::write_xlsx(specific_year, str_c(download_dir, "/metadata_", year, ".xlsx"))
  
}

map(seq(2022,2026), file_split)
