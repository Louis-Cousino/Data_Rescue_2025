#
#
#
#
#
#
# Agency: Department of Education, National Center for Education Statistics
# 
# Source Website: https://nces.ed.gov/datalab/table/library
# 
# Description: Estimates and standard errors from various NCES publications

# Loading Libraries

library(tidyverse)
library(polite)
library(rvest)
# library(httr)

# Loading helper functions

# source(here::here("helper_functions", "directory_creator.R"))
# source(here::here("helper_functions", "download_check.R"))
# source(here::here("helper_functions", "explicit_click.R"))
# source(here::here("helper_functions", "explicit_wait.R"))

table_links <- list()

for (i in 1:length(1980:2024)) {
  
  session <- bow(str_c("https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx?year=", as.character(i+1979)))
  
  html_page <- scrape(session)
  
  return <- html_page |> 
    html_elements("table#contentPlaceHolder_tblResult.idc_gridview") |>
    html_elements("a") |> 
    html_attr("href") |> 
    url_absolute("https://nces.ed.gov/ipeds/datacenter/") |> 
    as.data.frame()
  
  colnames(return) <- "baseURL"
  
  return <- return |> 
    mutate(year = i+1979)
  
  table_links[[i]] <- return
  
  Sys.sleep(5)
  
}

test <- table_links |> 
  bind_rows()

# Scraping target page

session <- bow("https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx?year=-1&sid=7b86b933-b5f1-4816-b3c9-993fc96cf754&rtid=7")

html_page <- scrape(session)

# Extracting data and URLs

IPEDS_table_list_links <- html_page |> 
  html_elements("table#contentPlaceHolder_tblResult.idc_gridview") |>
  html_elements("a") |> 
  html_attr("href") |> 
  url_absolute("https://nces.ed.gov/ipeds/datacenter/") |> 
  as.data.frame()

colnames(IPEDS_table_list_links) <- "baseURL"

# Extracting year

IPEDS_table_list_links <- IPEDS_table_list_links |> 
  mutate(four_dgt_year = str_extract(baseURL, "\\d{2,4}"), # Returns a group of 2-4 digits from the URL
         two_dgt_year = str_extract(four_dgt_year, "\\d{2}(?=\\D*$|$)")) |> # Returns the two-digit year that the file is from.
  distinct(baseURL, .keep_all = TRUE)

IPEDS_table_list_links <- IPEDS_table_list_links |> # Recoding file names that did not indicate a year in the file name (Student Financial Aid and Net Price, 1999)
  mutate(two_dgt_year = case_when(str_detect(baseURL, "Pub_student|Pub_Financial") ~ "99",
                                  TRUE ~ two_dgt_year),
         file = str_extract(baseURL, "[^/]+$"), # Extracting file name (everything after the last / in the file path)
         display_year = case_when(as.numeric(two_dgt_year) < 50 ~ str_c("20",two_dgt_year),
                                  as.numeric(two_dgt_year) > 50 ~ str_c("19",two_dgt_year)
         )
  ) 



