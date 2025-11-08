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
library(rvest)
library(httr2)
library(RSelenium)

# Loading helper functions

source(here::here("helper_functions", "directory_creator.R"))
source(here::here("helper_functions", "download_check.R"))
source(here::here("helper_functions", "explicit_click.R"))
source(here::here("helper_functions", "explicit_wait.R"))


# Defining download directory

download_directory <- here::here("temp")

# Extracting list of publications

# Starting Selenium Server with Chrome
driver <- rsDriver(verbose = F, port=netstat::free_port(),
                   check = FALSE)

remDr<- driver$client

# Getting initial list of tables

remDr$navigate("https://nces.ed.gov/datalab/table/library")

Sys.sleep(5)

all_release_button <- remDr$findElement(using = "css selector",
                                        value = "a.blue.underline.au-target")

all_release_button$clickElement()

Sys.sleep(5)

# Scraping data from the page (e.g., get page source)
page_source <- remDr$getPageSource()[[1]]

page_html <- read_html(page_source)

remDr$close()

driver$server$stop()

# Formatting publication metadata

metadata <- list()

metadata <- page_html |> 
  html_elements("table.table-no-vert-border") |>
  html_elements("tr") |> 
  map(~{
    
    tibble(
      
      pub_number = html_elements(.x, "td:nth-child(1)") |> 
        html_elements("span.my-auto") |> 
        html_text(),
      release_date = html_elements(.x, "td.strong") |> 
        html_text(),
      pub_title = html_elements(.x, "td:nth-child(3)") |> 
        html_elements("span.au-target") |> 
        html_text(),
      pub_link = str_c("https://nces.ed.gov/datalab/table/library/list/", pub_number)
      
    )
    
  })

metadata <- metadata |> 
  bind_rows()

# Creating function for scraping
## A note: Different download methods were used for estimate files and standard error files.
## During development, I had made an assumption about the API endpoint for standard error files that did not hold.
## (The assumption was that standard error files were located at endpoints that were one plus the ID of the estimate endpoints)
## Instead, the download button is clicked by the program and downloaded into a temporary directory.
## The files are then moved to their target destination.

datalab_scraper <- function(links_dataframe, download_directory, timeout) {
  
  timer <- timeR::createTimer()
  
  timer$start("DataLab Run")
  
  return_metadata <- list()
  
  temp_dir <- str_replace_all(file.path(Sys.getenv("USERPROFILE"), "Downloads"), "\\\\", "/")
  
  on.exit({
    
    return_metadata <- return_metadata |>
      bind_rows()
    
    
    # Closing RSelenium
    
    remDr$close()
    
    driver$server$stop()
    
    
    # Returning Publication Tables Metadata and Investigation List
    
    investigate_list <- investigate_list |>
      bind_rows()
    
    return_list <- list(return_metadata,
                        investigate_list)
    
    saveRDS(return_list, str_c(download_directory, "/full_links_", str_replace_all(ymd_hms(Sys.time()), " |:", "_"), ".RDS"))
    
    timer$stop("DataLab Run")
    
    return(return_list)
    
  }, add = TRUE)
  
  # Starting Selenium Server with Chrome
  
  chrome_prefs <- list(
    "download.prompt_for_download" = FALSE,
    "download.directory_upgrade" = TRUE,
    "safebrowsing.enabled" = TRUE,
    "profile.default_content_setting_values.automatic_downloads" = 1 # Allows chrome to download multiple files from the same site in one session
  )
  
  
  eCaps <- list(chromeOptions = list(
    args = c('--headless', '--disable-gpu', '--no-sandbox'),
    prefs = chrome_prefs
  ))
  
  driver <- rsDriver(verbose = F, port=netstat::free_port(),
                     extraCapabilities = eCaps,
                     check = FALSE)
  
  remDr<- driver$client
  
  # initializing return list
  investigate_list <- list()
  
  # Instantiating publication table list
  return_metadata <- list()
  
  for (i in 1:nrow(links_dataframe)){
    
    # Navigating to list of publication tables
    
    remDr$navigate(as.character(links_dataframe[i, 4]))
    
    # Waiting for the page to load
    
    remDr$setTimeout(type = "page load",
                     milliseconds = 120000)
    
    Sys.sleep(1)
    
    # Downloading Specification files
    
    download_response <- request(str_c("https://nces.ed.gov/datalab/files/zip/TablesLibrary/", as.character(links_dataframe[i, 1]), ".zip")) |>
      req_method("GET") |>
      req_perform()
    
    # Saving file from response
    
    ## Creating download location for this iteration of a table and saving the address
    
    download_location <- directory_creator(download_directory, as.character(links_dataframe[i, 1]))
    
    download_file <- str_c(download_location, "/", as.character(links_dataframe[i, 1]), ".zip")
    
    if (file.exists(download_file) == FALSE) {
      
      writeBin(resp_body_raw(download_response), download_file)
      
      # Waiting until the file is fully downloaded
      
      download_check(download_location)
      
    } else {
      
      message(str_c("File ", as.character(links_dataframe[i, 1]), ".zip", " exists. Skipping"))
      
    }
    
    # Getting the number of maximum tables
    
    Sys.sleep(3)
    
    number_of_tables <- remDr$findElement(using = "css selector",
                                          value = "div.d-flex > h4")
    
    number_of_tables <- number_of_tables$getElementText() |>
      as.data.frame() |> 
      pull() |> 
      str_extract("\\d*") |> 
      as.numeric()
    
    parent_url <- remDr$getCurrentUrl() |> 
      as.data.frame() |> 
      pull()
    
    # Navigating to child tables
    
    print(parent_url)
    
    set_number <- 0
    
    publication_tables_metadata <- list()
    
    for (j in 1:number_of_tables) {
      
      # Getting the metadata for the tables
      
      table_number <- explicit_wait(using = "css selector",
                                    element = str_c("#table-list > div > table > tbody > tr:nth-child(", j-(set_number*10), ") > td:nth-child(1) > div > div"),
                                    timeout = timeout,
                                    remDr = remDr)
      
      table_title <- explicit_wait(using = "css selector",
                                   element = str_c("#table-list > div > table > tbody > tr:nth-child(", j-(set_number*10), ") > td:nth-child(2) > a > div"),
                                   timeout = timeout,
                                   remDr = remDr)
      
      data_source <- explicit_wait(using = "css selector",
                                   element = str_c("#table-list > div > table > tbody > tr:nth-child(", j-(set_number*10), ") > td:nth-child(3) > div"),
                                   timeout = timeout,
                                   remDr = remDr)
      
      publication_tables_metadata[[j]] <- tibble(
        
        table_number = table_number$getElementText() |>
          as.data.frame() |> 
          pull(),
        table_title = table_title$getElementText() |>
          as.data.frame() |> 
          pull(),
        data_source = data_source$getElementText() |>
          as.data.frame() |> 
          pull(),
        publication = as.character(links_dataframe[i, 1])
        
      )
      
      
      # Explicit Check for Element
      child_table <- explicit_wait(using = "css selector",
                                   element = str_c("#table-list > div > table > tbody > tr:nth-child(", j-(set_number*10), ") > td.center > button"),
                                   timeout = timeout,
                                   remDr = remDr)
      
      Sys.sleep(0.5)
      
      explicit_click(button = child_table, timeout = timeout, remDr = remDr)
      
      Sys.sleep(0.5)
      
      table_link <- remDr$getCurrentUrl() |> 
        as.data.frame() |> 
        pull()
      
      # Getting Table ID number
      table_destination <- table_link |> 
        str_replace_all("\\\\", "/") |> 
        str_extract("\\d*$") |> 
        str_replace_all("\\:", "_")
      
      publication_tables_metadata[[j]] <- publication_tables_metadata[[j]] |> 
        mutate(table_link = table_link)
      
      download_options <- explicit_wait(using = "css selector",
                                        element = str_c("#view-option > a > div"),
                                        timeout = timeout,
                                        remDr = remDr)
      
      remDr$executeScript("arguments[0].click();", list(download_options))
      
      Sys.sleep(0.5)
      
      # Getting the number of downloadable files
      
      num_files <- explicit_wait(using = "xpath",
                                 element = "//*[@id='view-dropdown-content']/ul/li",
                                 timeout = timeout,
                                 remDr = remDr)
      
      num_files <- remDr$findElements(using = "xpath",
                                      value = "//*[@id='view-dropdown-content']/ul/li") |>
        length()
      
      # Getting the ID of the API
      
      api_id <- remDr$getCurrentUrl() |>
        str_extract("[^/]+$") |>
        as.numeric()
      
      cookies <- remDr$getAllCookies()
      
      cookie_list <- setNames(
        lapply(cookies, function(x) x$value),
        sapply(cookies, function(x) x$name)
      )
      
      for (k in 1:num_files) {
        
        # Downloading Files
        
        if (k == 1) {
          
          # Downloading files when a API is called
          
          download_response <- request(str_c("https://nces.ed.gov/datalab/api/v1/library/", as.character(api_id+k-1), "/download")) |> # +k-1 allows for the api_id to be called on the first file.
            req_method("GET")
          
          download_response <- do.call(req_cookies_set, c(list(download_response), cookie_list))
          
          download_response <- download_response |>
            req_perform()
          
          status_code <- download_response[["status_code"]]
          
          # Saving file from response
          
          file_name <- str_extract(download_response[["headers"]][["Content-Disposition"]], "[^']+$")
          
          file_name_record <- file_name # This is so the function can open a file connection and download the file without issue.
          
          download_location <- directory_creator(download_directory, as.character(links_dataframe[i, 1]), table_destination)
          
          download_file <- str_c(download_location, "/", file_name)
          
          if (length(file_name) == 0) {
            
            message("File does not exist")
            
            investigate_list[[length(investigate_list) + 1]] <- list(parent_url = parent_url,
                                                                     page_url = remDr$getCurrentUrl() |>
                                                                       as.data.frame() |>
                                                                       pull(),
                                                                     file = NA,
                                                                     exists = "N")
            
          } else if (file.exists(download_file) == TRUE) {
            
            message(str_c("File ", file_name, " exists. Skipping"))
            
            investigate_list[[length(investigate_list) + 1]] <- list(parent_url = parent_url,
                                                                     page_url = remDr$getCurrentUrl() |>
                                                                       as.data.frame() |>
                                                                       pull(),
                                                                     file = file_name,
                                                                     exists = "Y"
            )
            
            
          } else {
            
            writeBin(resp_body_raw(download_response), download_file)
            
            # Waiting until the file is fully downloaded
            
            download_check(download_location)
            
          }
          
        } else if (k == 2) {
          
          download_options <- explicit_wait(using = "css selector",
                                            element = "#download > a > span > i",
                                            timeout = timeout,
                                            remDr = remDr)
          
          explicit_click(download_options, timeout, remDr)
          
          se_download <- explicit_wait(using = "css selector",
                                       element = "#download-options > li:nth-child(2) > a > span",
                                       timeout = timeout,
                                       remDr = remDr)
          
          explicit_click(se_download, timeout, remDr)
          
          before <- list.files(temp_dir)
          
          repeat {
            
            after <- list.files(temp_dir)
            
            after <- after[!str_detect(after, "\\.crdownload$")]
            
            if (length(before) == length(after)) {break}
            
            Sys.sleep(1)
            
          }
          
          file_name <- before[str_detect(list.files(temp_dir), ".xlsx")]
          
          file_name_record <- file_name
          
          destination <- str_c(download_directory, "/", links_dataframe$pub_number[i], "/", table_destination, "/", file_name_record)
          
          if (length(file_name) == 0) {
            
            message("File does not exist")
            
            investigate_list[[length(investigate_list) + 1]] <- list(parent_url = parent_url,
                                                                     page_url = remDr$getCurrentUrl() |>
                                                                       as.data.frame() |>
                                                                       pull(),
                                                                     file = NA,
                                                                     exists = "N")
            
          } else if (file.exists(destination) == TRUE) {
            
            message(str_c("File ", file_name, " exists. Skipping"))
            
            investigate_list[[length(investigate_list) + 1]] <- list(parent_url = parent_url,
                                                                     page_url = remDr$getCurrentUrl() |>
                                                                       as.data.frame() |>
                                                                       pull(),
                                                                     file = file_name,
                                                                     exists = "Y"
            )
            
            
          } else {
            
            file.rename(str_c(temp_dir, "/", file_name_record), destination)
            
          }
          
          
        }
        
        
        
        if (length(file_name_record) == 0) {file_name_record <- NA}
        
        if (num_files == 1) {
          
          publication_tables_metadata[[j]] <- publication_tables_metadata[[j]] |> 
            mutate(estimates_file = file_name_record,
                   std_err_file = NA)
          
          
        } else if (num_files == 2 && k == 1) {
          
          publication_tables_metadata[[j]] <- publication_tables_metadata[[j]] |> 
            mutate(estimates_file = file_name_record)
          
          
        } else if (num_files == 2 && k == 2) {
          
          publication_tables_metadata[[j]] <- publication_tables_metadata[[j]] |> 
            mutate(std_err_file = file_name_record)
          
          
        }
        
        
        
      }
      
      # Returning to Tables list
      
      return_button <- explicit_wait(using = "css selector",
                                     element = "#toolbar-lightgray > div.flex-fill.d-flex.flex-row.align-items-center.justify-content-end.relative > div:nth-child(3) > a",
                                     timeout = timeout,
                                     remDr = remDr)
      
      # return_button <- remDr$findElement(using = "css selector",
      #                                    value = "#toolbar-lightgray > div.flex-fill.d-flex.flex-row.align-items-center.justify-content-end.relative > div:nth-child(3) > a")
      
      explicit_click(button = return_button, timeout = timeout, remDr = remDr)
      
      
      Sys.sleep(0.5)
      
      # Since not all the tables can fit onto one page (10 max), we need to loop through until all the tables are captured.
      
      # When the function is at an iteration wheree j/10 equals an integer, then it should page to the next set of tables.
      
      if (j > 9 && j < number_of_tables) { # When table_number is a multiple of 10, `j < table_number` prevents the script from clicking more times than there are set numbers.
        
        times_to_set <- j%/%10
        
        for (k in 1:times_to_set) {
          
          set_button <- explicit_wait(using = "css selector",
                                      element = "#table-list > div > div.d-flex.justify-content-end.mt-2 > div > div.paginator > div.nav-action.next.relative > a > span > i",
                                      timeout = timeout,
                                      remDr = remDr)
          
          explicit_click(button = set_button, timeout = timeout, remDr = remDr)
          
          Sys.sleep(0.1)
          
        }
        
        set_number <- j %/% 10
        
      }
      
    }
    
    return_metadata[[i]] <- publication_tables_metadata |> 
      bind_rows()
    
  }
}

# Downloading files

tables_metadata <- metadata |>
  slice(1) |>
  datalab_scraper(download_directory = download_directory,
                  timeout = 60)

# Exporting metadata

writexl::write_xlsx(tables_metadata, path = str_c(download_directory, "/file_directory.xlsx"))
