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
# Description: AltasPlus website, which helps users explore data on HIV, STD, and Social Determinates of Health

# Loading Libraries

library(tidyverse)
library(rvest)
library(httr2)
library(RSelenium)

# Loading helper functions

source(here::here("helper_functions", "directory_creator.R"))
source(here::here("helper_functions", "download_check.R"))
source(here::here("helper_functions", "explicit_wait.R"))

# Defining default download location (Download folder)
download_dir <- str_replace_all(file.path(Sys.getenv("USERPROFILE"), "Downloads"), "\\\\", "/")


# Getting the IDs of Indicators

driver <- rsDriver(verbose = F, port=netstat::free_port(),
                   check = FALSE)

remDr<- driver$client

remDr$navigate("https://gis.cdc.gov/grasp/nchhstpatlas/tables.html")

# Clicking the button for SDOH.
# Clicking SDOH first allows the script to get all of the indicators in this step

Sys.sleep(10)
remDr$executeScript("document.querySelector('#wizardQT-p-0 > fieldset > label:nth-child(3)').click();")

# Scraping data from the page (e.g., get page source)

page_source <- remDr$getPageSource()[[1]]

page_html <- read_html(page_source)

indicator_metadata <- list()

indicator_metadata <- page_html |> 
  html_elements("div.panel-body") |> 
  html_elements("label.checkbox-inline") |>
  map(~{
    
    tibble(
      
      text = html_text(.x),
      value = html_element(.x, "input") |> 
        html_attr("value"),
      parent_box = html_element(.x, xpath = "..") |> 
        html_attr("id")
      
    )
    
  })

indicator_metadata <- indicator_metadata |> 
  bind_rows()

indicator_metadata <- indicator_metadata |> 
  mutate(county = case_when(str_detect(text, "\\*") == TRUE ~ 1,
                            TRUE ~ 0),
         region = case_when(str_detect(text, "\\‡") == TRUE ~ 1,
                            TRUE ~ 0),
         msa = case_when(str_detect(text, "\\^") == TRUE ~ 1,
                         TRUE ~ 0),
         value = str_c("document.querySelector('#", parent_box, " > label > input[type=\\\"", "checkbox" ,"\\\"][value=\\\"",value,"\\\"]').click()"))

sdoh_metadata <- indicator_metadata |> 
  slice_tail(n = 6)

indicator_metadata <- indicator_metadata |> 
  slice_head(n = -6)


remDr$close()

driver$server$stop()

# Making a key for each file

geo_metadata <- tibble(
  
  text = c("National", "Region", "State", "County - All", "County - EHE", "MSA - Major", "MSA - All"),
  value = c("document.querySelector('#wizardQT-p-1 > div:nth-child(3) > div.panel-body > fieldset > label:nth-child(2)').click();",
            "document.querySelector('#wizardQT-p-1 > div:nth-child(3) > div.panel-body > fieldset > label:nth-child(3)').click(); document.querySelector('#radioRegions > label').click();",
            "document.querySelector('#wizardQT-p-1 > div:nth-child(3) > div.panel-body > fieldset > label:nth-child(4)').click(); document.querySelector('#radioStatesTerr > label').click();",
            "document.querySelector('#wizardQT-p-1 > div:nth-child(3) > div.panel-body > fieldset > label:nth-child(5)').click(); document.querySelector('#radioStatesTerr > label').click();",
            "document.querySelector('#wizardQT-p-1 > div:nth-child(3) > div.panel-body > fieldset > label:nth-child(5)').click(); document.querySelector('#radioEHE > label').click();",
            "document.querySelector('#wizardQT-p-1 > div:nth-child(3) > div.panel-body > fieldset > label:nth-child(6)').click(); document.querySelector('#ancSelectAllGeo').click();",
            "document.querySelector('#wizardQT-p-1 > div:nth-child(3) > div.panel-body > fieldset > label:nth-child(6)').click(); document.querySelector('#radioMSATotals > label').click(); document.querySelector('#ancSelectAllGeo').click();")
  
)

year_metadata <- tibble(
  
  text = "Year",
  value = "document.querySelector('#wizardQT-p-2 > div > div > div.panel-heading > div > label').click();"
  
)


age_metadata <- tibble(
  
  text = c("All ages 13 years and older" , "Age - 13 and older", "Age - 13 to 24", "Age - 50 and older"),
  value = c("document.querySelector('#allAge > label').click();",
            "document.querySelector('#specificAge > label > span').click(); document.querySelectorAll('#specificAgeGroupsList > fieldset > div:nth-child(n+2):nth-child(-n+7) > label').forEach(el => el.click());",
            "document.querySelector('#youngAgeGroup > label').click(); document.querySelectorAll('#youngAgeGroupsList > fieldset > div:nth-child(n+2):nth-child(-n+3) > label').forEach(el => el.click());",
            "document.querySelector('#olderAgeGroup > label').click(); document.querySelectorAll('#olderAgeGroupsList > fieldset > div:nth-child(n+2):nth-child(-n+9) > label').forEach(el => el.click());")
  
)

race_metadata <- tibble(
  
  text = c("All Races_Ethnicities",
           "Race_Ethnicity"),
  value = c("document.querySelector('#allRaces').click();",
            "document.querySelector('#specificRace > label').click(); document.querySelectorAll('#raceList > fieldset > div:nth-child(n+2):nth-child(-n+8) > label').forEach(el => el.click());")
  
)

sex_metadata <- tibble(
  
  text = c("Both Sexes", "Sex - Male", "Sex - Female"),
  value = c("document.querySelector('#allTC').click();",
            "document.querySelector('#specificTransCat > label').click(); document.querySelectorAll('#transcatList > fieldset > div:nth-child(n+2):nth-child(-n+6) > label').forEach(el => el.click());", 
            "document.querySelector('#specificTransCat > label').click(); document.querySelector('#transcatList > fieldset > div:nth-child(3) > label').click(); document.querySelector('#transcatList > fieldset > div:nth-child(5) > label').click(); document.querySelector('#transcatList > fieldset > div:nth-child(6) > label').click();")
  
)


transmission_metadata <- tibble(
  
  text = c("All Transmission", "Male Transmission Options", "Female Transmission Options"),
  value = c("document.querySelector('#divTransCat > div > div.panel-body > fieldset > div:nth-child(2) > label').click();",
            "document.querySelector('#specificTransCat > label').click(); document.querySelectorAll('#transcatList > fieldset > div:nth-child(n+2):nth-child(-n+6) > label').forEach(el => el.click());", 
            "document.querySelector('#specificTransCat > label').click(); document.querySelector('#transcatList > fieldset > div:nth-child(3) > label').click(); document.querySelector('#transcatList > fieldset > div:nth-child(5) > label').click(); document.querySelector('#transcatList > fieldset > div:nth-child(6) > label').click();")
  
)

# Making master list

master_tibble <- expand_grid(indicator = indicator_metadata$text,
                             geography = geo_metadata$text,
                             age = age_metadata$text,
                             race = race_metadata$text,
                             sex = sex_metadata$text,
                             transmission = transmission_metadata$text)

master_tibble <- master_tibble |> 
  arrange(indicator) |> 
  left_join(select(indicator_metadata, -value, -parent_box), by = join_by("indicator" == "text")) |> 
  rename("county_indicator" = county,
         "geo_indicator" = region,
         "msa_indicator" = msa)


sdoh_list <- list()


for (i in 1:nrow(sdoh_metadata)) {
  
  for (j in 1:nrow(geo_metadata)) {
    
    order_list <- tibble(
      
      indicator = sdoh_metadata$text[i],
      geography = geo_metadata$text[j],
      age = NA,
      race = NA,
      sex = NA,
      transmission = NA,
      county_indicator = sdoh_metadata$county[i],
      geo_indicator = sdoh_metadata$region[i],
      msa_indicator = sdoh_metadata$msa[i]
      
    )
    
    
    sdoh_list[[length(sdoh_list) + 1]] <- order_list
    
  }
  
}

sdoh_tibble <- sdoh_list |> 
  bind_rows()

master_tibble <- master_tibble |> 
  rbind(sdoh_tibble)



scraping_tibble <- master_tibble |> 
  filter(case_when(county_indicator == 0 ~ str_detect(geography, "County") == FALSE,
                   TRUE ~ TRUE)) |> 
  filter(case_when(geo_indicator == 0 ~ str_detect(geography, "Region") == FALSE,
                   TRUE ~ TRUE)) |> 
  filter(case_when(msa_indicator == 0 ~ str_detect(geography, "MSA") == FALSE,
                   TRUE ~ TRUE)) |> 
  select(-ends_with("_indicator"))

# Now that I have the list of runs, I need to implement a function to actually download the files

# Overall process:
# 1. Open the browser and navigate to the website
# 2. Select the indicator
# 3. Select geography
# 4. Select year
# 5. Select demographics (if the indicator is not a SDOH)
# 6. Create table
# 7. Select "Underlying data"
# 8. Select "Export" and download the file
# 9. Move the file to its proper location
# 10. Select "Start over"
# 11. Confirm the process of starting over
# 12. Repeat steps 2-11 until finished


selector_function <- function(column, selector_table, num) {
  
  section <- eval(parse(text = str_c("scraping_tibble$", column, "[", num, "]")))
  
  js_script <- selector_table |> 
    filter(text == section) |> 
    pull(value)
  
  remDr$executeScript(js_script)
  
}

# Making a function to explicitly wait while working with the JS structure of the page

JS_explicit_wait <- function(script, mode = c("text", "element"), timeout, remDr) {
  
  before_time <- Sys.time()
  
  # Setting check to TRUE. If the error does not happen then it should not change.
  check <- TRUE
  
  repeat{
    
    if (mode == "text"){
      
      result <- remDr$executeScript(script) |> 
        as.character()
      
      if (result != "none") {break}
    }
    
    if (mode == "element"){
      
      
      tryCatch({
        
        return_element <- remDr$executeScript(script)
        
      },
      error = function(e){
        
        print("Could Not Find Element")
        
        check <<- FALSE # <<- searches for a variable named "check" throughout parent scopes and changes it.
        
      })
      
      if(check == TRUE) {
        
        break
        
      }
    }
    
    Sys.sleep(1)
    
    # Calculating the runtime 
    runtime <- Sys.time() - before_time
    
    if (runtime > timeout) {
      
      stop(str_c("Error: Explicit wait reached a timeout of ", timeout, " seconds."))
      
    }
    
  }
}

# Running the explicit wait in reverse for checking if the table has been generated
explicit_wait_reverse <- function(using, element, timeout, remDr) {
  
  before_time <- Sys.time()
  
  repeat{
    
    # Setting check to TRUE. If the error does not happen then it should not change.
    check <- FALSE
    
    # Calculating the runtime 
    runtime <- Sys.time() - before_time
    
    tryCatch({
      
      return_element <- remDr$findElement(using = using,
                                          value = element)
      
      
    },
    error = function(e){
      
      check <<- TRUE # <<- searches for a variable named "check" throughout parent scopes and changes it.
      
    })
    
    if(check == TRUE) {
      
      Sys.sleep(1)
      
      break
      
    } else if (runtime > timeout) {
      
      stop(str_c("Error: Explicit wait reached a timeout of ", timeout, " seconds."))
      
    }
    
    Sys.sleep(1)
    
  }
}

next_btn <- "document.querySelector('#wizardQT > div:nth-child(2) > ul > li:nth-child(3) > a').click();"

chrome_prefs <- list(
  "download.prompt_for_download" = FALSE,
  "download.directory_upgrade" = TRUE,
  "safebrowsing.enabled" = TRUE,
  "profile.default_content_setting_values.automatic_downloads" = 1 # Allows chrome to download multiple files from the same site in one session
)


eCaps <- list(chromeOptions = list(
  # args = c('--headless', '--disable-gpu', '--no-sandbox'),
  prefs = chrome_prefs
))


# Filtering scraping tibble to remove redundancies and impossibilities for sex and transmission;

scraping_tibble <- scraping_tibble |> 
  filter(case_when(sex == "Both Sexes" & transmission == "Female Transmission Options" ~ FALSE,
                   sex == "Sex - Male" & transmission == "Female Transmission Options" ~ FALSE,
                   sex == "Sex - Female" & transmission == "Male Transmission Options" ~ FALSE,
                   TRUE ~ TRUE))

for (i in 153:165) {
  
  print(i)
  
  if (i == 153) {
    
    timer <- timeR::createTimer()
    
    timer$start("AltasPlus Run")
    
    return_metadata <- list()
    
    driver <- rsDriver(verbose = F, port=netstat::free_port(),
                       extraCapabilities = eCaps,
                       check = FALSE)
    
    remDr<- driver$client
    
    remDr$setTimeout(type="script", 100000)
    
  }
  
  if (i == 165) {
    
    # This is at the top so that if the last iteration gets skipped, then this still trips.
    
    # Closing RSelenium
    
    remDr$close()
    
    driver$server$stop()
    
    timer$stop("AltasPlus Run")
    
    break
    
  }
  
  console_log <- list()
  
  remDr$navigate("https://gis.cdc.gov/grasp/nchhstpatlas/tables.html")
  
  # Waiting for page to load
  repeat{
    if(remDr$executeScript("return document.readyState == 'complete';")[[1]] == TRUE) {
      Sys.sleep(1)
      break
    } else {
      Sys.sleep(1)
    }
  }
  
  indicator <- scraping_tibble[i, ] |> 
    mutate(indicator = str_replace_all(indicator, "\\*|\\^|\\‡", ""),
           indicator = str_trim(indicator)) |> 
    pull(indicator)
  
  indicator_list <- indicator_metadata |> 
    mutate(text = str_replace_all(text, "\\*|\\^|\\‡", ""),
           text = str_trim(text)) |> 
    pull(text) |> 
    str_c(collapse = "|")
  
  # Indicator Section
  JS_explicit_wait(script = "text = document.querySelector('#wizardQT-p-0'); 
                                  text_return = window.getComputedStyle(text).display;
                                  return text_return",
                   mode = "text", 30, remDr = remDr)
  
  selector_function("indicator", indicator_metadata, i)
  
  # Next Button
  remDr$executeScript(next_btn)
  
  # Geography Section
  JS_explicit_wait(script = "text = document.querySelector('#wizardQT-p-1'); 
                                  text_return = window.getComputedStyle(text).display;
                                  return text_return",
                   mode = "text", 30, remDr = remDr)
  
  selector_function("geography", geo_metadata, i)
  
  # Next Button
  remDr$executeScript(next_btn)
  
  # Year Selection
  JS_explicit_wait(script = "text = document.querySelector('#wizardQT-p-2'); 
                                  text_return = window.getComputedStyle(text).display;
                                  return text_return",
                   mode = "text", 30, remDr = remDr)
  
  remDr$executeScript(as.character(year_metadata[2]))
  
  # Next Button
  remDr$executeScript(next_btn)
  
  # Demographic Selection (If needed)
  
  if (str_detect(indicator, str_c(indicator_list)) == TRUE) {
    
    JS_explicit_wait(script = "text = document.querySelector('#wizardQT-p-3'); 
                                  text_return = window.getComputedStyle(text).display;
                                  return text_return",
                     mode = "text", 30, remDr = remDr)
    
    younger_age_option <- remDr$executeScript("text = document.querySelector('#youngAgeGroup'); 
                                  text_return = window.getComputedStyle(text).display;
                                  return text_return") |> 
      as.character()
    
    older_age_option <- remDr$executeScript("text = document.querySelector('#youngAgeGroup'); 
                                  text_return = window.getComputedStyle(text).display;
                                  return text_return") |> 
      as.character()
    
    age_class <- remDr$executeScript("text = document.querySelector('#allAge'); 
                                  text_return = text.className;
                                  return text_return") |> 
      as.character()
    
    if (str_detect(age_class, "disabled") == TRUE && scraping_tibble$race[i] != "All ages 13 years and older") {
      return_metadata[[length(return_metadata) + 1]] <- list(run = i,
                                                             location = "NA - Stratification not available")
      
      next
    }
    
    if (younger_age_option == "none" && scraping_tibble$age[i] == "Age - 13 to 24") {
      
      return_metadata[[length(return_metadata) + 1]] <- list(run = i,
                                                             location = "NA - Stratification not available")
      
      next
      
    } else if (older_age_option == "none" && scraping_tibble$age[i] == "Age - 50 and older") {
      
      return_metadata[[length(return_metadata) + 1]] <- list(run = i,
                                                             location = "NA - Stratification not available")
      
      next
      
    }
    
    selector_function("age", age_metadata, i)
    
    race_class <- remDr$executeScript("text = document.querySelector('#wizardQT-p-3 > div > div:nth-child(2) > div > div.panel-body > fieldset > div:nth-child(2)'); 
                                  text_return = text.className;
                                  return text_return") |> 
      as.character()
    
    if (str_detect(race_class, "disabled") == TRUE && scraping_tibble$race[i] != "All Races_Ethnicities") {
      return_metadata[[length(return_metadata) + 1]] <- list(run = i,
                                                             location = "NA - Stratification not available")
      
      next
    }
    
    selector_function("race", race_metadata, i)
    
    # Selecting different options for sex based on what panel is shown (there's two :/ )
    
    sex <- scraping_tibble$sex[i]
    
    sex_option <- remDr$executeScript("text = document.querySelector('#sex_panel'); 
                                  text_return = window.getComputedStyle(text).display;
                                  return text_return") |> 
      as.character()
    
    sex_class <- remDr$executeScript("text = document.querySelector('#sex_panel > div.panel-body > fieldset > div:nth-child(2)'); 
                                  text_return = text.className;
                                  return text_return") |> 
      as.character()
    
    
    if (str_detect(sex_class, "disabled") == TRUE && scraping_tibble$sex[i] != "Both Sexes") {
      return_metadata[[length(return_metadata) + 1]] <- list(run = i,
                                                             location = "NA - Stratification not available")
      
      next
    }
    
    if (sex_option == "none" && sex == "Sex - Male") {
      remDr$executeScript("document.querySelector('#genderListSex > fieldset > div:nth-child(3) > label').click();")
    } else if (sex_option == "none" && sex == "Sex - Female") {
      remDr$executeScript("document.querySelector('#genderListSex > fieldset > div:nth-child(4) > label').click();")
    } else if (sex_option == "none" && sex == "Both Sexes") {
      remDr$executeScript("document.querySelector('#genderListSex > fieldset > div:nth-child(2) > label');")
    } else if(sex_option != "none" && sex == "Sex - Male") {
      remDr$executeScript("document.querySelector('#sexList > fieldset > div:nth-child(2) > label').click();")
    } else if (sex_option != "none" && sex == "Sex - Female") {
      remDr$executeScript("document.querySelector('#sexList > fieldset > div:nth-child(3) > label').click();")
    } else if (sex_option != "none" && sex == "Both Sexes") {
      remDr$executeScript("document.querySelector('#allSexes').click();")
    }
    
    transmission_class <- remDr$executeScript("text = document.querySelector('#divTransCat > div > div.panel-body > fieldset > div:nth-child(2)'); 
                                  text_return = text.className;
                                  return text_return") |> 
      as.character()
    
    if (str_detect(transmission_class, "disabled") == TRUE && scraping_tibble$race[i] != "All Transmission") {
      return_metadata[[length(return_metadata) + 1]] <- list(run = i,
                                                             location = "NA - Stratification not available")
      
      next
    }
    
    selector_function("transmission", transmission_metadata, i)
    
  }
  
  # Creating Table
  remDr$executeScript("document.querySelector('#wizardQT > div:nth-child(2) > ul > li:nth-child(4) > a').click();")
  
  # Some options prevent a table from ever being generated. Since this throws an error in the console log, detecting an error means we can skip that iteration.
  
  Sys.sleep(3)
  
  console_log <- remDr$log("browser") |> 
    as.data.frame()
  
  if (nrow(console_log) != 0) {
    return_metadata[[length(return_metadata) + 1]] <- list(run = i,
                                                           location = "NA - Stratification not available. An error was thrown")
    
    Sys.sleep(1)
    
    next
  }
  
  # Waiting for table to be created
  explicit_wait_reverse(using = "css selector", element = "body > div.blockUI.blockMsg.blockPage", timeout = 300, remDr = remDr)
  
  
  # Underlying Data Button
  Sys.sleep(2)
  remDr$executeScript("document.querySelector('#btnResult').click();")
  
  # Checking if there is an option to select cases or rates. If there is, then we click it to get the maximum amount of data possible
  tbl_form <- remDr$executeScript("text = document.querySelector('#btnRateCases'); 
                                  text_return = window.getComputedStyle(text).display;
                                  return text_return") |> 
    as.character()
  
  if (tbl_form != "none") {remDr$executeScript("document.querySelector('#btnResult').click();")}
  
  
  
  # Export Button
  Sys.sleep(2)
  remDr$executeScript("document.querySelector('#btnExport').click();")
  
  #Waiting for file to download
  
  download_fail <- FALSE
  
  tryCatch({
    
    download_timeout <- 300
    
    download_check(download_dir, timeout = download_timeout)
    
  },
  error = function(e){
    
    download_fail <<- TRUE
    
  })
  
  if (download_fail == TRUE) {
    
    return_metadata[[length(return_metadata) + 1]] <- list(run = i,
                                                           location = str_c("Error: File was not downloaded before the timeout occured (", download_timeout, " seconds)"))
    
    next
    
  }
  
  
  # Creating directory for file location
  file_path <- directory_creator(here::here("temp"), indicator, scraping_tibble$geography[i], scraping_tibble$race[i], scraping_tibble$age[i], scraping_tibble$sex[i], scraping_tibble$transmission[i])
  
  # Moving downloaded file to the correct location
  file.rename(str_c(download_dir, "/", "AtlasPlusTableData.csv"), str_c(file_path, "/", "AtlasPlusTableData_", i ,".csv"))
  
  #Waiting until file is in its proper location
  repeat{
    if (file.exists(str_c(file_path, "/", "AtlasPlusTableData_", i ,".csv")) == TRUE) {
      Sys.sleep(1)
      break
    } else {
      Sys.sleep(1)
    }
  }
  
  # Logging metadata
  
  return_metadata[[length(return_metadata) + 1]] <- list(run = i,
                                                         location = str_replace(file_path, here::here("temp"), ""))
  
}

return_metadata <- return_metadata |>
  bind_rows()

saveRDS(return_metadata, str_c(here::here("temp"), "/return_metadata_", str_replace_all(ymd_hms(Sys.time()), " |:", "_"), ".RDS"))
