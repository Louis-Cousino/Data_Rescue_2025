# Author: Louis Cousino
# Date: 07-27-2025
# Description: A function for checking if a file has been downloaded in a given directory. Useful for files that may take time to download.



download_check <- function(directory) {
  
  start_time <- Sys.time()
  
  num_before <- list.files(directory)|> 
    as.data.frame() |> 
    set_names("filename") |> 
    filter(str_detect(filename, "\\.(csv)(?!\\.crdownload)") == TRUE) |> 
    nrow()
  
  num_after <- -1
  
  while(num_before == num_after) {
    
    Sys.sleep(1)
    
    num_after <- list.files(directory)|> 
      as.data.frame() |> 
      set_names("filename") |> 
      filter(str_detect(filename, "\\.(csv)(?!\\.crdownload)") == TRUE) |> 
      nrow()
    
    # Timing out to prevent infinite loops
    
    if (difftime(Sys.time(), start_time, units = "secs") > 10) {
      
      stop("Timed out after 2 minutes")
      
    }
    
  }
  
}
