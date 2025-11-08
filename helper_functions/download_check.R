# Author: Louis Cousino
# Date: 07-27-2025
# Description: A function for checking if a file has been downloaded in a given directory. Useful for files that may take time to download.



download_check <- function(directory, timeout) {
  
  start_time <- Sys.time()
  
  num_before <- list.files(directory)|> 
    as.data.frame() |> 
    rlang::set_names("filename") |> 
    dplyr::filter(str_detect(filename, "\\.(csv)(?!\\.crdownload)") == TRUE) |> 
    nrow()
  
  num_after <- 0
  
  repeat {
    
    if(num_before+1 == num_after) {
      
      print("File Downloaded")
      
      break
      
    } else {
      
      Sys.sleep(1)
      
      num_after <- list.files(directory)|> 
        as.data.frame() |> 
        rlang::set_names("filename") |> 
        dplyr::filter(str_detect(filename, "\\.(csv)(?!\\.crdownload)") == TRUE) |> 
        nrow()
      
      # Timing out to prevent infinite loops
      
      if (difftime(Sys.time(), start_time, units = "secs") > timeout) {
        
        stop(str_c("Timed out after", timeout, "seconds"))
        
      }
      
    }
  }
}
