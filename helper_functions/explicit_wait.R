# Author: Louis Cousino
# Date: 09-30-2025
# Description: A function for checking if an HTML element is present on the page.



explicit_wait <- function(element, timeout, remDr, reverse = "n") {
  
  before_time <- Sys.time()
  
  repeat{
    
    # Calculating the runtime 
    runtime <- Sys.time() - before_time
    
    check <- remDr$executeScript(stringr::str_c("element = document.querySelector('", element, "');
                      if (element) {return 'TRUE'} else {return 'FALSE'}; ")) |> 
      as.logical()
    
    if (reverse == "y") {check <- !check}

    if(check == TRUE) {
      
      Sys.sleep(1)
      
      break
      
    } else if (runtime > timeout) {
      
      stop(str_c("Error: Explicit wait reached a timeout of ", timeout, " seconds."))
      
    }
    
    Sys.sleep(2)
    
  }
}
