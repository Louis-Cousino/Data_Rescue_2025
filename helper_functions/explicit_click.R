# Author: Louis Cousino
# Date: 09-30-2025
# Description: A function for explicitly clicking an HTML element



explicit_click <- function(button, timeout, remDr) {
  
  before_time <- Sys.time()
  
  repeat{
    
    # Setting check to TRUE. If the error does not happen then it should not change.
    check <- TRUE
    
    # Calculating the runtime 
    runtime <- Sys.time() - before_time
    
    tryCatch({
      
      remDr$executeScript("arguments[0].click();", list(button))
      
    },
    error = function(e){
      
      print("Could Not Find Element")
      
      check <<- FALSE # <<- searches for a vaiable named "check" throughout parent scopes and changes it.
      
    })
    
    if(check == TRUE) {
      
      Sys.sleep(2)
      
      break
      
    } else if (runtime > timeout) {
      
      stop(str_c("Error: Explicit wait reached a timeout of ", timeout, " seconds."))
      
    }
    
    Sys.sleep(2)
    
  }
}
