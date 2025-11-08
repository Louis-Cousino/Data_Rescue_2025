# Author: Louis Cousino
# Date: 09-30-2025
# Description: A function for checking if an HTML element is present on the page.



explicit_wait <- function(using, element, timeout, remDr) {
  
  before_time <- Sys.time()
  
  repeat{
    
    # Setting check to TRUE. If the error does not happen then it should not change.
    check <- TRUE
    
    # Calculating the runtime 
    runtime <- Sys.time() - before_time
    
    tryCatch({
      
      return_element <- remDr$findElement(using = using,
                                          value = element)
      
    },
    error = function(e){
      
      print("Could Not Find Element")
      
      check <<- FALSE # <<- searches for a variable named "check" throughout parent scopes and changes it.
      
    })
    
    if(check == TRUE) {
      
      Sys.sleep(1)
      
      return(return_element)
      
    } else if (runtime > timeout) {
      
      stop(str_c("Error: Explicit wait reached a timeout of ", timeout, " seconds."))
      
    }
    
    Sys.sleep(1)
    
  }
}
