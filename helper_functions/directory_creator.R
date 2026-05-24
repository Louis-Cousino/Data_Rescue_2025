# Author: Louis Cousino
# Date: 07-27-2025
# Description: A function for creating nested folders. The function takes the base folder path and creates sub folders
# named after the subsequent arguments.

directory_creator <- function(base_directory, ...) {
  
  path <- list(...)
  
  # Removing entries that have at least one dot (indicating a file)
  path <- path[!stringr::str_detect(path, "\\.")]
  
  # Converting NA values into blank
  path[is.na(path)] <- ""
  
  # Initializing current path as provided base path
  current_path <- base_directory
  
  for (i in 1:length(path)) {
    
    # Adding folder to path chain
    current_path <- stringr::str_c(current_path, "/", path[[i]])
    
    # Removing excess slashes
    current_path <- stringr::str_replace_all(current_path, "/+", "/")
    
    # Removing hanging slashes at the end (Happens when NA is the last value provided)
    current_path <- stringr::str_replace_all(current_path, "/$", "")
    
    if (dir.exists(current_path) == FALSE) {

      dir.create(current_path)

    }
    
  }
  
  return(current_path)
  
}
