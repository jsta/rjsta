#'@name Mode
#'@title Mode
#'@description Returns the mode of a numeric array
#'@export
#'@param x numeric array
Mode <- function(x){
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#' gitignore
#' 
#' @param f file.path or wildcard to add to gitignore
#' @param dry.run logical 
#' @param verbose logical, print operation results?
#' 
#' @export
#' @examples \dontrun{
#' gitignore("test")
#' }
gitignore <- function(f, dry.run = FALSE, verbose = FALSE){
  message("Before:")
  current <- readLines(".gitignore")
  
  if(verbose){print(current)}
  
  message("\n After:")
  if(!dry.run & !(f %in% current)){
    if(verbose){print(c(current, f))}
    write(f, file = ".gitignore", append = TRUE)
  }else{
    if(verbose){print(current)}
  }
}

#' Download a file if it doesn't already exist
#' 
#' @param url url string
#' @param destfile file.path
#' @param overwrite logical force overwrite
#'
#' @importFrom utils download.file
#' @export
get_if_not_exists <- function(url, destfile, overwrite){
  if(!file.exists(destfile) | overwrite){
    download.file(url, destfile)
  }else{
    message(paste0("A local copy of ", url, " already exists on disk"))
  }
}