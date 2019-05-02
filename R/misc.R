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
#' @param x url string or function to be evaluated
#' @param destfile file.path
#' @param overwrite logical force overwrite
#' @param \dots parameters passed to x
#'
#' @importFrom utils download.file
#' @export
#' @examples \dontrun{
#' get_if_not_exists("http://www.omegahat.net/RCurl/data.gz", "data.gz")
#' junk_rds <- function(destfile, add_number){
#'      saveRDS(1 + add_number, destfile)
#'      return(1 + add_number)
#' }
#' get_if_not_exists(junk_rds, "junk.rds")
#' }
get_if_not_exists <- function(x, destfile, overwrite = FALSE, ...){
  
  if(is.function(x)){
    if(!file.exists(destfile) | overwrite){
      res <- x(destfile, ...)
      return(res)
    }else{
      message(paste0("A local evaulation of x already exists on disk"))
      if(length(grep("*.rds", destfile)) > 0){
        return(readRDS(destfile))
      }
    }
  } 
  
  if(!is.function(x)){
    if(!file.exists(destfile) | overwrite){
      download.file(x, destfile)
    }else{
      message(paste0("A local copy of ", x, " already exists on disk"))
    }
    invisible(x)
  }
}
