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