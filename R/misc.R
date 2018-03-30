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
#' 
#' @export
#' @examples \dontrun{
#' gitignore("test")
#' }
gitignore <- function(f, dry.run = FALSE){
  message("Before:")
  current <- readLines(".gitignore")
  print(current)
  
  message("\n After:")
  if(!dry.run & !(f %in% current)){
    print(c(current, f))
    write(f, file = ".gitignore", append = TRUE)
  }else{
    print(current)
  }
}