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
#' @param read_function function to read existing files, defaults to readRDS
#' @param ow logical force overwrite
#' @param \dots parameters passed to x
#'
#' @importFrom utils download.file
#' @export
#' @examples \dontrun{
#' # unlink("data.gz")
#' get_if_not_exists("http://www.omegahat.net/RCurl/data.gz", "data.gz", ow = TRUE)
#' 
#' junk_rds <- function(destfile, add_number){
#'      saveRDS(1 + add_number, destfile)
#'      return(1 + add_number)
#' }
#' # unlink("junk.rds")
#' x <- get_if_not_exists(junk_rds, "junk.rds", add_number = 1)
#' 
#' junk_csv <- function(destfile, add_number){
#'      write.csv(1 + add_number, destfile, row.names = FALSE)
#'      return(1 + add_number)
#' }
#' # unlink("junk.csv")
#' x <- get_if_not_exists(junk_csv, "junk.csv", read.csv, add_number = 1)
#' }
get_if_not_exists <- function(x, destfile, read_function = readRDS, 
                              ow = FALSE, ...){
  
  if(is.function(x)){
    if(!file.exists(destfile) | ow){
      res <- x(destfile, ...)
      return(res)
    }else{
      message(paste0("A local evaulation of x already exists on disk"))
      return(read_function(destfile))
    }
  } 
  
  if(!is.function(x)){
    if(!file.exists(destfile) | ow){
      download.file(x, destfile)
    }else{
      message(paste0("A local copy of ", x, " already exists on disk"))
    }
    invisible(x)
  }
}

#' Format a table for inclusion in roxygen docs
#' 
#' @param df data.frame
#' 
#' @references <http://r-pkgs.had.co.nz/man.html>
#' 
#' @export
#' @examples 
#' df <- data.frame(a_b = 1, b_c = 2, stringsAsFactors = FALSE)
#' tabular(df)
tabular <- function(df, ...) {
  stopifnot(is.data.frame(df))
  
  align <- function(x) if (is.numeric(x)) "r" else "l"
  col_align <- vapply(df, align, character(1))
  
  cols <- lapply(df, format, ...)
  contents <- do.call("paste",
                      c(cols, list(sep = " \\tab ", collapse = "\\cr\n  ")))
  col_names <- paste0("\\bold{",
      do.call("paste",
                       c(names(df), list(sep = "} \\tab \\bold{", collapse = "\\cr\n  "))), 
      "} \\cr")
  
  paste("\\tabular{", paste(col_align, collapse = ""), "}{\n",
        col_names,
        "\n",
        contents, "\n}\n", sep = "")
}
