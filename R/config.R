
#' Read a config.py file in R
#'
#' @param file a file path
#' @export 
#' @examples \dontrun{
#'  config()
#' }
config <- function(file="config.py"){
    res <- readLines(file)
    res <- strsplit(res, " ")
    keys <- unlist(lapply(res, function(x) strsplit(x, "=")[[1]][[1]]))
    values <- unlist(lapply(res, function(x) strsplit(x, "=")[[1]][[2]]))

    setNames(data.frame(t(values)), keys)
}
