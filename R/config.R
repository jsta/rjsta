
#' Read a config.py file in R
#'
#' @param file a file path
#' @export
#' @examples \dontrun{
#' dir(config()$a)
#' }
config <- function(file = "config.py") {
  res <- suppressWarnings(readLines(file))
  res <- strsplit(res, " ")
  keys <- unlist(lapply(res, function(x) x[[1]][[1]]))
  values <- unlist(lapply(res, function(x) x[[3]][[1]]))
  # keys <- unlist(lapply(res, function(x) strsplit(x, "=")[[1]][[1]]))
  # values <- unlist(lapply(res, function(x) strsplit(x, "=")[[1]][[2]]))

  res <- setNames(data.frame(t(values)), keys)

  norm_file_path <- function(x) {
    gsub('\\"', "", gsub("\\\\\\\\", "\\\\", x))
  }

  data.frame(t(apply(res, 2, norm_file_path)))
}