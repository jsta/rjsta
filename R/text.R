
#' Break a character string into two lines based on a specified maximum length
#' 
#' @param x character string
#' @param max.len maximum length of the first line
#' 
#' @importFrom stringr str_split
#' @export
#' 
#' @examples
#' break_word("a super package", 3)
#' break_word("a super package", 9)
break_word <- function(x, max.len){
  
  x_split <- stringr::str_split(x, " ")[[1]]
  n_chars <- sapply(x_split, nchar)
  
  first_line  <- cumsum(n_chars + 1) < max.len
  second_line <- !first_line
  
  res <- paste(x_split[first_line], collapse = " ")
  paste(res, "\n", paste(x_split[second_line], collapse = " "))
}

#' Zero pad character vectors
#' 
#' @param x character input to pad
#' @param digits numeric. number of zeros to pre-append
#' @examples \dontrun{
#' zero_pad(1, 2)
#' zero_pad(1, 3)
#' }
zero_pad <- function(x, digits){
  paste0(paste0(rep(0, digits), collapse = ""), x, collapse = "")
}