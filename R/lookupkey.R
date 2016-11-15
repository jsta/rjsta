#'@name lookupkey
#'@title Convert character vector based on lookup values
#'@param key data.frame or matrix where the first column are the strings to be corrected and the second is the "corrected" definitions.
#'@param input character vector
#'@export
#'@examples
#' key <- cbind(c("one", "two", "three", "four",
#'           "five"), c("one1", "two2", "three3", "over", "over"))
#' input <- c("one", "one", "two", "three", "four", "five")
#' lookupkey(key, input)
#'
lookupkey <- function(key, input){
  #add a check for duplicate 'to' key values
  
  input_length <- length(input)
  
  key <- data.frame(key, stringsAsFactors = FALSE)
  input <- data.frame(X1 = input, stringsAsFactors = FALSE)
  
  output <- merge(input, key, all.x = TRUE, all.y = FALSE, sort = FALSE)[,2]
  output[is.na(output)] <- merge(key, input, all.y = TRUE)[which(is.na(output)),1]
  
  if(input_length != length(output)){
    stop("truncated input due to mismatched key length")
  }
  
  output
}