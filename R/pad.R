#' align_dfcol
#'@title Align a data.frame to a template
#'@param template data.frame
#'@param target data.frame
#'@export
#'@examples
#'target <- data.frame(matrix(NA, ncol = 5))
#'template <- data.frame(matrix(NA, ncol = 6))
#'names(target) <- c(letters[c(3, 1, 2, 4, 8)])
#'names(template) <- c(letters[1:6])
#'align_dfcol(target =  target, template = template)

align_dfcol <- function(target, template){
  
  target <- target[,names(target) %in% names(template)]
  
  missing.names <- names(template)[!(names(template) %in% names(target))]
  pad.na <- data.frame(matrix(NA, nrow = nrow(target), ncol = length(missing.names)))
  names(pad.na) <- missing.names
  
  target <- cbind(target, pad.na)
  target[,match(names(template), names(target))]
  
}

#' zero_pad
#' @param x character input to pad
#' @param digits numeric. number of zeros to pre-append
#' @examples \dontrun{
#' zero_pad(1, 2)
#' zero_pad(1, 3)
#' }
zero_pad <- function(x, digits){
 paste0(paste0(rep(0, digits), collapse = ""), x, collapse = "")
}