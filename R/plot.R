
#' plot_invisible
#' @description Wrapper to silently create a plot
#' @details Stores plots as image files in the temporary workspace (See <http://stackoverflow.com/questions/20363266/how-can-i-suppress-the-creation-of-a-plot-while-calling-a-function-in-r>)
#' @param ... character plotting code to be evaluated
#' @export
#'
#' @examples
#' plot_invisible("plot(1)")
plot_invisible <- function(...){
  browser()
  ff <- tempfile()
  png(filename = ff)
  res <- eval(parse(text = ...))
  dev.off()
  unlink(ff)
  res
}

