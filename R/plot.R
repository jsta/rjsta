
#' plot_invisible
#' @description Wrapper to silently create a plot
#' @details Stores plots as image files in the temporary workspace (See <http://stackoverflow.com/questions/20363266/how-can-i-suppress-the-creation-of-a-plot-while-calling-a-function-in-r>)
#' @param text character plotting code to be evaluated
#' @param outpath file.path
#' @importFrom grDevices png dev.off
#' @export
#'
#' @examples \dontrun{
#' plot_invisible("plot(1)", "test.png")
#' }
plot_invisible <- function(text, outpath){
  ff <- outpath
  png(filename = ff)
  res <- eval(parse(text = text))
  dev.off()
  # unlink(ff)
  res
}

