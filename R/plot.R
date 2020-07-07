
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

#' gg_quantdot
#' 
#' @description Create a quantile dotplot in ggplot2
#' 
#' @param dt data.frame
#' @param grp grouping variable
#' @param var dependent variable
#' 
#' @importFrom ggplot2 geom_pointrange ggplot aes ylab
#' @importFrom stats quantile setNames
#' @export
#'
#' @examples \dontrun{
#' gg_quantdot(mtcars, cyl, "mpg")
#' }
gg_quantdot <- function(dt, grp, var){
  # dt <- mtcars; grp <- "cyl"; var <- "mpg"
  
  # https://tbradley1013.github.io/2018/10/01/calculating-quantiles-for-groups-with-dplyr-summarize-and-purrr-partial/
  p       <- c(0.05, 0.5, 0.95)
  p_names <- paste0(as.character(p*100), "%")
  p_funs  <- lapply(seq_along(p), function(x){
    function(...){quantile(probs = p[x], na.rm = TRUE, ...)}
    })
  p_funs <- setNames(p_funs, p_names)
  
  dt %>%
    group_by({{grp}}) %>%
    summarise_at({{var}}, p_funs) %>% 
    ggplot() + 
    geom_pointrange(aes(x = {{grp}}, y = .data$`50%`,
                      ymin = .data$`5%`, ymax = .data$`95%`)) +
    ylab(var)
}
