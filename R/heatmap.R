#' heatmap
#' 
#' @param dt data.frame
#' @importFrom corrr correlate
#' @importFrom pheatmap pheatmap
#' @import dplyr
#' @importFrom grDevices colorRampPalette
#' @importFrom RColorBrewer brewer.pal
#' @export
#' @examples \dontrun{
#' data(iris)
#' dt <- iris
#' heatmap(dt)
#' }
heatmap <- function(dt){
  # dt <- iris
  test <- dt %>%
    dplyr::select_if(is.numeric) %>%
    corrr::correlate() %>%
    data.frame()
  test_rowname    <- test$rowname
  test            <- dplyr::select(test, -"rowname")
  row.names(test) <- test_rowname
  
  hmap.palette_red <-  colorRampPalette(
    RColorBrewer::brewer.pal(n = 7, name = "Reds"))
  hmap.palette_blue <-  colorRampPalette(
    RColorBrewer::brewer.pal(n = 7, name = "Blues"))
  hmap_cols <- rev(c(rev(hmap.palette_red(10)), hmap.palette_blue(10)))
  
  pheatmap::pheatmap(t(test),
           color = hmap_cols)
}
