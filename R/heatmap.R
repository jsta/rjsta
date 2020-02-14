#' jheatmap
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
#' jheatmap(dt)
#' }
jheatmap <- function(dt){
  # dt <- ggplot2::diamonds
  test <- data.frame(dt) %>%
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
  
  frac_color <- ceiling(range(as.numeric(unlist(test)), na.rm = TRUE) * 10)
  frac_pos <- frac_color[which(frac_color > 0)]
  frac_neg <- frac_color[which(frac_color < 0)]
  
  if(length(frac_neg) == 0){
    hmap_cols <- rev(c(rev(hmap.palette_red(frac_pos))))
  }else{
    if(length(frac_pos) == 0){
      hmap_cols <- rev(c(hmap.palette_blue(abs(frac_neg))))
    }else{
      hmap_cols <- rev(c(rev(hmap.palette_red(frac_pos)), 
                         hmap.palette_blue(abs(frac_neg))))
    }
  }
  
  pheatmap::pheatmap(t(test), color = hmap_cols)
}
