#' Visualize a corrr matrix with pheatmap
#' 
#' @param dt data.frame
#' @param focal_columns character vector specifying column names to focus on
#' @importFrom corrr correlate
#' @importFrom pheatmap pheatmap
#' @import dplyr
#' @importFrom grDevices colorRampPalette
#' @importFrom RColorBrewer brewer.pal
#' @importFrom rlang UQ sym
#' @export
#' @examples \dontrun{
#' data(iris)
#' dt <- iris
#' jheatmap(dt)
#' jheatmap(dt, c("Sepal.Length"))
#' }
jheatmap <- function(dt, focal_columns = NULL){
  # dt <- ggplot2::diamonds
  test <- data.frame(dt) %>%
    dplyr::select_if(is.numeric) %>%
    corrr::correlate(quiet = TRUE) %>%
    data.frame()
  
  test_rowname    <- test$rowname
  test            <- dplyr::select(test, -"rowname")
  row.names(test) <- test_rowname
  
  if(length(focal_columns) > 0){
    test <- dplyr::select(test, focal_columns)
  }
  
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

  if(length(focal_columns) == 1){
    test_rowname <- row.names(test)[order(test[,focal_columns])]
    test <- arrange(test, rlang::UQ(rlang::sym(focal_columns))) %>%
      dplyr::filter(!is.na(rlang::UQ(rlang::sym(focal_columns))))
    test_rowname <- test_rowname[test_rowname != focal_columns]
    row.names(test) <- test_rowname
  }
  if(length(focal_columns) > 0){
    suppressWarnings(pheatmap::pheatmap(test, color = hmap_cols, 
                                        cluster_rows = FALSE, 
                                        cluster_cols = FALSE))
  }else{
    pheatmap::pheatmap(t(test), color = hmap_cols)
  }
}
