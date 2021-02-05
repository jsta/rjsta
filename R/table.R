#' Create a pdf snapshot of a data.frame or kable output
#' 
#' @param x data.frame or kable output
#' @param out_name character file name
#' @importFrom gridExtra grid.table
#' @importFrom grDevices pdf
#' @export
#' @examples \dontrun{
#' data(iris)
#' pdf_table(iris[rep(1:5, 12),])
#' pdf_table(knitr::kable(iris[rep(1:5, 12),]), out_name = "zz2.pdf")
#' }
pdf_table <- function(x, out_name = "test.pdf"){
  if(is.data.frame(x)){
    pdf(file = out_name)
    # Only fits 25 rows of data. Warn if greater?
    gridExtra::grid.table(x, rows = rep("", nrow(x)))
    dev.off()
    system(paste0("pdfcrop ", out_name, " ", out_name))
  }
  if(is.character(x)){
    zz <- file("test.md", "w")
    sink(zz)
    cat("\\pagenumbering{gobble}")
    print(x)
    sink()
    close(zz)
    system(paste0("pandoc -s test.md -o ", out_name))
    system(paste0("pdfcrop ", out_name, " ", out_name))
    unlink("test.md")
  }
}

