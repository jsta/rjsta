#' pdf_table
#' 
#' @description Create a pdf snapshot of a data.frame or kable output
#' @param x data.frame or kable output
#' @param out_name character file name
#' @importFrom gridExtra grid.table
#' @importFrom grDevices pdf
#' @export
#' @examples \dontrun{
#' data(iris)
#' pdf_table(iris[1:5,])
#' pdf_table(knitr::kable(iris[1:5,]), out_name = "zz2.pdf")
#' }
pdf_table <- function(x, out_name = "test.pdf"){
  if(is.data.frame(x)){
    pdf(file = out_name)
    gridExtra::grid.table(x, rows = rep("", nrow(x)))
    dev.off()
  }
  if(is.character(x)){
    zz <- file("test.md", "w")
    sink(zz)
    cat("\\pagenumbering{gobble}")
    print(x)
    sink()
    close(zz)
    system(paste0("pandoc -s test.md -o ", out_name))
    unlink("test.md")
  }
}

