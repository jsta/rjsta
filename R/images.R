
#' Convert an svg file to pdf
#'
#' @param url url
#' @param outname outname
#'
#' @export
#' @examples \dontrun{
#' svg_to_pdf(
#'   "https://img.shields.io/badge/Paper-10.1002/lno.11137-blue.svg")
#' }
svg_to_pdf <- function(url, outname = gsub(".svg", ".pdf", basename(url))) {
  # url <- "https://zenodo.org/badge/doi/10.5281/zenodo.2554212.svg"
  if (!file.exists(outname)) {
    if (require(rsvg)){
      rsvg::rsvg_pdf(url, file = outname)
    } else {
      stop("Install the 'rsvg' package to run this function.")
    }
  }
}
