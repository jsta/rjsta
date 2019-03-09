#' Download files from Zenodo record id
#' 
#' @param record_id record id
#' @param dest_folder destination folder for file downloads
#' @param token API key
#' 
#' @details https://github.com/zenodo/zenodo/issues/1629#issuecomment-435062462
#' 
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @export
#' 
#' @examples \dontrun{
#' download_zenodo(record_id = "2025865")#' 
#' }
download_zenodo <- function(record_id, dest_folder = "", 
                            token = Sys.getenv("ZENODO_PAT")){
  
  qry          <- paste0("https://zenodo.org/api/records/", record_id)
  
  api_response  <- httr::GET(qry, query = list(access_token = token))       
  api_response  <- jsonlite::fromJSON(httr::content(api_response, as = "text"))
  
  file_urls <- api_response$files$links$download
  
  invisible(lapply(file_urls, function(x) 
    download.file(x, 
                  file.path(dest_folder, basename(x)))))
}


