#' get_lake_wiki
#' @import WikipediR
#' @import rvest
#' @importFrom xml2 read_html
#' @export
#' @param lake_name character
#' @examples \dontrun{
#' res <- get_lake_wiki("Lake Nipigon")
#' } 
get_lake_wiki <- function(lake_name){
  res <- WikipediR::page_content("en", "wikipedia", page_name = lake_name,
                                 as_wikitext = FALSE)
  res <- res$parse$text[[1]]
  
  res <- xml2::read_html(res)
  res <- rvest::html_nodes(res, "table")
  res <- rvest::html_table(res[1])[[1]]
  
  # format coordinates ####
  coords <- res[which(res[,1] == "Coordinates"), 2]
  coords <- strsplit(coords, "\\/")[[1]]
  
  coords <- sapply(coords, function(x) strsplit(x, "Coordinates: "))
  coords <- sapply(coords, function(x) strsplit(x, " "))
  coords <- paste(unlist(coords), collapse = ",")
  coords <- strsplit(coords, ",")[[1]]
  
  coords <- coords[!(1:length(coords) %in% 
             c(which(nchar(coords) == 0),
               grep("W", coords),
               grep("N", coords))
              )][1:2]
  
  coords <- paste(gsub(";", "", coords), collapse = ",")
  res[which(res[,1] == "Coordinates"), 2] <- coords
  
  # rm junk rows
  if(length(grep("well-defined", res[,1])) != 0){
    res <- res[!(1:nrow(res) %in% grep("well-defined", res[,1])),]
    message("Shore length is not a well-defined measure.")
  }
  if(length(grep("Islands", res[,1])) != 0){
    res <- res[!(1:nrow(res) %in% grep("Islands", res[,1])),]
  }
  
  
  res
}
