#' get_lake_wiki
#' @import WikipediR
#' @import rvest
#' @importFrom xml2 read_html
#' @export
#' @param lake_name character
#' @examples \dontrun{
#' get_lake_wiki("Lake Nipigon")
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
  
  coords <- paste(as.numeric(gsub(";", "", coords)), collapse = ",")
  res[which(res[,1] == "Coordinates"), 2] <- coords
  
  # rm junk rows
  if(any(res[,1] == "")){
    res <- res[-which(res[,1] == ""),]
  }
  if(any(nchar(res[,1]) > 20)){
    res <- res[-which(nchar(res[,1]) > 20),]
  }
  if(length(grep("well-defined", res[,1])) != 0){
    res <- res[!(1:nrow(res) %in% grep("well-defined", res[,1])),]
    message("Shore length is not a well-defined measure.")
  }
  if(length(grep("Islands", res[,1])) != 0){
    res <- res[!(1:nrow(res) %in% grep("Islands", res[,1])),]
  }
  if(length(grep("Settlements", res[,1])) != 0){
    res <- res[!(1:nrow(res) %in% grep("Settlements", res[,1])),]
  }
  
  res
}

#' map_lake_wiki
#' @param lake_name character
#' @param ... arguments passed to maps::map
#' @importFrom maps map
#' @importFrom sp coordinates
#' @importFrom graphics points
#' @export
#' @examples \dontrun{
#' map_lake_wiki("Corey Lake", "usa")
#' 
#' map_lake_wiki("Lake Nipigon", regions = "Canada")
#' }
map_lake_wiki <- function(lake_name, ...){

  res <- get_lake_wiki(lake_name)
  coords <- as.numeric(strsplit(res[which(res[,1] == "Coordinates"), 2],
              ",")[[1]])
  res <- data.frame(matrix(rev(coords), ncol = 2))
  sp::coordinates(res) <- ~X1 + X2
   
  maps::map(...)
  points(res, col = "red", cex = 1.5, pch = 19)
}