
#' dms2dd
#' @description Convert numeric coordinate vectors in degrees, minutes, and seconds to decimal degrees
#' @param x numeric vector of length 3 corresonding to degrees, minutes, and seconds
#' @export
#' @examples
#' dt <- rbind(c(25,12,53.66),c(-80,32,00.61))
#' apply(dt, 1, function(x) dms2dd(x))
dms2dd <- function(x){
  if(x[1] > 0){
    x[1] + x[2]/60 + x[3]/60/60  
  }else{
    x[1] - x[2]/60 - x[3]/60/60  
  }
}

#' dd2dms
#' @description Convert decimal degree coordinates to degrees, minutes, and seconds.
#' @param x numeric vector of length 1 corresonding to decimal degrees
#' @export
#' @examples
#' dd <- 25.31015
#' dd2dms(dd)
#' 
#' dd <- -80.37198
#' dd2dms(dd)
dd2dms <- function(x){
  degrees <- trunc(x)
  minutes <- abs((x - degrees)) * 60
  seconds <- (minutes - trunc(minutes)) * 60
  minutes <- trunc(minutes)
  
  c(degrees, minutes, seconds)
}

#' point_in_poly
#' @description spatial join modified from spatialEco::point.in.poly
#' @param dt SpatialPointsDataFrame
#' @param poly_shape SpatialPolygonsDataFrame
#' @export
point_in_poly <- function(dt, poly_shape){
  dt_pp <- dt[!is.na(sp::over(dt, sp::geometry(poly_shape))),]
  dt_pp@data <- data.frame(dt_pp@data, sp::over(dt_pp, poly_shape))
  dt_pp@proj4string <- dt@proj4string
  dt_pp
}

#' usa_sf
#' @description United States basemap
#' @param crs projection string
#' @importFrom sf st_as_sf
#' @importFrom maps map
#' @importFrom dplyr filter
#' @import datasets
#' @import maptools
#' @importFrom rlang .data
#' @export
#' @examples
#' usa_sf() 
usa_sf <- function(crs){
  res <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
  state_key <- data.frame(state = datasets::state.abb,
                          ID = tolower(datasets::state.name),
                          stringsAsFactors = FALSE)
  res <- dplyr::left_join(res, state_key, by = "ID")
  dplyr::filter(res, !is.na(.data$state))
}

#' key_state
#' @param x object with an unabbreviated state column
#' @export
#' @examples 
#' key_state(data.frame(state.name = datasets::state.name, stringsAsFactors = FALSE))
key_state <- function(x){
  key <- data.frame(state.abb = datasets::state.abb, 
                    state.name = datasets::state.name, 
                    stringsAsFactors = FALSE)
  dplyr::left_join(x, key, 
                   by = c("state.name"))
}

#' Get sf objects within a source object
#' 
#' @param dt source sf object
#' @param bb bounding object
#' @importFrom sf st_within
#' @export
get_within <- function(dt, bb){
  dt[sapply(st_within(dt, bb), 
            function(x) length(x) > 0),]
}

#' Get intersecting sf subset
#' 
#' @param target target sf object
#' @param src source sf object
#' @param threshold the number of intersects required to be selected
#' @importFrom sf st_intersects
#' @export
get_intersects <- function(target, src, threshold = 0){
  target[unlist(lapply(st_intersects(target, src), 
            function(x) length(x) > threshold)),]
}
