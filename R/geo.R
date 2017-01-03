
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