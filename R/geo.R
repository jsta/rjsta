
#' Convert numeric coordinate vectors in degrees, minutes, and seconds to decimal degrees
#' 
#' @param x numeric vector of length 3 corresponding to degrees, minutes, and seconds
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

#' Convert decimal degree coordinates to degrees, minutes, and seconds.
#' 
#' @param x numeric vector of length 1 corresponding to decimal degrees
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

#' Return points in spatial (sp package) polygons
#'
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

#' United States basemap
#' 
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

#' Join US state abbreviations to a data frame
#' 
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

#' Detect if a USGS gage is upstream of a lake
#' 
#' @param site_no character to preserve leading zeros
#' @param distance_threshold numeric in units of km
#' 
#' @importFrom nhdplusTools get_nldi_feature navigate_nldi
#' @importFrom nhdR nhd_plus_query
#' @importFrom sf st_coordinates st_transform st_crs st_intersection st_distance st_area st_cast
#' @importFrom units set_units as_units
#' @importFrom rlang .data
#' @export
#' 
#' @examples \dontrun{
#' site_no <- "040871473"
#' is_lake_gage(site_no)$is_lake_gage # FALSE 
#' site_no <- "05427718"
#' is_lake_gage(site_no)$is_lake_gage # TRUE
#' site_no <- "03208950"
#' is_lake_gage(site_no)
#' site_no <- "07130500"
#' is_lake_gage(site_no)
#' }
is_lake_gage  <- function(site_no, distance_threshold = 20){
  # distance_threshold <- 20
  nldi_feature    <- list(featureSource = "nwissite",
                          featureID = paste0("USGS-", site_no))
  site            <- nhdplusTools::get_nldi_feature(nldi_feature)
  stream_down     <- suppressMessages(tryCatch(
    nhdplusTools::navigate_nldi(nldi_feature, 
                                mode = "DM", distance_km = distance_threshold), 
    error = function(e) NA))
  
  if(length(stream_down) == 2 & 
     suppressWarnings(tryCatch(!is.null(stream_down$DM_flowlines), error = function(e) FALSE))){
    # get lake polygons in buffer
    poly_buffer <- suppressMessages(suppressWarnings(nhd_plus_query(
      lon = st_coordinates(site)[1], lat = st_coordinates(site)[2],
      dsn = c("NHDWaterbody", "NHDFlowLine"), buffer_dist = 0.1,
      quiet = TRUE)))
    
    # only proceed if there are *any* lakes within the distance_threshold
    real_lakes <- dplyr::filter(poly_buffer$sp$NHDWaterbody, 
        !(.data$GNIS_NAME %in% 
            c("Lake Michigan", "Lake Superior", "Lake Erie", 
              "Lake Ontario")) | 
          is.na(.data$GNIS_NAME))
    real_lakes <- dplyr::filter(real_lakes, 
                                !(.data$FTYPE %in% c("SwampMarsh")), 
                                !(.data$FCODE %in% c(39001))) # intermittent lakes
    real_lakes <- real_lakes[st_area(real_lakes) > units::as_units(4, "ha"),]
    if(any(
      units::set_units(st_distance(
      st_transform(site, nhdR:::albers_conic()), 
      st_transform(real_lakes, nhdR:::albers_conic())
      ), "km") < 
      units::as_units(distance_threshold, "km"))
      ){
      # get downstream lakes
      stream_down         <- st_transform(stream_down$DM_flowlines,
                                          st_crs(poly_buffer$sp$NHDWaterbody))
      waterbodies_down    <- suppressMessages(real_lakes[
        unlist(lapply(
          st_intersects(real_lakes, stream_down),
          function(x) length(x) > 0)),])
      waterbodies_largest <- waterbodies_down[which.max(st_area(waterbodies_down)),]
      
      pour_point <- suppressWarnings(suppressMessages(
        st_intersection(stream_down, st_cast(waterbodies_largest,
                                             "MULTILINESTRING", group_or_split = FALSE))
        ))
      pour_point <- pour_point[
        which.min(st_distance(
          st_transform(site, st_crs(pour_point)), pour_point)),]
    }else{
      pour_point <- NA
      waterbodies_down <- NA  
    }
  }else{
    pour_point <- NA
    waterbodies_down <- NA
  }
  
  is_lake_gage <- ifelse(!is.null(nrow(pour_point)), nrow(waterbodies_down) > 0, FALSE)
  
  list(pour_point = pour_point, waterbodies_down = waterbodies_down,
       site = site, is_lake_gage = is_lake_gage)
}
