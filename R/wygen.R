#'@name wygen
#'@title Generate Water Year(WY) tags based on a POSIX date and specified last month of the WY.
#'@description Generate Water Year(WY) tags based on a POSIX date and specified last month of the WY.
#'@export
#'@param dates character date that passes coercion to POSIX formatting
#'@param last_month numeric last month of water year
#'@examples
#'dates <- "2014-01-01"
#'dates <- "2015-09-14"
#'wygen(dates)

wygen <- function(dates, last_month = 4){
  if(class(dates)[1] != "POSIXct"){
  dates <- as.POSIXct(dates)
    if(all(is.na(dates))){
      stop("Cannot coerce dates to POSIX format (see ??strptime")
    }
  }
  
  year <- as.numeric(strftime(dates, format = "%Y"))
  month <- as.numeric(strftime(dates, format = "%m"))
  
  wy <- NA
  if(!is.na(month)){
    if(month > last_month){
      wy <- year + 1
    }else{
      wy <- year
    }
  }
  print(c((last_month + 1):12, 1:last_month))
  wy_month <- which(c((last_month + 1):12, 1:last_month) %in% month)
  
  list(wy = wy, wy_month = wy_month)
}