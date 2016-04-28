#'@name wygen
#'@title Generate Water Year(WY) tags based on a May 1st - April 30th WY.
#'@export
#'@param dates character date that passes coercion to POSIX formatting
#'@examples
#'dates<-"2014-01-01"
#'wygen(dates)

wygen <- function(dates){
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
    if(month > 4){
      wy <- year + 1
    }else{
      wy <- year
    }
  }
  wy
}