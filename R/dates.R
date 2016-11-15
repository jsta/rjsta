#'@name mdy2mmyyyy
#'@title convert m/d/yy to mm/dd/yyyy
#'@description Pads dates in preparation for POSIX coercion
#'@param x character date to be formatted
#'@export
#'@examples
#' x <- "5/5/15"
#' y <- "6/27/1989"
#' mdy2mmyyyy(x)
mdy2mmyyyy <- function(x){
  #strsplit based on "/"
  month <- strsplit(x, "/")[[1]][1]
  if(nchar(month) < 2){
    month <- paste("0", month, sep="")
  }
  day <- strsplit(x, "/")[[1]][2]
  if(nchar(day) < 2){
    day <- paste("0", day ,sep="")
  }
  year <- strsplit(x, "/")[[1]][3]
  if(nchar(year) < 3 & as.numeric(year) < 80){
    year <- paste("20", year, sep="")
  }else{
    if(nchar(year) < 3){
      year <- paste("19", year, sep="")
    }
  }
   
  paste(month, "/", day, "/", year, sep="")
}

#'@name date456posix
#'@title Convert numeric dates in mddyy to POSIXct
#'@param x numeric where the first 1-2 digits specify the month attribute because leading zeros have been stripped
#'@param century numeric century recommended choice of "19" or "20"
#'@details Warning! Entries with a "real" two digit month but a "false" one digit day will assume that the return value should be a "false" one digit month and a "real" two digit day. For example, "10/6/2016" will return "2016-01-06". Consider using the \code{\link[jsta]{mdy2mmyyy}} function instead.
#'@seealso \code{\link[jsta]{mdy2mmyyy}}
#'@export
#'@examples
#'dates <- c("51514", "101214", "8714", "1214", "81412", "2315")
#'
#'date456posix(dates, century = "20")
date456posix <- function(x, century){
  year <- paste0(century, substring(x, (nchar(x) - 1), nchar(x)))
  day <- substring(x, (nchar(x) - 3), nchar(x) - 2)
  
  if(any(as.numeric(day) > 31)){
    day <- as.character(sapply(day, function(x){
      if(as.numeric(x) > 31){
        x <- substring(x, 2, 2)
      }
      x
    }))
  }
  
  if(any(nchar(x) < 5)){
    day <- as.character(sapply(x, function(x){
      if(nchar(x) <5){
        paste0("0", substring(x, (nchar(x) - 2), nchar(x) - 2))
      }else{
        substring(x, (nchar(x) - 3), nchar(x) - 2)
      }
    }))
  }
  
  mon <- substring(x, 1, nchar(x) - 4)
  
  if(any(nchar(mon) == 0)){
    mon[which(nchar(mon) == 0)] <- substring(x[which(nchar(mon) == 0)], 1, 1)
  }
  
  mon <- as.character(sapply(mon, function(x){
  if(as.numeric(x) < 10){
    x <- paste0("0", x)
  }else{
    x
  }
  }))
  
  date <- paste0(year, "-", mon, "-", day)
  return(as.POSIXct(strptime(date, format="%Y-%m-%d")))
}

#'@name yyyymm_expand
#'@title Expand numeric dates in yyyymm to yyyy-mm
#'@param x numeric in yyyymm format
#'@export
#'@examples
#'dates <- c("200812", "201201")
#'sapply(dates, yyyymm_expand)
yyyymm_expand <- function(x){
  strftime(as.POSIXct(strptime(paste0(substring(x, 1, 4), "-", substring(x, 5, 6), "-01"), format = "%Y-%m-%d")), "%Y-%m")
}