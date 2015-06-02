#'@name mdy2mmyyyy
#'@title convert m/d/yy to mm/dd/yyyy
#'@description Pads dates in preparation for POSIX coercion
#'@export
#'@examples
#' x <- "5/5/15"
#' mdy2mmyyyy(x)
#'
mdy2mmyyyy<-function(x){
  
  #strsplit based on "/"
  month<-strsplit(x,"/")[[1]][1]
  if(nchar(month)<2){month<-paste("0",month,sep="")}
  day<-strsplit(x,"/")[[1]][2]
  if(nchar(day)<2){day<-paste("0",day,sep="")}
  year<-strsplit(x,"/")[[1]][3]
  if(nchar(year)<3 & as.numeric(year)<80){
    year<-paste("20",year,sep="")
  }else{
    if(nchar(year)<3){
      year<-paste("19",year,sep="")
    }
  }
   
  paste(month,"/",day,"/",year,sep="")
}