#'@name lookupkey
#'@title convert character vector based on lookup values
#'@param key data.frame/matrix where the first column are the strings to be corrected and the second is the "corrected" definitions.
#'@examples
#'key<-cbind(c("one","two","three","four","five"),c("one1","two2","three3","over","over"))
#'input<-c("one","two","three","four","five")
#'lookupkey(key,input)
#'
lookupkey<-function(key,input){
  from<-key[,1]
  to2<-key[,2]
  input[match(from,input)]<-to2
  input
}