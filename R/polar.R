#'@name cart2pol
#'@title Convert cartographic (x,y) coordinates to polar coordinates
#'@export
#'@param x numeric coordinate
#'@param y numeric coordinate
#'@examples
#'cart2pol(1,sin(6)+2)
cart2pol<-function(x,y){
  rho<-sqrt(((x^2)+(y^2)))
  theta<-atan2(y,x)
  c(theta,rho)
}

#'@name pol2cart
#'@title Convert polar coordinates to cartographic (x,y) coordinates
#'@param theta numeric angle relative to the positive x-axis
#'@param rho numeric distance from the origin (0,0)
#'@export
#'@examples
#'pol2cart(1.0443,1.9901)
pol2cart<-function(theta,rho){
  x<-rho*cos(theta)
  y<-rho*sin(theta)
  c(x,y)
}