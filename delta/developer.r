#delta document

##FUNCIONES EXTRAS
##Calculo aproximado de integrales
numeric.integral<-function(data){
  x<-data$x
  y<-data$y
  dx<-diff(x)
  dy<-rowMeans(cbind(y[-1],y[-length(y)]))
  sum(dx*dy)
}

##Representacion grafica
plot.kernel<-function (data,col="red"){
  plot(data,main="Kernel density estimation",xlab="X",ylab="Density",col=col,type="l",lwd=2)
}