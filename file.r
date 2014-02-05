##Base document
##Metodo principal
kdensity<-function(sample,method,n=200,b=length(sample)^0.2){
  #Fijar la zona en la que calcular la densidad
  m=min(sample)-2*b
  M=max(sample)+2*b
  x.v=seq(from=m,to=M,by=(M-m)/n)
  #Definir la funcion kernel a utilizar
  kernel=switch(method,
                gauss=gaussian.kernel
                )
  #Obtener la densidad
  dens.v=sapply(x.v,FUN=kernel,sample=sample,b=b)/(length(sample)*b)
  #Resultado final
  list(x=x.v,y=dens.v)
}


##Funcion Gaussiana
gaussian.kernel<-function(x,sample,b){
  u<-(x-sample)/b
  sum((1/sqrt(2*3.14159))*exp(-0.5*u^2))
}

##Representacion grafica
plot.kernel<-function (data,col="red"){
  plot(data,main="Kernel density estimation",xlab="X",ylab="Density",col=col,type="l",lwd=2)
}
