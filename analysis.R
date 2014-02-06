source("base.r")

#Generacion de muestras aleatorias (usando una distribucion beta)
muestra<-random.beta()
muestra

#Generacion de la estimacion basada en el kernel triangular
triang.density<-kdensity(sample=muestra,method="triangular",n=200,b=0.005)
#Representacion grafica
x11()
plot.kernel(triang.density)
#Calculo del area (siempre deberia ser 1)
numeric.integral(triang.density)
