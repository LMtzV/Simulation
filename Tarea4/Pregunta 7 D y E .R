options(width = 150, digits = 3)
BGeo <- function(n, TT, a, b, S0 = 100){
  #Función para generar un proceso Browniano Geométrico
  #n es el número de puntos de partición del intervalo [0,TT]
  #a es el drift y b la volatilidad
  dt <- TT/n #incremento de los intervalos para cubrir [0,TT]
  S <- S0 #valor inicial
  for(i in 2:(n+1)){
    S <- append(S, S[i-1]*exp((a-b^2/2)*dt + b*sqrt(dt)*rnorm(1)))
  }
  return(S)
}
suma=0
for (i in 1:100){
  par(new=TRUE)
  bg<-BGeo(30, 1, -5/24, 0.5, 1)
  #plot(bg, type = "l",col= i, ylim = c(0,3),
      # xlab = "tiempo", ylab = expression(S[t]))
  suma=suma+bg[30]
}
promedio100<-suma/i
suma1=0
for (i in 1:1000){
  par(new=TRUE)
  bg1<-BGeo(30, 1, -5/24, 0.5, 1)
  #plot(bg, type = "l",col= i, ylim = c(0,3),
  # xlab = "tiempo", ylab = expression(S[t]))
  suma1=suma1+bg1[30]
}
promedio1000<-suma1/i
#Podemos decir con claridad que cuando n tiende a infinito el promedio tiende
#a la media de una distribución lognormal
