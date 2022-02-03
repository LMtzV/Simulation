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
for (i in 1:10){
par(new=TRUE)
bg<-BGeo(30, 1, -5/24, 0.5, 1)
plot(bg, type = "l",col= i, ylim = c(0,3),
     xlab = "tiempo", ylab = expression(S[t]))
suma=suma+bg[30]
}
promedio<-suma/i
# Se puede concluir que St tiene una distribución lognormal para t grande y fija 