#Simulacion.   
#Ejercicio simular cadena de markov donde Xn=(X1n,X2n).
#Xin = num componentes en el armario i al tiempo n (o despues de n horas)

#Espacio de estados S = {(0,0), (0,1), (0,2), (1,0), (2,0) }.
#La matriz de probabilidades de transicion entre esos 5 estados es
#P = [.5 .35 .15 0 0; .15 .5 0 0 .35; .35 0 .5 .15 0; 0 0 .5 .5 0; 0 .3 0 0 .7]

set.seed = 1234  #para poder replicar los resultados, ya que dependen de numeros aleatorios
probs = c(.5,.35,.15,0,0,.15,.5,0,0,.35,.35,0,.5,.15,0,0,0,.5,.5,0,0,.3,0,0,.7)
P <- matrix(data=probs,nrow=5,ncol=5,byrow=TRUE)

#------------------------------------
#funcion auxiliar para ver el estado estacionario de la cadena de markov
matrizPotencia <-function(P,pot){
  resultado=P
  for (j in 2:pot){
    resultado=resultado%*%P
  }
  matrizPotencia = resultado
}
###################################
numPasos = 500
pasoActual = 0
estadoActual=c(0,0)
numEdoAct = 1
print(numEdoAct)
listaEstadosVisito = rep(0,numPasos+1)
listaEstadosVisito[1] = 1
for (pasoActual in 1:numPasos){
  #numEdoAct = numEstadoActual(estadoActual[1],estadoActual[2])    #ver si estoy en el estado 1,2,3,4 o 5
  #print(numEdoAct)
  numEdoAct = sample(1:5, 1, prob = P[numEdoAct,1:5])
  #print(numEdoAct)
  listaEstadosVisito[pasoActual] = numEdoAct
}
print(listaEstadosVisito)
proporcionApagadaMaq1 = sum(listaEstadosVisito==4)/numPasos  
#proporcion de tiempo que esta en estado 4 ( X=(2,0) )

proporcionApagadaMaq2 = sum(listaEstadosVisito==5)/numPasos  
#proporcion de tiempo que esta en estado 5 ( X=(0,2) )

print(proporcionApagadaMaq1)
print(proporcionApagadaMaq2)
#salieron 0.002 y 0.484
#ESAS SON PARA ESTA MUESTRA, LAS PROPORCIONES EXACTAS SE OBTIENEN CON EL VECTOR
#DE ESTADO ESTACIONARIO (sale de resolver (pi1,pi2,...,pi5)=(pi1,pi2,...,pi5)*P junto con pi1+...+pi5=1)
#IGUAL SE PUEDE OBTENER ELEVANDO LA MATRIZ P A UNA POTENCIA GRANDE Y VIENDO SUS COLUMNAS:
PotenciadeP = matrizPotencia(P,100)   #estoy calculando P^100
print(PotenciadeP)
#ahi se ve (columnas 4 y 5) que las probabilidades (proporciones)
#son cercanas a 0.0194 y 0.4116  (1.94% y 41.16%)



