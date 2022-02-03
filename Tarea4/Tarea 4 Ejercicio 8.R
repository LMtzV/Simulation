WienerGeo <- function(n,TT,a,b,S0){
  dt <- TT / n
  S <- S0
  for(i in 2:(n+1)){
    S <- append(S, S[i-1]*exp((a-b^2/2)*dt + b*sqrt(dt)*rnorm(1))) }
  return(S)
}
WienerGeo(5000,5000/12, .10, .30 ,1) #Muestra




