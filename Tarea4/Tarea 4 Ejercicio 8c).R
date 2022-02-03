WienerGeo_mod1 <- function(n,TT,a,b,S0){
  dt <- TT / n
  S <- S0
  for(i in 2:(n+1)){
    k <- (S[i-1]*exp((a-b^2/2)*dt + b*sqrt(dt)*rnorm(1)))
    S <- append(S, k)
  }
  q <- max(S)
  for(i in 2:(n+1)){
    t <- (i-1)*dt
    S[i] <- ((S[i] - q*t)^2)/t
  }
  
  return(S)
}

X <- WienerGeo_mod(3000,3000/12, .10, .30 ,1) 

plot(X, type = "l", ylim = c(0,5000), xlab = "tiempo", ylab = expression(S[t]))
