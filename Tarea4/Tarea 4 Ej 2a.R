lambdat <- function(t, T0){
  for(i in 0:T0){
    if(t <= ((2*i) + 1) & t > 2*i) {
      z <- 3
      return(z)
    }
    else 
      z <- 5
  }
  return(z)
}

p.nohomogeneo <- function(lambdat,n){

  lambda <- 5 #constante que mayoriza
  TT <- rexp(n,lambda) 
  s <- cumsum(TT) 
  u <- runif(n) 
  
  ss <- s[u <= lambdat(s,n)/lambda]
  Ns <- 1:length(ss)
  plot(ss, Ns, type = "s") 
  return(list(epocas = ss, cuenta= Ns))
}
x <- p.nohomogeneo(lambdat,100)