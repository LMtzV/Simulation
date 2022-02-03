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

p.nohomogeneo <- function(lambdat){
  
  lambda <- 5 #constante que mayoriza
  i <- 1
  ss <- NULL
  while(length(ss) != 100)
  {
    TT <- rexp(i,lambda) 
    s <- cumsum(TT) 
    u <- runif(i) 
  
    ss <- s[u <= lambdat(s,i)/lambda]
    i <- i + 1
  }
  Ns <- 1:length(ss)
  plot(ss, Ns, type = "s") 
  return(list(epocas = ss, cuenta= Ns))
}

x <- p.nohomogeneo(lambdat)

