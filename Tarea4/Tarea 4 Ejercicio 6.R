n <- 190
lambda = 5
TA <- rexp(n,rate=1/lambda)
tau <- cumsum(TA) #tiempos en donde ocurren los eventos 
Nt <- 1:length(tau) 
plot(c(0,tau), c(0,Nt), type="S",main="Proceso Poisson", 
     xlab = expression(tau[n]), ylab=expression(N[t])) 
