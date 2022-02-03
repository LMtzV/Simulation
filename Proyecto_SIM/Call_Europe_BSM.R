precio.call<- function(x,t,Tf,r,sigma,K){
  d1<- (log(x/K) + (r - 0.5 * sigma^2) * (Tf-t)) / (sigma * sqrt(Tf-t))
  d2<- d1 + sigma * sqrt(Tf-t)
  x*pnorm(d2) - K* exp(-r * (Tf-t)) * pnorm(d1)
}

set.seed(152812)

S0<-sample(100:1000,1,FALSE) #Precio del call
K<- sample(S0:1250,1,FALSE) #Precio de strike/ejecucion
r<- runif(1,0,0.08)       #Tasa libre de riesgo
Tf <- 1/2                   #Tiempo de expiracion
sigma<- runif(1,0,1)       #Volatilidad

(Call<- precio.call(x=S0, t=0, Tf=Tf, r=r, sigma=sigma, K=K))


# Por Método Monte Carlo
f<- function(x) max(0,x-K)
precio.callMC <- function(x,t,Tf,r,sigma,nsim, f){
  #Podemos usar variables antiteticas para reducir varianza
  h<- function(n){
    u<- rnorm(n/2)
    aux<- c(x* exp((r-0.5*sigma^2)*(Tf-t)+ sigma*sqrt(Tf-t)*u),
            x*exp((r-0.5*sigma^2)*(Tf-t) + sigma*sqrt(Tf -t)*(-u)))
    mean(sapply(aux, function(y) f(y)))
  }
  precio <- h(nsim)
  precio* exp(-r *(Tf-t))
  
}
 
set.seed(152812)
nsim<- 10000
(Call.MC<- precio.callMC(S0,0,Tf,r,sigma,nsim ,f=f))

require(fOptions)
(blackscholes<-
    GBSOption('c', S=S0, X=K, Time= Tf, r=  r,b= r,sigma= sigma)@price)

precios<- sapply(1:nsim, function(n){precio.callMC(S0,0,Tf,r,sigma,n=n ,f=f)}
) 
precios<- na.omit(precios)

plot(1:length(precios), precios, type = 'l', xlab = "Number of Steps", ylab = "Prices")

ggplot(mapping = aes(x=1:length(precios),y= precios))+
  geom_line(size=0.1,aes(colour='Call Monte Carlo'))+
  geom_hline(size=1,aes(yintercept = blackscholes,colour='Black-Scholes') )+
  labs(x= 'Simulaciones',y= 'Precio', 
       title = 'Determinación de Precio de Call Europea')+theme_minimal()
  

 f.call <- function(x) sapply(x, function(x) max(c(x - K, 0))) 
 f.put <- function(x) sapply(x, function(x) max(c(K - x, 0))) 
 set.seed(152812)
 K <- sample(5:55,1,FALSE)

curve(f.call, 0, 2, main = "Payoff functions", col = "blue", lty = 1, lwd = 1, ylab = expression(f(x)))
curve(f.put, 0, 2, col ="black", add = TRUE, lty = 2, lwd = 2) 
legend(0.9, 0.8, c("call", "put"), lty = c(1, 2),col = c("blue","black"), lwd = c(2, 2))

base<- ggplot()+ xlim(0,2)

base + geom_function(fun= f.call, aes(colour='Call'))+
       geom_function(fun=f.put,aes(colour='Put'), lty=2)+
       labs(x='x', y=expression(f(x)), 
            title= 'Funciones de Pago para las Opciones Call vs. Put')+
       theme_minimal()
      
Asian<-function()
{
  paths<-10
  count<-100
  C<-0
  S<-0
  K<-80
  interval<-5/count
  r<-0.05
  mean<-0.06
  sigma<-0.3
  sample<-matrix(0,nrow=(count+1),ncol=paths)
  for(i in 1:paths)
  {
    sample[1,i]<-100
    S<-0
    for(j in 2:(count+1))
    {
      S<-S+sample[j-1,i]
      sample[j,i]<-sample[j-1,i]*exp(interval*(mean-((sigma)^2)/2)+((interval)^.5)*rnorm(1)*sigma) #Expression for Geometric Brownian Motion
    }
    S<-S+sample[count+1,i]
    S<-S/(count+1)
    C<-C+(exp(-r*5)*max((S-K),0))
  }
  C<-C/paths
  cat("The Asian Option Price is :::",C)
  cat("\n")
  matplot(sample,main='Asian Option')
}

Asian()  
  


