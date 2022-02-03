f<- function(x) sapply(x,function(x) max(0,x-K))
#b ramas
#d periodos
arbol.MC <- function(b, d, S0, sigma, Tf, r) {
    ramif <- sum(b^(1:(d - 1)))
    S <- numeric(ramif + 1)
    S[1] <- S0
    dt <- Tf/d 
    for(i in 0:(ramif-b^(d-1))){
      
      for (j in 1:b) {
        S[i * b + j + 1] <- S[i + 1] * exp((r - 0.5 * sigma^2) *
                                             dt + sigma * sqrt(dt) * rnorm(1))
           } 
      } 
    S
}

BG.Sup <- function(S, b, d, f) {
  tot <- sum(b^(1:(d - 1)))
  t0<-tot-b^(d-1)+1
  tf<-tot+1
  P<-S
  P[t0:tf] <- f(S[t0:tf])
  tot1 <- sum(b^(1:(d - 2)))
  for (i in tot1:0) {
    m <- mean(P[i * b+1:b+1]) 
    v<-f(S[i+1])
    P[i + 1] <- max(v, m)
      
      } 
  P 
}

set.seed(152812)

b<- sample(5:10,1,FALSE)
d<- sample(2:5,1,FALSE)
K<- sample(100:1000,1, FALSE)
Tf<- 1
r<- runif(1,0,1)
sigma<- runif(1,.25,.5)

S0<- K+rexp(1,rate=1/2)*100
S<- arbol.MC(b,d,S0,sigma,Tf,r)
(precio.CallBG<- mean(BG.Sup(S,b,d,f)))
precioBG<-BG.Sup(S,b,d,f)
ggplot()+geom_line(alpha=0.6,
                   aes(1:length(S), y=precioBG, 
                       colour=' Metodo Broadie Glasserman'))+
  labs(x= 'Simulaciones', y='Precio', 
       title='Determinacion del Precio por Metodo Broadie Glasserman')+
  ylim(100,5000)+
  theme_minimal()

binArbol<- BinomialTreeOption(TypeFlag = 'ce', S0,K,Time=Tf,r=r,b=b, 
                              sigma=sigma, n=d)
BinomialTreePlot(binArbol, ylim=c(-6,6),xlab='Periodos',ylab='Número de Pasos',
                 main='Árbol del Call' )
