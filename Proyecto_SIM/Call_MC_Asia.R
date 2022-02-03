#Monte Carlo Opcion Asiatica
asia.MC<- function(S0,K,t,Tf,mu,sigma,r,N,nsim) {
  require(foreach)#Sustituye %do%
  aux<- function(x){
    #Necesitamos paquete que resuelve ecuaciones diferenciales estocasticas
    require(sde)
    #Obtenemos medias de las columnas de la matriz 
    h<- colMeans(sde.sim(S0, model = 'BS', theta = c(mu,sigma), nsim=x, N=N))
    #Condicion de pago 
    f<- function(x) max(0,x-K)
    p.init<- mean(sapply(h,f))
  }
  #separamos las operaciones que se llevan acabo simultaneamente
  nodos.visitados<- getDoParWorkers() 
  precio.asia<- foreach(y=rep(nsim/nodos.visitados,nodos.visitados),
                        .combine = 'c') %dopar% h(y) #combinamos datos
  precio.asia<- mean(precio.asia)
  precio.asia* exp(-r * (Tf-t))/1000
}

nsim<- 5000; mu<- 0.1; s<- 0.6; K<- 110; r<- 0.01; Tf<- 1; S0<-100
set.seed(152812)
(p.MC<- asia.MC(S0 = S0, K = K, t=0, Tf = Tf, mu = mu, sigma = s,
                r = r, N = 250, nsim = nsim))

require(fAsianOptions)
(p.Geman <- 
    GemanYorAsianOption("c", S = S0, X = K, Time = T, r = r, 
                        sigma = s, doprint = FALSE)$price)
(p.Zhang <- 
    ZhangAsianOption("c", S = S0, X = K, Time = T, r = r, sigma = s,
     table = NA, correction = TRUE, nint = 800, eps = 1e-08, dt = 1e-10))


(LI.pcAsia<- p.MC-pnorm(.05/2, 0,1,lower.tail = FALSE ))
(LS.pcAsia<- p.MC+pnorm(.05/2, 0,1,lower.tail = FALSE ))

precios<- sapply(1:nsim, function(n){asia.MC(S0 = S0, K = K, t=0, Tf = Tf, mu = mu, sigma = s,
                                             r = r, N = 250, n=n)}
) 
precios<- na.omit(precios)

plot(1:length(precios), precios, type = 'l', xlab = "Number of Steps", ylab = "Prices")

ggplot(mapping = aes(x=1:length(precios),y= precios))+
  geom_path(size=0.1,aes(colour='Call Monte Carlo'))+
  geom_hline(size=1,aes(yintercept = p.Zhang,colour='Black-Scholes') )+
  labs(x= 'Simulaciones',y= 'Precio', 
       title = 'Determinación de Precio de Call Europea')+theme_minimal()
var(precios)




