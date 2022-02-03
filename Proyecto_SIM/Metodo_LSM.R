#S- El precio del activo en el tiempo cero
#r- La tasa de interés sin riesgo anualizada
#P_ejecucion- El precio de ejecución de la opción.
#sigma_1- parámetro de la distribución
#n- número de caminos simulados
#m- número de simulaciones

LSM <- function (s,P_ejecucion,sigma_1, r,n,m){
  smatriz <- fun_2(s,sigma_1,n,m)
  dmatriz <- as.matrix(ifelse(smatriz[,m]<P_ejecucion,P_ejecucion-smatriz[,m],0))
  m <- m-1
  dmatriz <- fun_1(P_ejecucion,r,smatriz,dmatriz,m)
  cont <- apply(dmatriz,1,function(x)
    ifelse(max(x)==0,1,which(x>0)[1]))
  matfinal<-dmatriz[cbind(1:n,cont)]*exp(-cont*r/50)
  return(mean(matfinal))
}

#regresión lsm, función recursiva
fun_1 <- function(P_ejecucion,r,smatriz,dmatriz,m){
  if(m==0) {return(dmatriz)}
  else{
    sumasmatriz <- sum(smatriz[,m]<P_ejecucion)
    sy <- numeric(2*sumasmatriz)
    dim(sy) <- c(sumasmatriz,2)
    ind_acept <-which(smatriz[,m]<P_ejecucion)
    dmatriz_acept <- dmatriz[ind_acept,]
    dmatriz_acept <- as.matrix(dmatriz_acept)
    sy[,1]<-smatriz[ind_acept,m]
    cont <- apply(dmatriz_acept,1,function(x)
      ifelse(max(x)==0,1,which(x>0)[1]))
    sy[,2] <-dmatriz[cbind(1:sumasmatriz,cont)]*exp(-cont*r)
    s<-sy[,1] 
    ssq<-sy[,1]^2 
    scu<-sy[,1]^3 
    sfo<-sy[,1]^4 
    sfi<-sy[,1]^5
    ssi<-sy[,1]^6 
    sse<-sy[,1]^7
    reglin<-lm(sy[,2]~(s+ssq+scu+sfo+sfi+ssi+sse)) #Regresión lineal
    
    datreg <- data.frame(
      s=smatriz[,m], ssq=smatriz[,m]^2, scu=smatriz[,m]^3, sfo=smatriz[,m]^4,
      sfi=smatriz[,m]^5, ssi=smatriz[,m]^6, sse=smatriz[,m]^7)
    
    m_aprox<-predict(reglin,datreg)
    
    nmatriz<-ifelse((smatriz[,m]<P_ejecucion)&((P_ejecucion-smatriz[,m])>m_aprox),
                    P_ejecucion-smatriz[,m],0)
    dmatriz<-cbind(nmatriz,smatriz)
    return(fun_1(P_ejecucion,r,smatriz,dmatriz,m-1))
  }
}
  
#Proceso geometrico browniano
fun_2 <- function(S,sigma_1,n,m){
    smatriz <-numeric(n*(m+1))
    dim(smatriz)<-c(n,(m+1))
    smatriz[,1]<- S
    for(j in 1:m){ a<-rnorm(n)
    mu=0.06
    smatriz[,j+1]<-smatriz[,j]*exp(((mu-(sigma_1**2/2))/50)+(sigma_1*sqrt(1/50)*a)) }
    return(smatriz[,2:(m+1)])
}

