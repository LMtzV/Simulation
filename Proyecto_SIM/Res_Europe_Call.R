#Por medio de Black Scholes
> (Call<- precio.call(x=S0, t=0, Tf=Tf, r=r, sigma=sigma, K=K))
[1] 120.0008

#Por medio de MC
> (Call.MC<- precio.callMC(S0,0,Tf,r,sigma,nsim ,f=f))
[1] 119.87785

#Por medio de Black-Scholes de fOptions
> require(fOptions)
> (blackscholes<-
   GBSOption('c', S=S0, X=K, Time= Tf, r=  r,b= r,sigma= sigma)@price)
[1] 120.0008
> 