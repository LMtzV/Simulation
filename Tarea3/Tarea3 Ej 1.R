library(copula)
copulanorm <- normalCopula(c(2*sin(0*pi/6), 2*sin(-0.7*pi/6), 2*sin(0.2*pi/6),
                             2*sin(0.5*pi/6), 2*sin(0.4*pi/6),2*sin(0*pi/6)),
                           dim = 4, dispstr = "un")
set.seed(50)
U <- rCopula(5000, copulanorm)
round(cor(U, method = "kendall"), 2)

W <- cbind(qnorm(U[,1], mean = 4, sd = 3), qbinom(p = U[,2],size = 25, 
                                                  prob = .6),
           qbeta(p = U[,3], shape1 = 2, shape2 = 3),
           qgamma(p = U[,4], shape = 3, rate = 2))
#pairs(W, pch = 16, cex = 0.5)

par(mfrow = c(1,2))
hist(W[,1], prob = T,breaks=50) 
points(sort(W[,1]), dnorm(sort(W[,1]),4,3),type="l",col="red")
hist(W[,2], prob = T,breaks=50)
points(sort(W[,2]), dbinom(sort(W[,2]),size = 25, prob = .6),type="l",col="red")
hist(W[,3], prob = T,breaks=50)
points(sort(W[,3]), dbeta(sort(W[,3]), shape1 = 2, shape2 = 3),
       type="l",col="red")
hist(W[,4], prob = T,breaks=50)
points(sort(W[,4]), dgamma(sort(W[,4]), shape = 3, rate = 2),type="l",col="red")
