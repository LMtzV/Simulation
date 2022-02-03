
copula.normal1 <- ellipCopula(family = "normal", dim = 2, dispstr = "un",
                              param = 0.9)
copula.normal2 <- ellipCopula(family = "normal", dim = 2, dispstr = "un",
                              param = 0.2)

u <- rCopula(1000,copula.normal1)
v <- rCopula(1000,copula.normal2)

pairs(u,pch=16, cex=0.5)

pairs(v,pch=16, cex=0.5)

