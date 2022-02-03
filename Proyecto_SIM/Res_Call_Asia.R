set.seed(152812)
(p.MC<- asia.MC(S0 = S0, K = K, t=0, Tf = Tf, mu = mu, sigma = s,
                  r = r, N = 250, nsim = nsim))
[1] 9.900498

require(fAsianOptions)
(p.Geman <- 
  GemanYorAsianOption("c", S = S0, X = K, Time = T, r = r, 
                      sigma = s, doprint = FALSE)$price)
[1] 10.19741
(p.Zhang <- 
  ZhangAsianOption("c", S = S0, X = K, Time = T, r = r, sigma = s,
                   table = NA, correction = TRUE, 
                   nint = 800, eps = 1e-08, dt = 1e-10))
[1] 10.72768