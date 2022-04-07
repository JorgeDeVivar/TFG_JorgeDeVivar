METODO_cohen <- function(datosy.r, Pi.r, m)
{
  r <- length(Pi.r)
  n <- r + m
  Pesos <- 1/Pi.r
  N.est <- sum(Pesos)
  Media <- (1/N.est)*sum(Pesos*datosy.r)
  Dr <- sqrt((1/N.est)*sum(Pesos*(datosy.r-Media)^2))
  m1 <- round(m/2)
  m2 <- m - m1
  Raiz <- sqrt(n+r+1)/sqrt(r-1)
  DONANTES.cohen <- c(rep(Media+Raiz*Dr,m1),rep(Media-Raiz*Dr,m2))
  DONANTES.cohen
}