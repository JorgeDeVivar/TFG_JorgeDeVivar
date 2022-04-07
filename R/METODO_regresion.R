METODO_regresion <- function(datosy.r, datosx.r, Pi.r, datosx.m)
{
  Pesos <- 1/Pi.r
  N.est <- sum(Pesos)
  MediaY <- (1/N.est)*sum(Pesos*datosy.r)
  MediaX <- (1/N.est)*sum(Pesos*datosx.r)
  Beta.est <- sum(Pesos*(datosx.r-MediaX)*(datosy.r-MediaY))/sum(Pesos*(datosx.r-MediaX)^2)
  DONANTES.reg <- (MediaY/MediaX)*datosx.m
  DONANTES.reg
}
