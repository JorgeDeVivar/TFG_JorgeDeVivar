METODO_razon <- function(datosy.r, datosx.r, Pi.r, datosx.m)
{
  Pesos <- 1/Pi.r
  N.est <- sum(Pesos)
  MediaY <- (1/N.est)*sum(Pesos*datosy.r)
  MediaX <- (1/N.est)*sum(Pesos*datosx.r)
  DONANTES.razon <- (MediaY/MediaX)*datosx.m
  DONANTES.razon
}
