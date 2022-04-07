METODO_regresion_aleatorio <- function(datosy.r, datosx.r, Pi.r, datosx.m)
{
  Pesos <- 1/Pi.r
  N.est <- sum(Pesos)
  Media <- (1/N.est)*sum(Pesos*datosy.r)
  Desviacion <- sqrt((1/N.est)*sum(Pesos*(datosy.r-Media)^2))
  DONANTES.regA <- METODO_regresion(datosy.r, datosx.r, Pi.r, datosx.m) + rnorm(m,0,Desviacion)
  DONANTES.regA
}

################################################################################