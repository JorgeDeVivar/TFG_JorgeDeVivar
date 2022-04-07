######################M?todo de imputaci?n RHD##################################
METODO_RHD <- function(datosy.r, Pi.r, m)
{
  Pesos <- 1/Pi.r
  Prob <- Pesos/sum(Pesos)
  DONANTES.RHD <- sample(datosy.r, m,replace=T, prob=Prob)
  DONANTES.RHD
}

################################################################################