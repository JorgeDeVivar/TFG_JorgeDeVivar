# Devuelve todos los valores de la variable
REALIZA_imputacion <- function(muestray, DONANTES, POS.faltantes)
{
  muestray[POS.faltantes] <- DONANTES
  muestray
}