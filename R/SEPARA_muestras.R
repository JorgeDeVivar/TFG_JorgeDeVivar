# Separar los datos
SEPARA_muestras <- function(muestray, muestrax, Pi)
{
  POS.faltantes <- is.na(muestray)
  POS.disponibles <- !POS.faltantes
  datosy.r <- muestray[POS.disponibles]
  datosx.r <- muestrax[POS.disponibles]
  Pi.r <- Pi[POS.disponibles]
  datosx.m <- muestrax[POS.faltantes]
  m <- length(datosx.m)
  list(datosy.r=datosy.r, datosx.r=datosx.r, Pi.r=Pi.r,
       m=m, datosx.m=datosx.m, POS.faltantes=POS.faltantes)
}