METODO_NNI <- function(datosy.r, datosx.r, datosx.m, m)
{
  DONANTES.NNI <- c()
  for (j in 1:m)
  {
    Diferencias <- abs(datosx.m[j] - datosx.r)
    Dif.min <- min(Diferencias)
    POS.min <- Dif.min==Diferencias
    DONANTES <- datosy.r[POS.min]
    Num.T <- sum(POS.min)
    if (Num.T==1) DONANTES.NNI <- c(DONANTES.NNI, DONANTES)
    else DONANTES.NNI <- c(DONANTES.NNI, sample(DONANTES,1))
  }
  DONANTES.NNI
}