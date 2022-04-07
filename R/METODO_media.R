METODO_media <- function(datosy.r, Pi.r, m)
{
  Pesos <- 1/Pi.r
  N.est <- sum(Pesos)
  Media <- (1/N.est)*sum(Pesos*datosy.r)
  DONANTES.media <- rep(Media,m)
  DONANTES.media
}