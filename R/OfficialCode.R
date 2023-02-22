# Se declaran las librerias

library(TSstudio)
library(plotly)
library(dplyr)
library(lubridate)
library(forecast)
library(readxl)
library(stats)
library(vars)
library(urca)
library(tidyverse)
library(ggplot2)
library(tseries)
library(tsbox)
library(h2o)
library(ggpubr)
library(Metrics)
library(corrplot)

source("R/plot_lm.R")
# Ruta de la consola y guardar en variable
ruta_excel <- "raw_data/OfficialData.xlsx"

# Hojas del Excel
excel_sheets(ruta_excel)

# Importar los datos de la Hoja Exterior
dato_exterior <- read_excel(ruta_excel, sheet = "Exterior")
dato_interior <- read_excel(ruta_excel, sheet = "Interior")


##############Analisis de Series Temporales######################################
# Cogemos datos cada 20 minutos, la frecuencia es 3 * 24
# Las creamos como series temporales
unidades <- length(dato_interior$Fecha)
humedad_interior_ts <- ts(data = dato_interior$`Humedad (%)`[1:unidades], start = 1, frequency = 3*24)
temperatura_interior_ts <- ts(data = dato_interior$`Temperatura (ºC)`[1:unidades], start = 1, frequency = 3*24)
humedad_exterior_ts <- ts (data = dato_exterior$`Humedad (%)`[1:unidades], start = 1, frequency = 3*24)
temperatura_exterior_ts <- ts(data = dato_exterior$`Temperatura (ºC)`[1:unidades], start = 1, frequency = 3*24)



# Imagenes de las series temporales
plot_hi_ts <- TSstudio::ts_plot(humedad_interior_ts, title = "Serie temporal de la humedad interior", Ytitle = "Humedad interior", Xtitle = "Días", Xgrid = TRUE, Ygrid = TRUE)
plot_ti_ts <- TSstudio::ts_plot(temperatura_interior_ts, title = "Serie temporal de la temperatura interior", Ytitle = "Temperatura interior", Xtitle = "Días", Xgrid = TRUE, Ygrid = TRUE)
plot_he_ts <- TSstudio::ts_plot(humedad_exterior_ts, title = "Serie temporal de la humedad exterior", Ytitle = "Humedad exterior", Xtitle = "Días", Xgrid = TRUE, Ygrid = TRUE)
plot_te_ts <- TSstudio::ts_plot(temperatura_exterior_ts, title = "Serie temporal de la temperatura exterior", Ytitle = "Temperatura exterior", Xtitle = "Días", Xgrid = TRUE, Ygrid = TRUE)


# Guardar los plots de las series temporales
plot_hi_ts
plot_ti_ts
plot_he_ts 
plot_te_ts

# Descomponer series temporales
# Aditivo
huminterior_decompose <- decompose(humedad_interior_ts)
teminterior_decompose <- decompose(temperatura_interior_ts)
humexterior_decompose <- decompose(humedad_exterior_ts)
temexterior_decompose <- decompose(temperatura_exterior_ts)

# Imagenes de las series temporales descompuestas
# Aditivo
plot_hi_decompose <- plot(huminterior_decompose)
plot_ti_decompose <- plot(teminterior_decompose)
plot_he_decompose <- plot(humexterior_decompose)
plot_te_decompose <- plot(temexterior_decompose)

# Comprobar la estacionalidad de las ts numericamente
# Tambien con esto se estudia la Autocorrelacion
ur.kpss(humedad_interior_ts) %>% summary()
ur.kpss(temperatura_interior_ts) %>% summary()
ur.kpss(humedad_exterior_ts) %>% summary()
ur.kpss(temperatura_exterior_ts) %>% summary()
# Si el valor critico de 1ptc es menor que el valor de test-statistic
# Es NO Estacionario
# Si es al contrario, es Estacionario
# La unica ts que es ESTACIONARIA es la HUMEDAD EXTERIOR

# Se comprueba con ACF y PACF
ts_cor(humedad_interior_ts, lag.max = 72) # Fuerte tendencia
ts_cor(temperatura_interior_ts, lag.max = 504) # Fuerte tendencia
ts_cor(humedad_exterior_ts, lag.max = 72) # Tendencia y estacionalidad
ts_cor(temperatura_exterior_ts, lag.max = 72) # Tendencia y estacionalidad
# Se guardan

## Lag analysis sirve para comprobar la autocorrelacion que hay entre los datos
ts_lags(temperatura_interior_ts, lags = c(1, 33, 72,144,216))

# Cross-correlation sirve para comprobar que haya relacion entre las distintas ts
correlacion <- data.frame( 
  "temperatura interior" = dato_interior$`Temperatura (ºC)`[1:unidades], 
  "humedad interior" = dato_interior$`Humedad (%)`[1:unidades], 
  "temperatura exterior" = dato_exterior$`Temperatura (ºC)`[1:unidades],
  "humedad exterior" = dato_exterior$`Humedad (%)`[1:unidades]
)

str(correlacion)
colnames(correlacion) <- c('Temperatura\nInterior', 
                           'Humedad\nInterior', 
                           'Temperatura\nExterior', 
                           'Humedad\nExterior')

M <- round(cor(correlacion), digits=3)

corrplot::corrplot.mixed(M)

######### A continuacion, se mostraran las predicciones de los datos anteriores
# Se quiere predecir los datos de la temperatura interior
ts_info(temperatura_interior_ts)
################################################################################

############################# Regresion lineal #################################
h=627
teminterior_split <- ts_split(temperatura_interior_ts, sample.out = h)
teminterior.train.ts <- teminterior_split$train
teminterior.test.ts <- teminterior_split$test

huminterior_split <- ts_split(humedad_interior_ts, sample.out = h)
huminterior.train.ts <- huminterior_split$train
huminterior.test.ts <- huminterior_split$test

temexterior_split <- ts_split(temperatura_exterior_ts, sample.out = h)
temexterior.train.ts <- teminterior_split$train
temexterior.test.ts <- teminterior_split$test

humexterior_split <- ts_split(humedad_exterior_ts, sample.out = h)
humexterior.train.ts <- humexterior_split$train
humexterior.test.ts <- humexterior_split$test

df <- tibble(ds = dato_interior$Fecha,weekday = weekdays(dato_interior$Fecha),
                     segundos = 60 * 10 * (as.numeric(rownames(dato_interior))-1),
                     dia_muestreo = 1 + (segundos - (segundos %% (3600 *12)))/(3600 *12),
                     segundos_dia_muestreo = segundos - (dia_muestreo-1) * (3600 *12),
                     y = temperatura_interior_ts)
head(df)

#df$trend <- decompose(temperatura_interior_ts)$trend
#df$seasonal <- decompose(temperatura_interior_ts)$seasonal
#df$lag72 <- dplyr::lag(as.numeric(df$y), n = 72)
#df$trend_sqr <- (decompose(temperatura_interior_ts)$trend)^2

#df$tetrend <- decompose(temperatura_exterior_ts)$trend
#df$teseasonal <- decompose(temperatura_exterior_ts)$seasonal
#df$tetrend_sqr <- (decompose(temperatura_exterior_ts)$trend^2)

#df$hitrend <- decompose(humedad_interior_ts)$trend
#df$hiseasonal <- decompose(humedad_interior_ts)$seasonal
#df$hitrend_sqr <- (decompose(humedad_interior_ts)$trend^2)

#df$hetrend <- decompose(humedad_exterior_ts)$trend
#df$heseasonal <- decompose(humedad_exterior_ts)$seasonal
#df$hetrend_sqr <- (decompose(humedad_exterior_ts)$trend^2)

train_df <- df[1:(nrow(df) - h), ]
test_df <- df[(nrow(df) - h + 1):nrow(df), ]

train_df$trend <- decompose(teminterior.train.ts)$trend
train_df$seasonal <- decompose(teminterior.train.ts)$seasonal
train_df$trend_sqr <- (decompose(teminterior.train.ts)$trend)^2
train_df$lag72 <- dplyr::lag(as.numeric(teminterior.train.ts), n = 72)

train_df$hitrend <- decompose(huminterior.train.ts)$trend
train_df$hiseasonal <- decompose(huminterior.train.ts)$seasonal
train_df$hitrend_sqr <- (decompose(huminterior.train.ts)$trend)^2

train_df$tetrend <- decompose(temexterior.train.ts)$trend
train_df$teseasonal <- decompose(temexterior.train.ts)$seasonal
train_df$tetrend_sqr <- (decompose(temexterior.train.ts)$trend)^2

train_df$hetrend <- decompose(humexterior.train.ts)$trend
train_df$heseasonal <- decompose(humexterior.train.ts)$seasonal
train_df$hetrend_sqr <- (decompose(humexterior.train.ts)$trend)^2

test_df$trend <- decompose(teminterior.test.ts)$trend
test_df$seasonal <- decompose(teminterior.test.ts)$seasonal
test_df$trend_sqr <- (decompose(teminterior.test.ts)$trend)^2
test_df$lag72 <- dplyr::lag(as.numeric(teminterior.test.ts), n = 72)

test_df$hitrend <- decompose(huminterior.test.ts)$trend
test_df$hiseasonal <- decompose(huminterior.test.ts)$seasonal
test_df$hitrend_sqr <- (decompose(huminterior.test.ts)$trend)^2

test_df$tetrend <- decompose(temexterior.test.ts)$trend
test_df$teseasonal <- decompose(temexterior.test.ts)$seasonal
test_df$tetrend_sqr <- (decompose(temexterior.test.ts)$trend)^2

test_df$hetrend <- decompose(humexterior.test.ts)$trend
test_df$heseasonal <- decompose(humexterior.test.ts)$seasonal
test_df$hetrend_sqr <- (decompose(humexterior.test.ts)$trend)^2

#### UNIVARIABLE
######## TEMPERATURA INTERIOR
# TENDENCIA
md1_1 <- tslm(train ~ trend, data = train_df)
checkresiduals(md1_1)

md2_1 <- auto.arima(train,
                  xreg = cbind(train_df$trend),
                  seasonal = TRUE,
                  stepwise = TRUE,
                  approximation = FALSE)
summary(md2_1)
checkresiduals(md2_1)
fc1_1 <- forecast(md1_1, newdata = test_df)
fc2_1 <- forecast(md2_1, xreg = cbind(test_df$trend))
forecast::accuracy(fc1_1, test)
forecast::accuracy(fc2_1, test)

plot(fc2_1)

#ESTACIONALIDAD
md1_2 <- tslm(train ~ seasonal, data = train_df)
checkresiduals(md1_2)

md2_2 <- auto.arima(train,
                    xreg = cbind(train_df$seasonal),
                    seasonal = TRUE,
                    stepwise = TRUE,
                    approximation = FALSE)
summary(md2_2)
checkresiduals(md2_2)
fc1_2 <- forecast(md1_2, newdata = test_df)
fc2_2 <- forecast(md2_2, xreg = cbind(test_df$seasonal))
forecast::accuracy(fc1_2, test)
forecast::accuracy(fc2_2, test)

plot(fc2_2)

# LAG72

md1_3 <- tslm(train ~ lag72, data = train_df)
checkresiduals(md1_3)

md2_3 <- auto.arima(train,
                    xreg = cbind(train_df$lag72),
                    seasonal = TRUE,
                    stepwise = TRUE,
                    approximation = FALSE)
summary(md2_3)
checkresiduals(md2_3)
fc1_3 <- forecast(md1_3, newdata = test_df)
fc2_3 <- forecast(md2_3, xreg = cbind(test_df$lag72))
forecast::accuracy(fc1_3, test)
forecast::accuracy(fc2_3, test)

plot(fc2_3)

####### MULTIVARIABLE
# TI TENDENCIA Y ESTACIONALIDAD
md1_4 <- tslm(train ~ trend + seasonal, data = train_df)
checkresiduals(md1_4)

md2_4 <- auto.arima(train,
                  xreg = cbind(train_df$trend,
                               train_df$seasonal),
                  seasonal = TRUE,
                  stepwise = TRUE,
                  approximation = FALSE)
summary(md2_4)
checkresiduals(md2_4)
fc1_4 <- forecast(md1_4, newdata = test_df)

test_model_4 <- cbind(test_df$trend,
                    test_df$seasonal)

fc2_4 <- forecast(md2_4, xreg = na.omit(test_model_4))
forecast::accuracy(fc1_4, test)
forecast::accuracy(fc2_4, test)
plot(fc2_4)

# TI TENDENCIA Y LAG72
md1_5 <- tslm(train ~ trend + lag72, data = train_df)
checkresiduals(md1_5)

md2_5 <- auto.arima(train,
                    xreg = cbind(train_df$trend,
                                 train_df$lag72),
                    seasonal = TRUE,
                    stepwise = TRUE,
                    approximation = FALSE)
summary(md2_5)
checkresiduals(md2_5)
fc1_5 <- forecast(md1_5, newdata = test_df)

test_model_5 <- cbind(test_df$trend,
                      test_df$lag72)

fc2_5 <- forecast(md2_5, xreg = na.omit(test_model_5))
forecast::accuracy(fc1_5, test)
forecast::accuracy(fc2_5, test)
plot(fc2_5)

# TI ESTACIONALIDAD Y LAG72
md1_6 <- tslm(train ~ seasonal + lag72, data = train_df)
checkresiduals(md1_6)

md2_6 <- auto.arima(train,
                    xreg = cbind(train_df$seasonal,
                                 train_df$lag72),
                    seasonal = TRUE,
                    stepwise = TRUE,
                    approximation = FALSE)
summary(md2_6)
checkresiduals(md2_6)
fc1_6 <- forecast(md1_6, newdata = test_df)

test_model_6 <- cbind(test_df$seasonal,
                      test_df$lag72)

fc2_6 <- forecast(md2_6, xreg = na.omit(test_model_6))
forecast::accuracy(fc1_6, test)
forecast::accuracy(fc2_6, test)
plot(fc2_6)

# TI TENDENCIA, ESTACIONALIDAD Y LAG72
md1_7 <- tslm(train ~ trend+ seasonal + lag72, data = train_df)
checkresiduals(md1_7)

md2_7 <- auto.arima(train,
                    xreg = cbind(train_df$trend,
                                 train_df$seasonal,
                                 train_df$lag72),
                    seasonal = TRUE,
                    stepwise = TRUE,
                    approximation = FALSE)
summary(md2_7)
checkresiduals(md2_7)
fc1_7 <- forecast(md1_7, newdata = test_df)

test_model_7 <- cbind(test_df$trend,
                      test_df$seasonal,
                      test_df$lag72)

fc2_7 <- forecast(md2_7, xreg = na.omit(test_model_7))
forecast::accuracy(fc1_7, test)
forecast::accuracy(fc2_7, test)
plot(fc2_7)

# TI TENDENCIA, ESTACIONALIDAD, LAG72 Y TENDENCIA CUADRADO
md1_8 <- tslm(train ~ trend + seasonal + lag72 + trend_sqr, data = train_df)
checkresiduals(md1_8)

md2_8 <- auto.arima(train,
                    xreg = cbind(train_df$trend,
                                 train_df$seasonal,
                                 train_df$lag72,
                                 train_df$trend_sqr),
                    seasonal = TRUE,
                    stepwise = TRUE,
                    approximation = FALSE)
summary(md2_8)
checkresiduals(md2_8)
fc1_8 <- forecast(md1_8, newdata = test_df)

test_model_8 <- cbind(test_df$trend,
                      test_df$seasonal,
                      test_df$lag72,
                      test_df$trend_sqr)

fc2_8 <- forecast(md2_8, xreg = na.omit(test_model_8))
forecast::accuracy(fc1_8, test)
forecast::accuracy(fc2_8, test)
plot(fc2_8)

# TI TENDENCIA, ESTACIONALIDAD, LAG72 Y TENDENCIA CUADRADO
# HI TENDENCIA
md1_9 <- tslm(train ~ trend + seasonal + lag72 + trend_sqr + hitrend, 
              data = train_df)
checkresiduals(md1_9)

md2_9 <- auto.arima(train,
                    xreg = cbind(train_df$trend,
                                 train_df$seasonal,
                                 train_df$lag72,
                                 train_df$trend_sqr,
                                 train_df$hitrend),
                    seasonal = TRUE,
                    stepwise = TRUE,
                    approximation = FALSE)
summary(md2_9)
checkresiduals(md2_9)
fc1_9 <- forecast(md1_9, newdata = test_df)

test_model_9 <- cbind(test_df$trend,
                      test_df$seasonal,
                      test_df$lag72,
                      test_df$trend_sqr,
                      test_df$hitrend)

fc2_9 <- forecast(md2_9, xreg = na.omit(test_model_9))
forecast::accuracy(fc1_9, test)
forecast::accuracy(fc2_9, test)
plot(fc2_9)

# TI TENDENCIA, ESTACIONALIDAD, LAG72 Y TENDENCIA CUADRADO
# HI TENDENCIA, ESTACIONALIDAD
md1_10 <- tslm(train ~ trend + seasonal + lag72 + trend_sqr + hitrend + hiseasonal, 
               data = train_df)
checkresiduals(md1_10)

md2_10 <- auto.arima(train,
                     xreg = cbind(train_df$trend,
                                  train_df$seasonal,
                                  train_df$lag72,
                                  train_df$trend_sqr,
                                  train_df$hitrend,
                                  train_df$hiseasonal),
                     seasonal = TRUE,
                     stepwise = TRUE,
                     approximation = FALSE)
summary(md2_10)
checkresiduals(md2_10)
fc1_10 <- forecast(md1_10, newdata = test_df)

test_model_10 <- cbind(test_df$trend,
                       test_df$seasonal,
                       test_df$lag72,
                       test_df$trend_sqr,
                       test_df$hitrend,
                       test_df$hiseasonal)

fc2_10 <- forecast(md2_10, xreg = na.omit(test_model_10))
forecast::accuracy(fc1_10, test)
forecast::accuracy(fc2_10, test)
plot(fc2_10)

# TI TENDENCIA, ESTACIONALIDAD, LAG72 Y TENDENCIA CUADRADO
# HI TENDENCIA, ESTACIONALIDAD Y TENDENCIA CUADRADO
md1_11 <- tslm(train ~ trend + seasonal + lag72 + trend_sqr + hitrend + hiseasonal + hitrend_sqr, 
               data = train_df)
checkresiduals(md1_11)

md2_11 <- auto.arima(train,
                     xreg = cbind(train_df$trend,
                                  train_df$seasonal,
                                  train_df$lag72,
                                  train_df$trend_sqr,
                                  train_df$hitrend,
                                  train_df$hiseasonal,
                                  train_df$hitrend_sqr),
                     seasonal = TRUE,
                     stepwise = TRUE,
                     approximation = FALSE)
summary(md2_11)
checkresiduals(md2_11)
fc1_11 <- forecast(md1_11, newdata = test_df)

test_model_11 <- cbind(test_df$trend,
                       test_df$seasonal,
                       test_df$lag72,
                       test_df$trend_sqr,
                       test_df$hitrend,
                       test_df$hiseasonal,
                       test_df$hitrend_sqr)

fc2_11 <- forecast(md2_11, xreg = na.omit(test_model_11))
forecast::accuracy(fc1_11, test)
forecast::accuracy(fc2_11, test)
plot(fc2_11)

# TI TENDENCIA, ESTACIONALIDAD, LAG72 Y TENDENCIA CUADRADO
# TE TENDENCIA
md1_12 <- tslm(train ~ trend + seasonal + lag72 + trend_sqr + tetrend, 
               data = train_df)
checkresiduals(md1_12)

md2_12 <- auto.arima(train,
                     xreg = cbind(train_df$trend,
                                  train_df$seasonal,
                                  train_df$lag72,
                                  train_df$trend_sqr,
                                  train_df$tetrend),
                     seasonal = TRUE,
                     stepwise = TRUE,
                     approximation = FALSE)
summary(md2_12)
checkresiduals(md2_12)
fc1_12 <- forecast(md1_12, newdata = test_df)

test_model_12 <- cbind(test_df$trend,
                       test_df$seasonal,
                       test_df$lag72,
                       test_df$trend_sqr,
                       test_df$tetrend)

fc2_12 <- forecast(md2_12, xreg = na.omit(test_model_12))
forecast::accuracy(fc1_12, test)
forecast::accuracy(fc2_12, test)
plot(fc2_12)

# TI TENDENCIA, ESTACIONALIDAD, LAG72 Y TENDENCIA CUADRADO
# TE TENDENCIA, ESTACIONALIDAD
md1_13 <- tslm(train ~ trend + seasonal + lag72 + trend_sqr + tetrend + teseasonal, 
               data = train_df)
checkresiduals(md1_13)

md2_13 <- auto.arima(train,
                     xreg = cbind(train_df$trend,
                                  train_df$seasonal,
                                  train_df$lag72,
                                  train_df$trend_sqr,
                                  train_df$tetrend,
                                  train_df$teseasonal),
                     seasonal = TRUE,
                     stepwise = TRUE,
                     approximation = FALSE)
summary(md2_13)
checkresiduals(md2_13)
fc1_13 <- forecast(md1_13, newdata = test_df)

test_model_13 <- cbind(test_df$trend,
                       test_df$seasonal,
                       test_df$lag72,
                       test_df$trend_sqr,
                       test_df$tetrend,
                       test_df$teseasonal)

fc2_13 <- forecast(md2_13, xreg = na.omit(test_model_13))
forecast::accuracy(fc1_13, test)
forecast::accuracy(fc2_13, test)
plot(fc2_13)

# TI TENDENCIA, ESTACIONALIDAD, LAG72 Y TENDENCIA CUADRADO
# TE TENDENCIA, ESTACIONALIDAD Y TENDENCIA CUADRADO
md1_14 <- tslm(train ~ trend + seasonal + lag72 + trend_sqr + tetrend + teseasonal + tetrend_sqr, 
               data = train_df)
checkresiduals(md1_14)

md2_14 <- auto.arima(train,
                     xreg = cbind(train_df$trend,
                                  train_df$seasonal,
                                  train_df$lag72,
                                  train_df$trend_sqr,
                                  train_df$tetrend,
                                  train_df$teseasonal,
                                  train_df$tetrend_sqr),
                     seasonal = TRUE,
                     stepwise = TRUE,
                     approximation = FALSE)
summary(md2_14)
checkresiduals(md2_14)
fc1_14 <- forecast(md1_14, newdata = test_df)

test_model_14 <- cbind(test_df$trend,
                       test_df$seasonal,
                       test_df$lag72,
                       test_df$trend_sqr,
                       test_df$tetrend,
                       test_df$teseasonal,
                       test_df$tetrend_sqr)

fc2_14 <- forecast(md2_14, xreg = na.omit(test_model_14))
forecast::accuracy(fc1_14, test)
forecast::accuracy(fc2_14, test)
plot(fc2_14)

# TI TENDENCIA, ESTACIONALIDAD, LAG72 Y TENDENCIA CUADRADO
# HE TENDENCIA
md1_15 <- tslm(train ~ trend + seasonal + lag72 + trend_sqr + hetrend, 
               data = train_df)
checkresiduals(md1_15)

md2_15 <- auto.arima(train,
                     xreg = cbind(train_df$trend,
                                  train_df$seasonal,
                                  train_df$lag72,
                                  train_df$trend_sqr,
                                  train_df$hetrend),
                     seasonal = TRUE,
                     stepwise = TRUE,
                     approximation = FALSE)
summary(md2_15)
checkresiduals(md2_15)
fc1_15 <- forecast(md1_15, newdata = test_df)

test_model_15 <- cbind(test_df$trend,
                       test_df$seasonal,
                       test_df$lag72,
                       test_df$trend_sqr,
                       test_df$hetrend)

fc2_15 <- forecast(md2_15, xreg = na.omit(test_model_15))
forecast::accuracy(fc1_15, test)
forecast::accuracy(fc2_15, test)
plot(fc2_15)

# TI TENDENCIA, ESTACIONALIDAD, LAG72 Y TENDENCIA CUADRADO
# HE TENDENCIA, ESTACIONALIDAD
md1_16 <- tslm(train ~ trend + seasonal + lag72 + trend_sqr + hetrend + heseasonal, 
               data = train_df)
checkresiduals(md1_16)

md2_16 <- auto.arima(train,
                     xreg = cbind(train_df$trend,
                                  train_df$seasonal,
                                  train_df$lag72,
                                  train_df$trend_sqr,
                                  train_df$hetrend,
                                  train_df$heseasonal),
                     seasonal = TRUE,
                     stepwise = TRUE,
                     approximation = FALSE)
summary(md2_16)
checkresiduals(md2_16)
fc1_16 <- forecast(md1_16, newdata = test_df)

test_model_16 <- cbind(test_df$trend,
                       test_df$seasonal,
                       test_df$lag72,
                       test_df$trend_sqr,
                       test_df$hetrend,
                       test_df$heseasonal)

fc2_16 <- forecast(md2_16, xreg = na.omit(test_model_16))
forecast::accuracy(fc1_16, test)
forecast::accuracy(fc2_16, test)
plot(fc2_16)

# TI TENDENCIA, ESTACIONALIDAD, LAG72 Y TENDENCIA CUADRADO
# HE TENDENCIA, ESTACIONALIDAD Y TENDENCIA CUADRADO
md1_17 <- tslm(train ~ trend + seasonal + lag72 + trend_sqr + hetrend + heseasonal + hetrend_sqr, 
               data = train_df)
checkresiduals(md1_17)

md2_17 <- auto.arima(train,
                     xreg = cbind(train_df$trend,
                                  train_df$seasonal,
                                  train_df$lag72,
                                  train_df$trend_sqr,
                                  train_df$hetrend,
                                  train_df$heseasonal,
                                  train_df$hetrend_sqr),
                     seasonal = TRUE,
                     stepwise = TRUE,
                     approximation = FALSE)
summary(md2_17)
checkresiduals(md2_17)
fc1_17 <- forecast(md1_17, newdata = test_df)

test_model_17 <- cbind(test_df$trend,
                       test_df$seasonal,
                       test_df$lag72,
                       test_df$trend_sqr,
                       test_df$hetrend,
                       test_df$heseasonal,
                       test_df$hetrend_sqr)

fc2_17 <- forecast(md2_17, xreg = na.omit(test_model_17))
forecast::accuracy(fc1_17, test)
forecast::accuracy(fc2_17, test)
plot(fc2_17)

# TI TENDENCIA, ESTACIONALIDAD, LAG72 Y TENDENCIA CUADRADO
# HI TENDENCIA, ESTACIONALIDAD Y TENDENCIA CUADRADO
# TE TENDENCIA, ESTACIONALIDAD Y TENDENCIA CUADRADO
# HE TENDENCIA, ESTACIONALIDAD Y TENDENCIA CUADRADO

md1_18 <- tslm(train ~ trend + seasonal + lag72 + trend_sqr + hitrend + hiseasonal + hitrend_sqr + tetrend + teseasonal + tetrend_sqr + hetrend + heseasonal + hetrend_sqr, 
               data = train_df)
checkresiduals(md1_18)

md2_18 <- auto.arima(train,
                     xreg = cbind(train_df$trend,
                                  train_df$seasonal,
                                  train_df$lag72,
                                  train_df$trend_sqr,
                                  train_df$hitrend,
                                  train_df$hiseasonal,
                                  train_df$hitrend_sqr,
                                  train_df$tetrend,
                                  train_df$teseasonal,
                                  train_df$tetrend_sqr,
                                  train_df$hetrend,
                                  train_df$heseasonal,
                                  train_df$hetrend_sqr),
                     seasonal = TRUE,
                     stepwise = TRUE,
                     approximation = FALSE)
summary(md2_18)
checkresiduals(md2_18)
fc1_18 <- forecast(md1_18, newdata = test_df)

test_model_18 <- cbind(test_df$trend,
                       test_df$seasonal,
                       test_df$lag72,
                       test_df$trend_sqr,
                       test_df$hitrend,
                       test_df$hiseasonal,
                       test_df$hitrend_sqr,
                       test_df$tetrend,
                       test_df$teseasonal,
                       test_df$tetrend_sqr,
                       test_df$hetrend,
                       test_df$heseasonal,
                       test_df$hetrend_sqr)

fc2_18 <- forecast(md2_18, xreg = na.omit(test_model_18))
forecast::accuracy(fc1_18, test)
forecast::accuracy(fc2_18, test)
plot(fc2_18)
################################################################################

##################### ARIMA ####################################################
# Serie temporal de la temperatura interior
ti_split <- ts_split(temperatura_interior_ts, sample.out = 627)

ti_train <- ti_split$train
ti_test <- ti_split$test

ti_auto_md1 <- auto.arima(ti_train)

ti_test_auto1 <- forecast(ti_auto_md1, h = 627)
forecast::accuracy(ti_test_auto1, ti_test)
checkresiduals(ti_auto_md1)

ti_auto_md2 <- auto.arima(ti_train,
                             max.order = 5,
                             D = 1,
                             d = 1,
                             stepwise = FALSE,
                             approximation = FALSE)

ti_test_auto2 <- forecast(ti_auto_md2, h = 627)
accuracy(ti_test_auto2, ti_test)

plot(ti_test_auto1)
plot(ti_test_auto2)
# Serie temporal de la temperatura interior en logaritmo
ti_split_log <- ts_split(h2o::log(temperatura_interior_ts), sample.out = 627)


ti_train_log <- ti_split_log$train
ti_test_log <- ti_split_log$test

ti_auto_md1_log <- auto.arima(ti_train_log)

ti_test_auto1_log <- forecast(ti_auto_md1_log, h = 627)
accuracy(ti_test_auto1_log, ti_test_log)

ti_auto_md2_log <- auto.arima(ti_train,
                          max.order = 5,
                          D = 1,
                          d = 1,
                          stepwise = FALSE,
                          approximation = FALSE)

ti_test_auto2_log <- forecast(ti_auto_md2_log, h = 627)
accuracy(ti_test_auto2_log, ti_test_log)

plot(ti_test_auto1_log)
plot(ti_test_auto2_log)
##############################################################################

##########################ML Gradient Boost###################################
########################## UNIVARIABLE
temint_dfMLGB <- tibble(date = dato_interior$Fecha,
                        segundos = 60 * 10 * (as.numeric(rownames(dato_interior))-1),
                        day = 1 + (segundos - (segundos %% (3600 *12)))/(3600 *12),
                        y = temperatura_interior_ts)
#temint_dfMLGB$lag72 <- dplyr::lag(as.numeric(temint_dfMLGB$y), n = 72)

#temint_dfMLGB$trend <- decompose(temperatura_interior_detrend)$trend
#temint_dfMLGB$seasonal <- decompose(temperatura_interior_detrend)$seasonal

h = 627

teminterior_split <- ts_split(temperatura_interior_ts, sample.out = h)
teminterior.train.ts <- teminterior_split$train
teminterior.test.ts <- teminterior_split$test

temint_train_MLGB<- temint_dfMLGB[1:(unidades - h), ]
temint_test_MLGB<- temint_dfMLGB[(unidades - h + 1):unidades, ]

temint_train_MLGB$trend <- decompose(teminterior.train.ts)$trend
temint_train_MLGB$seasonal <- decompose(teminterior.train.ts)$seasonal
temint_train_MLGB$trend_sqr <- (decompose(teminterior.train.ts)$trend)^2
temint_train_MLGB$lag72 <- dplyr::lag(as.numeric(teminterior.train.ts), n = 72)

temint_test_MLGB$trend <- decompose(teminterior.test.ts)$trend
temint_test_MLGB$seasonal <- decompose(teminterior.test.ts)$seasonal
temint_test_MLGB$trend_sqr <- (decompose(teminterior.test.ts)$trend)^2
temint_test_MLGB$lag72 <- dplyr::lag(as.numeric(teminterior.test.ts), n = 72)

temint_train_MLGB$trend <- as.vector(temint_train_MLGB$trend)
temint_train_MLGB$seasonal <- as.vector(temint_train_MLGB$seasonal)
temint_train_MLGB$trend_sqr <- as.vector(temint_train_MLGB$trend_sqr)
temint_train_MLGB$lag72 <- as.vector(temint_train_MLGB$lag72)
temint_test_MLGB$trend <- as.vector(temint_test_MLGB$trend)
temint_test_MLGB$seasonal <- as.vector(temint_test_MLGB$seasonal)
temint_test_MLGB$trend_sqr <- as.vector(temint_test_MLGB$trend_sqr)
temint_test_MLGB$lag72 <- as.vector(temint_test_MLGB$lag72)

h2o.init(max_mem_size = "16G")

train_h_MLGB <- as.h2o(temint_train_MLGB)
test_h_MLGB <- as.h2o(temint_test_MLGB)

### TENDENCIA

x <- c("trend")
y <- "y"

gbm_md1 <- h2o.gbm(
  training_frame = train_h_MLGB,
  nfolds = 5,
  x = x,
  y = y,
  max_depth = 20,
  distribution = "gaussian",
  ntrees = 500,
  learn_rate = 0.1,
  score_each_iteration = TRUE
)
test_h_MLGB$pred_gbm <- h2o.predict(gbm_md1, test_h_MLGB)
test_1 <- as.data.frame(test_h_MLGB)

plot_ly(data = test_1) %>%
  add_lines(x = ~ date, y = ~ y, name = "Actual") %>%
  add_lines(x = ~ date, y = ~ pred_gbm, name = "Gradient Boosting Machine", 
            line = list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Prediction (Gradient
Boosting Machine Trend)",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Día"))

sqrt( sum( (test_1$y - test_1$pred_gbm)^2 , na.rm = TRUE ) / nrow(test_1) )
sum((test_1$y - test_1$pred_gbm) , na.rm = TRUE) / nrow(test_1)
sum(abs((test_1$y - test_1$pred_gbm)/ nrow(test_1)) , na.rm = TRUE)  #MAPE

### ESTACIONALIDAD
x <- c("seasonal")
y <- "y"

gbm_md2 <- h2o.gbm(
  training_frame = train_h_MLGB,
  nfolds = 5,
  x = x,
  y = y,
  max_depth = 20,
  distribution = "gaussian",
  ntrees = 500,
  learn_rate = 0.1,
  score_each_iteration = TRUE
)
test_h_MLGB_seasonal$pred_gbm <- h2o.predict(gbm_md2, test_h_MLGB)
test_2 <- as.data.frame(test_h_MLGB)

plot_ly(data = test_2) %>%
  add_lines(x = ~ date, y = ~ y, name = "Actual") %>%
  add_lines(x = ~ date, y = ~ pred_gbm, name = "Gradient Boosting Machine", 
            line = list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Prediction (Gradient
Boosting Machine Seasonal)",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Día"))

sqrt( sum( (test_2$y - test_2$pred_gbm)^2 , na.rm = TRUE ) / nrow(test_2) )
sum((test_2$y - test_2$pred_gbm) , na.rm = TRUE) / nrow(test_2)
sum(abs((test_2$y - test_2$pred_gbm)/ nrow(test_2)) , na.rm = TRUE)  #MAPE

### LAG 72
x <- c("lag72")
y <- "y"

gbm_md3 <- h2o.gbm(
  training_frame = train_h_MLGB,
  nfolds = 5,
  x = x,
  y = y,
  max_depth = 20,
  distribution = "gaussian",
  ntrees = 500,
  learn_rate = 0.1,
  score_each_iteration = TRUE
)
test_h_MLGB_lag$pred_gbm <- h2o.predict(gbm_md3, test_h_MLGB_lag)
test_3 <- as.data.frame(test_h_MLGB_lag)

plot_ly(data = test_3) %>%
  add_lines(x = ~ date, y = ~ y, name = "Actual") %>%
  add_lines(x = ~ date, y = ~ pred_gbm, name = "Gradient Boosting Machine", 
            line = list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Prediction (Gradient
Boosting Machine Seasonal)",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Día"))

sqrt( sum( (test_3$y - test_3$pred_gbm)^2 , na.rm = TRUE ) / nrow(test_3) )
sum((test_3$y - test_3$pred_gbm) , na.rm = TRUE) / nrow(test_3)
sum(abs((test_3$y - test_3$pred_gbm)/ nrow(test_3)) , na.rm = TRUE)  #MAPE

########################## MULTIVARIABLE
teminterior_split <- ts_split(temperatura_interior_ts, sample.out = h)
teminterior.train.ts <- teminterior_split$train
teminterior.test.ts <- teminterior_split$test

huminterior_split <- ts_split(humedad_interior_ts, sample.out = h)
huminterior.train.ts <- huminterior_split$train
huminterior.test.ts <- huminterior_split$test

temexterior_split <- ts_split(temperatura_exterior_ts, sample.out = h)
temexterior.train.ts <- teminterior_split$train
temexterior.test.ts <- teminterior_split$test

humexterior_split <- ts_split(humedad_exterior_ts, sample.out = h)
humexterior.train.ts <- humexterior_split$train
humexterior.test.ts <- humexterior_split$test

temint_df_MMLGB <- tibble(date = dato_interior$Fecha,
                          segundos = 60 * 10 * (as.numeric(rownames(dato_interior))-1),
                          semana_muestreo = 1 + (segundos - (segundos %% (3600 *24*7)))/(3600 *24*7),
                          segundo_muestreo_semana = segundos - (semana_muestreo-1) * (3600 *24*7),
                          day = 1 + (segundos - (segundos %% (3600 *24)))/(3600 *24),
                          y = temperatura_interior_ts,
                          z = temperatura_exterior_ts,
                          w = humedad_interior_ts,
                          v = humedad_exterior_ts)
head(temint_df_MMLGB)

# temint_df_MMLGB$lag72 <- dplyr::lag(as.numeric(temint_df_MMLGB$y), n = 72)
# 
# temint_df_MMLGB$trend <- decompose(temperatura_interior_ts)$trend
# temint_df_MMLGB$seasonal <- decompose(temperatura_interior_ts)$seasonal
# temint_df_MMLGB$trend_sqr <- (decompose(temperatura_interior_ts)$trend^2)
# 
# temint_df_MMLGB$tetrend <- decompose(temperatura_exterior_ts)$trend
# temint_df_MMLGB$teseasonal <- decompose(temperatura_exterior_ts)$seasonal
# temint_df_MMLGB$tetrend_sqr <- (decompose(temperatura_exterior_ts)$trend ^ 2)
# 
# temint_df_MMLGB$hitrend <- decompose(humedad_interior_ts)$trend
# temint_df_MMLGB$hiseasonal <- decompose(humedad_interior_ts)$seasonal
# temint_df_MMLGB$hitrend_sqr <- (decompose(humedad_interior_ts)$trend ^ 2)
# 
# temint_df_MMLGB$hetrend <- decompose(humedad_exterior_ts)$trend
# temint_df_MMLGB$heseasonal <- decompose(humedad_exterior_ts)$seasonal
# temint_df_MMLGB$hetrend_sqr <- (decompose(humedad_exterior_ts)$trend ^ 2)

h <- 627


teminterior_split <- ts_split(temperatura_interior_ts, sample.out = h)
teminterior.train.ts <- teminterior_split$train
teminterior.test.ts <- teminterior_split$test

temint_train_MMLGB<- temint_df_MMLGB[1:(unidades - h), ]
temint_test_MMLGB<- temint_df_MMLGB[(unidades - h + 1):unidades, ]

temint_train_MMLGB$trend <- decompose(teminterior.train.ts)$trend
temint_train_MMLGB$seasonal <- decompose(teminterior.train.ts)$seasonal
temint_train_MMLGB$trend_sqr <- (decompose(teminterior.train.ts)$trend)^2
temint_train_MMLGB$lag72 <- dplyr::lag(as.numeric(teminterior.train.ts), n = 72)

temint_train_MMLGB$hitrend <- decompose(huminterior.train.ts)$trend
temint_train_MMLGB$hiseasonal <- decompose(huminterior.train.ts)$seasonal
temint_train_MMLGB$hitrend_sqr <- (decompose(huminterior.train.ts)$trend)^2

temint_train_MMLGB$tetrend <- decompose(temexterior.train.ts)$trend
temint_train_MMLGB$teseasonal <- decompose(temexterior.train.ts)$seasonal
temint_train_MMLGB$tetrend_sqr <- (decompose(temexterior.train.ts)$trend)^2

temint_train_MMLGB$hetrend <- decompose(humexterior.train.ts)$trend
temint_train_MMLGB$heseasonal <- decompose(humexterior.train.ts)$seasonal
temint_train_MMLGB$hetrend_sqr <- (decompose(humexterior.train.ts)$trend)^2

temint_test_MMLGB$trend <- decompose(teminterior.test.ts)$trend
temint_test_MMLGB$seasonal <- decompose(teminterior.test.ts)$seasonal
temint_test_MMLGB$trend_sqr <- (decompose(teminterior.test.ts)$trend)^2
temint_test_MMLGB$lag72 <- dplyr::lag(as.numeric(teminterior.test.ts), n = 72)

temint_test_MMLGB$hitrend <- decompose(huminterior.test.ts)$trend
temint_test_MMLGB$hiseasonal <- decompose(huminterior.test.ts)$seasonal
temint_test_MMLGB$hitrend_sqr <- (decompose(huminterior.test.ts)$trend)^2

temint_test_MMLGB$tetrend <- decompose(temexterior.test.ts)$trend
temint_test_MMLGB$teseasonal <- decompose(temexterior.test.ts)$seasonal
temint_test_MMLGB$tetrend_sqr <- (decompose(temexterior.test.ts)$trend)^2

temint_test_MMLGB$hetrend <- decompose(humexterior.test.ts)$trend
temint_test_MMLGB$heseasonal <- decompose(humexterior.test.ts)$seasonal
temint_test_MMLGB$hetrend_sqr <- (decompose(humexterior.test.ts)$trend)^2

temint_train_MMLGB$trend <- as.vector(temint_train_MMLGB$trend)
temint_train_MMLGB$seasonal <- as.vector(temint_train_MMLGB$seasonal)
temint_train_MMLGB$trend_sqr <- as.vector(temint_train_MMLGB$trend_sqr)
temint_train_MMLGB$lag72 <- as.vector(temint_train_MMLGB$lag72)
temint_train_MMLGB$hitrend <- as.vector(temint_train_MMLGB$hitrend)
temint_train_MMLGB$hiseasonal <- as.vector(temint_train_MMLGB$hiseasonal)
temint_train_MMLGB$hitrend_sqr <- as.vector(temint_train_MMLGB$hitrend_sqr)
temint_train_MMLGB$tetrend <- as.vector(temint_train_MMLGB$tetrend)
temint_train_MMLGB$teseasonal <- as.vector(temint_train_MMLGB$teseasonal)
temint_train_MMLGB$tetrend_sqr <- as.vector(temint_train_MMLGB$tetrend_sqr)
temint_train_MMLGB$hetrend <- as.vector(temint_train_MMLGB$hetrend)
temint_train_MMLGB$heseasonal <- as.vector(temint_train_MMLGB$heseasonal)
temint_train_MMLGB$hetrend_sqr <- as.vector(temint_train_MMLGB$hetrend_sqr)
temint_test_MMLGB$trend <- as.vector(temint_test_MMLGB$trend)
temint_test_MMLGB$seasonal <- as.vector(temint_test_MMLGB$seasonal)
temint_test_MMLGB$trend_sqr <- as.vector(temint_test_MMLGB$trend_sqr)
temint_test_MMLGB$lag72 <- as.vector(temint_test_MMLGB$lag72)
temint_test_MMLGB$hitrend <- as.vector(temint_test_MMLGB$hitrend)
temint_test_MMLGB$hiseasonal <- as.vector(temint_test_MMLGB$hiseasonal)
temint_test_MMLGB$hitrend_sqr <- as.vector(temint_test_MMLGB$hitrend_sqr)
temint_test_MMLGB$tetrend <- as.vector(temint_test_MMLGB$tetrend)
temint_test_MMLGB$teseasonal <- as.vector(temint_test_MMLGB$teseasonal)
temint_test_MMLGB$tetrend_sqr <- as.vector(temint_test_MMLGB$tetrend_sqr)
temint_test_MMLGB$hetrend <- as.vector(temint_test_MMLGB$hetrend)
temint_test_MMLGB$heseasonal <- as.vector(temint_test_MMLGB$heseasonal)
temint_test_MMLGB$hetrend_sqr <- as.vector(temint_test_MMLGB$hetrend_sqr)

h2o.init(max_mem_size = "16G")

train_h_MMLGB <- as.h2o(temint_train_MMLGB)
test_h_MMLGB <- as.h2o(temint_test_MMLGB)

############ TEMPERATURA INTERIOR
#### TENDENCIA Y ESTACIONALIDAD
x <- c("trend", "seasonal")
y <- "y"
gbm_md11 <- h2o.gbm(
  training_frame = train_h_MMLGB,
  nfolds = 5,
  x = x,
  y = y,
  max_depth = 20,
  distribution = "gaussian",
  ntrees = 500,
  learn_rate = 0.1,
  score_each_iteration = TRUE
)
par(mar=c(0,0,0,0))
h2o.varimp_plot(gbm_md11)
test_h_MMLGB$pred_gbm <- h2o.predict(gbm_md11, test_h_MMLGB)
test_11 <- as.data.frame(test_h_MMLGB)
plot_ly(data = test_11) %>%
  add_lines(x = ~ date, y = ~y, name = "Actual") %>%
  add_lines(x = ~ date, y = ~ pred_gbm, name = "Gradient Boosting Machine",
            line = list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Multivar Gradient
Boosting Machine",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Día"))

sqrt( sum( (test_11$y - test_11$pred_gbm)^2 , na.rm = TRUE ) / nrow(test_11) )
sum((test_11$y - test_11$pred_gbm) , na.rm = TRUE) / nrow(test_11)
sum(abs((test_11$y - test_11$pred_gbm)/ nrow(test_11)) , na.rm = TRUE)  #MAPE

#### TENDENCIA Y LAG72
x <- c("trend", "lag72")
y <- "y"
gbm_md12 <- h2o.gbm(
  training_frame = train_h_MMLGB,
  nfolds = 5,
  x = x,
  y = y,
  max_depth = 20,
  distribution = "gaussian",
  ntrees = 500,
  learn_rate = 0.1,
  score_each_iteration = TRUE
)
par(mar=c(0,0,0,0))
h2o.varimp_plot(gbm_md12)
test_h_MMLGB$pred_gbm <- h2o.predict(gbm_md12, test_h_MMLGB)
test_12 <- as.data.frame(test_h_MMLGB)
plot_ly(data = test_12) %>%
  add_lines(x = ~ date, y = ~y, name = "Actual") %>%
  add_lines(x = ~ date, y = ~ pred_gbm, name = "Gradient Boosting Machine",
            line = list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Multivar Gradient
Boosting Machine",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Día"))

sqrt( sum( (test_12$y - test_12$pred_gbm)^2 , na.rm = TRUE ) / nrow(test_12) )
sum((test_12$y - test_12$pred_gbm) , na.rm = TRUE) / nrow(test_12)
sum(abs((test_12$y - test_12$pred_gbm)/ nrow(test_12)) , na.rm = TRUE)  #MAPE

#### TENDENCIA, ESTACIONALIDAD Y LAG72
x <- c("trend", "seasonal","lag72")
y <- "y"
gbm_md13 <- h2o.gbm(
  training_frame = train_h_MMLGB,
  nfolds = 5,
  x = x,
  y = y,
  max_depth = 20,
  distribution = "gaussian",
  ntrees = 500,
  learn_rate = 0.1,
  score_each_iteration = TRUE
)
par(mar=c(0,0,0,0))
h2o.varimp_plot(gbm_md13)
test_h_MMLGB$pred_gbm <- h2o.predict(gbm_md13, test_h_MMLGB)
test_13 <- as.data.frame(test_h_MMLGB)
plot_ly(data = test_13) %>%
  add_lines(x = ~ date, y = ~y, name = "Actual") %>%
  add_lines(x = ~ date, y = ~ pred_gbm, name = "Gradient Boosting Machine",
            line = list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Multivar Gradient
Boosting Machine",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Día"))

sqrt( sum( (test_13$y - test_13$pred_gbm)^2 , na.rm = TRUE ) / nrow(test_13) )
sum((test_13$y - test_13$pred_gbm) , na.rm = TRUE) / nrow(test_13)
sum(abs((test_13$y - test_13$pred_gbm)/ nrow(test_13)) , na.rm = TRUE)  #MAPE

#### TI TENDENCIA, ESTACIONALIDAD, LAG72 Y TENDENCIA CUADRADO
x <- c("trend", "seasonal","lag72", "trend_sqr")
y <- "y"
gbm_md14 <- h2o.gbm(
  training_frame = train_h_MMLGB,
  nfolds = 5,
  x = x,
  y = y,
  max_depth = 20,
  distribution = "gaussian",
  ntrees = 500,
  learn_rate = 0.1,
  score_each_iteration = TRUE
)
par(mar=c(0,0,0,0))
h2o.varimp_plot(gbm_md14)
test_h_MMLGB$pred_gbm <- h2o.predict(gbm_md14, test_h_MMLGB)
test_14 <- as.data.frame(test_h_MMLGB)
plot_ly(data = test_14) %>%
  add_lines(x = ~ date, y = ~y, name = "Actual") %>%
  add_lines(x = ~ date, y = ~ pred_gbm, name = "Gradient Boosting Machine",
            line = list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Multivar Gradient
Boosting Machine",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Día"))

sqrt( sum( (test_14$y - test_14$pred_gbm)^2 , na.rm = TRUE ) / nrow(test_14) )
sum((test_14$y - test_14$pred_gbm) , na.rm = TRUE) / nrow(test_14)
sum(abs((test_14$y - test_14$pred_gbm)/ nrow(test_14)) , na.rm = TRUE)  #MAPE

#### TI TENDENCIA, ESTACIONALIDAD, LAG72 Y TENDENCIA CUADRADO.
# HI TENDENCIA
x <- c("trend", "seasonal","lag72", "trend_sqr", "hitrend")
y <- "y"
gbm_md15 <- h2o.gbm(
  training_frame = train_h_MMLGB,
  nfolds = 5,
  x = x,
  y = y,
  max_depth = 20,
  distribution = "gaussian",
  ntrees = 500,
  learn_rate = 0.1,
  score_each_iteration = TRUE
)
par(mar=c(0,0,0,0))
h2o.varimp_plot(gbm_md15)
test_h_MMLGB$pred_gbm <- h2o.predict(gbm_md15, test_h_MMLGB)
test_15 <- as.data.frame(test_h_MMLGB)
plot_ly(data = test_15) %>%
  add_lines(x = ~ date, y = ~y, name = "Actual") %>%
  add_lines(x = ~ date, y = ~ pred_gbm, name = "Gradient Boosting Machine",
            line = list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Multivar Gradient
Boosting Machine",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Día"))

sqrt( sum( (test_15$y - test_15$pred_gbm)^2 , na.rm = TRUE ) / nrow(test_15) )
sum((test_15$y - test_15$pred_gbm) , na.rm = TRUE) / nrow(test_15)
sum(abs((test_15$y - test_15$pred_gbm)/ nrow(test_15)) , na.rm = TRUE)  #MAPE

#### TI TENDENCIA, ESTACIONALIDAD, LAG72 Y TENDENCIA CUADRADO.
# HI TENDENCIA, ESTACIONALIDAD
x <- c("trend", "seasonal","lag72", "trend_sqr", "hitrend", "hiseasonal")
y <- "y"
gbm_md16 <- h2o.gbm(
  training_frame = train_h_MMLGB,
  nfolds = 5,
  x = x,
  y = y,
  max_depth = 20,
  distribution = "gaussian",
  ntrees = 500,
  learn_rate = 0.1,
  score_each_iteration = TRUE
)
par(mar=c(0,0,0,0))
h2o.varimp_plot(gbm_md16)
test_h_MMLGB$pred_gbm <- h2o.predict(gbm_md16, test_h_MMLGB)
test_16 <- as.data.frame(test_h_MMLGB)
plot_ly(data = test_16) %>%
  add_lines(x = ~ date, y = ~y, name = "Actual") %>%
  add_lines(x = ~ date, y = ~ pred_gbm, name = "Gradient Boosting Machine",
            line = list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Multivar Gradient
Boosting Machine",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Día"))

sqrt( sum( (test_16$y - test_16$pred_gbm)^2 , na.rm = TRUE ) / nrow(test_16) )
sum((test_16$y - test_16$pred_gbm) , na.rm = TRUE) / nrow(test_16)
sum(abs((test_16$y - test_16$pred_gbm)/ nrow(test_16)) , na.rm = TRUE)  #MAPE

#### TI TENDENCIA, ESTACIONALIDAD, LAG72 Y TENDENCIA CUADRADO.
# HI TENDENCIA, ESTACIONALIDAD Y TENDENCIA CUADRADO
x <- c("trend", "seasonal","lag72", "trend_sqr", "hitrend", "hiseasonal", "hitrend_sqr")
y <- "y"
gbm_md17 <- h2o.gbm(
  training_frame = train_h_MMLGB,
  nfolds = 5,
  x = x,
  y = y,
  max_depth = 20,
  distribution = "gaussian",
  ntrees = 500,
  learn_rate = 0.1,
  score_each_iteration = TRUE
)
par(mar=c(0,0,0,0))
h2o.varimp_plot(gbm_md17)
test_h_MMLGB$pred_gbm <- h2o.predict(gbm_md17, test_h_MMLGB)
test_17 <- as.data.frame(test_h_MMLGB)
plot_ly(data = test_17) %>%
  add_lines(x = ~ date, y = ~y, name = "Actual") %>%
  add_lines(x = ~ date, y = ~ pred_gbm, name = "Gradient Boosting Machine",
            line = list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Multivar Gradient
Boosting Machine",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Día"))

sqrt( sum( (test_17$y - test_17$pred_gbm)^2 , na.rm = TRUE ) / nrow(test_17) )
sum((test_17$y - test_17$pred_gbm) , na.rm = TRUE) / nrow(test_17)
sum(abs((test_17$y - test_17$pred_gbm)/ nrow(test_17)) , na.rm = TRUE)  #MAPE

#### TI TENDENCIA, ESTACIONALIDAD, LAG72 Y TENDENCIA CUADRADO.
# TE TENDENCIA
x <- c("trend", "seasonal","lag72", "trend_sqr", "tetrend")
y <- "y"
gbm_md18 <- h2o.gbm(
  training_frame = train_h_MMLGB,
  nfolds = 5,
  x = x,
  y = y,
  max_depth = 20,
  distribution = "gaussian",
  ntrees = 500,
  learn_rate = 0.1,
  score_each_iteration = TRUE
)
par(mar=c(0,0,0,0))
h2o.varimp_plot(gbm_md18)
test_h_MMLGB$pred_gbm <- h2o.predict(gbm_md18, test_h_MMLGB)
test_18 <- as.data.frame(test_h_MMLGB)
plot_ly(data = test_18) %>%
  add_lines(x = ~ date, y = ~y, name = "Actual") %>%
  add_lines(x = ~ date, y = ~ pred_gbm, name = "Gradient Boosting Machine",
            line = list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Multivar Gradient
Boosting Machine",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Día"))

sqrt( sum( (test_18$y - test_18$pred_gbm)^2 , na.rm = TRUE ) / nrow(test_18) )
sum((test_18$y - test_18$pred_gbm) , na.rm = TRUE) / nrow(test_18)
sum(abs((test_18$y - test_18$pred_gbm)/ nrow(test_18)) , na.rm = TRUE)  #MAPE

#### TI TENDENCIA, ESTACIONALIDAD, LAG72 Y TENDENCIA CUADRADO.
# TE TENDENCIA, ESTACIONALIDAD
x <- c("trend", "seasonal","lag72", "trend_sqr", "tetrend", "teseasonal")
y <- "y"
gbm_md19 <- h2o.gbm(
  training_frame = train_h_MMLGB,
  nfolds = 5,
  x = x,
  y = y,
  max_depth = 20,
  distribution = "gaussian",
  ntrees = 500,
  learn_rate = 0.1,
  score_each_iteration = TRUE
)
par(mar=c(0,0,0,0))
h2o.varimp_plot(gbm_md19)
test_h_MMLGB$pred_gbm <- h2o.predict(gbm_md19, test_h_MMLGB)
test_19 <- as.data.frame(test_h_MMLGB)
plot_ly(data = test_19) %>%
  add_lines(x = ~ date, y = ~y, name = "Actual") %>%
  add_lines(x = ~ date, y = ~ pred_gbm, name = "Gradient Boosting Machine",
            line = list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Multivar Gradient
Boosting Machine",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Día"))

sqrt( sum( (test_19$y - test_19$pred_gbm)^2 , na.rm = TRUE ) / nrow(test_19) )
sum((test_19$y - test_19$pred_gbm) , na.rm = TRUE) / nrow(test_19)
sum(abs((test_19$y - test_19$pred_gbm)/ nrow(test_19)) , na.rm = TRUE)  #MAPE

#### TI TENDENCIA, ESTACIONALIDAD, LAG72 Y TENDENCIA CUADRADO.
# TE TENDENCIA, ESTACIONALIDAD Y TENDENCIA CUADRADO
x <- c("trend", "seasonal","lag72", "trend_sqr", "tetrend", "teseasonal", "tetrend_sqr")
y <- "y"
gbm_md20 <- h2o.gbm(
  training_frame = train_h_MMLGB,
  nfolds = 5,
  x = x,
  y = y,
  max_depth = 20,
  distribution = "gaussian",
  ntrees = 500,
  learn_rate = 0.1,
  score_each_iteration = TRUE
)
par(mar=c(0,0,0,0))
h2o.varimp_plot(gbm_md20)
test_h_MMLGB$pred_gbm <- h2o.predict(gbm_md20, test_h_MMLGB)
test_20 <- as.data.frame(test_h_MMLGB)
plot_ly(data = test_20) %>%
  add_lines(x = ~ date, y = ~y, name = "Actual") %>%
  add_lines(x = ~ date, y = ~ pred_gbm, name = "Gradient Boosting Machine",
            line = list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Multivar Gradient
Boosting Machine",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Día"))

sqrt( sum( (test_20$y - test_20$pred_gbm)^2 , na.rm = TRUE ) / nrow(test_20) )
sum((test_20$y - test_20$pred_gbm) , na.rm = TRUE) / nrow(test_20)
sum(abs((test_20$y - test_20$pred_gbm)/ nrow(test_20)) , na.rm = TRUE)  #MAPE

#### TI TENDENCIA, ESTACIONALIDAD, LAG72 Y TENDENCIA CUADRADO.
# HE TENDENCIA
x <- c("trend", "seasonal","lag72", "trend_sqr", "hetrend")
y <- "y"
gbm_md21 <- h2o.gbm(
  training_frame = train_h_MMLGB,
  nfolds = 5,
  x = x,
  y = y,
  max_depth = 20,
  distribution = "gaussian",
  ntrees = 500,
  learn_rate = 0.1,
  score_each_iteration = TRUE
)
par(mar=c(0,0,0,0))
h2o.varimp_plot(gbm_md21)
test_h_MMLGB$pred_gbm <- h2o.predict(gbm_md21, test_h_MMLGB)
test_21 <- as.data.frame(test_h_MMLGB)
plot_ly(data = test_21) %>%
  add_lines(x = ~ date, y = ~y, name = "Actual") %>%
  add_lines(x = ~ date, y = ~ pred_gbm, name = "Gradient Boosting Machine",
            line = list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Multivar Gradient
Boosting Machine",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Día"))

sqrt( sum( (test_21$y - test_21$pred_gbm)^2 , na.rm = TRUE ) / nrow(test_21) )
sum((test_21$y - test_21$pred_gbm) , na.rm = TRUE) / nrow(test_21)
sum(abs((test_21$y - test_21$pred_gbm)/ nrow(test_21)) , na.rm = TRUE)  #MAPE

#### TI TENDENCIA, ESTACIONALIDAD, LAG72 Y TENDENCIA CUADRADO.
# HE TENDENCIA, ESTACIONALIDAD
x <- c("trend", "seasonal","lag72", "trend_sqr", "hetrend", "heseasonal")
y <- "y"
gbm_md22 <- h2o.gbm(
  training_frame = train_h_MMLGB,
  nfolds = 5,
  x = x,
  y = y,
  max_depth = 20,
  distribution = "gaussian",
  ntrees = 500,
  learn_rate = 0.1,
  score_each_iteration = TRUE
)
par(mar=c(0,0,0,0))
h2o.varimp_plot(gbm_md22)
test_h_MMLGB$pred_gbm <- h2o.predict(gbm_md22, test_h_MMLGB)
test_22 <- as.data.frame(test_h_MMLGB)
plot_ly(data = test_22) %>%
  add_lines(x = ~ date, y = ~y, name = "Actual") %>%
  add_lines(x = ~ date, y = ~ pred_gbm, name = "Gradient Boosting Machine",
            line = list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Multivar Gradient
Boosting Machine",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Día"))

sqrt( sum( (test_22$y - test_22$pred_gbm)^2 , na.rm = TRUE ) / nrow(test_22) )
sum((test_22$y - test_22$pred_gbm) , na.rm = TRUE) / nrow(test_22)
sum(abs((test_22$y - test_22$pred_gbm)/ nrow(test_22)) , na.rm = TRUE)  #MAPE

#### TI TENDENCIA, ESTACIONALIDAD, LAG72 Y TENDENCIA CUADRADO.
# HE TENDENCIA, ESTACIONALIDAD Y TENDENCIA CUADRADO
x <- c("trend", "seasonal","lag72", "trend_sqr", "hetrend", "heseasonal", "hetrend_sqr")
y <- "y"
gbm_md23 <- h2o.gbm(
  training_frame = train_h_MMLGB,
  nfolds = 5,
  x = x,
  y = y,
  max_depth = 20,
  distribution = "gaussian",
  ntrees = 500,
  learn_rate = 0.1,
  score_each_iteration = TRUE
)
par(mar=c(0,0,0,0))
h2o.varimp_plot(gbm_md23)
test_h_MMLGB$pred_gbm <- h2o.predict(gbm_md23, test_h_MMLGB)
test_23 <- as.data.frame(test_h_MMLGB)
plot_ly(data = test_23) %>%
  add_lines(x = ~ date, y = ~y, name = "Actual") %>%
  add_lines(x = ~ date, y = ~ pred_gbm, name = "Gradient Boosting Machine",
            line = list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Multivar Gradient
Boosting Machine",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Día"))

sqrt( sum( (test_23$y - test_23$pred_gbm)^2 , na.rm = TRUE ) / nrow(test_23) )
sum((test_23$y - test_23$pred_gbm) , na.rm = TRUE) / nrow(test_23)
sum(abs((test_23$y - test_23$pred_gbm)/ nrow(test_23)) , na.rm = TRUE)  #MAPE

#### TI TENDENCIA, ESTACIONALIDAD, LAG72 Y TENDENCIA CUADRADO.
# HI TENDENCIA, ESTACIONALIDAD Y TENDENCIA CUADRADO
# TE TENDENCIA, ESTACIONALIDAD Y TENDENCIA CUADRADO
# HE TENDENCIA, ESTACIONALIDAD Y TENDENCIA CUADRADO
x <- c("trend", "seasonal","lag72", "trend_sqr", 
       "hetrend", "heseasonal", "hetrend_sqr",
       "tetrend", "teseasonal", "tetrend_sqr",
       "hitrend", "hiseasonal", "hitrend_sqr")
y <- "y"
gbm_md24 <- h2o.gbm(
  training_frame = train_h_MMLGB,
  nfolds = 5,
  x = x,
  y = y,
  max_depth = 20,
  distribution = "gaussian",
  ntrees = 500,
  learn_rate = 0.1,
  score_each_iteration = TRUE
)
par(mar=c(0,0,0,0))
h2o.varimp_plot(gbm_md24)
test_h_MMLGB$pred_gbm <- h2o.predict(gbm_md24, test_h_MMLGB)
test_24 <- as.data.frame(test_h_MMLGB)
plot_ly(data = test_24) %>%
  add_lines(x = ~ date, y = ~y, name = "Actual") %>%
  add_lines(x = ~ date, y = ~ pred_gbm, name = "Gradient Boosting Machine",
            line = list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Multivar Gradient
Boosting Machine",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Día"))

sqrt( sum( (test_24$y - test_24$pred_gbm)^2 , na.rm = TRUE ) / nrow(test_24) )
sum((test_24$y - test_24$pred_gbm) , na.rm = TRUE) / nrow(test_24)
sum(abs((test_24$y - test_24$pred_gbm)/ nrow(test_24)) , na.rm = TRUE)  #MAPE

##############################################################################

#########################ML AutoML############################################
################## UNIVARIABLE
temint_dfMLAuto <- tibble(date = dato_interior$Fecha,
                        segundos = 60 * 10 * (as.numeric(rownames(dato_interior))-1),
                        day = 1 + (segundos - (segundos %% (3600 *12)))/(3600 *12),
                        y = temperatura_interior_ts)
# temint_dfMLAuto$lag72 <- dplyr::lag(as.numeric(temint_dfMLAuto$y), n = 72)
# 
# temint_dfMLAuto$trend <- decompose(temperatura_interior_ts)$trend
# temint_dfMLAuto$seasonal <- decompose(temperatura_interior_ts)$seasonal

h <- 627

teminterior_split <- ts_split(temperatura_interior_ts, sample.out = h)
teminterior.train.ts <- teminterior_split$train
teminterior.test.ts <- teminterior_split$test

temint_train_MLAuto<- temint_dfMLAuto[1:(unidades - h), ]
temint_test_MLAuto<- temint_dfMLAuto[(unidades - h + 1):unidades, ]

temint_train_MLAuto$trend <- decompose(teminterior.train.ts)$trend
temint_train_MLAuto$seasonal <- decompose(teminterior.train.ts)$seasonal
temint_train_MLAuto$trend_sqr <- (decompose(teminterior.train.ts)$trend)^2
temint_train_MLAuto$lag72 <- dplyr::lag(as.numeric(teminterior.train.ts), n = 72)

temint_test_MLAuto$trend <- decompose(teminterior.test.ts)$trend
temint_test_MLAuto$seasonal <- decompose(teminterior.test.ts)$seasonal
temint_test_MLAuto$trend_sqr <- (decompose(teminterior.test.ts)$trend)^2
temint_test_MLAuto$lag72 <- dplyr::lag(as.numeric(teminterior.test.ts), n = 72)

temint_train_MLAuto$trend <- as.vector(temint_train_MLAuto$trend)
temint_train_MLAuto$seasonal <- as.vector(temint_train_MLAuto$seasonal)
temint_train_MLAuto$trend_sqr <- as.vector(temint_train_MLAuto$trend_sqr)
temint_train_MLAuto$lag72 <- as.vector(temint_train_MLAuto$lag72)
temint_test_MLAuto$trend <- as.vector(temint_test_MLAuto$trend)
temint_test_MLAuto$seasonal <- as.vector(temint_test_MLAuto$seasonal)
temint_test_MLAuto$trend_sqr <- as.vector(temint_test_MLAuto$trend_sqr)
temint_test_MLAuto$lag72 <- as.vector(temint_test_MLAuto$lag72)

h2o.init(max_mem_size = "16G")

train_h_MLAuto <- as.h2o(temint_train_MLAuto)
test_h_MLAuto <- as.h2o(temint_test_MLAuto)

### TENDENCIA
x <- c("trend")
y <- "y"

autoML5 <- h2o.automl(training_frame = train_h_MLAuto,
                      x = x,
                      y = y,
                      nfolds = 5,
                      max_runtime_secs = 60*20,
                      seed = 1234)

test_h_MLAuto$pred_autoML <- h2o.predict(autoML5@leader, test_h_MLAuto)
test_5 <- as.data.frame(test_h_MLAuto)
plot_ly(data = test_5) %>%
  add_lines(x = ~ date, y = ~y, name = "Actual") %>%
  add_lines(x = ~ date, y = ~ pred_autoML, name = "autoML") %>%
  layout(title = "Temperatura interior - Actual vs. Prediction (Auto ML
Model)",
         yaxis = list(title = "Thousands of Units"),
         xaxis = list(title = "Month"))

sqrt( sum( (test_5$y - test_5$pred_autoML)^2 , na.rm = TRUE ) / nrow(test_5) )
sum((test_5$y - test_5$pred_autoML) , na.rm = TRUE) / nrow(test_5)
sum(abs((test_5$y - test_5$autoML)/ nrow(test_5)) , na.rm = TRUE)  #MAPE

### ESTACIONALIDAD
x <- c("seasonal")
y <- "y"
autoML6 <- h2o.automl(training_frame = train_h_MLAuto,
                      x = x,
                      y = y,
                      nfolds = 5,
                      max_runtime_secs = 60*20,
                      seed = 1234)

test_h_MLAuto$pred_autoML <- h2o.predict(autoML6@leader, test_h_MLAuto)
test_6 <- as.data.frame(test_h_MLAuto)
plot_ly(data = test_6) %>%
  add_lines(x = ~ date, y = ~y, name = "Actual") %>%
  add_lines(x = ~ date, y = ~ pred_autoML, name = "autoML") %>%
  layout(title = "Temperatura interior - Actual vs. Prediction (Auto ML
Model)",
         yaxis = list(title = "Thousands of Units"),
         xaxis = list(title = "Month"))

sqrt( sum( (test_6$y - test_6$pred_autoML)^2 , na.rm = TRUE ) / nrow(test_6) )
sum((test_6$y - test_6$pred_autoML) , na.rm = TRUE) / nrow(test_6)
sum(abs((test_6$y - test_6$autoML)/ nrow(test_6)) , na.rm = TRUE)  #MAPE

### LAG 72
x <- c("lag72")
y <- "y"
autoML7 <- h2o.automl(training_frame = train_h_MLAuto,
                      x = x,
                      y = y,
                      nfolds = 5,
                      max_runtime_secs = 60*20,
                      seed = 1234)

test_h_MLAuto$pred_autoML <- h2o.predict(autoML7@leader, test_h_MLAuto)
test_7 <- as.data.frame(test_h_MLAuto)
plot_ly(data = test_7) %>%
  add_lines(x = ~ date, y = ~y, name = "Actual") %>%
  add_lines(x = ~ date, y = ~ pred_autoML, name = "autoML") %>%
  layout(title = "Temperatura interior - Actual vs. Prediction (Auto ML
Model)",
         yaxis = list(title = "Thousands of Units"),
         xaxis = list(title = "Month"))

sqrt( sum( (test_7$y - test_7$pred_autoML)^2 , na.rm = TRUE ) / nrow(test_7) )
sum((test_7$y - test_7$pred_autoML) , na.rm = TRUE) / nrow(test_7)
sum(abs((test_7$y - test_7$autoML)/ nrow(test_7)) , na.rm = TRUE)  #MAPE


################## MULTIVARIABLE
temint_df_MMLAuto <- tibble(date = dato_interior$Fecha,
                            segundos = 60 * 10 * (as.numeric(rownames(dato_interior))-1),
                            semana_muestreo = 1 + (segundos - (segundos %% (3600 *24*7)))/(3600 *24*7),
                            segundo_muestreo_semana = segundos - (semana_muestreo-1) * (3600 *24*7),
                            day = 1 + (segundos - (segundos %% (3600 *24)))/(3600 *24),
                            y = temperatura_interior_ts,
                            z = temperatura_exterior_ts,
                            w = humedad_interior_ts,
                            v = humedad_exterior_ts)
head(temint_df_MMLAuto)

# temint_df_MMLAuto$lag72 <- dplyr::lag(as.numeric(temint_df_MMLAuto$y), n = 72)
# 
# temint_df_MMLAuto$trend <- decompose(temperatura_interior_ts)$trend
# temint_df_MMLAuto$seasonal <- decompose(temperatura_interior_ts)$seasonal
# temint_df_MMLAuto$trend_sqr <- (decompose(temperatura_interior_ts)$trend^ 2)
# 
# temint_df_MMLAuto$tetrend <- decompose(temperatura_exterior_ts)$trend
# temint_df_MMLAuto$teseasonal <- decompose(temperatura_exterior_ts)$seasonal
# temint_df_MMLAuto$tetrend_sqr <- (decompose(temperatura_exterior_ts)$trend^ 2)
# 
# temint_df_MMLAuto$hitrend <- decompose(humedad_interior_ts)$trend
# temint_df_MMLAuto$hiseasonal <- decompose(humedad_interior_ts)$seasonal
# temint_df_MMLAuto$hitrend_sqr <- (decompose(humedad_interior_ts)$trend^ 2)
# 
# temint_df_MMLAuto$hetrend <- decompose(humedad_exterior_ts)$trend
# temint_df_MMLAuto$heseasonal <- decompose(humedad_exterior_ts)$seasonal
# temint_df_MMLAuto$hetrend_sqr <- (decompose(humedad_exterior_ts)$trend^ 2)

h <- 627

teminterior_split <- ts_split(temperatura_interior_ts, sample.out = h)
teminterior.train.ts <- teminterior_split$train
teminterior.test.ts <- teminterior_split$test

temint_train_MMLAuto<- temint_df_MMLAuto[1:(unidades - h), ]
temint_test_MMLAuto<- temint_df_MMLAuto[(unidades - h + 1):unidades, ]

temint_train_MMLAuto$trend <- decompose(teminterior.train.ts)$trend
temint_train_MMLAuto$seasonal <- decompose(teminterior.train.ts)$seasonal
temint_train_MMLAuto$trend_sqr <- (decompose(teminterior.train.ts)$trend)^2
temint_train_MMLAuto$lag72 <- dplyr::lag(as.numeric(teminterior.train.ts), n = 72)

temint_train_MMLAuto$hitrend <- decompose(huminterior.train.ts)$trend
temint_train_MMLAuto$hiseasonal <- decompose(huminterior.train.ts)$seasonal
temint_train_MMLAuto$hitrend_sqr <- (decompose(huminterior.train.ts)$trend)^2

temint_train_MMLAuto$tetrend <- decompose(temexterior.train.ts)$trend
temint_train_MMLAuto$teseasonal <- decompose(temexterior.train.ts)$seasonal
temint_train_MMLAuto$tetrend_sqr <- (decompose(temexterior.train.ts)$trend)^2

temint_train_MMLAuto$hetrend <- decompose(humexterior.train.ts)$trend
temint_train_MMLAuto$heseasonal <- decompose(humexterior.train.ts)$seasonal
temint_train_MMLAuto$hetrend_sqr <- (decompose(humexterior.train.ts)$trend)^2

temint_test_MMLAuto$trend <- decompose(teminterior.test.ts)$trend
temint_test_MMLAuto$seasonal <- decompose(teminterior.test.ts)$seasonal
temint_test_MMLAuto$trend_sqr <- (decompose(teminterior.test.ts)$trend)^2
temint_test_MMLAuto$lag72 <- dplyr::lag(as.numeric(teminterior.test.ts), n = 72)

temint_test_MMLAuto$hitrend <- decompose(huminterior.test.ts)$trend
temint_test_MMLAuto$hiseasonal <- decompose(huminterior.test.ts)$seasonal
temint_test_MMLAuto$hitrend_sqr <- (decompose(huminterior.test.ts)$trend)^2

temint_test_MMLAuto$tetrend <- decompose(temexterior.test.ts)$trend
temint_test_MMLAuto$teseasonal <- decompose(temexterior.test.ts)$seasonal
temint_test_MMLAuto$tetrend_sqr <- (decompose(temexterior.test.ts)$trend)^2

temint_test_MMLAuto$hetrend <- decompose(humexterior.test.ts)$trend
temint_test_MMLAuto$heseasonal <- decompose(humexterior.test.ts)$seasonal
temint_test_MMLAuto$hetrend_sqr <- (decompose(humexterior.test.ts)$trend)^2

temint_train_MMLAuto$trend <- as.vector(temint_train_MMLAuto$trend)
temint_train_MMLAuto$seasonal <- as.vector(temint_train_MMLAuto$seasonal)
temint_train_MMLAuto$trend_sqr <- as.vector(temint_train_MMLAuto$trend_sqr)
temint_train_MMLAuto$lag72 <- as.vector(temint_train_MMLAuto$lag72)
temint_train_MMLAuto$hitrend <- as.vector(temint_train_MMLAuto$hitrend)
temint_train_MMLAuto$hiseasonal <- as.vector(temint_train_MMLAuto$hiseasonal)
temint_train_MMLAuto$hitrend_sqr <- as.vector(temint_train_MMLAuto$hitrend_sqr)
temint_train_MMLAuto$tetrend <- as.vector(temint_train_MMLAuto$tetrend)
temint_train_MMLAuto$teseasonal <- as.vector(temint_train_MMLAuto$teseasonal)
temint_train_MMLAuto$tetrend_sqr <- as.vector(temint_train_MMLAuto$tetrend_sqr)
temint_train_MMLAuto$hetrend <- as.vector(temint_train_MMLAuto$hetrend)
temint_train_MMLAuto$heseasonal <- as.vector(temint_train_MMLAuto$heseasonal)
temint_train_MMLAuto$hetrend_sqr <- as.vector(temint_train_MMLAuto$hetrend_sqr)
temint_test_MMLAuto$trend <- as.vector(temint_test_MMLAuto$trend)
temint_test_MMLAuto$seasonal <- as.vector(temint_test_MMLAuto$seasonal)
temint_test_MMLAuto$trend_sqr <- as.vector(temint_test_MMLAuto$trend_sqr)
temint_test_MMLAuto$lag72 <- as.vector(temint_test_MMLAuto$lag72)
temint_test_MMLAuto$hitrend <- as.vector(temint_test_MMLAuto$hitrend)
temint_test_MMLAuto$hiseasonal <- as.vector(temint_test_MMLAuto$hiseasonal)
temint_test_MMLAuto$hitrend_sqr <- as.vector(temint_test_MMLAuto$hitrend_sqr)
temint_test_MMLAuto$tetrend <- as.vector(temint_test_MMLAuto$tetrend)
temint_test_MMLAuto$teseasonal <- as.vector(temint_test_MMLAuto$teseasonal)
temint_test_MMLAuto$tetrend_sqr <- as.vector(temint_test_MMLAuto$tetrend_sqr)
temint_test_MMLAuto$hetrend <- as.vector(temint_test_MMLAuto$hetrend)
temint_test_MMLAuto$heseasonal <- as.vector(temint_test_MMLAuto$heseasonal)
temint_test_MMLAuto$hetrend_sqr <- as.vector(temint_test_MMLAuto$hetrend_sqr)

h2o.init(max_mem_size = "16G")

train_h_MMLAuto <- as.h2o(temint_train_MMLAuto)
test_h_MMLAuto <- as.h2o(temint_test_MMLAuto)

############ TEMPERATURA INTERIOR
#### TENDENCIA Y ESTACIONALIDAD
x <- c("trend", "seasonal")
y <- "y"
autoML11 <- h2o.automl(
  training_frame = train_h_MMLAuto,
  x = x,
  y = y,
  nfolds = 5,
  max_runtime_secs = 60*20,
  seed = 1234
)
par(mar=c(0,0,0,0))
h2o.varimp_plot(autoML11)
test_h_MMLAuto$pred_gbm <- h2o.predict(autoML11, test_h_MMLAuto)
test_25 <- as.data.frame(test_h_MMLAuto)
plot_ly(data = test_25) %>%
  add_lines(x = ~ date, y = ~y, name = "Actual") %>%
  add_lines(x = ~ date, y = ~ pred_gbm, name = "Gradient Boosting Machine",
            line = list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Multivar Gradient
Boosting Machine",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Día"))

sqrt( sum( (test_25$y - test_25$pred_gbm)^2 , na.rm = TRUE ) / nrow(test_25) )
sum((test_25$y - test_25$pred_gbm) , na.rm = TRUE) / nrow(test_25)
sum(abs((test_25$y - test_25$pred_gbm)/ nrow(test_25)) , na.rm = TRUE)  #MAPE

#### TENDENCIA Y LAG72
x <- c("trend", "lag72")
y <- "y"
autoML12 <- h2o.automl(
  training_frame = train_h_MMLAuto,
  x = x,
  y = y,
  nfolds = 5,
  max_runtime_secs = 60*20,
  seed = 1234
)
par(mar=c(0,0,0,0))
h2o.varimp_plot(autoML12)
test_h_MMLAuto$pred_gbm <- h2o.predict(autoML12, test_h_MMLAuto)
test_26 <- as.data.frame(test_h_MMLAuto)
plot_ly(data = test_26) %>%
  add_lines(x = ~ date, y = ~y, name = "Actual") %>%
  add_lines(x = ~ date, y = ~ pred_gbm, name = "Gradient Boosting Machine",
            line = list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Multivar Gradient
Boosting Machine",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Día"))

sqrt( sum( (test_26$y - test_26$pred_gbm)^2 , na.rm = TRUE ) / nrow(test_26) )
sum((test_26$y - test_26$pred_gbm) , na.rm = TRUE) / nrow(test_26)
sum(abs((test_26$y - test_26$pred_gbm)/ nrow(test_26)) , na.rm = TRUE)  #MAPE

#### TENDENCIA, ESTACIONALIDAD Y LAG72
x <- c("trend", "seasonal","lag72")
y <- "y"
autoML13 <- h2o.automl(
  training_frame = train_h_MMLAuto,
  x = x,
  y = y,
  nfolds = 5,
  max_runtime_secs = 60*20,
  seed = 1234
)
par(mar=c(0,0,0,0))
h2o.varimp_plot(autoML13)
test_h_MMLAuto$pred_gbm <- h2o.predict(autoML13, test_h_MMLAuto)
test_27 <- as.data.frame(test_h_MMLAuto)
plot_ly(data = test_27) %>%
  add_lines(x = ~ date, y = ~y, name = "Actual") %>%
  add_lines(x = ~ date, y = ~ pred_gbm, name = "Gradient Boosting Machine",
            line = list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Multivar Gradient
Boosting Machine",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Día"))

sqrt( sum( (test_27$y - test_27$pred_gbm)^2 , na.rm = TRUE ) / nrow(test_27) )
sum((test_27$y - test_27$pred_gbm) , na.rm = TRUE) / nrow(test_27)
sum(abs((test_27$y - test_27$pred_gbm)/ nrow(test_27)) , na.rm = TRUE)  #MAPE

#### TI TENDENCIA, ESTACIONALIDAD, LAG72 Y TENDENCIA CUADRADO
x <- c("trend", "seasonal","lag72", "trend_sqr")
y <- "y"
autoML14 <- h2o.automl(
  training_frame = train_h_MMLAuto,
  x = x,
  y = y,
  nfolds = 5,
  max_runtime_secs = 60*20,
  seed = 1234
)
par(mar=c(0,0,0,0))
h2o.varimp_plot(autoML14)
test_h_MMLAuto$pred_gbm <- h2o.predict(autoML14, test_h_MMLAuto)
test_28 <- as.data.frame(test_h_MMLAuto)
plot_ly(data = test_28) %>%
  add_lines(x = ~ date, y = ~y, name = "Actual") %>%
  add_lines(x = ~ date, y = ~ pred_gbm, name = "Gradient Boosting Machine",
            line = list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Multivar Gradient
Boosting Machine",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Día"))

sqrt( sum( (test_28$y - test_28$pred_gbm)^2 , na.rm = TRUE ) / nrow(test_28) )
sum((test_28$y - test_28$pred_gbm) , na.rm = TRUE) / nrow(test_28)
sum(abs((test_28$y - test_28$pred_gbm)/ nrow(test_28)) , na.rm = TRUE)  #MAPE

#### TI TENDENCIA, ESTACIONALIDAD, LAG72 Y TENDENCIA CUADRADO.
# HI TENDENCIA
x <- c("trend", "seasonal","lag72", "trend_sqr", "hitrend")
y <- "y"
autoML15 <- h2o.automl(
  training_frame = train_h_MMLAuto,
  x = x,
  y = y,
  nfolds = 5,
  max_runtime_secs = 60*20,
  seed = 1234
)
par(mar=c(0,0,0,0))
h2o.varimp_plot(autoML15)
test_h_MMLAuto$pred_gbm <- h2o.predict(autoML15, test_h_MMLAuto)
test_29 <- as.data.frame(test_h_MMLAuto)
plot_ly(data = test_29) %>%
  add_lines(x = ~ date, y = ~y, name = "Actual") %>%
  add_lines(x = ~ date, y = ~ pred_gbm, name = "Gradient Boosting Machine",
            line = list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Multivar Gradient
Boosting Machine",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Día"))

sqrt( sum( (test_29$y - test_29$pred_gbm)^2 , na.rm = TRUE ) / nrow(test_29) )
sum((test_29$y - test_29$pred_gbm) , na.rm = TRUE) / nrow(test_29)
sum(abs((test_29$y - test_29$pred_gbm)/ nrow(test_29)) , na.rm = TRUE)  #MAPE

#### TI TENDENCIA, ESTACIONALIDAD, LAG72 Y TENDENCIA CUADRADO.
# HI TENDENCIA, ESTACIONALIDAD
x <- c("trend", "seasonal","lag72", "trend_sqr", "hitrend", "hiseasonal")
y <- "y"
autoML16 <- h2o.automl(
  training_frame = train_h_MMLAuto,
  x = x,
  y = y,
  nfolds = 5,
  max_runtime_secs = 60*20,
  seed = 1234
)
par(mar=c(0,0,0,0))
h2o.varimp_plot(autoML16)
test_h_MMLAuto$pred_gbm <- h2o.predict(autoML16, test_h_MMLAuto)
test_30 <- as.data.frame(test_h_MMLAuto)
plot_ly(data = test_30) %>%
  add_lines(x = ~ date, y = ~y, name = "Actual") %>%
  add_lines(x = ~ date, y = ~ pred_gbm, name = "Gradient Boosting Machine",
            line = list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Multivar Gradient
Boosting Machine",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Día"))

sqrt( sum( (test_30$y - test_30$pred_gbm)^2 , na.rm = TRUE ) / nrow(test_30) )
sum((test_30$y - test_30$pred_gbm) , na.rm = TRUE) / nrow(test_30)
sum(abs((test_30$y - test_30$pred_gbm)/ nrow(test_30)) , na.rm = TRUE)  #MAPE

#### TI TENDENCIA, ESTACIONALIDAD, LAG72 Y TENDENCIA CUADRADO.
# HI TENDENCIA, ESTACIONALIDAD Y TENDENCIA CUADRADO
x <- c("trend", "seasonal","lag72", "trend_sqr", "hitrend", "hiseasonal", "hitrend_sqr")
y <- "y"
autoML17 <- h2o.automl(
  training_frame = train_h_MMLAuto,
  x = x,
  y = y,
  nfolds = 5,
  max_runtime_secs = 60*20,
  seed = 1234
)
par(mar=c(0,0,0,0))
h2o.varimp_plot(autoML17)
test_h_MMLAuto$pred_gbm <- h2o.predict(autoML17, test_h_MMLAuto)
test_31 <- as.data.frame(test_h_MMLAuto)
plot_ly(data = test_31) %>%
  add_lines(x = ~ date, y = ~y, name = "Actual") %>%
  add_lines(x = ~ date, y = ~ pred_gbm, name = "Gradient Boosting Machine",
            line = list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Multivar Gradient
Boosting Machine",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Día"))

sqrt( sum( (test_31$y - test_31$pred_gbm)^2 , na.rm = TRUE ) / nrow(test_31) )
sum((test_31$y - test_31$pred_gbm) , na.rm = TRUE) / nrow(test_31)
sum(abs((test_31$y - test_31$pred_gbm)/ nrow(test_31)) , na.rm = TRUE)  #MAPE

#### TI TENDENCIA, ESTACIONALIDAD, LAG72 Y TENDENCIA CUADRADO.
# TE TENDENCIA
x <- c("trend", "seasonal","lag72", "trend_sqr", "tetrend")
y <- "y"
autoML18 <- h2o.automl(
  training_frame = train_h_MMLAuto,
  x = x,
  y = y,
  nfolds = 5,
  max_runtime_secs = 60*20,
  seed = 1234
)
par(mar=c(0,0,0,0))
h2o.varimp_plot(autoML18)
test_h_MMLAuto$pred_gbm <- h2o.predict(autoML18, test_h_MMLAuto)
test_32 <- as.data.frame(test_h_MMLAuto)
plot_ly(data = test_32) %>%
  add_lines(x = ~ date, y = ~y, name = "Actual") %>%
  add_lines(x = ~ date, y = ~ pred_gbm, name = "Gradient Boosting Machine",
            line = list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Multivar Gradient
Boosting Machine",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Día"))

sqrt( sum( (test_32$y - test_32$pred_gbm)^2 , na.rm = TRUE ) / nrow(test_32) )
sum((test_32$y - test_32$pred_gbm) , na.rm = TRUE) / nrow(test_32)
sum(abs((test_32$y - test_32$pred_gbm)/ nrow(test_32)) , na.rm = TRUE)  #MAPE

#### TI TENDENCIA, ESTACIONALIDAD, LAG72 Y TENDENCIA CUADRADO.
# TE TENDENCIA, ESTACIONALIDAD
x <- c("trend", "seasonal","lag72", "trend_sqr", "tetrend", "teseasonal")
y <- "y"
autoML19 <- h2o.automl(
  training_frame = train_h_MMLAuto,
  x = x,
  y = y,
  nfolds = 5,
  max_runtime_secs = 60*20,
  seed = 1234
)
par(mar=c(0,0,0,0))
h2o.varimp_plot(autoML19)
test_h_MMLAuto$pred_gbm <- h2o.predict(autoML19, test_h_MMLAuto)
test_33 <- as.data.frame(test_h_MMLAuto)
plot_ly(data = test_33) %>%
  add_lines(x = ~ date, y = ~y, name = "Actual") %>%
  add_lines(x = ~ date, y = ~ pred_gbm, name = "Gradient Boosting Machine",
            line = list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Multivar Gradient
Boosting Machine",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Día"))

sqrt( sum( (test_33$y - test_33$pred_gbm)^2 , na.rm = TRUE ) / nrow(test_33) )
sum((test_33$y - test_33$pred_gbm) , na.rm = TRUE) / nrow(test_33)
sum(abs((test_33$y - test_33$pred_gbm)/ nrow(test_33)) , na.rm = TRUE)  #MAPE

#### TI TENDENCIA, ESTACIONALIDAD, LAG72 Y TENDENCIA CUADRADO.
# TE TENDENCIA, ESTACIONALIDAD Y TENDENCIA CUADRADO
x <- c("trend", "seasonal","lag72", "trend_sqr", "tetrend", "teseasonal", "tetrend_sqr")
y <- "y"
autoML20 <- h2o.automl(
  training_frame = train_h_MMLAuto,
  x = x,
  y = y,
  nfolds = 5,
  max_runtime_secs = 60*20,
  seed = 1234
)
par(mar=c(0,0,0,0))
h2o.varimp_plot(autoML20)
test_h_MMLAuto$pred_gbm <- h2o.predict(autoML20, test_h_MMLAuto)
test_34 <- as.data.frame(test_h_MMLAuto)
plot_ly(data = test_34) %>%
  add_lines(x = ~ date, y = ~y, name = "Actual") %>%
  add_lines(x = ~ date, y = ~ pred_gbm, name = "Gradient Boosting Machine",
            line = list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Multivar Gradient
Boosting Machine",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Día"))

sqrt( sum( (test_34$y - test_34$pred_gbm)^2 , na.rm = TRUE ) / nrow(test_34) )
sum((test_34$y - test_34$pred_gbm) , na.rm = TRUE) / nrow(test_34)
sum(abs((test_34$y - test_34$pred_gbm)/ nrow(test_34)) , na.rm = TRUE)  #MAPE

#### TI TENDENCIA, ESTACIONALIDAD, LAG72 Y TENDENCIA CUADRADO.
# HE TENDENCIA
x <- c("trend", "seasonal","lag72", "trend_sqr", "hetrend")
y <- "y"
autoML21 <- h2o.automl(
  training_frame = train_h_MMLAuto,
  x = x,
  y = y,
  nfolds = 5,
  max_runtime_secs = 60*20,
  seed = 1234
)
par(mar=c(0,0,0,0))
h2o.varimp_plot(autoML21)
test_h_MMLAuto$pred_gbm <- h2o.predict(autoML21, test_h_MMLAuto)
test_35 <- as.data.frame(test_h_MMLAuto)
plot_ly(data = test_35) %>%
  add_lines(x = ~ date, y = ~y, name = "Actual") %>%
  add_lines(x = ~ date, y = ~ pred_gbm, name = "Gradient Boosting Machine",
            line = list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Multivar Gradient
Boosting Machine",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Día"))

sqrt( sum( (test_35$y - test_35$pred_gbm)^2 , na.rm = TRUE ) / nrow(test_35) )
sum((test_35$y - test_35$pred_gbm) , na.rm = TRUE) / nrow(test_35)
sum(abs((test_35$y - test_35$pred_gbm)/ nrow(test_35)) , na.rm = TRUE)  #MAPE

#### TI TENDENCIA, ESTACIONALIDAD, LAG72 Y TENDENCIA CUADRADO.
# HE TENDENCIA, ESTACIONALIDAD
x <- c("trend", "seasonal","lag72", "trend_sqr", "hetrend", "heseasonal")
y <- "y"
autoML22 <- h2o.automl(
  training_frame = train_h_MMLAuto,
  x = x,
  y = y,
  nfolds = 5,
  max_runtime_secs = 60*20,
  seed = 1234
)
par(mar=c(0,0,0,0))
h2o.varimp_plot(autoML22)
test_h_MMLAuto$pred_gbm <- h2o.predict(autoML22, test_h_MMLAuto)
test_36 <- as.data.frame(test_h_MMLAuto)
plot_ly(data = test_36) %>%
  add_lines(x = ~ date, y = ~y, name = "Actual") %>%
  add_lines(x = ~ date, y = ~ pred_gbm, name = "Gradient Boosting Machine",
            line = list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Multivar Gradient
Boosting Machine",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Día"))

sqrt( sum( (test_36$y - test_36$pred_gbm)^2 , na.rm = TRUE ) / nrow(test_36) )
sum((test_36$y - test_36$pred_gbm) , na.rm = TRUE) / nrow(test_36)
sum(abs((test_36$y - test_36$pred_gbm)/ nrow(test_36)) , na.rm = TRUE)  #MAPE

#### TI TENDENCIA, ESTACIONALIDAD, LAG72 Y TENDENCIA CUADRADO.
# HE TENDENCIA, ESTACIONALIDAD Y TENDENCIA CUADRADO
x <- c("trend", "seasonal","lag72", "trend_sqr", "hetrend", "heseasonal", "tetrend_sqr")
y <- "y"
autoML23 <- h2o.automl(
  training_frame = train_h_MMLAuto,
  x = x,
  y = y,
  nfolds = 5,
  max_runtime_secs = 60*20,
  seed = 1234
)
par(mar=c(0,0,0,0))
h2o.varimp_plot(autoML23)
test_h_MMLAuto$pred_gbm <- h2o.predict(autoML23, test_h_MMLAuto)
test_37 <- as.data.frame(test_h_MMLAuto)
plot_ly(data = test_37) %>%
  add_lines(x = ~ date, y = ~y, name = "Actual") %>%
  add_lines(x = ~ date, y = ~ pred_gbm, name = "Gradient Boosting Machine",
            line = list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Multivar Gradient
Boosting Machine",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Día"))

sqrt( sum( (test_37$y - test_37$pred_gbm)^2 , na.rm = TRUE ) / nrow(test_37) )
sum((test_37$y - test_37$pred_gbm) , na.rm = TRUE) / nrow(test_37)
sum(abs((test_37$y - test_37$pred_gbm)/ nrow(test_37)) , na.rm = TRUE)  #MAPE

#### TI TENDENCIA, ESTACIONALIDAD, LAG72 Y TENDENCIA CUADRADO.
# HI TENDENCIA, ESTACIONALIDAD Y TENDENCIA CUADRADO
# TE TENDENCIA, ESTACIONALIDAD Y TENDENCIA CUADRADO
# HE TENDENCIA, ESTACIONALIDAD Y TENDENCIA CUADRADO
x <- c("trend", "seasonal","lag72", "trend_sqr", 
       "hetrend", "heseasonal", "hetrend_sqr",
       "tetrend", "teseasonal", "tetrend_sqr",
       "hitrend", "hiseasonal", "hitrend_sqr")
y <- "y"
autoML24 <- h2o.automl(
  training_frame = train_h_MMLAuto,
  x = x,
  y = y,
  nfolds = 5,
  max_runtime_secs = 60*20,
  seed = 1234
)
par(mar=c(0,0,0,0))
h2o.varimp_plot(autoML24)
test_h_MMLAuto$pred_gbm <- h2o.predict(autoML24, test_h_MMLAuto)
test_38 <- as.data.frame(test_h_MMLAuto)
plot_ly(data = test_38) %>%
  add_lines(x = ~ date, y = ~y, name = "Actual") %>%
  add_lines(x = ~ date, y = ~ pred_gbm, name = "Gradient Boosting Machine",
            line = list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Multivar Gradient
Boosting Machine",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Día"))

sqrt( sum( (test_38$y - test_38$pred_gbm)^2 , na.rm = TRUE ) / nrow(test_38) )
sum((test_38$y - test_38$pred_gbm) , na.rm = TRUE) / nrow(test_38)
sum(abs((test_38$y - test_38$pred_gbm)/ nrow(test_38)) , na.rm = TRUE)  #MAPE
##############################################################################
