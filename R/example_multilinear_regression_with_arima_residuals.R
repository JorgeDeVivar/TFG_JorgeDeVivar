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
colnames(correlacion) <- c('TI', 
                           'HI', 
                           'TE', 
                           'HE')

M <- round(cor(correlacion), digits=3)

corrplot::corrplot.mixed(M)

######### A continuacion, se mostraran las predicciones de los datos anteriores
# Se quiere predecir los datos de la temperatura interior
ts_info(temperatura_interior_ts)
################################################################################

############################# Regresion lineal #################################
h=627
# teminterior_split <- ts_split(temperatura_interior_ts, sample.out = h)
# train <- teminterior_split$train
# test <- teminterior_split$test

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

df <- tibble(ds = dato_interior$Fecha, weekday = as.factor(weekdays(dato_interior$Fecha)),
             segundos = 60 * 10 * (as.numeric(rownames(dato_interior))-1),
             dia_muestreo = 1 + (segundos - (segundos %% (3600 *12)))/(3600 *12),
             segundos_dia_muestreo = segundos - (dia_muestreo-1) * (3600 *12),
             ti = temperatura_interior_ts)
head(df)

df$trend <- 1:nrow(df)
df$ti_lag72 <- dplyr::lag(as.numeric(df$ti), n = 72) # Es lag 72 de temperatura interior
df$trend_sqr <- (df$trend)^2

df$te <- temperatura_exterior_ts
df$hi <- humedad_interior_ts
df$he <- humedad_exterior_ts

df$te_lag72 <- dplyr::lag(as.numeric(df$te), n = 72) # Es lag 72 de temperatura exterior
df$hi_lag72 <- dplyr::lag(as.numeric(df$hi), n = 72) # Es lag 72 de humedad interior
df$he_lag72 <- dplyr::lag(as.numeric(df$he), n = 72) # Es lag 72 de humedad exterior

train_df <- df[1:(nrow(df) - h), ]
test_df <- df[(nrow(df) - h + 1):nrow(df), ]


####### MULTIVARIABLE
# TI TENDENCIA, DIA DE LA SEMANA, ti_lag72 Y HUMEDAD INTERNA
md1_4 <- tslm(teminterior.train.ts ~ trend + weekday + ti_lag72 + hi, data = train_df)
checkresiduals(md1_4)

x_reg_md2_4 = cbind(
  train_df$trend,
  train_df$ti_lag72,
  train_df$hi,
  model.matrix(~ weekday,train_df)[,-1]
  )

md2_4 <- auto.arima(teminterior.train.ts,
                    xreg = x_reg_md2_4,
                    seasonal = TRUE,
                    stepwise = TRUE,
                    approximation = FALSE)
summary(md2_4)
checkresiduals(md2_4)
fc1_4 <- forecast(md1_4, newdata = test_df)

test_model_4 <- cbind(
  test_df$trend,
  test_df$ti_lag72,
  test_df$hi,
  model.matrix(~ weekday,test_df)[,-1]
)

fc2_4 <- forecast(md2_4, xreg = na.omit(test_model_4))
forecast::accuracy(fc1_4, teminterior.test.ts)
forecast::accuracy(fc2_4, teminterior.test.ts)
plot(fc1_4)
plot(fc2_4)
