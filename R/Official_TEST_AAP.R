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

source("R/plot_lm.R")
# Ruta de la consola y guardar en variable
ruta_excel <- "raw_data/OfficialData.xlsx"

# Hojas del Excel
excel_sheets(ruta_excel)

# Importar los datos de la Hoja Exterior
dato_exterior <- read_excel(ruta_excel, sheet = "Exterior")
dato_interior <- read_excel(ruta_excel, sheet = "Interior")


##############Analisis de Series Temporales
# Cogemos datos cada 20 minutos, la frecuencia sera 3 * 24
unidades <- length(dato_interior$Fecha)
humedad_interior_ts <- ts(data = dato_interior$`Humedad (%)`[1:unidades], start = 1, frequency = 3*24)
temperatura_interior_ts <- ts(data = dato_interior$`Temperatura (ºC)`[1:unidades], start = 1, frequency = 3*24)
dioxidocarb_interior_ts <- ts(data = dato_interior$`valor CO2 (ppm)`[1:unidades], start = 1, frequency = 3*24)
monoxidocarb_interior_ts <- ts(data = dato_interior$`valor CO (ppm)`[1:unidades], start = 1, frequency = 3*24)
humedad_exterior_ts <- ts (data = dato_exterior$`Humedad (%)`[1:unidades], start = 1, frequency = 3*24)
temperatura_exterior_ts <- ts(data = dato_exterior$`Temperatura (ºC)`[1:unidades], start = 1, frequency = 3*24)



# Imagenes de las series temporales
plot_hi_ts <- TSstudio::ts_plot(humedad_interior_ts, title = "Serie temporal de la humedad interior", Ytitle = "Humedad interior", Xtitle = "Días", Xgrid = TRUE, Ygrid = TRUE)
plot_ti_ts <- TSstudio::ts_plot(temperatura_interior_ts, title = "Serie temporal de la temperatura interior", Ytitle = "Temperatura interior", Xtitle = "Días", Xgrid = TRUE, Ygrid = TRUE)
plot_CO2_ts <- TSstudio::ts_plot(dioxidocarb_interior_ts, title = "Serie temporal de la dióxido de carbono interior", Ytitle = "Dióxido de carbono interior", Xtitle = "Días", Xgrid = TRUE, Ygrid = TRUE)
plot_CO_ts <- TSstudio::ts_plot(monoxidocarb_interior_ts, title = "Serie temporal de la monóxido de carbono interior", Ytitle = "Monóxido de carbono interior", Xtitle = "Días", Xgrid = TRUE, Ygrid = TRUE)
plot_he_ts <- TSstudio::ts_plot(humedad_exterior_ts, title = "Serie temporal de la humedad exterior", Ytitle = "Humedad exterior", Xtitle = "Días", Xgrid = TRUE, Ygrid = TRUE)
plot_te_ts <- TSstudio::ts_plot(temperatura_exterior_ts, title = "Serie temporal de la temperatura exterior", Ytitle = "Temperatura exterior", Xtitle = "Días", Xgrid = TRUE, Ygrid = TRUE)

ggseasonplot(temperatura_interior_ts, year.labels = T, continuous = T)
ggseasonplot(temperatura_interior_ts, polar = T)

# Guardar los plots de las series temporales
plot_hi_ts
plot_ti_ts
plot_CO2_ts
plot_CO_ts 
plot_he_ts 
plot_te_ts

# Descomponer series temporales
# Aditivo
huminterior_decompose <- decompose(humedad_interior_ts)
teminterior_decompose <- decompose(temperatura_interior_ts)
diointerior_decompose <- decompose(dioxidocarb_interior_ts)
moninterior_decompose <- decompose(monoxidocarb_interior_ts)
humexterior_decompose <- decompose(humedad_exterior_ts)
temexterior_decompose <- decompose(temperatura_exterior_ts)

# Multiplicativo
huminterior_decompose_mul <- decompose(humedad_interior_ts, type = "multiplicative")
teminterior_decompose_mul <- decompose(temperatura_interior_ts, type = "multiplicative")
diointerior_decompose_mul <- decompose(dioxidocarb_interior_ts, type = "multiplicative")
moninterior_decompose_mul <- decompose(monoxidocarb_interior_ts, type = "multiplicative")
humexterior_decompose_mul <- decompose(humedad_exterior_ts, type = "multiplicative")
temexterior_decompose_mul <- decompose(temperatura_exterior_ts, type = "multiplicative")

# Imagenes de las series temporales descompuestas
# Aditivo
plot_hi_decompose <- plot(huminterior_decompose)
plot_ti_decompose <- plot(teminterior_decompose)
plot_CO2_decompose <- plot(diointerior_decompose)
plot_CO_decompose <- plot(moninterior_decompose)
plot_he_decompose <- plot(humexterior_decompose)
plot_te_decompose <- plot(temexterior_decompose)

# Multiplicativo
plot_hi_decompose_mul <- plot(huminterior_decompose_mul)
plot_ti_decompose_mul <- plot(teminterior_decompose_mul)
plot_CO2_decompose_mul <- plot(diointerior_decompose_mul)
plot_CO_decompose_mul <- plot(moninterior_decompose_mul)
plot_he_decompose_mul <- plot(humexterior_decompose_mul)
plot_te_decompose_mul <- plot(temexterior_decompose_mul)



# Comprobar la estacionalidad de las ts numericamente
# Tambien con esto se estudia la Autocorrelacion
ur.kpss(humedad_interior_ts) %>% summary()
ur.kpss(temperatura_interior_ts) %>% summary()
ur.kpss(dioxidocarb_interior_ts) %>% summary()
ur.kpss(monoxidocarb_interior_ts) %>% summary()
ur.kpss(humedad_exterior_ts) %>% summary()
ur.kpss(temperatura_exterior_ts) %>% summary()
# Si el valor critico de 1ptc es menor que el valor de test-statistic
# Es NO Estacionario
# Si es al contrario, es Estacionario
# La unica ts que es ESTACIONARIA es la HUMEDAD EXTERIOR

# Tambien se comprueba con ACF y PACF
ts_cor(humedad_interior_ts, lag.max = 144) # Fuerte tendencia
ts_cor(temperatura_interior_ts, lag.max = 144) # Fuerte tendencia
ts_cor(dioxidocarb_interior_ts, lag.max = 144) # Fuerte tendencia
ts_cor(monoxidocarb_interior_ts, lag.max = 144) # Tendencia y estacionalidad
ts_cor(humedad_exterior_ts, lag.max = 144) # Tendencia y estacionalidad
ts_cor(temperatura_exterior_ts, lag.max = 144) # Tendencia y estacionalidad
# Se guardan

# Se quita la tendencia para ver si asi se pasa a estacionaria
# La humedad exterior no es necesario ya que es estacionaria
humedad_interior_detrend <- humedad_interior_ts - huminterior_decompose$trend
temperatura_interior_detrend <- temperatura_interior_ts - teminterior_decompose$trend
monoxido_interior_detrend <- monoxidocarb_interior_ts - moninterior_decompose$trend
dioxido_interior_detrend <- dioxidocarb_interior_ts - diointerior_decompose$trend
humedad_exterior_detrend <- humedad_exterior_ts - humexterior_decompose$trend
temperatura_exterior_detrend <- temperatura_exterior_ts - temexterior_decompose$trend

# Imagenes sin tendencia
ts_plot(humedad_interior_detrend)
ts_plot(temperatura_interior_detrend)
ts_plot(dioxido_interior_detrend)
ts_plot(monoxido_interior_detrend)
ts_plot(humedad_exterior_detrend)
ts_plot(temperatura_exterior_detrend)

# AAP: El siguiente bloque lo comento porque creo que no tiene sentido
# # Descomposicion de las ts sin tendencia
# plot(decompose(temperatura_interior_detrend))
# plot(decompose(temperatura_exterior_detrend))
# plot(decompose(humedad_exterior_detrend)) # Ya es estacionaria sin el detrend
# plot(decompose(humedad_interior_detrend))

# Comprobar si son estacionarias
ur.kpss(humedad_interior_detrend) %>% summary() 
ur.kpss(temperatura_interior_detrend) %>% summary() 
ur.kpss(dioxido_interior_detrend) %>% summary() 
ur.kpss(monoxido_interior_detrend) %>% summary()
ur.kpss(humedad_exterior_detrend) %>% summary()
ur.kpss(temperatura_exterior_detrend) %>% summary()
# Con esto ya serian todas estacionarias

# ACF y PACF de las ts sin tendencia: Se tienen que observar la estacionalidad
ts_cor(na.remove(humedad_interior_detrend), lag.max = 144)
ts_cor(na.remove(temperatura_interior_detrend), lag.max = 144)
ts_cor(na.remove(dioxido_interior_detrend), lag.max = 144)
ts_cor(na.remove(monoxido_interior_detrend), lag.max = 144)
ts_cor(na.remove(humedad_exterior_detrend), lag.max = 144)
ts_cor(na.remove(temperatura_exterior_detrend), lag.max = 144)

## Lag analysis sirve para comprobar la autocorrelacion que hay entre los datos
ts_lags(temperatura_interior_ts, lags = c(1, 33, 72,144))
ts_lags(temperatura_interior_detrend, lags = c(1, 33, 72,144))
ts_lags(humedad_interior_ts, lags = c(1, 33, 72,144))
ts_lags(humedad_interior_detrend, lags = c(1, 33, 72,144))
ts_lags(temperatura_exterior_ts, lags = c(1, 33, 72,144))
ts_lags(na.remove(temperatura_exterior_detrend), lags = c(1, 33, 72,144))
ts_lags(humedad_exterior_ts, lags = c(1, 33, 72,144))
ts_lags(na.remove(humedad_exterior_detrend), lags = c(1, 33, 72,144))
ts_lags(monoxidocarb_interior_ts, lags = c(1, 33, 72,144))
ts_lags(na.remove(monoxido_interior_detrend), lags = c(1, 33, 72,144))
ts_lags(dioxidocarb_interior_ts, lags = c(1, 33, 72,144))
ts_lags(na.remove(dioxido_interior_detrend), lags = c(1, 33, 72,144))

# Cross-correlation sirve para comprobar que haya relacion entre las distintas ts
par(mar=c(1,1,1,1))

acf(na.remove(temperatura_interior_detrend), lag.max = 72)
ts_lags(na.remove(temperatura_interior_detrend), lags = c(33, 72, 144))
ccf_plot(x = na.remove(temperatura_interior_detrend), y = na.remove(temperatura_interior_detrend), lags = c(1, 33, 72, 144))

ccf_plot(x = na.remove(humedad_interior_detrend), y = na.remove(temperatura_interior_detrend), lags = c(1, 33, 72, 144))

ccf_plot(x = na.remove(temperatura_exterior_detrend), y = na.remove(temperatura_interior_detrend), lags = c(1, 33, 72, 144))

ccf_plot(x = na.remove(humedad_exterior_ts), y = na.remove(temperatura_interior_detrend), lags = c(1, 33, 72, 144))

######### A continuacion, se mostraran las predicciones de los datos anteriores
# Se quiere predecir los datos de la temperatura interior
ts_info(temperatura_interior_ts)
ts_info(temperatura_interior_detrend)

# Se separan los datos de la temperatura interior con los que se van a entrenar el modelo
# y los de testeo
# A continuacion, las plantillas de como se dividirian los datos de la ts
# train_temint <- window(temperatura_interior_ts, start = time(temperatura_interior_ts)[1], end = time(temperatura_interior_ts)[length(temperatura_interior_ts)-627])
# train_temintdent <- window(temperatura_interior_detrend, start = time(temperatura_interior_detrend)[1], end = time(temperatura_interior_detrend)[length(temperatura_interior_detrend)-627])
# test_temint <- window(temperatura_interior_ts, start = time(temperatura_interior_ts)[length(temperatura_interior_ts)-627+1], end = time(temperatura_interior_ts)[length(temperatura_interior_ts)])
# test_temintdet <- window(temperatura_interior_detrend, start = time(temperatura_interior_detrend)[length(temperatura_interior_detrend)-627+1], end = time(temperatura_interior_detrend)[length(temperatura_interior_detrend)])
# O
# h <- 627
# tempint_train <- tempint_df[1:(unidades - h), ]
# tempint_test <- tempint_df[(unidades - h + 1):unidades, ]
# Se entrena a la ts con los datos de entrenamiento
# Se crean los datos de testeo
# Se observa el 
# Se hallan los errores
# Se hace siempre lo mismo con las predicciones
# DNE --> DATOS NO ESTACIONARIOS
# DE --> DATOS ESTACIONARIOS

############################# Regresion lineal Univariante DNE #####################
tempint_df <- tibble(ds = dato_interior$Fecha,weekday = weekdays(dato_interior$Fecha),
                     segundos = 60 * 10 * (as.numeric(rownames(dato_interior))-1),
                     dia_muestreo = 1 + (segundos - (segundos %% (3600 *12)))/(3600 *12),
                     segundos_dia_muestreo = segundos - (dia_muestreo-1) * (3600 *12),
                     y = temperatura_interior_detrend)
head(tempint_df)

tempint_df$trend <- decompose(temperatura_interior_ts)$trend
tempint_df$seasonal <- decompose(temperatura_interior_ts)$seasonal
tempint_df$det_trend <- decompose(temperatura_interior_detrend)$trend
tempint_df$det_seasonal <- decompose(temperatura_interior_detrend)$seasonal
h <- 627

# Solo la tendencia de la Temperatura interior

tempint_train_RL_trend <- tempint_df[1:(unidades - h), ]
tempint_test_RL_trend <- tempint_df[(unidades - h + 1):unidades, ]
md_trend <- lm(y ~ det_trend, data = tempint_train_RL_trend)
summary(md_trend)
tempint_train_RL_trend$yhat <- predict(md_trend, newdata = tempint_train_RL_trend)
tempint_test_RL_trend$yhat <- predict(md_trend, newdata = tempint_test_RL_trend)
plot_lm(data = tempint_df,
        train = tempint_train_RL_trend,
        test = tempint_test_RL_trend,
        title = "Predicción de la tendencia de la serie")
forecast::accuracy(md_trend)
mape_trend <- c(mean(abs(tempint_train_RL_trend$y - tempint_train_RL_trend$yhat)/ tempint_train_RL_trend$y),
                mean(abs(tempint_test_RL_trend$y - tempint_test_RL_trend$yhat)/ tempint_test_RL_trend$y))
mape_trend



# Solo la estacionalidad
tempint_train_RL_seas <- tempint_df[1:(unidades - h), ]
tempint_test_RL_seas <- tempint_df[(unidades - h + 1):unidades, ]
md_seasonal <- lm(y ~ seasonal, data = tempint_train_RL_seas)
summary(md_seasonal)
tempint_train_RL_seas$yhat <- predict(md_seasonal, newdata = tempint_train_RL_seas)
tempint_test_RL_seas$yhat <- predict(md_seasonal, newdata = tempint_test_RL_seas)
plot_lm(data = tempint_df,
        train = tempint_train_RL_seas,
        test = tempint_test_RL_seas,
        title = "Predicción de la tendencia de la serie")
forecast::accuracy(md_seasonal)
mape_seasonal <- c(mean(abs(tempint_train_RL_seas$y - tempint_train_RL_seas$yhat)/ tempint_train_RL_seas$y),
                   mean(abs(tempint_test_RL_seas$y - tempint_test_RL_seas$yhat)/ tempint_test_RL_seas$y))
mape_seasonal

# Regresion lineal con la tendencia y estacionalidad
tempint_train_RL_trendseas <- tempint_df[1:(unidades - h), ]
tempint_test_RL_trendseas <- tempint_df[(unidades - h + 1):unidades, ]
md1 <- lm(y ~ det_seasonal + det_trend, data = tempint_train_RL_trendseas)
summary(md1)
tempint_train_RL_trendseas$yhat <- predict(md1, newdata = tempint_train_RL_trendseas)
tempint_test_RL_trendseas$yhat <- predict(md1, newdata = tempint_test_RL_trendseas)

plot_lm(data = tempint_df,
        train = tempint_train_RL_trendseas,
        test = tempint_test_RL_trendseas,
        title = "Predicción de la tendencia y estacionalidad de la serie")
forecast::accuracy(md1)

mape_md1 <- c(mean(abs(tempint_train_RL_trendseas$y - tempint_train_RL_trendseas$yhat)/ tempint_train_RL_trendseas$y),
              mean(abs(tempint_test_RL_trendseas$y - tempint_test_RL_trendseas$yhat)/ tempint_test_RL_trendseas$y))
mape_md1

# Regresion lineal con Estacionalidad, tendencia y error
tempint_train_RL_trendseasruid <- tempint_df[1:(unidades - h), ]
tempint_test_RL_trendseasruid <- tempint_df[(unidades - h + 1):unidades, ]
md2 <- lm(y ~ det_seasonal + det_trend + I(det_trend^2), data = tempint_train_RL_trendseasruid)
summary(md2)
car::vif(md2)

tempint_train_RL_trendseasruid$yhat <- predict(md2, newdata = tempint_train_RL_trendseasruid)
tempint_test_RL_trendseasruid$yhat <- predict(md2, newdata = tempint_test_RL_trendseasruid)

plot_lm(data = tempint_df,
        train = tempint_train_RL_trendseasruid,
        test = tempint_test_RL_trendseasruid,
        title = "Predicción de la tendencia y estacionalidad de la serie")
forecast::accuracy(md2)

mape_md2 <- c(mean(abs(tempint_train_RL_trendseasruid$y - tempint_train_RL_trendseasruid$yhat)/ tempint_train_RL_trendseasruid$y),
              mean(abs(tempint_test_RL_trendseasruid$y - tempint_test_RL_trendseasruid$yhat)/ tempint_test_RL_trendseasruid$y))
mape_md2

#####################################
checkresiduals(md2) # Los residuos NO son ruido blanco: Muestran autocorrelacion
res_md2 <- residuals(md2)
max(res_md2) # El max. coincide con la imagen que reporta checkresiduals
min(res_md2) # El min. coincide con la imagen que reporta checkresiduals

# Solución intentar modelar los residuos del modelo md2 con un arima
model_residuals_md2 <- auto.arima(res_md2)
summary(model_residuals_md2)
res_model_residuals_md2 <- residuals(model_residuals_md2)

acf(res_model_residuals_md2,144) # Más o menos limpio
pacf(res_model_residuals_md2) # Más o menos limpio

checkresiduals(model_residuals_md2)


##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################

