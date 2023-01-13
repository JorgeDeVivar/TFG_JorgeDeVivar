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
plot_hi_ts <- ts_plot(humedad_interior_ts, title = "Serie temporal de la humedad interior", Ytitle = "Humedad interior", Xtitle = "Días", Xgrid = TRUE, Ygrid = TRUE)
plot_ti_ts <- ts_plot(temperatura_interior_ts, title = "Serie temporal de la temperatura interior", Ytitle = "Temperatura interior", Xtitle = "Días", Xgrid = TRUE, Ygrid = TRUE)
plot_CO2_ts <- ts_plot(dioxidocarb_interior_ts, title = "Serie temporal de la dióxido de carbono interior", Ytitle = "Dióxido de carbono interior", Xtitle = "Días", Xgrid = TRUE, Ygrid = TRUE)
plot_CO_ts <- ts_plot(monoxidocarb_interior_ts, title = "Serie temporal de la monóxido de carbono interior", Ytitle = "Monóxido de carbono interior", Xtitle = "Días", Xgrid = TRUE, Ygrid = TRUE)
plot_he_ts <- ts_plot(humedad_exterior_ts, title = "Serie temporal de la humedad exterior", Ytitle = "Humedad exterior", Xtitle = "Días", Xgrid = TRUE, Ygrid = TRUE)
plot_te_ts <- ts_plot(temperatura_exterior_ts, title = "Serie temporal de la temperatura exterior", Ytitle = "Temperatura exterior", Xtitle = "Días", Xgrid = TRUE, Ygrid = TRUE)

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
ts_cor(humedad_interior_ts)
ts_cor(temperatura_interior_ts)
ts_cor(dioxidocarb_interior_ts)
ts_cor(monoxidocarb_interior_ts)
ts_cor(humedad_exterior_ts, lag.max = 144)
ts_cor(temperatura_exterior_ts)
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

# Descomposicion de las ts sin tendencia
plot(decompose(temperatura_interior_detrend))
plot(decompose(temperatura_exterior_detrend))
plot(decompose(humedad_exterior_detrend)) # Ya es estacionaria sin el detrend
plot(decompose(humedad_interior_detrend))

# Comprobar si son estacionarias
ur.kpss(humedad_interior_detrend) %>% summary() 
ur.kpss(temperatura_interior_detrend) %>% summary() 
ur.kpss(dioxido_interior_detrend) %>% summary() 
ur.kpss(monoxido_interior_detrend) %>% summary()
ur.kpss(humedad_exterior_detrend) %>% summary()
ur.kpss(temperatura_exterior_detrend) %>% summary()
# Con esto ya serian todas estacionarias

# ACF y PACF de las ts sin tendencia
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
tempint_df <- tibble(ds = dato_interior$Fecha,
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
tempint_train_RL_trend$yhat <- predict(md_trend, newdata = tempint_train_RL_trend)
tempint_test_RL_trend$yhat <- predict(md_trend, newdata = tempint_test_RL_trend)
plot_lm(data = tempint_df,
        train = tempint_train_RL_trend,
        test = tempint_test_RL_trend,
        title = "Predicción de la tendencia de la serie")
accuracy(md_trend)
mape_trend <- c(mean(abs(tempint_train_RL_trend$y - tempint_train_RL_trend$yhat)/ tempint_train_RL_trend$y),
                mean(abs(tempint_test_RL_trend$y - tempint_test_RL_trend$yhat)/ tempint_test_RL_trend$y))
mape_trend



# Solo la estacionalidad
tempint_train_RL_seas <- tempint_df[1:(unidades - h), ]
tempint_test_RL_seas <- tempint_df[(unidades - h + 1):unidades, ]
md_seasonal <- lm(y ~ seasonal, data = tempint_train_RL_seas)
tempint_train_RL_seas$yhat <- predict(md_seasonal, newdata = tempint_train_RL_seas)
tempint_test_RL_seas$yhat <- predict(md_seasonal, newdata = tempint_test_RL_seas)
plot_lm(data = tempint_df,
        train = tempint_train_RL_seas,
        test = tempint_test_RL_seas,
        title = "Predicción de la tendencia de la serie")
accuracy(md_seasonal)
mape_seasonal <- c(mean(abs(tempint_train_RL_seas$y - tempint_train_RL_seas$yhat)/ tempint_train_RL_seas$y),
                   mean(abs(tempint_test_RL_seas$y - tempint_test_RL_seas$yhat)/ tempint_test_RL_seas$y))
mape_seasonal

# Regresion lineal con la tendencia y estacionalidad
tempint_train_RL_trendseas <- tempint_df[1:(unidades - h), ]
tempint_test_RL_trendseas <- tempint_df[(unidades - h + 1):unidades, ]
md1 <- lm(y ~ det_seasonal + det_trend, data = tempint_train_RL_trendseas)
tempint_train_RL_trendseas$yhat <- predict(md1, newdata = tempint_train_RL_trendseas)
tempint_test_RL_trendseas$yhat <- predict(md1, newdata = tempint_test_RL_trendseas)

plot_lm(data = tempint_df,
        train = tempint_train_RL_trendseas,
        test = tempint_test_RL_trendseas,
        title = "Predicción de la tendencia y estacionalidad de la serie")
accuracy(md1)

mape_md1 <- c(mean(abs(tempint_train_RL_trendseas$y - tempint_train_RL_trendseas$yhat)/ tempint_train_RL_trendseas$y),
              mean(abs(tempint_test_RL_trendseas$y - tempint_test_RL_trendseas$yhat)/ tempint_test_RL_trendseas$y))
mape_md1

# Regresion lineal con Estacionalidad, tendencia y error
tempint_train_RL_trendseasruid <- tempint_df[1:(unidades - h), ]
tempint_test_RL_trendseasruid <- tempint_df[(unidades - h + 1):unidades, ]
md2 <- lm(y ~ det_seasonal + det_trend + I(det_trend^2), data = tempint_train_RL_trendseasruid)
summary(md2)

tempint_train_RL_trendseasruid$yhat <- predict(md2, newdata = tempint_train_RL_trendseasruid)
tempint_test_RL_trendseasruid$yhat <- predict(md2, newdata = tempint_test_RL_trendseasruid)

plot_lm(data = tempint_df,
        train = tempint_train_RL_trendseasruid,
        test = tempint_test_RL_trendseasruid,
        title = "Predicción de la tendencia y estacionalidad de la serie")
accuracy(md2)

mape_md2 <- c(mean(abs(tempint_train_RL_trendseasruid$y - tempint_train_RL_trendseasruid$yhat)/ tempint_train_RL_trendseasruid$y),
              mean(abs(tempint_test_RL_trendseasruid$y - tempint_test_RL_trendseasruid$yhat)/ tempint_test_RL_trendseasruid$y))
mape_md2

################################################################################

############################ ARIMA Univariante DNE #############################################
# AR process
temint_md <- arima(temperatura_interior_detrend, order = c(1,1,0))
summary(temint_md)
checkresiduals(temint_md)

# ARIMA model
ti_split <- ts_split(diff(temperatura_interior_ts), sample.out = 627)

ti_train <- ti_split$train
ti_test <- ti_split$test

ti_best_md <- arima(na.remove(ti_train), 
                    order = c(1,1,1), 
                    seasonal = list(order = c(2,1,1)),
                    method = "CSS")

ti_test_fc <- forecast(ti_best_md, h = 627)
accuracy(ti_test_fc, ti_test)

test_forecast(diff(temperatura_interior_ts),
              forecast.obj = ti_test_fc,
              test = ti_test)
####
final_fc <- arima(na.remove(ti_train), 
                  order = c(1,1,1), 
                  seasonal = list(order = c(2,1,1)),
                  method = "CSS")

checkresiduals(final_fc)
accuracy(ti_fc)
ti_fc <- forecast(final_fc, h = 627)

plot_forecast(ti_fc,
              title = "Temperatura interior - Forecast",
              Ytitle = "Temperatura  (ºC)",
              Xtitle = "Días")

# Auto Arima
ti_auto_md1 <- auto.arima(ti_train)

ti_autoarima_md1 <- arima(na.remove(ti_train), 
                          order = c(2,0,3), 
                          seasonal = list(order = c(2,0,0)),
                          method = "CSS")

ti_test_auto1 <- forecast(ti_autoarima_md1, h = 627)
accuracy(ti_autoarima_md1)

test_forecast(diff(temperatura_interior_ts),
              forecast.obj = ti_test_auto1,
              test = ti_test)

ti_auto_md2 <- auto.arima(ti_train,
                          max.order = 5,
                          D = 1,
                          d = 1,
                          stepwise = FALSE,
                          approximation = FALSE)

ti_test_auto2 <- forecast(ti_autoarima_md2, h = 627)
accuracy(ti_autoarima_md2)

test_forecast(diff(temperatura_interior_ts),
              forecast.obj = ti_test_auto2,
              test = ti_test)
################################################################################

############################ Machine Learning Gradient Boost Univariante DNE ###################
temint_dfMLGB <- tibble(date = dato_interior$Fecha,
                    segundos = 60 * 10 * (as.numeric(rownames(dato_interior))-1),
                    day = 1 + (segundos - (segundos %% (3600 *12)))/(3600 *12),
                    y = temperatura_interior_detrend)
temint_dfMLGB$lag12 <- stats::lag(temint_dfMLGB$y, n = 72)

temint_dfMLGB$trend <- decompose(temperatura_interior_detrend)$trend
temint_dfMLGB$seasonal <- decompose(temperatura_interior_detrend)$seasonal
h <- 627
temint_train_MLGB <- temint_dfMLGB[1:(unidades - h), ]
temint_test_MLGB <- temint_dfMLGB[(unidades - h + 1):unidades, ]

lr <- lm(y ~ day + lag12 + trend + seasonal, data = temint_train_MLGB)

temint_test_MLGB$yhat <- predict(lr, newdata = temint_test_MLGB)

mape_lr <- mean(abs(temint_test_MLGB$y - temint_test_MLGB$yhat)/temint_test_MLGB$y)

h2o.init(max_mem_size = "16G")

train_h_MLGB <- as.h2o(temint_train_MLGB)
test_h_MLGB <- as.h2o(temint_test_MLGB)

x <- c("day", "lag12", "trend", "seasonal")
y <- "y"

gbm_md <- h2o.gbm(
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
test_h_MLGB$pred_gbm <- h2o.predict(gbm_md, test_h_MLGB)
test_1 <- as.data.frame(test_h_MLGB)

plot_ly(data = test_1) %>%
  add_lines(x = ~ date, y = ~ y, name = "Actual") %>%
  add_lines(x = ~ date, y = ~ pred_gbm, name = "Gradient Boosting Machine", 
            line = list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Prediction (Gradient
Boosting Machine)",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Día"))

mape_gbm <- mean(abs(test_1$y - test_1$pred_gbm) / test_1$y)
mape_gbm

sqrt( sum( (test_1$y - test_1$pred_gbm)^2 , na.rm = TRUE ) / nrow(test_1) )
sum((test_1$y - test_1$pred_gbm) , na.rm = TRUE) / nrow(test_1)

################################################################################

############################ Machine Learning AutoML Univariante DNE ###########################
temint_dfMLAuto <- tibble(date = dato_interior$Fecha,
                    segundos = 60 * 10 * (as.numeric(rownames(dato_interior))-1),
                    day = 1 + (segundos - (segundos %% (3600 *12)))/(3600 *12),
                    y = temperatura_interior_detrend)

temint_dfMLAuto$lag12 <- lag(temint_df$y, n = 72)

temint_dfMLAuto$trend <- decompose(temperatura_interior_detrend)$trend
temint_dfMLAuto$seasonal <- decompose(temperatura_interior_detrend)$seasonal

h <- 627
temint_train_MLAuto <- temint_dfMLAuto[1:(unidades - h), ]
temint_test_MLAuto <- temint_dfMLAuto[(unidades - h + 1):unidades, ]

lr <- lm(y ~ day + lag12 + trend + seasonal, data = temint_train_MLAuto)

temint_dfMLAuto$yhat <- predict(lr, newdata = temint_test_MLAuto)

mape_lr <- mean(abs(temint_test_MLAuto$y - temint_test_MLAuto$yhat)/temint_test_MLAuto$y)

h2o.init(max_mem_size = "16G")

train_h_MLAuto <- as.h2o(temint_train_MLAuto)
test_h_MLAuto <- as.h2o(temint_test_MLAuto)

x <- c("day", "lag12", "trend", "seasonal")
y <- "y"

autoML1 <- h2o.automl(training_frame = train_h_MLAuto,
                      x = x,
                      y = y,
                      nfolds = 5,
                      max_runtime_secs = 60*20,
                      seed = 1234)

test_h_MLAuto$pred_autoML <- h2o.predict(autoML1@leader, test_h_MLAuto)
test_2 <- as.data.frame(test_h_MLAuto)
mape_autoML <- mean(abs(test_2$y - test_2$pred_autoML) / test_2$y)
mape_autoML

plot_ly(data = test_2) %>%
  add_lines(x = ~ date, y = ~y, name = "Actual") %>%
  add_lines(x = ~ date, y = ~ pred_autoML, name = "autoML") %>%
  layout(title = "Temperatura interior - Actual vs. Prediction (Auto ML
Model)",
         yaxis = list(title = "Thousands of Units"),
         xaxis = list(title = "Month"))

sqrt( sum( (test_2$y - test_2$pred_autoML)^2 , na.rm = TRUE ) / nrow(test_2) )
sum((test_2$y - test_2$pred_autoML) , na.rm = TRUE) / nrow(test_2)
################################################################################

############################ Regresion lineal Multivariante DNE #############################################
tempint_df_MRL <- tibble(ds = dato_interior$Fecha,
                     segundos = 60 * 10 * (as.numeric(rownames(dato_interior))-1),
                     dia_muestreo = 1 + (segundos - (segundos %% (3600 *12)))/(3600 *12),
                     segundos_dia_muestreo = segundos - (dia_muestreo-1) * (3600 *12),
                     y = temperatura_interior_detrend,
                     z = temperatura_exterior_detrend,
                     w = humedad_interior_detrend,
                     v = humedad_exterior_ts)
head(tempint_df_MRL)

tempint_df_MRL$trend <- decompose(temperatura_interior_detrend)$trend
tempint_df_MRL$seasonal <- decompose(temperatura_interior_detrend)$seasonal

tempint_df_MRL$tetrend <- decompose(temperatura_exterior_detrend)$trend
tempint_df_MRL$teseasonal <- decompose(temperatura_exterior_detrend)$seasonal

tempint_df_MRL$hitrend <- decompose(humedad_interior_detrend)$trend
tempint_df_MRL$hiseasonal <- decompose(humedad_interior_detrend)$seasonal

tempint_df_MRL$hetrend <- decompose(humedad_exterior_ts)$trend
tempint_df_MRL$heseasonal <- decompose(humedad_exterior_ts)$seasonal

h <- 627


########################### Temperatura exterior
# Se utiliza para la regresión lineal los datos de temperatura exterior
tempint_train_MRL_te <- tempint_df_MRL[1:(unidades - h), ]
tempint_test_MRL_te <- tempint_df_MRL[(unidades - h + 1):unidades, ]

md_te <- lm(y ~ z, data = tempint_train_MRL_te)
summary(md_te)

tempint_train_MRL_te$yhat <- predict(md_te, newdata = tempint_train_MRL_te)
tempint_test_MRL_te$yhat <- predict(md_te, newdata = tempint_test_MRL_te)

plot_lm(data = tempint_df_MRL,
        train = tempint_train_MRL_te,
        test = tempint_test_MRL_te,
        title = "Predicción MRL con Temperatura Exterior")

mape_te <- c(mean(abs(tempint_train_MRL_te$y - tempint_train_MRL_te$yhat)/ tempint_train_MRL_te$y),
             mean(abs(tempint_test_MRL_te$y - tempint_test_MRL_te$yhat)/ tempint_test_MRL_te$y))
mape_te
accuracy(md_te)

# Se utiliza para la regresión lineal la trend de la temperatura exterior
tempint_train_MRL_tetrend <- tempint_df_MRL[1:(unidades - h), ]
tempint_test_MRL_tetrend <- tempint_df_MRL[(unidades - h + 1):unidades, ]

md_tetrend <- lm(y ~ tetrend, data = tempint_train_MRL_tetrend)
summary(md_tetrend)

tempint_train_MRL_tetrend$yhat <- predict(md_tetrend, newdata = tempint_train_MRL_tetrend)
tempint_test_MRL_tetrend$yhat <- predict(md_tetrend, newdata = tempint_test_MRL_tetrend)

plot_lm(data = tempint_df_MRL,
        train = tempint_train_MRL_tetrend,
        test = tempint_test_MRL_tetrend,
        title = "Predicción de la tendencia de la serie")

mape_seasonal <- c(mean(abs(tempint_train_MRL_tetrend$y - tempint_train_MRL_tetrend$yhat)/ tempint_train_MRL_tetrend$y),
                   mean(abs(tempint_test_MRL_tetrend$y - tempint_test_MRL_tetrend$yhat)/ tempint_test_MRL_tetrend$y))
mape_seasonal
accuracy(md_tetrend)

# Regresión lineal con datos temperatura exterior season and trend 
tempint_train_MRL_tetrendseas <- tempint_df_MRL[1:(unidades - h), ]
tempint_test_MRL_tetrendseas <- tempint_df_MRL[(unidades - h + 1):unidades, ]

md_tetrendseasonal <- lm(y ~ teseasonal + tetrend, data = tempint_train_MRL_tetrendseas)
summary(md_tetrendseasonal)

tempint_train_MRL_tetrendseas$yhat <- predict(md_tetrendseasonal, newdata = tempint_train_MRL_tetrendseas)
tempint_test_MRL_tetrendseas$yhat <- predict(md_tetrendseasonal, newdata = tempint_test_MRL_tetrendseas)

plot_lm(data = tempint_df_MRL,
        train = tempint_train_MRL_tetrendseas,
        test = tempint_test_MRL_tetrendseas,
        title = "Predicción con la tendencia y estacionalidad de la TE")

mape_md_tetrendseasonal <- c(mean(abs(tempint_train_MRL_tetrendseas$y - tempint_train_MRL_tetrendseas$yhat)/ tempint_train_MRL_tetrendseas$y),
                             mean(abs(tempint_test_MRL_tetrendseas$y - tempint_test_MRL_tetrendseas$yhat)/ tempint_test_MRL_tetrendseas$y))
mape_md_tetrendseasonal
accuracy(md_tetrendseasonal)
# Regresión lineal con datos temperatura exterior season and trend con error
tempint_train_MRL_tetrendseaserr <- tempint_df_MRL[1:(unidades - h), ]
tempint_test_MRL_tetrendseaserr <- tempint_df_MRL[(unidades - h + 1):unidades, ]

md_tetrendseasonalerr <- lm(y ~ teseasonal + tetrend + I(tetrend^2), data = tempint_train_MRL_tetrendseaserr)
summary(md_tetrendseasonalerr)

tempint_train_MRL_tetrendseaserr$yhat <- predict(md_tetrendseasonalerr, newdata = tempint_train_MRL_tetrendseaserr)
tempint_test_MRL_tetrendseaserr$yhat <- predict(md_tetrendseasonalerr, newdata = tempint_test_MRL_tetrendseaserr)

plot_lm(data = tempint_df_MRL,
        train = tempint_train_MRL_tetrendseaserr,
        test = tempint_test_MRL_tetrendseaserr,
        title = "Predicción con la tendencia, estacionalidad y error de la TE")

mape_md_tetrendseasonalerr <- c(mean(abs(tempint_train_MRL_tetrendseaserr$y - tempint_train_MRL_tetrendseaserr$yhat)/ tempint_train_MRL_tetrendseaserr$y),
                                mean(abs(tempint_test_MRL_tetrendseaserr$y - tempint_test_MRL_tetrendseaserr$yhat)/ tempint_test_MRL_tetrendseaserr$y))
mape_md_tetrendseasonalerr
accuracy(md_tetrendseasonalerr)

######################## Temperatura exterior e interior
# Se utiliza para la regresión lineal los datos de trend 
# temperatura interior + temperatura exterior
tempint_train_MRL_titetrend <- tempint_df_MRL[1:(unidades - h), ]
tempint_test_MRL_titetrend <- tempint_df_MRL[(unidades - h + 1):unidades, ]

md_tite <- lm(y ~ tetrend + trend, data = tempint_train_MRL_titetrend)
summary(md_tite)

tempint_train_MRL_titetrend$yhat <- predict(md_tite, newdata = tempint_train_MRL_titetrend)
tempint_test_MRL_titetrend$yhat <- predict(md_tite, newdata = tempint_test_MRL_titetrend)

plot_lm(data = tempint_df_MRL,
        train = tempint_train_MRL_titetrend,
        test = tempint_test_MRL_titetrend,
        title = "Predicción con la tendencia de la TE y TI")

mape_tite <- c(mean(abs(tempint_train_MRL_titetrend$y - tempint_train_MRL_titetrend$yhat)/ tempint_train_MRL_titetrend$y),
               mean(abs(tempint_test_MRL_titetrend$y - tempint_test_MRL_titetrend$yhat)/ tempint_test_MRL_titetrend$y))
mape_tite
accuracy(md_tite)
# Se utiliza para la regresión lineal los datos de seasonal
# temperatura interior + temperatura exterior
tempint_train_MRL_titeseasonal <- tempint_df_MRL[1:(unidades - h), ]
tempint_test_MRL_titeseasonal <- tempint_df_MRL[(unidades - h + 1):unidades, ]

md_titeseasonal <- lm(y ~ teseasonal + seasonal, data = tempint_train_MRL_titeseasonal)
summary(md_tite)

tempint_train_MRL_titeseasonal$yhat <- predict(md_titeseasonal, newdata = tempint_train_MRL_titeseasonal)
tempint_test_MRL_titeseasonal$yhat <- predict(md_titeseasonal, newdata = tempint_test_MRL_titeseasonal)

plot_lm(data = tempint_df_MRL,
        train = tempint_train_MRL_titeseasonal,
        test = tempint_test_MRL_titeseasonal,
        title = "Predicción con la estacionalidad de TE y TI")

mape_titeseasonal <- c(mean(abs(tempint_train_MRL_titeseasonal$y - tempint_train_MRL_titeseasonal$yhat)/ tempint_train_MRL_titeseasonal$y),
                       mean(abs(tempint_test_MRL_titeseasonal$y - tempint_test_MRL_titeseasonal$yhat)/ tempint_test_MRL_titeseasonal$y))
mape_titeseasonal
accuracy(md_titeseasonal)

# Se utiliza para la regresión lineal de la trend + seasonal 
# temperatura exterior + temperatura interior
tempint_train_MRL_titetrendseas <- tempint_df_MRL[1:(unidades - h), ]
tempint_test_MRL_titetrendseas <- tempint_df_MRL[(unidades - h + 1):unidades, ]

md_titetrendseasonal <- lm(y ~ tetrend + trend + teseasonal + seasonal, data = tempint_train_MRL_titetrendseas)
summary(md_titetrendseasonal)

tempint_train_MRL_titetrendseas$yhat <- predict(md_titetrendseasonal, newdata = tempint_train_MRL_titetrendseas)
tempint_test_MRL_titetrendseas$yhat <- predict(md_titetrendseasonal, newdata = tempint_test_MRL_titetrendseas)

plot_lm(data = tempint_df_MRL,
        train = tempint_train_MRL_titetrendseas,
        test = tempint_test_MRL_titetrendseas,
        title = "Predicción de la tendencia de la serie")

mape_md_titetrendseasonal <- c(mean(abs(tempint_train_MRL_titetrendseas$y - tempint_train_MRL_titetrendseas$yhat)/ tempint_train_MRL_titetrendseas$y),
                               mean(abs(tempint_test_MRL_titetrendseas$y - tempint_test_MRL_titetrendseas$yhat)/ tempint_test_MRL_titetrendseas$y))
mape_md_titetrendseasonal
accuracy(md_titetrendseasonal)
# Regresión lineal con datos seasonal and trend and error
# temperatura exterior + temperatura interior
tempint_train_MRL_titetrendseaserr <- tempint_df_MRL[1:(unidades - h), ]
tempint_test_MRL_titetrendseaserr <- tempint_df_MRL[(unidades - h + 1):unidades, ]

md_titetrendseasonalerr <- lm(y ~ tetrend + trend + teseasonal + seasonal + I(trend^2) + I(tetrend^2)
                              , data = tempint_train_MRL_titetrendseaserr)
summary(md_titetrendseasonalerr)

tempint_train_MRL_titetrendseaserr$yhat <- predict(md_titetrendseasonalerr, newdata = tempint_train_MRL_titetrendseaserr)
tempint_test_MRL_titetrendseaserr$yhat <- predict(md_titetrendseasonalerr, newdata = tempint_test_MRL_titetrendseaserr)

plot_lm(data = tempint_df_MRL,
        train = tempint_train_MRL_titetrendseaserr,
        test = tempint_test_MRL_titetrendseaserr,
        title = "Predicción de la tendencia de la serie")

mape_md_titetrendseasonalerr <- c(mean(abs(tempint_train_MRL_titetrendseaserr$y - tempint_train_MRL_titetrendseaserr$yhat)/ tempint_train_MRL_titetrendseaserr$y),
                                  mean(abs(tempint_test_MRL_titetrendseaserr$y - tempint_test_MRL_titetrendseaserr$yhat)/ tempint_test_MRL_titetrendseaserr$y))
mape_md_titetrendseasonalerr
accuracy(md_titetrendseasonalerr)

############Temperatura exterior, interior y humedad interior
# Se utiliza para la regresión lineal los datos de trend 
# temperatura interior + temperatura exterior + humedad interior
tempint_train_MRL_titehitrend <- tempint_df_MRL[1:(unidades - h), ]
tempint_test_MRL_titehitrend <- tempint_df_MRL[(unidades - h + 1):unidades, ]

md_titehi <- lm(y ~ tetrend + trend + hitrend, data = tempint_train_MRL_titehitrend)
summary(md_titehi)

tempint_train_MRL_titehitrend$yhat <- predict(md_titehi, newdata = tempint_train_MRL_titehitrend)
tempint_test_MRL_titehitrend$yhat <- predict(md_titehi, newdata = tempint_test_MRL_titehitrend)

plot_lm(data = tempint_df_MRL,
        train = tempint_train_MRL_titehitrend,
        test = tempint_test_MRL_titehitrend,
        title = "Predicción con las tendencias de la TE, TI y HI")

mape_titehi <- c(mean(abs(tempint_train_MRL_titehitrend$y - tempint_train_MRL_titehitrend$yhat)/ tempint_train_MRL_titehitrend$y),
                 mean(abs(tempint_test_MRL_titehitrend$y - tempint_test_MRL_titehitrend$yhat)/ tempint_test_MRL_titehitrend$y))
mape_titehi
accuracy(md_titehi)
# Se utiliza para la regresión lineal los datos de seasonal
# temperatura interior + temperatura exterior + humedad interior
tempint_train_MRL_titehiseasonal <- tempint_df_MRL[1:(unidades - h), ]
tempint_test_MRL_titehiseasonal <- tempint_df_MRL[(unidades - h + 1):unidades, ]

md_titehiseasonal <- lm(y ~ teseasonal + seasonal + hiseasonal, 
                        data = tempint_train_MRL_titehiseasonal)
summary(md_titehiseasonal)

tempint_train_MRL_titehiseasonal$yhat <- predict(md_titehiseasonal, newdata = tempint_train_MRL_titehiseasonal)
tempint_test_MRL_titehiseasonal$yhat <- predict(md_titehiseasonal, newdata = tempint_test_MRL_titehiseasonal)

plot_lm(data = tempint_df_MRL,
        train = tempint_train_MRL_titehiseasonal,
        test = tempint_test_MRL_titehiseasonal,
        title = "Predicción de la tendencia de la serie")

mape_titehiseasonal <- c(mean(abs(tempint_train_MRL_titehiseasonal$y - tempint_train_MRL_titehiseasonal$yhat)/ tempint_train_MRL_titehiseasonal$y),
                         mean(abs(tempint_test_MRL_titehiseasonal$y - tempint_test_MRL_titehiseasonal$yhat)/ tempint_test_MRL_titehiseasonal$y))
mape_titehiseasonal

# Se utiliza para la regresión lineal de la trend + seasonal 
# temperatura exterior + temperatura interior + humedad interior
tempint_train_MRL_titehitrendseas <- tempint_df_MRL[1:(unidades - h), ]
tempint_test_MRL_titehitrendseas <- tempint_df_MRL[(unidades - h + 1):unidades, ]

md_titehitrendseasonal <- lm(y ~ tetrend + trend + teseasonal + seasonal + hiseasonal + hitrend,
                             data = tempint_train_MRL_titehitrendseas)
summary(md_titehitrendseasonal)

tempint_train_MRL_titehitrendseas$yhat <- predict(md_titehitrendseasonal, newdata = tempint_train_MRL_titehitrendseas)
tempint_test_MRL_titehitrendseas$yhat <- predict(md_titehitrendseasonal, newdata = tempint_test_MRL_titehitrendseas)

plot_lm(data = tempint_df_MRL,
        train = tempint_train_MRL_titehitrendseas,
        test = tempint_test_MRL_titehitrendseas,
        title = "Predicción de la tendencia de la serie")

mape_md_titehitrendseasonal <- c(mean(abs(tempint_train_MRL_titehitrendseas$y - tempint_train_MRL_titehitrendseas$yhat)/ tempint_train_MRL_titehitrendseas$y),
                                 mean(abs(tempint_test_MRL_titehitrendseas$y - tempint_test_MRL_titehitrendseas$yhat)/ tempint_test_MRL_titehitrendseas$y))
mape_md_titehitrendseasonal
accuracy(md_titehitrendseasonal)
# Regresión lineal con datos seasonal and trend and error
# temperatura exterior + temperatura interior + humedad interior
tempint_train_MRL_titehitrendseaserr <- tempint_df_MRL[1:(unidades - h), ]
tempint_test_MRL_titehitrendseaserr <- tempint_df_MRL[(unidades - h + 1):unidades, ]

md_titehitrendseasonalerr <- lm(y ~ tetrend + trend + teseasonal + seasonal + I(trend^2) + I(tetrend^2) + hitrend + hiseasonal + I(hitrend^2), 
                                data = tempint_train_MRL_titehitrendseaserr)
summary(md_titehitrendseasonalerr)

tempint_train_MRL_titehitrendseaserr$yhat <- predict(md_titehitrendseasonalerr, newdata = tempint_train_MRL_titehitrendseaserr)
tempint_test_MRL_titehitrendseaserr$yhat <- predict(md_titehitrendseasonalerr, newdata = tempint_test_MRL_titehitrendseaserr)

plot_lm(data = tempint_df_MRL,
        train = tempint_train_MRL_titehitrendseaserr,
        test = tempint_test_MRL_titehitrendseaserr,
        title = "Predicción de la tendencia, estacionalidad y error de la TI, TE y HI")

mape_md_titehitrendseasonalerr <- c(mean(abs(tempint_train_MRL_titehitrendseaserr$y - tempint_train_MRL_titehitrendseaserr$yhat)/ tempint_train_MRL_titehitrendseaserr$y),
                                    mean(abs(tempint_test_MRL_titehitrendseaserr$y - tempint_test_MRL_titehitrendseaserr$yhat)/ tempint_test_MRL_titehitrendseaserr$y))
mape_md_titehitrendseasonalerr

############Temperatura exterior, interior y humedad interior, exterior
# Se utiliza para la regresión lineal los datos de trend 
# temperatura interior + temperatura exterior + humedad interior + humedad exterior
tempint_train_MRL_titehihetrend <- tempint_df_MRL[1:(unidades - h), ]
tempint_test_MRL_titehihetrend <- tempint_df_MRL[(unidades - h + 1):unidades, ]

md_titehihe <- lm(y ~ tetrend + trend + hitrend + hetrend,
                  data = tempint_train_MRL_titehihetrend)
summary(md_titehi)

tempint_train_MRL_titehihetrend$yhat <- predict(md_titehihe, newdata = tempint_train_MRL_titehihetrend)
tempint_test_MRL_titehihetrend$yhat <- predict(md_titehihe, newdata = tempint_test_MRL_titehihetrend)

plot_lm(data = tempint_df_MRL,
        train = tempint_train_MRL_titehihetrend,
        test = tempint_test_MRL_titehihetrend,
        title = "Predicción con las tendencias de TI, TE, HI y HE")

mape_titehihe <- c(mean(abs(tempint_train_MRL_titehihetrend$y - tempint_train_MRL_titehihetrend$yhat)/ tempint_train_MRL_titehihetrend$y),
                   mean(abs(tempint_test_MRL_titehihetrend$y - tempint_test_MRL_titehihetrend$yhat)/ tempint_test_MRL_titehihetrend$y))
mape_titehihe
accuracy(md_titehihe)
# Se utiliza para la regresión lineal los datos de seasonal
# temperatura interior + temperatura exterior + humedad interior + humedad exterior
tempint_train_MRL_titehiheseas <- tempint_df_MRL[1:(unidades - h), ]
tempint_test_MRL_titehiheseas <- tempint_df_MRL[(unidades - h + 1):unidades, ]

md_titehiheseasonal <- lm(y ~ teseasonal + seasonal + hiseasonal + heseasonal, 
                          data = tempint_train_MRL_titehiheseas)
summary(md_titehiheseasonal)

tempint_train_MRL_titehiheseas$yhat <- predict(md_titehiheseasonal, newdata = tempint_train_MRL_titehiheseas)
tempint_test_MRL_titehiheseas$yhat <- predict(md_titehiheseasonal, newdata = tempint_test_MRL_titehiheseas)

plot_lm(data = tempint_df_MRL,
        train = tempint_train_MRL_titehiheseas,
        test = tempint_test_MRL_titehiheseas,
        title = "Predicción con la estacionalidad de TI, TE, HI y HE")

mape_titehiheseasonal <- c(mean(abs(tempint_train_MRL_titehiheseas$y - tempint_train_MRL_titehiheseas$yhat)/ tempint_train_MRL_titehiheseas$y),
                           mean(abs(tempint_test_MRL_titehiheseas$y - tempint_test_MRL_titehiheseas$yhat)/ tempint_test_MRL_titehiheseas$y))
mape_titehiheseasonal

accuracy(md_titehiheseasonal)
# Se utiliza para la regresión lineal de la trend + seasonal 
# temperatura exterior + temperatura interior + humedad interior + humedad exterior
tempint_train_MRL_titehihetrendseas <- tempint_df_MRL[1:(unidades - h), ]
tempint_test_MRL_titehihetrendseas <- tempint_df_MRL[(unidades - h + 1):unidades, ]

md_titehihetrendseasonal <- lm(y ~ tetrend + trend + teseasonal + seasonal + hiseasonal + hitrend + heseasonal + hetrend,
                               data = tempint_train_MRL_titehihetrendseas)
summary(md_titehihetrendseasonal)

tempint_train_MRL_titehihetrendseas$yhat <- predict(md_titehihetrendseasonal, newdata = tempint_train_MRL_titehihetrendseas)
tempint_test_MRL_titehihetrendseas$yhat <- predict(md_titehihetrendseasonal, newdata = tempint_test_MRL_titehihetrendseas)

plot_lm(data = tempint_df_MRL,
        train = tempint_train_MRL_titehihetrendseas,
        test = tempint_test_MRL_titehihetrendseas,
        title = "Predicción de la tendencia de la serie")

mape_md_titehihetrendseasonal <- c(mean(abs(tempint_train_MRL_titehihetrendseas$y - tempint_train_MRL_titehihetrendseas$yhat)/ tempint_train_MRL_titehihetrendseas$y),
                                   mean(abs(tempint_test_MRL_titehihetrendseas$y - tempint_test_MRL_titehihetrendseas$yhat)/ tempint_test_MRL_titehihetrendseas$y))
mape_md_titehihetrendseasonal
accuracy(md_titehihetrendseasonal)
# Regresión lineal con datos seasonal and trend and error
# temperatura exterior + temperatura interior + humedad interior + humedad exterior
tempint_train_MRL_titehihetrendseaserr <- tempint_df_MRL[1:(unidades - h), ]
tempint_test_MRL_titehihetrendseaserr <- tempint_df_MRL[(unidades - h + 1):unidades, ]

md_titehihetrendseasonalerr <- lm(y ~ tetrend + trend + teseasonal + seasonal + I(trend^2) + I(tetrend^2) + hitrend + hiseasonal + I(hitrend^2) + hetrend + heseasonal + I(hetrend^2), 
                                  data = tempint_train_MRL_titehihetrendseaserr)
summary(md_titehihetrendseasonalerr)

tempint_train_MRL_titehihetrendseaserr$yhat <- predict(md_titehihetrendseasonalerr, newdata = tempint_train_MRL_titehihetrendseaserr)
tempint_test_MRL_titehihetrendseaserr$yhat <- predict(md_titehihetrendseasonalerr, newdata = tempint_test_MRL_titehihetrendseaserr)

plot_lm(data = tempint_df_MRL,
        train = tempint_train_MRL_titehihetrendseaserr,
        test = tempint_test_MRL_titehihetrendseaserr,
        title = "Predicción de la tendencia de la serie")

mape_md_titehihetrendseasonalerr <- c(mean(abs(tempint_train_MRL_titehihetrendseaserr$y - tempint_train_MRL_titehihetrendseaserr$yhat)/ tempint_train_MRL_titehihetrendseaserr$y),
                                      mean(abs(tempint_test_MRL_titehihetrendseaserr$y - tempint_test_MRL_titehihetrendseaserr$yhat)/ tempint_test_MRL_titehihetrendseaserr$y))
mape_md_titehihetrendseasonalerr

accuracy(md_titehihetrendseasonalerr)
################################################################################

############################ ARIMA Multivariante DNE #############################################
x = cbind(temperatura_interior_detrend, 
          temperatura_exterior_detrend, 
          humedad_interior_detrend, 
          humedad_exterior_ts)
summary(VAR(na.remove(x), p = 1, type = 'both'))

VARselect(na.remove(x), lag.max = 10, type = "both")

par(mar=c(1,1,1,1))
summary(fit <- VAR(na.remove(x), p = 2, type = "both"))
acf(resid(fit), 52)
serial.test(fit, lags.pt = 12, type = "PT.adjusted")
(fit.pr = predict(fit, n.ahead = 24, ci = 0.95))
fanchart(fit.pr)

################################################################################

############################ Machine Learning GB Multivariante DNE #############
temint_df_MMLGB <- tibble(date = dato_interior$Fecha,
                    segundos = 60 * 10 * (as.numeric(rownames(dato_interior))-1),
                    semana_muestreo = 1 + (segundos - (segundos %% (3600 *24*7)))/(3600 *24*7),
                    segundo_muestreo_semana = segundos - (semana_muestreo-1) * (3600 *24*7),
                    day = 1 + (segundos - (segundos %% (3600 *24)))/(3600 *24),
                    y = temperatura_interior_detrend,
                    z = temperatura_exterior_detrend,
                    w = humedad_interior_detrend,
                    v = humedad_exterior_ts)
head(temint_df)

temint_df_MMLGB$lag12 <- stats::lag(temint_df_MMLGB$y, n = 72)

temint_df_MMLGB$trend <- decompose(temperatura_interior_detrend)$trend
temint_df_MMLGB$trend_sqr <- temint_df$trend ^ 2

temint_df_MMLGB$tetrend <- decompose(temperatura_exterior_detrend)$trend
temint_df_MMLGB$tetrend_sqr <- temint_df$tetrend ^ 2

temint_df_MMLGB$hitrend <- decompose(humedad_interior_detrend)$trend
temint_df_MMLGB$hitrend_sqr <- temint_df$hitrend ^ 2

temint_df_MMLGB$hetrend <- decompose(humedad_exterior_detrend)$trend
temint_df_MMLGB$hetrend_sqr <- temint_df$hetrend ^ 2

h <- 627

###########Temperatura exterior + temperatura interior
# Trend +lag
temint_train_MMLAuto_lagtitetrend <- temint_df_MMLGB[1:(unidades - h), ]
temint_test_MMLAuto_lagtitetrend <- temint_df_MMLGB[(unidades - h + 1):unidades, ]

lr <- lm(y ~ day + lag12 + trend + tetrend, 
         data = temint_train_MMLAuto_lagtitetrend)

temint_test_MMLAuto_lagtitetrend$yhat <- predict(lr, newdata = temint_test_MMLAuto_lagtitetrend)

mape_lr <- mean(abs(temint_test_MMLAuto_lagtitetrend$y - temint_test_MMLAuto_lagtitetrend$yhat)/temint_test_MMLAuto_lagtitetrend$y)

h2o.init(max_mem_size = "16G")

train_h_MMLAuto_lagtitetrend <- as.h2o(temint_train_MMLAuto_lagtitetrend)
test_h_MMLAuto_lagtitetrend <- as.h2o(temint_test_MMLAuto_lagtitetrend)

x <- c("day", "lag12", "trend", "tetrend")
y <- "y"

gbm_md11 <- h2o.gbm(
  training_frame = train_h_MMLAuto_lagtitetrend,
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
test_h_MMLAuto_lagtitetrend$pred_gbm11 <- h2o.predict(gbm_md11, test_h_MMLAuto_lagtitetrend)
test_11 <- as.data.frame(test_h_MMLAuto_lagtitetrend)
mape_gbm <- mean(abs(test_11$y - test_11$pred_gbm) / test_11$y)
mape_gbm

plot_ly(data = test_11) %>%
  add_lines(x = ~ date, y = ~y, name = "Actual") %>%
  add_lines(x = ~ date, y = ~ pred_gbm11, name = "Gradient Boosting Machine",
            line = list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Multivar Gradient
Boosting Machine",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Día"))

sqrt( sum( (test_11$y - test_11$pred_gbm)^2 , na.rm = TRUE ) / nrow(test_11) )
sum((test_11$y - test_11$pred_gbm) , na.rm = TRUE) / nrow(test_11)
# Trend + lag + trend^2
temint_train_MMLAuto_lagtitetrenderr <- temint_df_MMLGB[1:(unidades - h), ]
temint_test_MMLAuto_lagtitetrenderr <- temint_df_MMLGB[(unidades - h + 1):unidades, ]

lr <- lm(y ~ day + lag12 + trend + trend_sqr + tetrend + tetrend_sqr, 
         data = temint_train_MMLAuto_lagtitetrenderr)

temint_test_MMLAuto_lagtitetrenderr$yhat <- predict(lr, newdata = temint_test_MMLAuto_lagtitetrenderr)

mape_lr <- mean(abs(temint_test_MMLAuto_lagtitetrenderr$y - temint_test_MMLAuto_lagtitetrenderr$yhat)/temint_test_MMLAuto_lagtitetrenderr$y)

h2o.init(max_mem_size = "16G")

train_h_MMLAuto_lagtitetrenderr <- as.h2o(temint_train_MMLAuto_lagtitetrenderr)
test_h_MMLAuto_lagtitetrenderr <- as.h2o(temint_test_MMLAuto_lagtitetrenderr)

x <- c("day", "lag12", "trend", "trend_sqr", "tetrend", "tetrend_sqr")
y <- "y"

gbm_md12 <- h2o.gbm(
  training_frame = train_h_MMLAuto_lagtitetrend,
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
test_h_MMLAuto_lagtitetrend$pred_gbm12 <- h2o.predict(gbm_md12, test_h_MMLAuto_lagtitetrend)
test_12 <- as.data.frame(test_h_MMLAuto_lagtitetrend)
mape_gbm <- mean(abs(test_12$y - test_12$pred_gbm) / test_12$y)
mape_gbm

plot_ly(data = test_12) %>%
  add_lines(x = ~ date, y = ~y, name = "Actual") %>%
  #add_lines(x = ~ date, y = ~ yhat, name = "Linear Regression", line =
  #list(dash = "dot")) %>%
  add_lines(x = ~ date, y = ~ pred_gbm12, name = "Gradient Boosting Machine",
            line = list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Prediction (Gradient
Boosting Machine)",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Día"))

sqrt( sum( (test_12$y - test_12$pred_gbm)^2 , na.rm = TRUE ) / nrow(test_12) )
sum((test_12$y - test_12$pred_gbm) , na.rm = TRUE) / nrow(test_12)
########Temperatura exterior + temperatura interior + Humedad interior
# Trend +lag
temint_train_MMLAuto_lagtitehitrend <- temint_df_MMLGB[1:(unidades - h), ]
temint_test_MMLAuto_lagtitehitrend <- temint_df_MMLGB[(unidades - h + 1):unidades, ]

lr <- lm(y ~ day + lag12 + trend + tetrend + hitrend, 
         data = temint_train_MMLAuto_lagtitehitrend)

temint_test_MMLAuto_lagtitehitrend$yhat <- predict(lr, newdata = temint_test_MMLAuto_lagtitehitrend)

mape_lr <- mean(abs(temint_test_MMLAuto_lagtitehitrend$y - temint_test_MMLAuto_lagtitehitrend$yhat)/temint_test_MMLAuto_lagtitehitrend$y)

h2o.init(max_mem_size = "16G")

train_h_MMLAuto_lagtitehitrend <- as.h2o(temint_train_MMLAuto_lagtitehitrend)
test_h_MMLAuto_lagtitehitrend <- as.h2o(temint_test_MMLAuto_lagtitehitrend)

x <- c("day", "lag12", "trend", "tetrend", "hitrend")
y <- "y"

gbm_md13 <- h2o.gbm(
  training_frame = train_h_MMLAuto_lagtitetrend,
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
test_h_MMLAuto_lagtitehitrend$pred_gbm <- h2o.predict(gbm_md13, test_h_MMLAuto_lagtitehitrend)
test_13 <- as.data.frame(test_h_MMLAuto_lagtitehitrend)
mape_gbm <- mean(abs(test_13$y - test_13$pred_gbm) / test_13$y)
mape_gbm

plot_ly(data = test_13) %>%
  add_lines(x = ~ date, y = ~y, name = "Actual") %>%
  #add_lines(x = ~ date, y = ~ yhat, name = "Linear Regression", line =
  #list(dash = "dot")) %>%
  add_lines(x = ~ date, y = ~ pred_gbm, name = "Gradient Boosting Machine",
            line = list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Prediction (Gradient
Boosting Machine)",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Día"))

sqrt( sum( (test_13$y - test_13$pred_gbm)^2 , na.rm = TRUE ) / nrow(test_13) )
sum((test_13$y - test_13$pred_gbm) , na.rm = TRUE) / nrow(test_13)
# Trend + lag + trend^2
temint_train_MMLAuto_lagtitehitrenderr <- temint_df_MMLGB[1:(unidades - h), ]
temint_test_MMLAuto_lagtitehitrenderr <- temint_df_MMLGB[(unidades - h + 1):unidades, ]

lr <- lm(y ~ day + lag12 + trend + trend_sqr + tetrend + tetrend_sqr + hitrend + hitrend_sqr, 
         data = temint_train_MMLAuto_lagtitehitrenderr)

temint_test_MMLAuto_lagtitehitrend$yhat <- predict(lr, newdata = temint_test_MMLAuto_lagtitehitrenderr)

mape_lr <- mean(abs(temint_test_MMLAuto_lagtitehitrenderr$y - temint_test_MMLAuto_lagtitehitrenderr$yhat)/temint_test_MMLAuto_lagtitehitrenderr$y)

h2o.init(max_mem_size = "16G")

train_h_MMLAuto_lagtitehitrenderr <- as.h2o(temint_train_MMLAuto_lagtitehitrenderr)
test_h_MMLAuto_lagtitehitrenderr <- as.h2o(temint_test_MMLAuto_lagtitehitrenderr)

x <- c("day", "lag12", "trend", "trend_sqr", "tetrend", "tetrend_sqr", "hitrend", "hitrend_sqr")
y <- "y"

gbm_md14 <- h2o.gbm(
  training_frame = train_h_MMLAuto_lagtitehitrenderr,
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
h2o.varimp_plot(gbm_md)
test_h_MMLAuto_lagtitehitrenderr$pred_gbm <- h2o.predict(gbm_md14, test_h_MMLAuto_lagtitehitrenderr)
test_14 <- as.data.frame(test_h_MMLAuto_lagtitehitrenderr)
mape_gbm <- mean(abs(test_14$y - test_14$pred_gbm) / test_14$y)
mape_gbm

plot_ly(data = test_14) %>%
  add_lines(x = ~ date, y = ~y, name = "Actual") %>%
  #add_lines(x = ~ date, y = ~ yhat, name = "Linear Regression", line =
              #list(dash = "dot")) %>%
  add_lines(x = ~ date, y = ~ pred_gbm, name = "Gradient Boosting Machine",
            line = list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Prediction (Gradient
Boosting Machine)",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Día"))

sqrt( sum( (test_14$y - test_14$pred_gbm)^2 , na.rm = TRUE ) / nrow(test_14) )
sum((test_14$y - test_14$pred_gbm) , na.rm = TRUE) / nrow(test_14)
#Temperatura exterior + temperatura interior + Humedad interior + humedad exterior#
# Trend +lag
temint_train_MMLAuto_lagtitehihetrend <- temint_df_MMLGB[1:(unidades - h), ]
temint_test_MMLAuto_lagtitehihetrend <- temint_df_MMLGB[(unidades - h + 1):unidades, ]

lr <- lm(y ~ day + lag12 + trend + tetrend + hitrend + hetrend, 
         data = temint_train_MMLAuto_lagtitehihetrend)

temint_test_MMLAuto_lagtitehitrenderr$yhat <- predict(lr, newdata = temint_test_MMLAuto_lagtitehihetrend)

mape_lr <- mean(abs(temint_test_MMLAuto_lagtitehihetrend$y - temint_test_MMLAuto_lagtitehihetrend$yhat)/temint_test_MMLAuto_lagtitehihetrend$y)

h2o.init(max_mem_size = "16G")

train_h_MMLAuto_lagtitehihetrend <- as.h2o(temint_train_MMLAuto_lagtitehihetrend)
test_h_MMLAuto_lagtitehihetrend <- as.h2o(temint_test_MMLAuto_lagtitehihetrend)

x <- c("day", "lag12", "trend", "tetrend", "hitrend", "hetrend")
y <- "y"

gbm_md15 <- h2o.gbm(
  training_frame = train_h_MMLAuto_lagtitehihetrend,
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
test_h_MMLAuto_lagtitehihetrend$pred_gbm <- h2o.predict(gbm_md15, test_h_MMLAuto_lagtitehihetrend)
test_15 <- as.data.frame(test_h_MMLAuto_lagtitehihetrend)
mape_gbm <- mean(abs(test_15$y - test_15$pred_gbm) / test_15$y)
mape_gbm

plot_ly(data = test_15) %>%
  add_lines(x = ~ date, y = ~y, name = "Actual") %>%
  add_lines(x = ~ date, y = ~ pred_gbm, name = "Gradient Boosting Machine",
            line = list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Prediction (Gradient
Boosting Machine)",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Día"))

sqrt( sum( (test_15$y - test_15$pred_gbm)^2 , na.rm = TRUE ) / nrow(test_15) )
sum((test_15$y - test_15$pred_gbm) , na.rm = TRUE) / nrow(test_15)
# Trend + lag + trend^2
temint_train_MMLAuto_lagtitehihetrenderr <- temint_df_MMLGB[1:(unidades - h), ]
temint_test_MMLAuto_lagtitehihetrenderr <- temint_df_MMLGB[(unidades - h + 1):unidades, ]

lr <- lm(y ~ day + lag12 + trend + trend_sqr + tetrend + tetrend_sqr + hitrend + hitrend_sqr + hetrend + hetrend_sqr, 
         data = temint_train_MMLAuto_lagtitehihetrenderr)

temint_test_MMLAuto_lagtitehihetrenderr$yhat <- predict(lr, newdata = temint_test_MMLAuto_lagtitehihetrenderr)

mape_lr <- mean(abs(temint_test_MMLAuto_lagtitehihetrenderr$y - temint_test_MMLAuto_lagtitehihetrenderr$yhat)/temint_test_MMLAuto_lagtitehihetrenderr$y)

h2o.init(max_mem_size = "16G")

train_h_MMLAuto_lagtitehihetrenderr <- as.h2o(temint_train_MMLAuto_lagtitehihetrenderr)
test_h_MMLAuto_lagtitehihetrenderr <- as.h2o(temint_test_MMLAuto_lagtitehihetrenderr)

x <- c("day", "lag12", "trend", "trend_sqr", "tetrend", "tetrend_sqr", "hitrend", "hitrend_sqr", "hetrend", "hetrend_sqr")
y <- "y"

gbm_md16 <- h2o.gbm(
  training_frame = train_h_MMLAuto_lagtitehihetrenderr,
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
test_h_MMLAuto_lagtitehihetrenderr$pred_gbm <- h2o.predict(gbm_md16, test_h_MMLAuto_lagtitehihetrenderr)
test_16 <- as.data.frame(test_h_MMLAuto_lagtitehihetrenderr)
mape_gbm <- mean(abs(test_16$y - test_16$pred_gbm) / test_16$y)
mape_gbm

plot_ly(data = test_16) %>%
  add_lines(x = ~ date, y = ~y, name = "Actual") %>%
  #add_lines(x = ~ date, y = ~ yhat, name = "Linear Regression", line =
              #list(dash = "dot")) %>%
  add_lines(x = ~ date, y = ~ pred_gbm, name = "Gradient Boosting Machine",
            line = list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Prediction (Gradient
Boosting Machine)",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Día"))

sqrt( sum( (test_16$y - test_16$pred_gbm)^2 , na.rm = TRUE ) / nrow(test_16) )
sum((test_16$y - test_16$pred_gbm) , na.rm = TRUE) / nrow(test_16)
################################################################################

############################ Machine Learning AutoML Multivariante DNE #############################################
temint_df_MMLAuto <- tibble(date = dato_interior$Fecha,
                          segundos = 60 * 10 * (as.numeric(rownames(dato_interior))-1),
                          semana_muestreo = 1 + (segundos - (segundos %% (3600 *24*7)))/(3600 *24*7),
                          segundo_muestreo_semana = segundos - (semana_muestreo-1) * (3600 *24*7),
                          day = 1 + (segundos - (segundos %% (3600 *24)))/(3600 *24),
                          y = temperatura_interior_detrend,
                          z = temperatura_exterior_detrend,
                          w = humedad_interior_detrend,
                          v = humedad_exterior_ts)
head(temint_df_MMLAuto)

temint_df_MMLAuto$lag12 <- lag(temint_df$y, n = 72)

temint_df_MMLAuto$trend <- decompose(temperatura_interior_ts)$trend
temint_df_MMLAuto$trend_sqr <- temint_df$trend ^ 2

temint_df_MMLAuto$tetrend <- decompose(temperatura_exterior_ts)$trend
temint_df_MMLAuto$tetrend_sqr <- temint_df$tetrend ^ 2

temint_df_MMLAuto$hitrend <- decompose(humedad_interior_ts)$trend
temint_df_MMLAuto$hitrend_sqr <- temint_df$hitrend ^ 2

temint_df_MMLAuto$hetrend <- decompose(humedad_exterior_ts)$trend
temint_df_MMLAuto$hetrend_sqr <- temint_df$hetrend ^ 2

h <- 627

###########Temperatura exterior + temperatura interior
# Trend +lag

temint_train_MMLAML_lagtetrend <- temint_df_MMLAuto[1:(unidades - h), ]
temint_test_MMLAML_lagtetrend <- temint_df_MMLAuto[(unidades - h + 1):unidades, ]

lr <- lm(y ~ day + lag12 + trend + tetrend, 
         data = temint_train_MMLAML_lagtetrend)

temint_test_MMLAML_lagtetrend$yhat <- predict(lr, newdata = temint_test_MMLAML_lagtetrend)

mape_lr <- mean(abs(temint_test_MMLAML_lagtetrend$y - temint_test_MMLAML_lagtetrend$yhat)/temint_test_MMLAML_lagtetrend$y)

h2o.init(max_mem_size = "16G")

train_h_MMLAML_lagtetrend <- as.h2o(temint_train_MMLAML_lagtetrend)
test_h_MMLAML_lagtetrend <- as.h2o(temint_test_MMLAML_lagtetrend)

x <- c("day", "lag12", "trend", "tetrend")
y <- "y"



autoML21 <- h2o.automl(training_frame = train_h_MMLAML_lagtetrend,
                      x = x,
                      y = y,
                      nfolds = 5,
                      max_runtime_secs = 60*20,
                      seed = 1234)

test_h_MMLAML_lagtetrend$pred_autoML <- h2o.predict(autoML21@leader, test_h_MMLAML_lagtetrend)
test_21 <- as.data.frame(test_h_MMLAML_lagtetrend)
mape_autoML <- mean(abs(test_21$y - test_21$pred_autoML) / test_21$y)
mape_autoML

plot_ly(data = test_21) %>%
  add_lines(x = ~ date, y = ~y, name = "Actual") %>%
  add_lines(x = ~ date, y = ~ pred_autoML, name = "autoML", line =
              list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Prediction (Auto ML
Model)",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Días"))

sqrt( sum( (test_21$y - test_21$pred_autoML)^2 , na.rm = TRUE ) / nrow(test_21) )
sum((test_21$y - test_21$pred_autoML) , na.rm = TRUE) / nrow(test_21)
# Trend + lag + trend^2
temint_train_MMLAML_lagtetrenderr <- temint_df_MMLAuto[1:(unidades - h), ]
temint_test_MMLAML_lagtetrenderr <- temint_df_MMLAuto[(unidades - h + 1):unidades, ]

lr <- lm(y ~ day + lag12 + trend + trend_sqr + tetrend + tetrend_sqr, 
         data = temint_train_MMLAML_lagtetrenderr)

temint_test_MMLAML_lagtetrend$yhat <- predict(lr, newdata = temint_test_MMLAML_lagtetrenderr)

mape_lr <- mean(abs(temint_test_MMLAML_lagtetrenderr$y - temint_test_MMLAML_lagtetrenderr$yhat)/temint_test_MMLAML_lagtetrenderr$y)

h2o.init(max_mem_size = "16G")

train_h_MMLAML_lagtetrenderr <- as.h2o(temint_train_MMLAML_lagtetrenderr)
test_h_MMLAML_lagtetrenderr <- as.h2o(temint_test_MMLAML_lagtetrenderr)

x <- c("day", "lag12", "trend", "trend_sqr", "tetrend", "tetrend_sqr")
y <- "y"



autoML22 <- h2o.automl(training_frame = train_h_MMLAML_lagtetrenderr,
                      x = x,
                      y = y,
                      nfolds = 5,
                      max_runtime_secs = 60*20,
                      seed = 1234)

test_h_MMLAML_lagtetrenderr$pred_autoML <- h2o.predict(autoML22@leader, test_h_MMLAML_lagtetrenderr)
test_22 <- as.data.frame(test_h_MMLAML_lagtetrenderr)
mape_autoML <- mean(abs(test_22$y - test_22$pred_autoML) / test_22$y)
mape_autoML

plot_ly(data = test_22) %>%
  add_lines(x = ~ date, y = ~y, name = "Actual") %>%
  add_lines(x = ~ date, y = ~ pred_autoML, name = "autoML", line =
              list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Prediction (Auto ML
Model)",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Días"))

sqrt( sum( (test_22$y - test_22$pred_autoML)^2 , na.rm = TRUE ) / nrow(test_22) )
sum((test_22$y - test_22$pred_autoML) , na.rm = TRUE) / nrow(test_22)
########Temperatura exterior + temperatura interior + Humedad interior
# Trend +lag
temint_train_MMLAML_lagtitehitrend <- temint_df_MMLAuto[1:(unidades - h), ]
temint_test_MMLAML_lagtitehitrend <- temint_df_MMLAuto[(unidades - h + 1):unidades, ]

lr <- lm(y ~ day + lag12 + trend + tetrend + hitrend, 
         data = temint_train_MMLAML_lagtitehitrend)

temint_test_MMLAML_lagtitehitrend$yhat <- predict(lr, newdata = temint_test_MMLAML_lagtitehitrend)

mape_lr <- mean(abs(temint_test_MMLAML_lagtitehitrend$y - temint_test_MMLAML_lagtitehitrend$yhat)/temint_test_MMLAML_lagtitehitrend$y)

h2o.init(max_mem_size = "16G")

train_h_MMLAML_lagtitehitrend <- as.h2o(temint_train_MMLAML_lagtitehitrend)
test_h_MMLAML_lagtitehitrend <- as.h2o(temint_test_MMLAML_lagtitehitrend)

x <- c("day", "lag12", "trend", "tetrend", "hitrend")
y <- "y"



autoML23 <- h2o.automl(training_frame = train_h_MMLAML_lagtitehitrend,
                      x = x,
                      y = y,
                      nfolds = 5,
                      max_runtime_secs = 60*20,
                      seed = 1234)

test_h_MMLAML_lagtitehitrend$pred_autoML <- h2o.predict(autoML23@leader, test_h_MMLAML_lagtitehitrend)
test_23 <- as.data.frame(test_h_MMLAML_lagtitehitrend)
mape_autoML <- mean(abs(test_23$y - test_23$pred_autoML) / test_23$y)
mape_autoML

plot_ly(data = test_23) %>%
  add_lines(x = ~ date, y = ~y, name = "Actual") %>%
  add_lines(x = ~ date, y = ~ pred_autoML, name = "autoML", line =
              list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Prediction (Auto ML
Model)",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Días"))

sqrt( sum( (test_23$y - test_23$pred_autoML)^2 , na.rm = TRUE ) / nrow(test_23) )
sum((test_23$y - test_23$pred_autoML) , na.rm = TRUE) / nrow(test_23)
# Trend + lag + trend^2
temint_train_MMLAML_lagtitehitrenderr <- temint_df_MMLAuto[1:(unidades - h), ]
temint_test_MMLAML_lagtitehitrenderr <- temint_df_MMLAuto[(unidades - h + 1):unidades, ]

lr <- lm(y ~ day + lag12 + trend + trend_sqr + tetrend + tetrend_sqr + hitrend + hitrend_sqr, 
         data = temint_train_MMLAML_lagtitehitrenderr)

temint_test_MMLAML_lagtitehitrenderr$yhat <- predict(lr, newdata = temint_test_MMLAML_lagtitehitrenderr)

mape_lr <- mean(abs(temint_test_MMLAML_lagtitehitrenderr$y - temint_test_MMLAML_lagtitehitrenderr$yhat)/temint_test_MMLAML_lagtitehitrenderr$y)

h2o.init(max_mem_size = "16G")

train_h_MMLAML_lagtitehitrenderr <- as.h2o(temint_train_MMLAML_lagtitehitrenderr)
test_h_MMLAML_lagtitehitrenderr <- as.h2o(temint_test_MMLAML_lagtitehitrenderr)

x <- c("day", "lag12", "trend", "trend_sqr", "tetrend", "tetrend_sqr", "hitrend", "hitrend_sqr")
y <- "y"



autoML24 <- h2o.automl(training_frame = train_h_MMLAML_lagtitehitrenderr,
                      x = x,
                      y = y,
                      nfolds = 5,
                      max_runtime_secs = 60*20,
                      seed = 1234)

test_h_MMLAML_lagtitehitrenderr$pred_autoML <- h2o.predict(autoML24@leader, test_h_MMLAML_lagtitehitrenderr)
test_24 <- as.data.frame(test_h_MMLAML_lagtitehitrenderr)
mape_autoML <- mean(abs(test_24$y - test_24$pred_autoML) / test_24$y)
mape_autoML

plot_ly(data = test_24) %>%
  add_lines(x = ~ date, y = ~y, name = "Actual") %>%
  add_lines(x = ~ date, y = ~ pred_autoML, name = "autoML", line =
              list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Prediction (Auto ML
Model)",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Días"))

sqrt( sum( (test_24$y - test_24$pred_autoML)^2 , na.rm = TRUE ) / nrow(test_24) )
sum((test_24$y - test_24$pred_autoML) , na.rm = TRUE) / nrow(test_24)
#Temperatura exterior + temperatura interior + Humedad interior + humedad exterior
# Trend +lag
temint_train_MMLAML_lagtitehihetrend <- temint_df_MMLAuto[1:(unidades - h), ]
temint_test_MMLAML_lagtitehihetrend <- temint_df_MMLAuto[(unidades - h + 1):unidades, ]

lr <- lm(y ~ day + lag12 + trend + tetrend + hitrend + hetrend, 
         data = temint_train_MMLAML_lagtitehihetrend)

temint_test_MMLAML_lagtitehihetrend$yhat <- predict(lr, newdata = temint_test_MMLAML_lagtitehihetrend)

mape_lr <- mean(abs(temint_test_MMLAML_lagtitehihetrend$y - temint_test_MMLAML_lagtitehihetrend$yhat)/temint_test_MMLAML_lagtitehihetrend$y)

h2o.init(max_mem_size = "16G")

train_h_MMLAML_lagtitehihetrend <- as.h2o(temint_train_MMLAML_lagtitehihetrend)
test_h_MMLAML_lagtitehihetrend <- as.h2o(temint_test_MMLAML_lagtitehihetrend)

x <- c("day", "lag12", "trend", "tetrend", "hitrend", "hetrend")
y <- "y"



autoML25 <- h2o.automl(training_frame = train_h_MMLAML_lagtitehihetrend,
                      x = x,
                      y = y,
                      nfolds = 5,
                      max_runtime_secs = 60*20,
                      seed = 1234)

test_h_MMLAML_lagtitehihetrend$pred_autoML <- h2o.predict(autoML25@leader, test_h_MMLAML_lagtitehihetrend)
test_25 <- as.data.frame(test_h_MMLAML_lagtitehihetrend)
mape_autoML <- mean(abs(test_25$y - test_25$pred_autoML) / test_25$y)
mape_autoML

plot_ly(data = test_25) %>%
  add_lines(x = ~ date, y = ~y, name = "Actual") %>%
  add_lines(x = ~ date, y = ~ pred_autoML, name = "autoML", line =
              list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Prediction (Auto ML
Model)",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Días"))

sqrt( sum( (test_25$y - test_25$pred_autoML)^2 , na.rm = TRUE ) / nrow(test_25) )
sum((test_25$y - test_25$pred_autoML) , na.rm = TRUE) / nrow(test_25)
# Trend + lag + trend^2
temint_train_MMLAML_lagtitehihetrenderr <- temint_df_MMLAuto[1:(unidades - h), ]
temint_test_MMLAML_lagtitehihetrenderr <- temint_df_MMLAuto[(unidades - h + 1):unidades, ]

lr <- lm(y ~ day + lag12 + trend + trend_sqr + tetrend + tetrend_sqr + hitrend + hitrend_sqr + hetrend + hetrend_sqr, 
         data = temint_train_MMLAML_lagtitehihetrenderr)

temint_test_MMLAML_lagtitehihetrenderr$yhat <- predict(lr, newdata = temint_test_MMLAML_lagtitehihetrenderr)

mape_lr <- mean(abs(temint_test_MMLAML_lagtitehihetrenderr$y - temint_test_MMLAML_lagtitehihetrenderr$yhat)/temint_test_MMLAML_lagtitehihetrenderr$y)

h2o.init(max_mem_size = "16G")

train_h_MMLAML_lagtitehihetrenderr <- as.h2o(temint_train_MMLAML_lagtitehihetrenderr)
test_h_MMLAML_lagtitehihetrenderr <- as.h2o(temint_test_MMLAML_lagtitehihetrenderr)

x <- c("day", "lag12", "trend", "trend_sqr", "tetrend", "tetrend_sqr", "hitrend", "hitrend_sqr", "hetrend", "hetrend_sqr")
y <- "y"



autoML26 <- h2o.automl(training_frame = train_h_MMLAML_lagtitehihetrenderr,
                      x = x,
                      y = y,
                      nfolds = 5,
                      max_runtime_secs = 60*20,
                      seed = 1234)

test_h_MMLAML_lagtitehihetrenderr$pred_autoML <- h2o.predict(autoML26@leader, test_h_MMLAML_lagtitehihetrenderr)
test_26 <- as.data.frame(test_h_MMLAML_lagtitehihetrenderr)
mape_autoML <- mean(abs(test_26$y - test_26$pred_autoML) / test_26$y)
mape_autoML

plot_ly(data = test_26) %>%
  add_lines(x = ~ date, y = ~y, name = "Actual") %>%
  add_lines(x = ~ date, y = ~ pred_autoML, name = "autoML", line =
              list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Prediction (Auto ML
Model)",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Días"))

sqrt( sum( (test_26$y - test_26$pred_autoML)^2 , na.rm = TRUE ) / nrow(test_26) )
sum((test_26$y - test_26$pred_autoML) , na.rm = TRUE) / nrow(test_26)
################################################################################