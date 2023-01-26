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
ts_cor(temperatura_interior_ts, lag.max = 72) # Fuerte tendencia
ts_cor(humedad_exterior_ts, lag.max = 72) # Tendencia y estacionalidad
ts_cor(temperatura_exterior_ts, lag.max = 72) # Tendencia y estacionalidad
# Se guardan

## Lag analysis sirve para comprobar la autocorrelacion que hay entre los datos
ts_lags(temperatura_interior_ts, lags = c(1, 33, 72,144))

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
tempint_df <- tibble(ds = dato_interior$Fecha,weekday = weekdays(dato_interior$Fecha),
                     segundos = 60 * 10 * (as.numeric(rownames(dato_interior))-1),
                     dia_muestreo = 1 + (segundos - (segundos %% (3600 *12)))/(3600 *12),
                     segundos_dia_muestreo = segundos - (dia_muestreo-1) * (3600 *12),
                     y = temperatura_interior_ts)
head(tempint_df)

tempint_df$trend <- decompose(temperatura_interior_ts)$trend
tempint_df$seasonal <- decompose(temperatura_interior_ts)$seasonal
h <- 627

# Solo la tendencia de la Temperatura interior

tempint_train_RL_trend <- tempint_df[1:(unidades - h), ]
tempint_test_RL_trend <- tempint_df[(unidades - h + 1):unidades, ]
md_trend <- lm(y ~ trend, data = tempint_train_RL_trend)
summary(md_trend)
car::vif(md_trend)

checkresiduals(md_trend)
res_trend <- residuals(md_trend)
model_residuals_md_trend <- auto.arima(res_trend)
summary(model_residuals_md_trend)
res_model_residuals_md_trend <- residuals(model_residuals_md_trend)

ts_cor(res_model_residuals_md_trend, lag.max = 72)
ts_cor(res_model_residuals_md_trend)

checkresiduals(model_residuals_md_trend)

tempint_train_RL_trend$yhat <- predict(model_residuals_md_trend, newdata = tempint_train_RL_trend)
tempint_test_RL_trend$yhat <- predict(model_residuals_md_trend, newdata = tempint_test_RL_trend)

plot_lm(data = tempint_df,
        train = tempint_train_RL_trend,
        test = tempint_test_RL_trend,
        title = "Predicción de la tendencia de la serie")

forecast::accuracy(model_residuals_md_trend)
sqrt( sum( (test_14$y - test_14$pred_gbm)^2 , na.rm = TRUE ) / nrow(test_14) )
sum((test_14$y - test_14$pred_gbm) , na.rm = TRUE) / nrow(test_14)
sum(abs((test_14$y - test_14$pred_gbm)/ nrow(test_14)) , na.rm = TRUE)  #MAPE



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

checkresiduals(md2) # Los residuos NO son ruido blanco: Muestran autocorrelacion
res_md2 <- residuals(md2)
max(res_md2) # El max. coincide con la imagen que reporta checkresiduals
min(res_md2) # El min. coincide con la imagen que reporta checkresiduals

# Solución intentar modelar los residuos del modelo md2 con un arima
model_residuals_md2 <- auto.arima(res_md2)
summary(model_residuals_md2)
res_model_residuals_md2 <- residuals(model_residuals_md2)

acf(res_model_residuals_md2) # Más o menos limpio
pacf(res_model_residuals_md2) # Más o menos limpio
ts_cor(res_model_residuals_md2, lag.max = 72)
ts_cor(res_model_residuals_md2)

checkresiduals(model_residuals_md2)
ti_split_md2 <- ts_split(temperatura_interior_ts, sample.out = 627)

ti_train_md2 <- ti_split_md2$train
ti_test_md2 <- ti_split$test

ti_best_md2 <- arima(na.remove(ti_train_md2), 
                    order = c(5,0,0), 
                    seasonal = list(order = c(0,0,0)),
                    method = "CSS")

ti_test_fc <- forecast(ti_best_md2, h = 627)


test_forecast(temperatura_interior_ts,
              forecast.obj = ti_test_fc,
              test = ti_test)

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



##################### ARIMA ####################################################

##############################################################################

##########################ML Gradient Boost###################################
########################## UNIVARIABLE
temint_dfMLGB <- tibble(date = dato_interior$Fecha,
                        segundos = 60 * 10 * (as.numeric(rownames(dato_interior))-1),
                        day = 1 + (segundos - (segundos %% (3600 *12)))/(3600 *12),
                        y = temperatura_interior_detrend)
temint_dfMLGB$lag12 <- stats::lag(temint_dfMLGB$y, n = 72)

temint_dfMLGB$trend <- decompose(temperatura_interior_detrend)$trend
temint_dfMLGB$seasonal <- decompose(temperatura_interior_detrend)$seasonal
h <- 627

### TENDENCIA
temint_train_MLGB_trend <- temint_dfMLGB[1:(unidades - h), ]
temint_test_MLGB_trend <- temint_dfMLGB[(unidades - h + 1):unidades, ]

h2o.init(max_mem_size = "16G")

train_h_MLGB_trend <- as.h2o(temint_train_MLGB_trend)
test_h_MLGB_trend <- as.h2o(temint_test_MLGB_trend)

x <- c("trend")
y <- "y"

gbm_md <- h2o.gbm(
  training_frame = train_h_MLGB_trend,
  nfolds = 5,
  x = x,
  y = y,
  max_depth = 20,
  distribution = "gaussian",
  ntrees = 500,
  learn_rate = 0.1,
  score_each_iteration = TRUE
)
test_h_MLGB_trend$pred_gbm <- h2o.predict(gbm_md, test_h_MLGB_trend)
test_1 <- as.data.frame(test_h_MLGB_trend)

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
temint_train_MLGB_seasonal <- temint_dfMLGB[1:(unidades - h), ]
temint_test_MLGB_seasonal <- temint_dfMLGB[(unidades - h + 1):unidades, ]

h2o.init(max_mem_size = "16G")

train_h_MLGB_seasonal <- as.h2o(temint_train_MLGB_seasonal)
test_h_MLGB_seasonal <- as.h2o(temint_test_MLGB_seasonal)

x <- c("seasonal")
y <- "y"

gbm_md <- h2o.gbm(
  training_frame = train_h_MLGB_seasonal,
  nfolds = 5,
  x = x,
  y = y,
  max_depth = 20,
  distribution = "gaussian",
  ntrees = 500,
  learn_rate = 0.1,
  score_each_iteration = TRUE
)
test_h_MLGB_seasonal$pred_gbm <- h2o.predict(gbm_md, test_h_MLGB_seasonal)
test_2 <- as.data.frame(test_h_MLGB_seasonal)

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
temint_train_MLGB_lag <- temint_dfMLGB[1:(unidades - h), ]
temint_test_MLGB_lag <- temint_dfMLGB[(unidades - h + 1):unidades, ]

h2o.init(max_mem_size = "16G")

train_h_MLGB_lag <- as.h2o(temint_train_MLGB_lag)
test_h_MLGB_lag <- as.h2o(temint_test_MLGB_lag)

x <- c("lag12")
y <- "y"

gbm_md <- h2o.gbm(
  training_frame = train_h_MLGB_lag,
  nfolds = 5,
  x = x,
  y = y,
  max_depth = 20,
  distribution = "gaussian",
  ntrees = 500,
  learn_rate = 0.1,
  score_each_iteration = TRUE
)
test_h_MLGB_lag$pred_gbm <- h2o.predict(gbm_md, test_h_MLGB_lag)
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
sum(abs((test_3$y - test_2$pred_gbm)/ nrow(test_3)) , na.rm = TRUE)  #MAPE
##############################################################################

#########################ML AutoML############################################
##############################################################################
##############################################################################
##############################################################################
