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


##############Analisis de Series Temporales#####################################
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

####################### DECLARAR EL DATA FRAME##################################
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
################################################################################

########################### REGRESION LINEAL ###################################
########################## UNIVARIABLE
# TENDENCIA
md1_1 <- tslm(teminterior.train.ts ~ trend, data = train_df)
checkresiduals(md1_1)

x_reg_md2_1 = cbind(
  train_df$trend
)

md2_1 <- auto.arima(teminterior.train.ts,
                    xreg = x_reg_md2_1,
                    seasonal = TRUE,
                    stepwise = TRUE,
                    approximation = FALSE)
summary(md2_1)
checkresiduals(md2_1)
fc1_1 <- forecast(md1_1, newdata = test_df)

test_model_1 <- cbind(
  test_df$trend
)

fc2_1 <- forecast(md2_1, xreg = na.omit(test_model_1))
forecast::accuracy(fc1_1, teminterior.test.ts)
forecast::accuracy(fc2_1, teminterior.test.ts)
plot(fc1_1)
plot(fc2_1) # LINEAL PERO CONCUERDA CON LA TRAYECTORIA DE TESTEO

# TEMPERATURA INTERIOR --> SALE LO MISMO QUE LA TEMPERATURA INTERIOR
md1_2 <- tslm(teminterior.train.ts ~ ti, data = train_df)
checkresiduals(md1_2)

fc1_2 <- forecast(md1_2, newdata = test_df)

forecast::accuracy(fc1_2, teminterior.test.ts)

plot(fc1_2) #NO CUENTA AL SER DIREACTAMENTE LOS DATOS DE MUESTREO

# LAG 72 TEMPERATURA INTERIOR
md1_3 <- tslm(teminterior.train.ts ~ ti_lag72, data = train_df)
checkresiduals(md1_3)

x_reg_md2_3 = cbind(
  train_df$ti_lag72
)

md2_3 <- auto.arima(teminterior.train.ts,
                    xreg = x_reg_md2_3,
                    seasonal = TRUE,
                    stepwise = TRUE,
                    approximation = FALSE)
summary(md2_3)
checkresiduals(md2_3)
fc1_3 <- forecast(md1_3, newdata = test_df)

test_model_3 <- cbind(
  test_df$ti_lag72
)

fc2_3 <- forecast(md2_3, xreg = na.omit(test_model_3))
forecast::accuracy(fc1_3, teminterior.test.ts)
forecast::accuracy(fc2_3, teminterior.test.ts)
plot(fc1_3)
plot(fc2_3) # DEMASIADO RECTA

# TENDENCIA AL CUADRADO
md1_4 <- tslm(teminterior.train.ts ~ trend_sqr, data = train_df)
checkresiduals(md1_4)

x_reg_md2_4 = cbind(
  train_df$trend_sqr
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
  test_df$trend_sqr
)

fc2_4 <- forecast(md2_4, xreg = na.omit(test_model_4))
forecast::accuracy(fc1_4, teminterior.test.ts)
forecast::accuracy(fc2_4, teminterior.test.ts)
plot(fc1_4)
plot(fc2_4) # DESCIENDE MUCHO, PERO ES LA SEGUNDA MEJOR

# DIA DE LA SEMANA
md1_5 <- tslm(teminterior.train.ts ~ weekday, data = train_df)
checkresiduals(md1_5)

x_reg_md2_5 = cbind(
  model.matrix(~ weekday,train_df)[,-1]
)

md2_5 <- auto.arima(teminterior.train.ts,
                    xreg = x_reg_md2_5,
                    seasonal = TRUE,
                    stepwise = TRUE,
                    approximation = FALSE)
summary(md2_5)
checkresiduals(md2_5)
fc1_5 <- forecast(md1_5, newdata = test_df)

test_model_5 <- cbind(
  model.matrix(~ weekday,test_df)[,-1]
)

fc2_5 <- forecast(md2_5, xreg = na.omit(test_model_5))
forecast::accuracy(fc1_5, teminterior.test.ts)
forecast::accuracy(fc2_5, teminterior.test.ts)
plot(fc1_5)
plot(fc2_5) # TAMBIEN MUY RECTA

######################### MULTIVARIABLE
# SE UTILIZARA LA TENDENCIA COMO BASE PARA 
# TENDENCIA Y TENDENCIA CUADRADO
md1_6 <- tslm(teminterior.train.ts ~ trend + trend_sqr, data = train_df)
checkresiduals(md1_6)

x_reg_md2_6 = cbind(
  train_df$trend,
  train_df$trend_sqr
)

md2_6 <- auto.arima(teminterior.train.ts,
                    xreg = x_reg_md2_6,
                    seasonal = TRUE,
                    stepwise = TRUE,
                    approximation = FALSE)
summary(md2_6)
checkresiduals(md2_6)
fc1_6 <- forecast(md1_6, newdata = test_df)

test_model_6 <- cbind(
  test_df$trend,
  test_df$trend_sqr
)

fc2_6 <- forecast(md2_6, xreg = na.omit(test_model_6))
forecast::accuracy(fc1_6, teminterior.test.ts)
forecast::accuracy(fc2_6, teminterior.test.ts)
plot(fc1_6)
plot(fc2_6)

# TENDENCIA Y HUMEDAD INTERIOR
md1_7 <- tslm(teminterior.train.ts ~ trend + hi, data = train_df)
checkresiduals(md1_7)

x_reg_md2_7 = cbind(
  train_df$trend,
  train_df$hi
)

md2_7 <- auto.arima(teminterior.train.ts,
                    xreg = x_reg_md2_7,
                    seasonal = TRUE,
                    stepwise = TRUE,
                    approximation = FALSE)
summary(md2_7)
checkresiduals(md2_7)
fc1_7 <- forecast(md1_7, newdata = test_df)

test_model_7 <- cbind(
  test_df$trend,
  test_df$hi
)

fc2_7 <- forecast(md2_7, xreg = na.omit(test_model_7))
forecast::accuracy(fc1_7, teminterior.test.ts)
forecast::accuracy(fc2_7, teminterior.test.ts)
plot(fc1_7)
plot(fc2_7)

# TENDENCIA Y LAG72
md1_8 <- tslm(teminterior.train.ts ~ trend + ti_lag72, data = train_df)
checkresiduals(md1_8)

x_reg_md2_8 = cbind(
  train_df$trend,
  train_df$ti_lag72
)

md2_8 <- auto.arima(teminterior.train.ts,
                    xreg = x_reg_md2_8,
                    seasonal = TRUE,
                    stepwise = TRUE,
                    approximation = FALSE)
summary(md2_8)
checkresiduals(md2_8)
fc1_8 <- forecast(md1_8, newdata = test_df)

test_model_8 <- cbind(
  test_df$trend,
  test_df$ti_lag72
)

fc2_8 <- forecast(md2_8, xreg = na.omit(test_model_8))
forecast::accuracy(fc1_8, teminterior.test.ts)
forecast::accuracy(fc2_8, teminterior.test.ts)
plot(fc1_8)
plot(fc2_8)

# TENDENCIA Y TEMPERATURA EXTERIOR
md1_9 <- tslm(teminterior.train.ts ~ trend + te, data = train_df)
checkresiduals(md1_9)

x_reg_md2_9 = cbind(
  train_df$trend,
  train_df$te
)

md2_9 <- auto.arima(teminterior.train.ts,
                    xreg = x_reg_md2_9,
                    seasonal = TRUE,
                    stepwise = TRUE,
                    approximation = FALSE)
summary(md2_9)
checkresiduals(md2_9)
fc1_9 <- forecast(md1_9, newdata = test_df)

test_model_9 <- cbind(
  test_df$trend,
  test_df$te
)

fc2_9 <- forecast(md2_9, xreg = na.omit(test_model_9))
forecast::accuracy(fc1_9, teminterior.test.ts)
forecast::accuracy(fc2_9, teminterior.test.ts)
plot(fc1_9)
plot(fc2_9)

# TENDENCIA Y HUMEDAD EXTERIOR
md1_10 <- tslm(teminterior.train.ts ~ trend + he, data = train_df)
checkresiduals(md1_10)

x_reg_md2_10 = cbind(
  train_df$trend,
  train_df$he
)

md2_10 <- auto.arima(teminterior.train.ts,
                    xreg = x_reg_md2_10,
                    seasonal = TRUE,
                    stepwise = TRUE,
                    approximation = FALSE)
summary(md2_10)
checkresiduals(md2_10)
fc1_10 <- forecast(md1_10, newdata = test_df)

test_model_10 <- cbind(
  test_df$trend,
  test_df$he
)

fc2_10 <- forecast(md2_10, xreg = na.omit(test_model_10))
forecast::accuracy(fc1_10, teminterior.test.ts)
forecast::accuracy(fc2_10, teminterior.test.ts)
plot(fc1_10)
plot(fc2_10)

# TENDENCIA, HUMEDAD INTERIOR, TEMPERATURA EXTERIOR Y HUMEDAD EXTERIOR
md1_11 <- tslm(teminterior.train.ts ~ trend + te + hi + he, data = train_df)
checkresiduals(md1_11)

x_reg_md2_11 = cbind(
  train_df$trend,
  train_df$te,
  train_df$hi,
  train_df$he
)

md2_11 <- auto.arima(teminterior.train.ts,
                    xreg = x_reg_md2_11,
                    seasonal = TRUE,
                    stepwise = TRUE,
                    approximation = FALSE)
summary(md2_11)
checkresiduals(md2_11)
fc1_11 <- forecast(md1_11, newdata = test_df)

test_model_11 <- cbind(
  train_df$trend,
  train_df$te,
  train_df$hi,
  train_df$he
)

fc2_11 <- forecast(md2_11, xreg = na.omit(test_model_11))
forecast::accuracy(fc1_11, teminterior.test.ts)
forecast::accuracy(fc2_11, teminterior.test.ts)
plot(fc1_11)
plot(fc2_11)
################################################################################

########################### ARIMA ##############################################

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
forecast::accuracy(ti_test_auto2, ti_test)

plot(ti_test_auto1)
plot(ti_test_auto2)
# Serie temporal de la temperatura interior en logaritmo
ti_split_log <- ts_split(h2o::log(temperatura_interior_ts), sample.out = 627)


ti_train_log <- ti_split_log$train
ti_test_log <- ti_split_log$test

ti_auto_md1_log <- auto.arima(ti_train_log)

ti_test_auto1_log <- forecast(ti_auto_md1_log, h = 627)
forecast::accuracy(ti_test_auto1_log, ti_test_log)

ti_auto_md2_log <- auto.arima(ti_train,
                              max.order = 5,
                              D = 1,
                              d = 1,
                              stepwise = FALSE,
                              approximation = FALSE)

ti_test_auto2_log <- forecast(ti_auto_md2_log, h = 627)
forecast::accuracy(ti_test_auto2_log, ti_test_log)

plot(ti_test_auto1_log)
plot(ti_test_auto2_log)

################################################################################

#################### MACHINE LEARNING GRADIENT BOOST ###########################
########################## UNIVARIABLE
h2o.init(max_mem_size = "16G")
train_h <- as.h2o(train_df)
test_h <- as.h2o(test_df)

# TENDENCIA
x <- c("trend")
y <- "ti"

gbm_md_1 <- h2o.gbm(
  training_frame = train_h,
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
h2o.varimp_plot(gbm_md_1)
test_h$pred_gbm_1 <- h2o.predict(gbm_md_1, test_h)
test_1 <- as.data.frame(test_h)

plot_ly(data = test_1) %>%
  add_lines(x = ~ ds, y = ~ ti, name = "Actual") %>%
  add_lines(x = ~ ds, y = ~ pred_gbm_1, name = "Gradient Boosting ML",
            line = list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Prediction (Gradient
Boosting ML)",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Día"))

sqrt( sum( (test_1$ti - test_1$pred_gbm_1)^2 , na.rm = TRUE ) / nrow(test_1) )
sum((test_1$ti - test_1$pred_gbm_1) , na.rm = TRUE) / nrow(test_1)
sum(abs((test_1$ti - test_1$pred_gbm_1)/ nrow(test_1)) , na.rm = TRUE)  #MAPE

# LAG 72 TEMPERATURA INTERIOR
x <- c("ti_lag72")
y <- "ti"

gbm_md_2 <- h2o.gbm(
  training_frame = train_h,
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
h2o.varimp_plot(gbm_md_2)
test_h$pred_gbm_2 <- h2o.predict(gbm_md_2, test_h)
test_2 <- as.data.frame(test_h)

plot_ly(data = test_2) %>%
  add_lines(x = ~ ds, y = ~ ti, name = "Actual") %>%
  add_lines(x = ~ ds, y = ~ pred_gbm_2, name = "Gradient Boosting ML",
            line = list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Prediction (Gradient
Boosting ML)",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Día"))

sqrt( sum( (test_2$ti - test_2$pred_gbm_2)^2 , na.rm = TRUE ) / nrow(test_2) )
sum((test_2$ti - test_2$pred_gbm_2) , na.rm = TRUE) / nrow(test_2)
sum(abs((test_2$ti - test_2$pred_gbm_2)/ nrow(test_2)) , na.rm = TRUE)  #MAPE

# TENDENCIA AL CUADRADO
x <- c("trend_sqr")
y <- "ti"

gbm_md_3 <- h2o.gbm(
  training_frame = train_h,
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
h2o.varimp_plot(gbm_md_3)
test_h$pred_gbm_3 <- h2o.predict(gbm_md_3, test_h)
test_3 <- as.data.frame(test_h)

plot_ly(data = test_3) %>%
  add_lines(x = ~ ds, y = ~ ti, name = "Actual") %>%
  add_lines(x = ~ ds, y = ~ pred_gbm_3, name = "Gradient Boosting ML",
            line = list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Prediction (Gradient
Boosting ML)",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Día"))

sqrt( sum( (test_3$ti - test_3$pred_gbm_3)^2 , na.rm = TRUE ) / nrow(test_3) )
sum((test_3$ti - test_3$pred_gbm_3) , na.rm = TRUE) / nrow(test_3)
sum(abs((test_3$ti - test_3$pred_gbm_3)/ nrow(test_3)) , na.rm = TRUE)  #MAPE
# DIA DE LA SEMANA
x <- c("weekday")
y <- "ti"

gbm_md_4 <- h2o.gbm(
  training_frame = train_h,
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
h2o.varimp_plot(gbm_md_4)
test_h$pred_gbm_4 <- h2o.predict(gbm_md_4, test_h)
test_4 <- as.data.frame(test_h)

plot_ly(data = test_4) %>%
  add_lines(x = ~ ds, y = ~ ti, name = "Actual") %>%
  add_lines(x = ~ ds, y = ~ pred_gbm_4, name = "Gradient Boosting ML",
            line = list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Prediction (Gradient
Boosting ML)",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Día"))

sqrt( sum( (test_4$ti - test_4$pred_gbm_4)^2 , na.rm = TRUE ) / nrow(test_4) )
sum((test_4$ti - test_4$pred_gbm_4) , na.rm = TRUE) / nrow(test_4)
sum(abs((test_4$ti - test_4$pred_gbm_4)/ nrow(test_4)) , na.rm = TRUE)  #MAPE
######################### MULTIVARIABLE
# TENDENCIA Y TENDENCIA CUADRADO
x <- c("trend", "trend_sqr")
y <- "ti"

gbm_md_5 <- h2o.gbm(
  training_frame = train_h,
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
h2o.varimp_plot(gbm_md_5)
test_h$pred_gbm_5 <- h2o.predict(gbm_md_5, test_h)
test_5 <- as.data.frame(test_h)

plot_ly(data = test_5) %>%
  add_lines(x = ~ ds, y = ~ ti, name = "Actual") %>%
  add_lines(x = ~ ds, y = ~ pred_gbm_5, name = "Gradient Boosting ML",
            line = list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Prediction (Gradient
Boosting ML)",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Día"))

sqrt( sum( (test_5$ti - test_5$pred_gbm_5)^2 , na.rm = TRUE ) / nrow(test_5) )
sum((test_5$ti - test_5$pred_gbm_5) , na.rm = TRUE) / nrow(test_5)
sum(abs((test_5$ti - test_5$pred_gbm_5)/ nrow(test_5)) , na.rm = TRUE)  #MAPE
# TENDENCIA Y HUMEDAD INTERIOR
x <- c("trend", "hi")
y <- "ti"

gbm_md_6 <- h2o.gbm(
  training_frame = train_h,
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
h2o.varimp_plot(gbm_md_6)
test_h$pred_gbm_6 <- h2o.predict(gbm_md_6, test_h)
test_6 <- as.data.frame(test_h)

plot_ly(data = test_6) %>%
  add_lines(x = ~ ds, y = ~ ti, name = "Actual") %>%
  add_lines(x = ~ ds, y = ~ pred_gbm_6, name = "Gradient Boosting ML",
            line = list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Prediction (Gradient
Boosting ML)",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Día"))

sqrt( sum( (test_6$ti - test_6$pred_gbm_6)^2 , na.rm = TRUE ) / nrow(test_6) )
sum((test_6$ti - test_6$pred_gbm_6) , na.rm = TRUE) / nrow(test_6)
sum(abs((test_6$ti - test_6$pred_gbm_6)/ nrow(test_6)) , na.rm = TRUE)  #MAPE
# TENDENCIA Y LAG72
x <- c("trend", "ti_lag72")
y <- "ti"

gbm_md_7 <- h2o.gbm(
  training_frame = train_h,
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
h2o.varimp_plot(gbm_md_7)
test_h$pred_gbm_7 <- h2o.predict(gbm_md_7, test_h)
test_7 <- as.data.frame(test_h)

plot_ly(data = test_7) %>%
  add_lines(x = ~ ds, y = ~ ti, name = "Actual") %>%
  add_lines(x = ~ ds, y = ~ pred_gbm_7, name = "Gradient Boosting ML",
            line = list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Prediction (Gradient
Boosting ML)",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Día"))

sqrt( sum( (test_7$ti - test_7$pred_gbm_7)^2 , na.rm = TRUE ) / nrow(test_7) )
sum((test_7$ti - test_7$pred_gbm_7) , na.rm = TRUE) / nrow(test_7)
sum(abs((test_7$ti - test_7$pred_gbm_7)/ nrow(test_7)) , na.rm = TRUE)  #MAPE
# TENDENCIA Y TEMPERATURA EXTERIOR
x <- c("trend", "te")
y <- "ti"

gbm_md_8 <- h2o.gbm(
  training_frame = train_h,
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
h2o.varimp_plot(gbm_md_8)
test_h$pred_gbm_8 <- h2o.predict(gbm_md_8, test_h)
test_8 <- as.data.frame(test_h)

plot_ly(data = test_8) %>%
  add_lines(x = ~ ds, y = ~ ti, name = "Actual") %>%
  add_lines(x = ~ ds, y = ~ pred_gbm_8, name = "Gradient Boosting ML",
            line = list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Prediction (Gradient
Boosting ML)",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Día"))

sqrt( sum( (test_8$ti - test_8$pred_gbm_8)^2 , na.rm = TRUE ) / nrow(test_8) )
sum((test_8$ti - test_8$pred_gbm_8) , na.rm = TRUE) / nrow(test_8)
sum(abs((test_8$ti - test_8$pred_gbm_8)/ nrow(test_8)) , na.rm = TRUE)  #MAPE
# TENDENCIA Y HUMEDAD EXTERIOR
x <- c("trend", "he")
y <- "ti"

gbm_md_9 <- h2o.gbm(
  training_frame = train_h,
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
h2o.varimp_plot(gbm_md_9)
test_h$pred_gbm_9 <- h2o.predict(gbm_md_9, test_h)
test_9 <- as.data.frame(test_h)

plot_ly(data = test_9) %>%
  add_lines(x = ~ ds, y = ~ ti, name = "Actual") %>%
  add_lines(x = ~ ds, y = ~ pred_gbm_9, name = "Gradient Boosting ML",
            line = list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Prediction (Gradient
Boosting ML)",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Día"))

sqrt( sum( (test_9$ti - test_9$pred_gbm_9)^2 , na.rm = TRUE ) / nrow(test_9) )
sum((test_9$ti - test_9$pred_gbm_9) , na.rm = TRUE) / nrow(test_9)
sum(abs((test_9$ti - test_9$pred_gbm_9)/ nrow(test_9)) , na.rm = TRUE)  #MAPE
# TENDENCIA, HUMEDAD INTERIOR, TEMPERATURA EXTERIOR Y HUMEDAD EXTERIOR
x <- c("trend", "hi", "te", "he")
y <- "ti"

gbm_md_10 <- h2o.gbm(
  training_frame = train_h,
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
h2o.varimp_plot(gbm_md_10)
test_h$pred_gbm_10 <- h2o.predict(gbm_md_10, test_h)
test_10 <- as.data.frame(test_h)

plot_ly(data = test_10) %>%
  add_lines(x = ~ ds, y = ~ ti, name = "Actual") %>%
  add_lines(x = ~ ds, y = ~ pred_gbm_10, name = "Gradient Boosting ML",
            line = list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Prediction (Gradient
Boosting ML)",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Día"))

sqrt( sum( (test_10$ti - test_10$pred_gbm_10)^2 , na.rm = TRUE ) / nrow(test_10) )
sum((test_10$ti - test_10$pred_gbm_10) , na.rm = TRUE) / nrow(test_10)
sum(abs((test_10$ti - test_10$pred_gbm_10)/ nrow(test_10)) , na.rm = TRUE)  #MAPE

# TENDENCIA, HUMEDAD INTERIOR, TEMPERATURA EXTERIOR Y HUMEDAD EXTERIOR
x <- c("trend", "hi", "te", "he", "ti_lag72")
y <- "ti"

gbm_md_11 <- h2o.gbm(
  training_frame = train_h,
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
h2o.varimp_plot(gbm_md_11)
test_h$pred_gbm_11 <- h2o.predict(gbm_md_11, test_h)
test_11 <- as.data.frame(test_h)

plot_ly(data = test_11) %>%
  add_lines(x = ~ ds, y = ~ ti, name = "Actual") %>%
  add_lines(x = ~ ds, y = ~ pred_gbm_11, name = "Gradient Boosting ML",
            line = list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Prediction (Gradient
Boosting ML)",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Día"))

sqrt( sum( (test_11$ti - test_11$pred_gbm_11)^2 , na.rm = TRUE ) / nrow(test_11) )
sum((test_11$ti - test_11$pred_gbm_11) , na.rm = TRUE) / nrow(test_11)
sum(abs((test_11$ti - test_11$pred_gbm_11)/ nrow(test_11)) , na.rm = TRUE)  #MAPE
################################################################################

######################## MACHINE LEARNING AUTOML ###############################
########################## UNIVARIABLE
# TENDENCIA
x <- c("trend")
y <- "ti"

autoML_1 <- h2o.automl(training_frame = train_h,
                      x = x,
                      y = y,
                      nfolds = 5,
                      max_runtime_secs = 60*20,
                      seed = 1234)


test_h$pred_autoML_1 <- h2o.predict(autoML_1@leader, test_h)
test_31 <- as.data.frame(test_h)
plot_ly(data = test_31) %>%
  add_lines(x = ~ ds, y = ~ ti, name = "Actual") %>%
  add_lines(x = ~ ds, y = ~ pred_autoML_1, name = "autoML") %>%
  layout(title = "Temperatura interior - Actual vs. Prediction (Auto ML
Model)",
         yaxis = list(title = "Thousands of Units"),
         xaxis = list(title = "Month"))

sqrt( sum( (test_31$ti - test_31$pred_autoML_1)^2 , na.rm = TRUE ) / nrow(test_31) )
sum((test_31$ti - test_31$pred_autoML_1) , na.rm = TRUE) / nrow(test_31)
sum(abs((test_31$ti - test_31$pred_autoML_1)/ nrow(test_31)) , na.rm = TRUE)  #MAPE

# LAG 72 TEMPERATURA INTERIOR
x <- c("ti_lag72")
y <- "ti"

autoML_2 <- h2o.automl(training_frame = train_h,
                       x = x,
                       y = y,
                       nfolds = 5,
                       max_runtime_secs = 60*20,
                       seed = 1234)


test_h$pred_autoML_2 <- h2o.predict(autoML_2@leader, test_h)
test_32 <- as.data.frame(test_h)
plot_ly(data = test_32) %>%
  add_lines(x = ~ ds, y = ~ ti, name = "Actual") %>%
  add_lines(x = ~ ds, y = ~ pred_autoML_2, name = "autoML") %>%
  layout(title = "Temperatura interior - Actual vs. Prediction (Auto ML
Model)",
         yaxis = list(title = "Thousands of Units"),
         xaxis = list(title = "Month"))

sqrt( sum( (test_32$ti - test_32$pred_autoML_2)^2 , na.rm = TRUE ) / nrow(test_32) )
sum((test_32$ti - test_32$pred_autoML_2) , na.rm = TRUE) / nrow(test_32)
sum(abs((test_32$ti - test_32$pred_autoML_2)/ nrow(test_32)) , na.rm = TRUE)  #MAPE
# TENDENCIA AL CUADRADO
x <- c("trend_sqr")
y <- "ti"

autoML_3 <- h2o.automl(training_frame = train_h,
                       x = x,
                       y = y,
                       nfolds = 5,
                       max_runtime_secs = 60*20,
                       seed = 1234)


test_h$pred_autoML_3 <- h2o.predict(autoML_3@leader, test_h)
test_33 <- as.data.frame(test_h)
plot_ly(data = test_33) %>%
  add_lines(x = ~ ds, y = ~ ti, name = "Actual") %>%
  add_lines(x = ~ ds, y = ~ pred_autoML_3, name = "autoML") %>%
  layout(title = "Temperatura interior - Actual vs. Prediction (Auto ML
Model)",
         yaxis = list(title = "Thousands of Units"),
         xaxis = list(title = "Month"))

sqrt( sum( (test_33$ti - test_33$pred_autoML_3)^2 , na.rm = TRUE ) / nrow(test_33) )
sum((test_33$ti - test_33$pred_autoML_3) , na.rm = TRUE) / nrow(test_33)
sum(abs((test_33$ti - test_33$pred_autoML_3)/ nrow(test_33)) , na.rm = TRUE)  #MAPE
# DIA DE LA SEMANA
x <- c("weekday")
y <- "ti"

autoML_4 <- h2o.automl(training_frame = train_h,
                       x = x,
                       y = y,
                       nfolds = 5,
                       max_runtime_secs = 60*20,
                       seed = 1234)


test_h$pred_autoML_4 <- h2o.predict(autoML_4@leader, test_h)
test_34 <- as.data.frame(test_h)
plot_ly(data = test_34) %>%
  add_lines(x = ~ ds, y = ~ ti, name = "Actual") %>%
  add_lines(x = ~ ds, y = ~ pred_autoML_4, name = "autoML") %>%
  layout(title = "Temperatura interior - Actual vs. Prediction (Auto ML
Model)",
         yaxis = list(title = "Thousands of Units"),
         xaxis = list(title = "Month"))

sqrt( sum( (test_34$ti - test_34$pred_autoML_4)^2 , na.rm = TRUE ) / nrow(test_34) )
sum((test_34$ti - test_34$pred_autoML_4) , na.rm = TRUE) / nrow(test_34)
sum(abs((test_34$ti - test_34$pred_autoML_4)/ nrow(test_34)) , na.rm = TRUE)  #MAPE
######################### MULTIVARIABLE
# TENDENCIA Y TENDENCIA CUADRADO
x <- c("trend", "trend_sqr")
y <- "ti"

autoML_5 <- h2o.automl(training_frame = train_h,
                       x = x,
                       y = y,
                       nfolds = 5,
                       max_runtime_secs = 60*20,
                       seed = 1234)


test_h$pred_autoML_5 <- h2o.predict(autoML_5@leader, test_h)
test_35 <- as.data.frame(test_h)
plot_ly(data = test_35) %>%
  add_lines(x = ~ ds, y = ~ ti, name = "Actual") %>%
  add_lines(x = ~ ds, y = ~ pred_autoML_5, name = "autoML") %>%
  layout(title = "Temperatura interior - Actual vs. Prediction (Auto ML
Model)",
         yaxis = list(title = "Thousands of Units"),
         xaxis = list(title = "Month"))

sqrt( sum( (test_35$ti - test_35$pred_autoML_5)^2 , na.rm = TRUE ) / nrow(test_35) )
sum((test_35$ti - test_35$pred_autoML_5) , na.rm = TRUE) / nrow(test_35)
sum(abs((test_35$ti - test_35$pred_autoML_5)/ nrow(test_35)) , na.rm = TRUE)  #MAPE
# TENDENCIA Y HUMEDAD INTERIOR
x <- c("trend", "hi")
y <- "ti"

autoML_6 <- h2o.automl(training_frame = train_h,
                       x = x,
                       y = y,
                       nfolds = 5,
                       max_runtime_secs = 60*20,
                       seed = 1234)


test_h$pred_autoML_6 <- h2o.predict(autoML_6@leader, test_h)
test_36 <- as.data.frame(test_h)
plot_ly(data = test_36) %>%
  add_lines(x = ~ ds, y = ~ ti, name = "Actual") %>%
  add_lines(x = ~ ds, y = ~ pred_autoML_6, name = "autoML") %>%
  layout(title = "Temperatura interior - Actual vs. Prediction (Auto ML
Model)",
         yaxis = list(title = "Thousands of Units"),
         xaxis = list(title = "Month"))

sqrt( sum( (test_36$ti - test_36$pred_autoML_6)^2 , na.rm = TRUE ) / nrow(test_36) )
sum((test_36$ti - test_36$pred_autoML_6) , na.rm = TRUE) / nrow(test_36)
sum(abs((test_36$ti - test_36$pred_autoML_6)/ nrow(test_36)) , na.rm = TRUE)  #MAPE
# TENDENCIA Y LAG72
x <- c("trend", "ti_lag72")
y <- "ti"

autoML_7 <- h2o.automl(training_frame = train_h,
                       x = x,
                       y = y,
                       nfolds = 5,
                       max_runtime_secs = 60*20,
                       seed = 1234)


test_h$pred_autoML_7 <- h2o.predict(autoML_7@leader, test_h)
test_37 <- as.data.frame(test_h)
plot_ly(data = test_37) %>%
  add_lines(x = ~ ds, y = ~ ti, name = "Actual") %>%
  add_lines(x = ~ ds, y = ~ pred_autoML_7, name = "autoML") %>%
  layout(title = "Temperatura interior - Actual vs. Prediction (Auto ML
Model)",
         yaxis = list(title = "Thousands of Units"),
         xaxis = list(title = "Month"))

sqrt( sum( (test_37$ti - test_37$pred_autoML_7)^2 , na.rm = TRUE ) / nrow(test_37) )
sum((test_37$ti - test_37$pred_autoML_7) , na.rm = TRUE) / nrow(test_37)
sum(abs((test_37$ti - test_37$pred_autoML_7)/ nrow(test_37)) , na.rm = TRUE)  #MAPE
# TENDENCIA Y TEMPERATURA EXTERIOR
x <- c("trend", "te")
y <- "ti"

autoML_8 <- h2o.automl(training_frame = train_h,
                       x = x,
                       y = y,
                       nfolds = 5,
                       max_runtime_secs = 60*20,
                       seed = 1234)


test_h$pred_autoML_8 <- h2o.predict(autoML_8@leader, test_h)
test_38 <- as.data.frame(test_h)
plot_ly(data = test_38) %>%
  add_lines(x = ~ ds, y = ~ ti, name = "Actual") %>%
  add_lines(x = ~ ds, y = ~ pred_autoML_8, name = "autoML") %>%
  layout(title = "Temperatura interior - Actual vs. Prediction (Auto ML
Model)",
         yaxis = list(title = "Thousands of Units"),
         xaxis = list(title = "Month"))

sqrt( sum( (test_38$ti - test_38$pred_autoML_8)^2 , na.rm = TRUE ) / nrow(test_38) )
sum((test_38$ti - test_38$pred_autoML_8) , na.rm = TRUE) / nrow(test_38)
sum(abs((test_38$ti - test_38$pred_autoML_8)/ nrow(test_38)) , na.rm = TRUE)  #MAPE
# TENDENCIA Y HUMEDAD EXTERIOR
x <- c("trend", "he")
y <- "ti"

autoML_9 <- h2o.automl(training_frame = train_h,
                       x = x,
                       y = y,
                       nfolds = 5,
                       max_runtime_secs = 60*20,
                       seed = 1234)


test_h$pred_autoML_9 <- h2o.predict(autoML_9@leader, test_h)
test_39 <- as.data.frame(test_h)
plot_ly(data = test_39) %>%
  add_lines(x = ~ ds, y = ~ ti, name = "Actual") %>%
  add_lines(x = ~ ds, y = ~ pred_autoML_9, name = "autoML") %>%
  layout(title = "Temperatura interior - Actual vs. Prediction (Auto ML
Model)",
         yaxis = list(title = "Thousands of Units"),
         xaxis = list(title = "Month"))

sqrt( sum( (test_39$ti - test_39$pred_autoML_9)^2 , na.rm = TRUE ) / nrow(test_39) )
sum((test_39$ti - test_39$pred_autoML_9) , na.rm = TRUE) / nrow(test_39)
sum(abs((test_39$ti - test_39$pred_autoML_9)/ nrow(test_39)) , na.rm = TRUE)  #MAPE
# TENDENCIA, HUMEDAD INTERIOR, TEMPERATURA EXTERIOR Y HUMEDAD EXTERIOR
x <- c("trend", "hi", "te", "he")
y <- "ti"

autoML_10 <- h2o.automl(training_frame = train_h,
                       x = x,
                       y = y,
                       nfolds = 5,
                       max_runtime_secs = 60*20,
                       seed = 1234)


test_h$pred_autoML_10 <- h2o.predict(autoML_10@leader, test_h)
test_310 <- as.data.frame(test_h)
plot_ly(data = test_310) %>%
  add_lines(x = ~ ds, y = ~ ti, name = "Actual") %>%
  add_lines(x = ~ ds, y = ~ pred_autoML_10, name = "autoML") %>%
  layout(title = "Temperatura interior - Actual vs. Prediction (Auto ML
Model)",
         yaxis = list(title = "Thousands of Units"),
         xaxis = list(title = "Month"))

sqrt( sum( (test_310$ti - test_310$pred_autoML_10)^2 , na.rm = TRUE ) / nrow(test_310) )
sum((test_310$ti - test_310$pred_autoML_10) , na.rm = TRUE) / nrow(test_310)
sum(abs((test_310$ti - test_310$pred_autoML_10)/ nrow(test_310)) , na.rm = TRUE)  #MAPE
# TENDENCIA, HUMEDAD INTERIOR, TEMPERATURA EXTERIOR Y HUMEDAD EXTERIOR
x <- c("trend", "hi", "te", "he", "ti_lag72")
y <- "ti"

autoML_11 <- h2o.automl(training_frame = train_h,
                        x = x,
                        y = y,
                        nfolds = 5,
                        max_runtime_secs = 60*20,
                        seed = 1234)


test_h$pred_autoML_11<- h2o.predict(autoML_11@leader, test_h)
test_311 <- as.data.frame(test_h)
plot_ly(data = test_311) %>%
  add_lines(x = ~ ds, y = ~ ti, name = "Actual") %>%
  add_lines(x = ~ ds, y = ~ pred_autoML_11, name = "autoML") %>%
  layout(title = "Temperatura interior - Actual vs. Prediction (Auto ML
Model)",
         yaxis = list(title = "Thousands of Units"),
         xaxis = list(title = "Month"))

sqrt( sum( (test_311$ti - test_311$pred_autoML_11)^2 , na.rm = TRUE ) / nrow(test_311) )
sum((test_311$ti - test_311$pred_autoML_11) , na.rm = TRUE) / nrow(test_311)
sum(abs((test_311$ti - test_311$pred_autoML_11)/ nrow(test_311)) , na.rm = TRUE)  #MAPE
################################################################################