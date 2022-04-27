library(tidyverse)
library(readxl)
library(lubridate)
library(dplyr)
library(ggplot2)
library(plotly)
library(forecast)
library(TSstudio)
library(tseries)

# Seasonality Analysis with Official Data
# Ruta de la consola y guardar en variable
ruta_excel <- "raw_data/OfficialData.xlsx"

# Hojas del Excel
excel_sheets(ruta_excel)

# Importar los datos de la Hoja Exterior
dato_exterior <- read_excel(ruta_excel, sheet = "Exterior")
dato_interior <- read_excel(ruta_excel, sheet = "Interior")

# We set the day as unit of frequency, 3 data per hour
unidades <- length(dato_interior$Fecha)
humedad_interior_ts <- ts(data = dato_interior$`Humedad (%)`[1:unidades], 
                          start = 1,
                          frequency = 3*24)

temperatura_interior_ts <- ts(data = dato_interior$`Temperatura (ºC)`[1:unidades],
                              start = 1,
                              frequency = 3*24)

dioxidocarb_interior_ts <- ts(data = dato_interior$`valor CO2 (ppm)`[1:unidades],
                              start = 1,
                              frequency = 3*24)

monoxidocarb_interior_ts <- ts(data = dato_interior$`valor CO (ppm)`[1:unidades],
                               start = 1,
                               frequency = 3*24)

humedad_exterior_ts <- ts (data = dato_exterior$`Humedad (%)`[1:unidades],
                           start = 1,
                           frequency = 3*24)

temperatura_exterior_ts <- ts(data = dato_exterior$`Temperatura (ºC)`[1:unidades],
                              start = 1,
                              frequency = 3*24)

# Training with simple and testing partitions, data´s 30% = 627
ts_info(temperatura_interior_ts)

train_temint <- window(temperatura_interior_ts, 
                       start = time(temperatura_interior_ts)[1],
                       end = time(temperatura_interior_ts)[length(temperatura_interior_ts)-627])

test_temint <- window(temperatura_interior_ts, 
                      start = time(temperatura_interior_ts)[length(temperatura_interior_ts)-627+1],
                      end = time(temperatura_interior_ts)[length(temperatura_interior_ts)])
# Alternative 
# temint_partitions <- ts_split(temperatura_interior_ts, sample.out = 627)
# train_temint <- temint_partitions$train
# test_temint <- temint_partitions$test

# Residual Analysis 
ti_auto <- auto.arima(train_temint)
checkresiduals(ti_auto)

# Scoring the forecast
# MSE = Mean Squared Error
# RMSE = Root Mean Squared Error
# MAE = Mean Absolute Error
# MAPE = Mean Absolute Percentage Error

fcti <- forecast(ti_auto, h = 627)
accuracy(fcti,test_temint)
test_forecast(actual = temperatura_interior_ts,
              forecast.obj = fcti,
              test = test_temint)

# Forecast banchmark
naive_model_ti <- naive(train_temint, h = 627)
accuracy(naive_model_ti,test_temint)
test_forecast(actual = temperatura_interior_ts,
              forecast.obj = naive_model_ti,
              test = test_temint)

snaive_model_ti <- snaive(train_temint, h = 627)
accuracy(snaive_model_ti,test_temint)
test_forecast(actual = temperatura_interior_ts,
              forecast.obj = snaive_model_ti,
              test = test_temint)

# Finalizing the forecast
plot_forecast(fcti,
              title = "Predicción de la temperatura interior",
              Xtitle = "Días",
              Ytitle = "Temperatura interior (ºC)")

# Confidence interval
fcti_confidence <- forecast(ti_auto,
                            h = 1463,
                            level = c(80,90))

plot_forecast(fcti_confidence,
              title = "Predicción de la temperatura interior",
              Xtitle = "Días",
              Ytitle = "Temperatura interior (ºC)")

# Simulation
fcti_simulation <- forecast_sim(model = ti_auto,
                                h = 1463,
                                n = 500)
fcti_simulation$plot %>% 
  layout(title = "Predicción de la temperatura interior",
         Xtitle = "Días",
         Ytitle = "Temperatura interior (ºC)")

# The horse race... Hay que mirarlo
methods <- list(ets1 = list(method = "ets",
                            method_arg = list(opt.crit = "lik"),
                            notes = "ETS model with opt.crit = lik"),
                ets2 = list(method = "ets",
                            method_arg = list(opt.crit = "amse"),
                            notes = "ETS model with opt.crit = amse"),
                arima1 = list(method = "arima",
                              method_arg = list(order = c(2,1,0)),
                              notes = "ARIMA(2,1,0)"),
                arima2 = list(method = "arima",
                              method_arg = list(order = c(2,1,2),
                                                seasonal = list(order = c(1,1,1))),
                              notes = "SARIMA(2,1,2)(1,1,1)"),
                hw = list(method = "HoltWinters",
                          method_arg = NULL,
                          notes = "HoltWinters Model"),
                tslm = list(method = "tslm",
                            method_arg = list(formula = input ~ trend + season),
                            notes = "tslm model with trend and seasonal components"))

tempint_forecast <- train_model(input = temperatura_interior_ts,
                                methods = methods,
                                train_method = list(partitions = 6,
                                                    sample.out = 12,
                                                    space = 3),
                                horizon = 12,
                                error = "MAPE")

plot_error(model.obj = tempint_forecast)
