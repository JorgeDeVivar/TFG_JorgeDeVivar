library(TSstudio)
library(plotly)
library(dplyr)
library(lubridate)
library(forecast)
library(readxl)
library(stats)
library(datasets)
library(base)

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

temperatura_interior_ts <- ts(data = dato_interior$`Temperatura (ºC)`[1:unidades],
                              start = 1,
                              frequency = 3*24)

ts_plot(temperatura_interior_ts,
        title = "Serie temporal de la temperatura interior",
        Ytitle = "Temperatura interior",
        Xtitle = "Días",
        Xgrid = TRUE,
        Ygrid = TRUE)

ts_plot(diff(temperatura_interior_ts, lag = 1),
        title = "Temperatura interior ts - Primera Diferenciación",
        Ytitle = "Temperatura interior",
        Xtitle = "Días",
        Xgrid = TRUE,
        Ygrid = TRUE)

ts_plot(diff(diff(temperatura_interior_ts, lag = 1), 12),
        title = "Temperatura interior ts - Primera Diferenciación Estacional",
        Ytitle = "Temperatura interior",
        Xtitle = "Días",
        Xgrid = TRUE,
        Ygrid = TRUE)

ts_plot(diff(log(temperatura_interior_ts), lag = 1),
        title = "Temperatura interior ts - Primera Diferenciación con 
        transformación de Log",
        Ytitle = "Temperatura interior",
        Xtitle = "Días",
        Xgrid = TRUE,
        Ygrid = TRUE)

########### Univariable ARIMA Model

# AR process
par(mar=c(1,1,1,1), mfrow = c(1,2))
acf(temperatura_interior_ts)
pacf(temperatura_interior_ts)

temperatura_interior_ts_d1 <- diff(temperatura_interior_ts)
acf(temperatura_interior_ts_d1)
pacf(temperatura_interior_ts_d1)

temint_md <- arima(temperatura_interior_ts, order = c(1,1,0))
summary(temint_md)
checkresiduals(temint_md)

# ARIMA model
ti_split <- ts_split(temperatura_interior_ts, sample.out = 627)

ti_train <- ti_split$train
ti_test <- ti_split$test

acf(ti_train, lag.max = 60)
pacf(ti_train, lag.max = 60)

ti_d12 <- diff(temperatura_interior_ts, 12)
ts_plot(ti_d12,
        title = "Temperatura interior - Primera Diferenciación Estacional",
        Ytitle = "Temperatura interior",
        Xtitle = "Días",
        Xgrid = TRUE,
        Ygrid = TRUE)
acf(ti_d12, lag.max = 60)
pacf(ti_d12, lag.max = 60)

ti_d12_1 <- diff(diff(temperatura_interior_ts, 12),1)
ts_plot(ti_d12_1,
        title = "Temperatura interior - Primera Diferenciación Estacional y no Estacional",
        Ytitle = "Temperatura interior",
        Xtitle = "Días",
        Xgrid = TRUE,
        Ygrid = TRUE)

acf(ti_d12_1, lag.max = 60)
pacf(ti_d12_1, lag.max = 60)

ti_best_md <- arima(ti_train, 
                    order = c(1,1,1), 
                    seasonal = list(order = c(2,1,1)),
                    method = "CSS")

ti_test_fc <- forecast(ti_best_md, h = 627)
accuracy(ti_test_fc, ti_test)

test_forecast(temperatura_interior_ts,
              forecast.obj = ti_test_fc,
              test = ti_test)

final_fc <- arima(ti_train, 
                    order = c(1,1,1), 
                    seasonal = list(order = c(2,1,1)),
                    method = "CSS")

checkresiduals(final_md)

ti_fc <- forecast(final_fc, h = 627)

plot_forecast(ti_fc,
              title = "Temperatura interior - Forecast",
              Ytitle = "Temperatura  (ºC)",
              Xtitle = "Días")

# Auto Arima
ti_auto_md1 <- auto.arima(ti_train)

ti_autoarima_md1 <- arima(ti_train, 
                    order = c(2,1,3), 
                    seasonal = list(order = c(2,0,0)),
                    method = "CSS")

ti_test_auto1 <- forecast(ti_autoarima_md1, h = 627)
accuracy(ti_autoarima_md1, ti_test)

test_forecast(temperatura_interior_ts,
              forecast.obj = ti_test_auto1,
              test = ti_test)

ti_auto_md2 <- auto.arima(ti_train,
                          max.order = 5,
                          D = 1,
                          d = 1,
                          stepwise = FALSE,
                          approximation = FALSE)


# Violation of white noise assumption.... #(370)