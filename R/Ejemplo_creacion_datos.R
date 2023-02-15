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
temperatura_interior_ts <- ts(data = dato_interior$`Temperatura (ºC)`[1:unidades], start = 1, frequency = 3*24)



# Imagenes de las series temporales
plot_ti_ts <- TSstudio::ts_plot(temperatura_interior_ts, title = "Serie temporal de la temperatura interior", Ytitle = "Temperatura interior", Xtitle = "Días", Xgrid = TRUE, Ygrid = TRUE)


# Guardar los plots de las series temporales
plot_hi_ts
plot_ti_ts
plot_he_ts 
plot_te_ts

# Descomponer series temporales
# Aditivo

teminterior_decompose <- decompose(temperatura_interior_ts)
plot_ti_decompose <- plot(teminterior_decompose)


# Split en training y test
h=627
teminterior_split <- ts_split(temperatura_interior_ts, sample.out = h)
teminterior.train.ts <- teminterior_split$train
teminterior.test.ts <- teminterior_split$test

# Descomponer de la serie de training
teminterior.train.ts_decompose <- decompose(teminterior.train.ts)
plot_ti.train_decompose <- plot(teminterior.train.ts_decompose)

TREND_teminterior.train.ts_decompose_NO_NAs <- na.omit(teminterior.train.ts_decompose$trend)
TREND_teminterior.train.ts_decompose_NAs <- sum(is.na(teminterior.train.ts_decompose$trend))
sum(teminterior_decompose$trend[(0.5*TREND_teminterior.train.ts_decompose_NAs+1):length(TREND_teminterior.train.ts_decompose_NO_NAs)]==TREND_teminterior.train.ts_decompose_NO_NAs)
error_trend <- teminterior_decompose$trend[(0.5*TREND_teminterior.train.ts_decompose_NAs+1):length(TREND_teminterior.train.ts_decompose_NO_NAs)]-TREND_teminterior.train.ts_decompose_NO_NAs
error_trend_values <- as.numeric(error_trend) %>% as.data.frame()
plot(as.numeric(error_trend)) # Surgen errores cerca del punto de corte


SEASON_teminterior.train.ts_decompose_NO_NAs <- na.omit(teminterior.train.ts_decompose$seasonal)
SEASON_teminterior.train.ts_decompose_NAs <- sum(is.na(teminterior.train.ts_decompose$seasonal))
sum(teminterior_decompose$seasonal[(0.5*SEASON_teminterior.train.ts_decompose_NAs+1):length(SEASON_teminterior.train.ts_decompose_NO_NAs)]==SEASON_teminterior.train.ts_decompose_NO_NAs)
error_seasonal <- teminterior_decompose$seasonal[(0.5*SEASON_teminterior.train.ts_decompose_NAs+1):length(SEASON_teminterior.train.ts_decompose_NO_NAs)]-SEASON_teminterior.train.ts_decompose_NO_NAs
error_seasonal_values <- as.numeric(error_trend) %>% as.data.frame()
plot(as.numeric(error_seasonal)) # Se observa un error periódico. Es relativamente grande


###############################################
# CREACION DE DATOS PARA EL MODELO DE REGRESION
###############################################


df <- tibble(ds = dato_interior$Fecha,weekday = weekdays(dato_interior$Fecha),
             segundos = 60 * 10 * (as.numeric(rownames(dato_interior))-1),
             dia_muestreo = 1 + (segundos - (segundos %% (3600 *12)))/(3600 *12),
             segundos_dia_muestreo = segundos - (dia_muestreo-1) * (3600 *12),
             y = temperatura_interior_ts)

df$trend <- decompose(temperatura_interior_ts)$trend
df$seasonal <- decompose(temperatura_interior_ts)$seasonal
df$lag72 <- dplyr::lag(as.numeric(df$y), n = 72) # dplyr::lag(df$y, n = 72)
df$trend_sqr <- (decompose(temperatura_interior_ts)$trend)^2# temint_df$trend ^ 2


test_df <- df[(nrow(df) - h + 1):nrow(df), ]
train_df <- df[1:(nrow(df) - h), ]
train_df$trend <- decompose(teminterior.train.ts)$trend
train_df$seasonal <- decompose(teminterior.train.ts)$seasonal
train_df$trend_sqr <- (decompose(teminterior.train.ts)$trend)^2


####### TEMPERATURA INTERIOR
# TENDENCIA
md1_1 <- lm(y ~ trend, data = train_df)
checkresiduals(md1_1)

md2_1 <- auto.arima(teminterior.train.ts,
                    xreg = cbind(train_df$trend),
                    seasonal = TRUE,
                    stepwise = TRUE,
                    approximation = FALSE)
summary(md2_1)
checkresiduals(md2_1)
fc1_1 <- forecast(md1_1, newdata = test_df)
fc2_1 <- forecast(md2_1, xreg = cbind(na.omit(test_df$trend)))
forecast::accuracy(fc1_1, teminterior.test.ts)
forecast::accuracy(fc2_1, teminterior.test.ts)

plot(fc2_1)
