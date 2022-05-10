library(TSstudio)
library(plotly)
library(dplyr)
library(lubridate)
library(forecast)
library(readxl)
library(stats)

source("R/plot_lm.R")
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

########### Univariable Linear Regresion
ts_decompose(temperatura_interior_ts)
tempint_df <- tibble(ds = dato_interior$Fecha,
                         segundos = 60 * 10 * (as.numeric(rownames(dato_interior))-1),
                         dia_muestreo = 1 + (segundos - (segundos %% (3600 *12)))/(3600 *12),
                         segundos_dia_muestreo = segundos - (dia_muestreo-1) * (3600 *12),
                         y = dato_interior$`Temperatura (ºC)`)
head(tempint_df)

tempint_df$trend <- decompose(temperatura_interior_ts)$trend
tempint_df$seasonal <- decompose(temperatura_interior_ts)$seasonal

h <- 627
tempint_train <- tempint_df[1:(unidades - h), ]
tempint_test <- tempint_df[(unidades - h + 1):unidades, ]

# Regresion lineal con la tendencia
md_trend <- lm(y ~ trend, data = tempint_train)
summary(md_trend)

tempint_train$yhat <- predict(md_trend, newdata = tempint_train)
tempint_test$yhat <- predict(md_trend, newdata = tempint_test)

plot_lm(data = tempint_df,
        train = tempint_train,
        test = tempint_test,
        title = "Predicción de la tendencia de la serie")

mape_trend <- c(mean(abs(tempint_train$y - tempint_train$yhat)/ tempint_train$y),
                mean(abs(tempint_test$y - tempint_test$yhat)/ tempint_test$y))
mape_trend

# Regresión lineal con la estacionalidad
md_seasonal <- lm(y ~ seasonal, data = tempint_train)
summary(md_seasonal)

tempint_train$yhat <- predict(md_seasonal, newdata = tempint_train)
tempint_test$yhat <- predict(md_seasonal, newdata = tempint_test)

plot_lm(data = tempint_df,
        train = tempint_train,
        test = tempint_test,
        title = "Predicción de la tendencia de la serie")

mape_seasonal <- c(mean(abs(tempint_train$y - tempint_train$yhat)/ tempint_train$y),
                   mean(abs(tempint_test$y - tempint_test$yhat)/ tempint_test$y))
mape_seasonal

# Regresion lineal con la tendencia y estacionalidad
md1 <- lm(y ~ seasonal + trend, data = tempint_train)
summary(md1)

tempint_train$yhat <- predict(md1, newdata = tempint_train)
tempint_test$yhat <- predict(md1, newdata = tempint_test)

plot_lm(data = tempint_df,
        train = tempint_train,
        test = tempint_test,
        title = "Predicción de la tendencia y estacionalidad de la serie")

mape_md1 <- c(mean(abs(tempint_train$y - tempint_train$yhat)/ tempint_train$y),
              mean(abs(tempint_test$y - tempint_test$yhat)/ tempint_test$y))
mape_md1

# Regresion lineal con Estacionalidad, tendencia y error?
md2 <- lm(y ~ seasonal + trend + I(trend^2), data = tempint_train)
summary(md2)

tempint_train$yhat <- predict(md2, newdata = tempint_train)
tempint_test$yhat <- predict(md2, newdata = tempint_test)

plot_lm(data = tempint_df,
        train = tempint_train,
        test = tempint_test,
        title = "Predicción de la tendencia y estacionalidad de la serie")

mape_md2 <- c(mean(abs(tempint_train$y - tempint_train$yhat)/ tempint_train$y),
              mean(abs(tempint_test$y - tempint_test$yhat)/ tempint_test$y))
mape_md2

# Regresion lineal tslm
tempint_split <- ts_split(temperatura_interior_ts, sample.out = h)
train.ts <- tempint_split$train
test.ts <- tempint_split$test

md3 <- tslm(train.ts ~ season + trend + I(trend^2), data = tempint_train)
summary(md3)

tempint_train$yhat <- predict(md3, newdata = tempint_train)
tempint_test$yhat <- predict(md3, newdata = tempint_test)

plot_lm(data = tempint_df,
        train = tempint_train,
        test = tempint_test,
        title = "Predicción de tslm de la serie")

# Hasta 268