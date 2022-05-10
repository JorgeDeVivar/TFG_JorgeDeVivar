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

humedad_interior_ts <- ts(data = dato_interior$`Humedad (%)`[1:unidades],
                              start = 1,
                              frequency = 3*24)

temperatura_exterior_ts <- ts(data = dato_exterior$`Temperatura (ºC)`[1:unidades],
                              start = 1,
                              frequency = 3*24)

humedad_exterior_ts <- ts(data = dato_exterior$`Humedad (%)`[1:unidades],
                              start = 1,
                              frequency = 3*24)


ts_plot(temperatura_interior_ts,
        title = "Serie temporal de la temperatura interior",
        Ytitle = "Temperatura interior",
        Xtitle = "Días",
        Xgrid = TRUE,
        Ygrid = TRUE)

########### Multivariable Linear Regresion
ts_decompose(temperatura_interior_ts)
tempint_df <- tibble(ds = dato_interior$Fecha,
                     segundos = 60 * 10 * (as.numeric(rownames(dato_interior))-1),
                     dia_muestreo = 1 + (segundos - (segundos %% (3600 *12)))/(3600 *12),
                     segundos_dia_muestreo = segundos - (dia_muestreo-1) * (3600 *12),
                     y = dato_interior$`Temperatura (ºC)`,
                     z = dato_exterior$`Temperatura (ºC)`,
                     w = dato_interior$`Humedad (%)`,
                     v = dato_exterior$`Humedad (%)`)
head(tempint_df)

tempint_df$trend <- 1:nrow(temint_df)
tempint_df$seasonal <- decompose(temperatura_interior_ts)$seasonal

tempint_df$tetrend <- decompose(temperatura_exterior_ts)$trend
tempint_df$teseasonal <- decompose(temperatura_exterior_ts)$seasonal

tempint_df$hitrend <- decompose(humedad_interior_ts)$trend
tempint_df$hiseasonal <- decompose(humedad_interior_ts)$seasonal

tempint_df$hetrend <- decompose(humedad_exterior_ts)$trend
tempint_df$heseasonal <- decompose(humedad_exterior_ts)$seasonal

h <- 627
tempint_train <- tempint_df[1:(unidades - h), ]
tempint_test <- tempint_df[(unidades - h + 1):unidades, ]
########################### Temperatura exterior#######################
# Se utiliza para la regresión lineal los datos de temperatura exterior
md_te <- lm(y ~ z, data = tempint_train)
summary(md_te)

tempint_train$yhat <- predict(md_te, newdata = tempint_train)
tempint_test$yhat <- predict(md_te, newdata = tempint_test)

plot_lm(data = tempint_df,
        train = tempint_train,
        test = tempint_test,
        title = "Predicción de la tendencia de la serie")

mape_te <- c(mean(abs(tempint_train$y - tempint_train$yhat)/ tempint_train$y),
                mean(abs(tempint_test$y - tempint_test$yhat)/ tempint_test$y))
mape_te

# Se utiliza para la regresión lineal la trend de la temperatura exterior
md_tetrend <- lm(y ~ tetrend, data = tempint_train)
summary(md_seasonal)

tempint_train$yhat <- predict(md_tetrend, newdata = tempint_train)
tempint_test$yhat <- predict(md_tetrend, newdata = tempint_test)

plot_lm(data = tempint_df,
        train = tempint_train,
        test = tempint_test,
        title = "Predicción de la tendencia de la serie")

mape_seasonal <- c(mean(abs(tempint_train$y - tempint_train$yhat)/ tempint_train$y),
                   mean(abs(tempint_test$y - tempint_test$yhat)/ tempint_test$y))
mape_seasonal

# Regresión lineal con datos temperatura exterior season and trend 
md_tetrendseasonal <- lm(y ~ teseasonal + tetrend, data = tempint_train)
summary(md1)

tempint_train$yhat <- predict(md_tetrendseasonal, newdata = tempint_train)
tempint_test$yhat <- predict(md_tetrendseasonal, newdata = tempint_test)

plot_lm(data = tempint_df,
        train = tempint_train,
        test = tempint_test,
        title = "Predicción de la tendencia y estacionalidad de la serie")

mape_md_tetrendseasonal <- c(mean(abs(tempint_train$y - tempint_train$yhat)/ tempint_train$y),
                           mean(abs(tempint_test$y - tempint_test$yhat)/ tempint_test$y))
mape_md_tetrendseasonal

# Regresión lineal con datos temperatura exterior season and trend con error
md_tetrendseasonalerr <- lm(y ~ teseasonal + tetrend + I(tetrend^2), data = tempint_train)
summary(md_tetrendseasonalerr)

tempint_train$yhat <- predict(md_tetrendseasonalerr, newdata = tempint_train)
tempint_test$yhat <- predict(md_tetrendseasonalerr, newdata = tempint_test)

plot_lm(data = tempint_df,
        train = tempint_train,
        test = tempint_test,
        title = "Predicción de la tendencia y estacionalidad de la serie")

mape_md_tetrendseasonalerr <- c(mean(abs(tempint_train$y - tempint_train$yhat)/ tempint_train$y),
                             mean(abs(tempint_test$y - tempint_test$yhat)/ tempint_test$y))
mape_md_tetrendseasonalerr
################################################################################

######################## Temperatura exterior e interior########################
# Se utiliza para la regresión lineal los datos de trend 
# temperatura interior + temperatura exterior
md_tite <- lm(y ~ tetrend + trend, data = tempint_train)
summary(md_tite)

tempint_train$yhat <- predict(md_tite, newdata = tempint_train)
tempint_test$yhat <- predict(md_tite, newdata = tempint_test)

plot_lm(data = tempint_df,
        train = tempint_train,
        test = tempint_test,
        title = "Predicción de la tendencia de la serie")

mape_tite <- c(mean(abs(tempint_train$y - tempint_train$yhat)/ tempint_train$y),
             mean(abs(tempint_test$y - tempint_test$yhat)/ tempint_test$y))
mape_tite

# Se utiliza para la regresión lineal los datos de seasonal
# temperatura interior + temperatura exterior
md_titeseasonal <- lm(y ~ teseasonal + seasonal, data = tempint_train)
summary(md_tite)

tempint_train$yhat <- predict(md_titeseasonal, newdata = tempint_train)
tempint_test$yhat <- predict(md_titeseasonal, newdata = tempint_test)

plot_lm(data = tempint_df,
        train = tempint_train,
        test = tempint_test,
        title = "Predicción de la tendencia de la serie")

mape_titeseasonal <- c(mean(abs(tempint_train$y - tempint_train$yhat)/ tempint_train$y),
               mean(abs(tempint_test$y - tempint_test$yhat)/ tempint_test$y))
mape_titeseasonal

# Se utiliza para la regresión lineal de la trend + seasonal 
# temperatura exterior + temperatura interior
md_titetrendseasonal <- lm(y ~ tetrend + trend + teseasonal + seasonal, data = tempint_train)
summary(md_titetrendseasonal)

tempint_train$yhat <- predict(md_titetrendseasonal, newdata = tempint_train)
tempint_test$yhat <- predict(md_titetrendseasonal, newdata = tempint_test)

plot_lm(data = tempint_df,
        train = tempint_train,
        test = tempint_test,
        title = "Predicción de la tendencia de la serie")

mape_md_titetrendseasonal <- c(mean(abs(tempint_train$y - tempint_train$yhat)/ tempint_train$y),
                             mean(abs(tempint_test$y - tempint_test$yhat)/ tempint_test$y))
mape_md_titetrendseasonal

# Regresión lineal con datos seasonal and trend and error
# temperatura exterior + temperatura interior
md_titetrendseasonalerr <- lm(y ~ tetrend + trend + teseasonal + seasonal + I(trend^2) + I(tetrend^2)
                              , data = tempint_train)
summary(md_titetrendseasonalerr)

tempint_train$yhat <- predict(md_titetrendseasonalerr, newdata = tempint_train)
tempint_test$yhat <- predict(md_titetrendseasonalerr, newdata = tempint_test)

plot_lm(data = tempint_df,
        train = tempint_train,
        test = tempint_test,
        title = "Predicción de la tendencia de la serie")

mape_md_titetrendseasonalerr <- c(mean(abs(tempint_train$y - tempint_train$yhat)/ tempint_train$y),
                               mean(abs(tempint_test$y - tempint_test$yhat)/ tempint_test$y))
mape_md_titetrendseasonalerr
################################################################################

############Temperatura exterior, interior y humedad interior###################
# Se utiliza para la regresión lineal los datos de trend 
# temperatura interior + temperatura exterior + humedad interior
md_titehi <- lm(y ~ tetrend + trend + hitrend, data = tempint_train)
summary(md_titehi)

tempint_train$yhat <- predict(md_titehi, newdata = tempint_train)
tempint_test$yhat <- predict(md_titehi, newdata = tempint_test)

plot_lm(data = tempint_df,
        train = tempint_train,
        test = tempint_test,
        title = "Predicción de la tendencia de la serie")

mape_titehi <- c(mean(abs(tempint_train$y - tempint_train$yhat)/ tempint_train$y),
               mean(abs(tempint_test$y - tempint_test$yhat)/ tempint_test$y))
mape_titehi

# Se utiliza para la regresión lineal los datos de seasonal
# temperatura interior + temperatura exterior + humedad interior
md_titehiseasonal <- lm(y ~ teseasonal + seasonal + hiseasonal, 
                        data = tempint_train)
summary(md_titehiseasonal)

tempint_train$yhat <- predict(md_titehiseasonal, newdata = tempint_train)
tempint_test$yhat <- predict(md_titehiseasonal, newdata = tempint_test)

plot_lm(data = tempint_df,
        train = tempint_train,
        test = tempint_test,
        title = "Predicción de la tendencia de la serie")

mape_titehiseasonal <- c(mean(abs(tempint_train$y - tempint_train$yhat)/ tempint_train$y),
                       mean(abs(tempint_test$y - tempint_test$yhat)/ tempint_test$y))
mape_titehiseasonal

# Se utiliza para la regresión lineal de la trend + seasonal 
# temperatura exterior + temperatura interior + humedad interior
md_titehitrendseasonal <- lm(y ~ tetrend + trend + teseasonal + seasonal + hiseasonal + hitrend,
                           data = tempint_train)
summary(md_titehitrendseasonal)

tempint_train$yhat <- predict(md_titehitrendseasonal, newdata = tempint_train)
tempint_test$yhat <- predict(md_titehitrendseasonal, newdata = tempint_test)

plot_lm(data = tempint_df,
        train = tempint_train,
        test = tempint_test,
        title = "Predicción de la tendencia de la serie")

mape_md_titehitrendseasonal <- c(mean(abs(tempint_train$y - tempint_train$yhat)/ tempint_train$y),
                               mean(abs(tempint_test$y - tempint_test$yhat)/ tempint_test$y))
mape_md_titehitrendseasonal

# Regresión lineal con datos seasonal and trend and error
# temperatura exterior + temperatura interior + humedad interior
md_titehitrendseasonalerr <- lm(y ~ tetrend + trend + teseasonal + seasonal + I(trend^2) + I(tetrend^2) + hitrend + hiseasonal + I(hitrend^2), 
                                data = tempint_train)
summary(md_titehitrendseasonalerr)

tempint_train$yhat <- predict(md_titehitrendseasonalerr, newdata = tempint_train)
tempint_test$yhat <- predict(md_titehitrendseasonalerr, newdata = tempint_test)

plot_lm(data = tempint_df,
        train = tempint_train,
        test = tempint_test,
        title = "Predicción de la tendencia de la serie")

mape_md_titehitrendseasonalerr <- c(mean(abs(tempint_train$y - tempint_train$yhat)/ tempint_train$y),
                                  mean(abs(tempint_test$y - tempint_test$yhat)/ tempint_test$y))
mape_md_titehitrendseasonalerr
################################################################################

############Temperatura exterior, interior y humedad interior, exterior#########
# Se utiliza para la regresión lineal los datos de trend 
# temperatura interior + temperatura exterior + humedad interior + humedad exterior
md_titehihe <- lm(y ~ tetrend + trend + hitrend + hetrend,
                  data = tempint_train)
summary(md_titehi)

tempint_train$yhat <- predict(md_titehihe, newdata = tempint_train)
tempint_test$yhat <- predict(md_titehihe, newdata = tempint_test)

plot_lm(data = tempint_df,
        train = tempint_train,
        test = tempint_test,
        title = "Predicción de la tendencia de la serie")

mape_titehihe <- c(mean(abs(tempint_train$y - tempint_train$yhat)/ tempint_train$y),
                 mean(abs(tempint_test$y - tempint_test$yhat)/ tempint_test$y))
mape_titehihe

# Se utiliza para la regresión lineal los datos de seasonal
# temperatura interior + temperatura exterior + humedad interior + humedad exterior
md_titehiheseasonal <- lm(y ~ teseasonal + seasonal + hiseasonal + heseasonal, 
                        data = tempint_train)
summary(md_titehiheseasonal)

tempint_train$yhat <- predict(md_titehiheseasonal, newdata = tempint_train)
tempint_test$yhat <- predict(md_titehiheseasonal, newdata = tempint_test)

plot_lm(data = tempint_df,
        train = tempint_train,
        test = tempint_test,
        title = "Predicción de la tendencia de la serie")

mape_titehiheseasonal <- c(mean(abs(tempint_train$y - tempint_train$yhat)/ tempint_train$y),
                         mean(abs(tempint_test$y - tempint_test$yhat)/ tempint_test$y))
mape_titehiheseasonal

# Se utiliza para la regresión lineal de la trend + seasonal 
# temperatura exterior + temperatura interior + humedad interior + humedad exterior
md_titehihetrendseasonal <- lm(y ~ tetrend + trend + teseasonal + seasonal + hiseasonal + hitrend + heseasonal + hetrend,
                             data = tempint_train)
summary(md_titehihetrendseasonal)

tempint_train$yhat <- predict(md_titehihetrendseasonal, newdata = tempint_train)
tempint_test$yhat <- predict(md_titehihetrendseasonal, newdata = tempint_test)

plot_lm(data = tempint_df,
        train = tempint_train,
        test = tempint_test,
        title = "Predicción de la tendencia de la serie")

mape_md_titehihetrendseasonal <- c(mean(abs(tempint_train$y - tempint_train$yhat)/ tempint_train$y),
                                 mean(abs(tempint_test$y - tempint_test$yhat)/ tempint_test$y))
mape_md_titehihetrendseasonal

# Regresión lineal con datos seasonal and trend and error
# temperatura exterior + temperatura interior + humedad interior + humedad exterior
md_titehihetrendseasonalerr <- lm(y ~ tetrend + trend + teseasonal + seasonal + I(trend^2) + I(tetrend^2) + hitrend + hiseasonal + I(hitrend^2) + hetrend + heseasonal + I(hetrend^2), 
                                data = tempint_train)
summary(md_titehihetrendseasonalerr)

tempint_train$yhat <- predict(md_titehihetrendseasonalerr, newdata = tempint_train)
tempint_test$yhat <- predict(md_titehihetrendseasonalerr, newdata = tempint_test)

plot_lm(data = tempint_df,
        train = tempint_train,
        test = tempint_test,
        title = "Predicción de la tendencia de la serie")

mape_md_titehihetrendseasonalerr <- c(mean(abs(tempint_train$y - tempint_train$yhat)/ tempint_train$y),
                                    mean(abs(tempint_test$y - tempint_test$yhat)/ tempint_test$y))
mape_md_titehihetrendseasonalerr
