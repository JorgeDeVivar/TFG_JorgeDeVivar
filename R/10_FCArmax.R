library(TSstudio)
library(plotly)
library(dplyr)
library(lubridate)
library(forecast)
library(readxl)
library(stats)
library(vars)

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

tempint_df$trend <- decompose(temperatura_interior_ts)$trend
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

# ARMAX

x = cbind(temperatura_interior_ts, temperatura_exterior_ts, humedad_interior_ts, humedad_exterior_ts)
summary(VAR(x, p = 1, type = 'both'))

VARselect(x, lag.max = 10, type = "both")

par(mar=c(1,1,1,1))
summary(fit <- VAR(x, p = 2, type = "both"))
acf(resid(fit), 52)
serial.test(fit, lags.pt = 12, type = "PT.adjusted")
(fit.pr = predict(fit, n.ahead = 24, ci = 0.95))
fanchart(fit.pr)


