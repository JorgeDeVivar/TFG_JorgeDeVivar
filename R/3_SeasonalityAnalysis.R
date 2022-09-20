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

# Ts outputs
ts_plot(humedad_interior_ts,
        title = "Serie temporal de la humedad interior",
        Ytitle = "Humedad interior",
        Xtitle = "Días",
        Xgrid = TRUE,
        Ygrid = TRUE)

ts_plot(temperatura_interior_ts,
        title = "Serie temporal de la temperatura interior",
        Ytitle = "Temperatura interior",
        Xtitle = "Días",
        Xgrid = TRUE,
        Ygrid = TRUE)

ts_plot(dioxidocarb_interior_ts,
        title = "Serie temporal de la dióxido de carbono interior",
        Ytitle = "Dióxido de carbono interior",
        Xtitle = "Días",
        Xgrid = TRUE,
        Ygrid = TRUE)

ts_plot(monoxidocarb_interior_ts,
        title = "Serie temporal de la monóxido de carbono interior",
        Ytitle = "Monóxido de carbono interior",
        Xtitle = "Días",
        Xgrid = TRUE,
        Ygrid = TRUE)

ts_plot(humedad_exterior_ts,
        title = "Serie temporal de la humedad exterior",
        Ytitle = "Humedad exterior",
        Xtitle = "Días",
        Xgrid = TRUE,
        Ygrid = TRUE)

ts_plot(temperatura_exterior_ts,
        title = "Serie temporal de la temperatura exterior",
        Ytitle = "Temperatura exterior",
        Xtitle = "Días",
        Xgrid = TRUE,
        Ygrid = TRUE)

ggseasonplot(temperatura_interior_ts, year.labels = T, continuous = T)
ggseasonplot(temperatura_interior_ts, polar = T)

# Decomposition of additive time series
huminterior_decompose <- decompose(humedad_interior_ts)
teminterior_decompose <- decompose(temperatura_interior_ts)
diointerior_decompose <- decompose(dioxidocarb_interior_ts)
moninterior_decompose <- decompose(monoxidocarb_interior_ts)
humexterior_decompose <- decompose(humedad_exterior_ts)
temexterior_decompose <- decompose(temperatura_exterior_ts)

plot(huminterior_decompose)
plot(teminterior_decompose)
plot(diointerior_decompose)
plot(moninterior_decompose)
plot(humexterior_decompose)
plot(temexterior_decompose)

# Decomposition of multiplicative time series
huminterior_decompose_mul <- decompose(humedad_interior_ts, type = "multiplicative")
teminterior_decompose_mul <- decompose(temperatura_interior_ts, type = "multiplicative")
diointerior_decompose_mul <- decompose(dioxidocarb_interior_ts, type = "multiplicative")
moninterior_decompose_mul <- decompose(monoxidocarb_interior_ts, type = "multiplicative")
humexterior_decompose_mul <- decompose(humedad_exterior_ts, type = "multiplicative")
temexterior_decompose_mul <- decompose(temperatura_exterior_ts, type = "multiplicative")

plot(huminterior_decompose_mul)
plot(teminterior_decompose_mul)
plot(diointerior_decompose_mul)
plot(moninterior_decompose_mul)
plot(humexterior_decompose_mul)
plot(temexterior_decompose_mul)
################# Solo para prueba, solo se utilizará el aditivo

#Detrend
humedad_interior_detrend <- humedad_interior_ts - huminterior_decompose$trend
temperatura_interior_detrend <- temperatura_interior_ts - teminterior_decompose$trend
monoxido_interior_detrend <- monoxidocarb_interior_ts - moninterior_decompose$trend
dioxido_interior_detrend <- dioxidocarb_interior_ts - diointerior_decompose$trend
humedad_exterior_detrend <- humedad_exterior_ts - humexterior_decompose$trend
temperatura_exterior_detrend <- temperatura_exterior_ts - temexterior_decompose$trend

ts_plot(humedad_interior_detrend)

temperatura_int_df <- tibble(Fecha = dato_interior$Fecha,
                         segundos = 60 * 20 * (as.numeric(rownames(dato_interior))-1),
                         semana_muestreo = 1 + (segundos - (segundos %% (3600 *12*7)))/(3600 *12*7),
                         segundo_muestreo_semana = segundos - (semana_muestreo-1) * (3600 *12*7),
                         dia_muestreo = 1 + (segundos - (segundos %% (3600 *12)))/(3600 *12),
                         segundo_muestreo_dia = segundos - (dia_muestreo-1) * (3600 *12),
                         hora_muestreo_dia = floor(segundo_muestreo_dia/3600),
                         minuto_muestreo_dia = floor(segundo_muestreo_dia/60),
                         segundo_muestreo_hora = segundo_muestreo_dia - hora_muestreo_dia*3600,
                         Temperatura = dato_interior$`Temperatura (ºC)`)

temperatura_int_detrend_df <- tibble(Fecha = dato_interior$Fecha,
                                 segundos = 60 * 20 * (as.numeric(rownames(dato_interior))-1),
                                 semana_muestreo = 1 + (segundos - (segundos %% (3600 *12*7)))/(3600 *12*7),
                                 segundo_muestreo_semana = segundos - (semana_muestreo-1) * (3600 *12*7),
                                 dia_muestreo = 1 + (segundos - (segundos %% (3600 *12)))/(3600 *12),
                                 segundo_muestreo_dia = segundos - (dia_muestreo-1) * (3600 *12),
                                 hora_muestreo_dia = floor(segundo_muestreo_dia/3600),
                                 minuto_muestreo_dia = floor(segundo_muestreo_dia/60),
                                 segundo_muestreo_hora = segundo_muestreo_dia - hora_muestreo_dia*3600,
                                 Temperatura = as.matrix(temperatura_interior_detrend))

ggplot(temperatura_int_df)+
  geom_line(aes(x = segundo_muestreo_semana, y = Temperatura, 
                group = semana_muestreo, color = as.factor(semana_muestreo)))+
  theme_bw()+
  labs(x="Tiempo de muestreo [s]", y = "Temperatura [ºC]", color= "Semana", title = "Temperatura")

ggplot(temperatura_int_detrend_df)+
  geom_line(aes(x = segundo_muestreo_semana, y = Temperatura, 
                group = semana_muestreo, color = as.factor(semana_muestreo)))+
  theme_bw()+
  labs(x="Tiempo de muestreo [s]", y = "Temperatura [ºC]", color= "Semana", title = "Temperatura (sin tendencia)")

ggplot(temperatura_int_df)+
  geom_line(aes(x = segundo_muestreo_dia, y = Temperatura, 
                group = dia_muestreo, color = as.factor(dia_muestreo)))+
  theme_bw()+
  labs(x="Tiempo de muestreo [s]", y = "Temperatura [ºC]", color= "Día", title = "Temperatura")

ggplot(temperatura_int_detrend_df)+
  geom_line(aes(x = segundo_muestreo_dia, y = Temperatura, 
                group = dia_muestreo, color = as.factor(dia_muestreo)))+
  theme_bw()+
  labs(x="Tiempo de muestreo [s]", y = "Temperatura [ºC]", color= "Día", title = "Temperatura (sin tendencia)")

ggplot(temperatura_int_df)+
  geom_line(aes(x = dia_muestreo, y = Temperatura, 
                group = minuto_muestreo_dia, color = as.factor(hora_muestreo_dia)))+
  theme_bw()+
  labs(x="Día de muestreo", y = "Temperatura [ºC]", color= "Hora de\nmuestreo", title = "Temperatura")

ggplot(temperatura_int_detrend_df)+
  geom_line(aes(x = dia_muestreo, y = Temperatura, 
                group = minuto_muestreo_dia, color = as.factor(hora_muestreo_dia)))+
  theme_bw()+
  labs(x="Día de muestreo", y = "Temperatura [ºC]", color= "Hora de\nmuestreo", title = "Temperatura (sin tendencia)")

ggplot(temperatura_int_df)+
  geom_boxplot(aes(x = as.factor(hora_muestreo_dia), y = Temperatura, 
                   color = as.factor(hora_muestreo_dia)))+
  theme_bw()+
  labs(x="Tiempo de muestreo [h]", y = "Temperatura [ºC]", color= NULL, title = "Temperatura")+
  theme(legend.position="none")

ggplot(temperatura_int_detrend_df)+
  geom_boxplot(aes(x = as.factor(hora_muestreo_dia), y = Temperatura, 
                   color = as.factor(hora_muestreo_dia)))+
  theme_bw()+
  labs(x="Tiempo de muestreo [h]", y = "Temperatura [ºC]", color= NULL, title = "Temperatura (sin tendencia)")+
  theme(legend.position="none")

ggplot(temperatura_int_df, aes(x = segundo_muestreo_dia, y = dia_muestreo, fill = Temperatura)) + geom_tile()+
  theme_bw()+
  labs(x="Segundo de muestreo", y = "Día de muestreo", color= "Temperatura [ºC]", title = "Temperatura")

ggplot(temperatura_int_detrend_df, aes(x = segundo_muestreo_dia, y = dia_muestreo, fill = Temperatura)) + geom_tile()+
  theme_bw()+
  labs(x="Segundo de muestreo", y = "Día de muestreo", color= "Temperatura [ºC]", title = "Temperatura (sin tendencia)")


ggplot(temperatura_int_df %>% group_by(segundo_muestreo_dia) %>%
         summarise(median = median(Temperatura),quantile_25 = quantile(Temperatura,.25),
                   quantile_75 = quantile(Temperatura,.75)))+
  geom_ribbon(aes(x = segundo_muestreo_dia,ymin=quantile_25,ymax=quantile_75), fill="deepskyblue", alpha=1)+
  geom_line(aes(x = segundo_muestreo_dia, y = median),size = 1)+
  theme_bw()+
  labs(x="Tiempo de muestreo diario [s]", y = "Temperatura [ºC]", title = "Temperatura - Quantile plot")

ggplot(temperatura_int_detrend_df %>% group_by(segundo_muestreo_dia) %>%
         summarise(median = median(Temperatura, na.rm = T),quantile_25 = quantile(Temperatura,.25, na.rm = T),
                   quantile_75 = quantile(Temperatura,.75, na.rm = T)))+
  geom_ribbon(aes(x = segundo_muestreo_dia,ymin=quantile_25,ymax=quantile_75), fill="deepskyblue", alpha=1)+
  geom_line(aes(x = segundo_muestreo_dia, y = median),size = 1)+
  theme_bw()+
  labs(x="Tiempo de muestreo diario [s]", y = "Temperatura [ºC]", title = "Temperatura (sin tendencia) - Quantile plot")

ggplot(temperatura_int_df %>% group_by(segundo_muestreo_dia,hora_muestreo_dia,segundo_muestreo_hora) %>%
         summarise(median = median(Temperatura),quantile_25 = quantile(Temperatura,.25),
                   quantile_75 = quantile(Temperatura,.75)))+
  geom_ribbon(aes(x = segundo_muestreo_hora, ymin=quantile_25,ymax=quantile_75), fill="deepskyblue", alpha=1)+
  geom_line(aes(x = segundo_muestreo_hora, y = median),size = 1)+
  facet_wrap(~hora_muestreo_dia)+
  theme_bw()+
  labs(x="Tiempo de muestreo diario [s]", y = "Temperatura [ºC]", title = "Quantile plot")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(temperatura_int_detrend_df %>% group_by(segundo_muestreo_dia,hora_muestreo_dia,segundo_muestreo_hora) %>%
         summarise(median = median(Temperatura, na.rm = T),quantile_25 = quantile(Temperatura,.25, na.rm = T),
                   quantile_75 = quantile(Temperatura,.75, na.rm = T)))+
  geom_ribbon(aes(x = segundo_muestreo_hora, ymin=quantile_25,ymax=quantile_75), fill="deepskyblue", alpha=1)+
  geom_line(aes(x = segundo_muestreo_hora, y = median),size = 1)+
  facet_wrap(~hora_muestreo_dia)+
  theme_bw()+
  labs(x="Tiempo de muestreo diario [s]", y = "Temperatura [ºC]", title = "Temperatura (sin tendencia) - Quantile plot")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Correlation analysis
# Autocorrelation analysis temperatura interior
ts_cor(temperatura_interior_ts)
ts_cor(na.remove(temperatura_interior_detrend))

# Autocorrelation analysis humedad interior
ts_cor(humedad_interior_ts)
ts_cor(na.remove(humedad_interior_detrend))

# Lag analysis
ts_lags(temperatura_interior_ts)
ts_lags(temperatura_interior_detrend)
ts_lags(humedad_interior_ts)
ts_lags(humedad_interior_detrend)

# Cross-correlation analysis temperatura interior
par(mar=c(1,1,1,1))

ccf(x = temperatura_interior_ts, y = humedad_interior_ts, lag.max = 14)
ccf_plot(x = temperatura_interior_ts, y = humedad_interior_ts, lags = 0:14)
ccf(x = na.remove(temperatura_interior_detrend), y = na.remove(humedad_interior_detrend), lag.max = 14)
ccf_plot(x = na.remove(temperatura_interior_detrend), y = na.remove(humedad_interior_detrend), lags = 0:14)

ccf(x = temperatura_interior_ts, y = dioxidocarb_interior_ts, lag.max = 14)
ccf_plot(x = temperatura_interior_ts, y = dioxidocarb_interior_ts, lags = 0:14)
ccf(x = na.remove(temperatura_interior_detrend), y = na.remove(dioxido_interior_detrend), lag.max = 14)
ccf_plot(x = na.remove(temperatura_interior_detrend), y = na.remove(dioxido_interior_detrend), lags = 0:14)

ccf(x = temperatura_interior_ts, y = monoxidocarb_interior_ts, lag.max = 14)
ccf_plot(x = temperatura_interior_ts, y = monoxidocarb_interior_ts, lags = 0:14)
ccf(x = na.remove(temperatura_interior_detrend), y = na.remove(monoxido_interior_detrend), lag.max = 14)
ccf_plot(x = na.remove(temperatura_interior_detrend), y = na.remove(monoxido_interior_detrend), lags = 0:14)

ccf(x = temperatura_interior_ts, y = temperatura_exterior_ts, lag.max = 14)
ccf_plot(x = temperatura_interior_ts, y = temperatura_exterior_ts, lags = 0:14)
ccf(x = na.remove(temperatura_interior_detrend), y = na.remove(temperatura_exterior_detrend), lag.max = 14)
ccf_plot(x = na.remove(temperatura_interior_detrend), y = na.remove(temperatura_exterior_detrend), lags = 0:14)

ccf(x = temperatura_interior_ts, y = humedad_exterior_ts, lag.max = 14)
ccf_plot(x = temperatura_interior_ts, y = humedad_exterior_ts, lags = 0:14)
ccf(x = na.remove(temperatura_interior_detrend), y = na.remove(humedad_exterior_detrend), lag.max = 14)
ccf_plot(x = na.remove(temperatura_interior_detrend), y = na.remove(humedad_exterior_detrend), lags = 0:14)

# Cross-correlation analysis humedad interior
ccf(x = humedad_interior_ts, y = temperatura_interior_ts, lag.max = 14)
ccf_plot(x = humedad_interior_ts, y = temperatura_interior_ts, lags = 0:14)
ccf(x = na.remove(humedad_interior_detrend), y = na.remove(temperatura_interior_detrend), lag.max = 14)
ccf_plot(x = na.remove(humedad_interior_detrend), y = na.remove(temperatura_interior_detrend), lags = 0:14)

ccf(x = humedad_interior_ts, y = dioxidocarb_interior_ts, lag.max = 14)
ccf_plot(x = humedad_interior_ts, y = dioxidocarb_interior_ts, lags = 0:14)
ccf(x = na.remove(humedad_interior_detrend), y = na.remove(dioxido_interior_detrend), lag.max = 14)
ccf_plot(x = na.remove(humedad_interior_detrend), y = na.remove(dioxido_interior_detrend), lags = 0:14)

ccf(x = humedad_interior_ts, y = monoxidocarb_interior_ts, lag.max = 14)
ccf_plot(x = humedad_interior_ts, y = monoxidocarb_interior_ts, lags = 0:14)
ccf(x = na.remove(humedad_interior_detrend), y = na.remove(monoxido_interior_detrend), lag.max = 14)
ccf_plot(x = na.remove(humedad_interior_detrend), y = na.remove(monoxido_interior_detrend), lags = 0:14)

ccf(x = humedad_interior_ts, y = temperatura_exterior_ts, lag.max = 14)
ccf_plot(x = humedad_interior_ts, y = temperatura_exterior_ts, lags = 0:14)
ccf(x = na.remove(humedad_interior_detrend), y = na.remove(temperatura_exterior_detrend), lag.max = 14)
ccf_plot(x = na.remove(humedad_interior_detrend), y = na.remove(temperatura_exterior_detrend), lags = 0:14)

ccf(x = humedad_interior_ts, y = humedad_exterior_ts, lag.max = 14)
ccf_plot(x = humedad_interior_ts, y = humedad_exterior_ts, lags = 0:14)
ccf(x = na.remove(humedad_interior_detrend), y = na.remove(humedad_exterior_detrend), lag.max = 14)
ccf_plot(x = na.remove(humedad_interior_detrend), y = na.remove(humedad_exterior_detrend), lags = 0:14)


# Seasonality Analysis with Official Data
# Ruta de la consola y guardar en variable
ruta_excel <- "raw_data/OfficialData.xlsx"
ruta <- "raw_data/DatosTFG.xlsx"

# Hojas del Excel
excel_sheets(ruta_excel)

# Importar los datos de la Hoja Exterior
dato_exterior <- read_excel(ruta, sheet = "Exterior")
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

humedad_exterior_ts <- ts (data = dato_exterior$Humedad[1:unidades],
                           start = 1,
                           frequency = 3*24)

temperatura_exterior_ts <- ts(data = dato_exterior$Temperatura[1:unidades],
                              start = 1,
                              frequency = 3*24)

# Ts outputs
ts_plot(humedad_interior_ts,
        title = "Serie temporal de la humedad interior",
        Ytitle = "Humedad interior",
        Xtitle = "Días",
        Xgrid = TRUE,
        Ygrid = TRUE)

ts_plot(temperatura_interior_ts,
        title = "Serie temporal de la temperatura interior",
        Ytitle = "Temperatura interior",
        Xtitle = "Días",
        Xgrid = TRUE,
        Ygrid = TRUE)

ts_plot(dioxidocarb_interior_ts,
        title = "Serie temporal de la dióxido de carbono interior",
        Ytitle = "Dióxido de carbono interior",
        Xtitle = "Días",
        Xgrid = TRUE,
        Ygrid = TRUE)

ts_plot(monoxidocarb_interior_ts,
        title = "Serie temporal de la monóxido de carbono interior",
        Ytitle = "Monóxido de carbono interior",
        Xtitle = "Días",
        Xgrid = TRUE,
        Ygrid = TRUE)

ts_plot(humedad_exterior_ts,
        title = "Serie temporal de la humedad exterior",
        Ytitle = "Humedad exterior",
        Xtitle = "Días",
        Xgrid = TRUE,
        Ygrid = TRUE)

ts_plot(temperatura_exterior_ts,
        title = "Serie temporal de la temperatura exterior",
        Ytitle = "Temperatura exterior",
        Xtitle = "Días",
        Xgrid = TRUE,
        Ygrid = TRUE)
