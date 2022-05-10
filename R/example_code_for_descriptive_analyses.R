library(tidyverse)
library(readxl)
library(lubridate)

# Raw data ---------------------------
# Ruta de la consola y guardar en variable
ruta_excel <- "raw_data/DatosTFG_raw.xlsx"

# Hojas del Excel
excel_sheets(ruta_excel)

# Importar los datos de la Hoja Exterior
dato_exterior <- read_excel(ruta_excel, sheet = "Exterior") %>% 
  mutate(Fecha = `Fecha...2`)

dato_interior <- read_excel(ruta_excel, sheet = "Interior") %>% 
  mutate(Fecha = `Fecha...2`)

dato_exterior <- dato_exterior[,c("Fecha","Humedad (%)","Temperatura (ºC)")]
dato_interior <- dato_interior[,c("Fecha","Humedad (%)","Temperatura (ºC)","valor CO2 (ppm)","valor CO (ppm)")]

# We set the day as unit of frequency
start_point <- c(year(min(dato_interior$Fecha)),month(min(dato_interior$Fecha)))
unidades <- length(dato_interior$Fecha)# - 1
humedad_interior_ts <- ts(data = dato_interior$`Humedad (%)`[1:unidades], 
                          start = 0,#start_point,
                          frequency = 6*24)

temperatura_interior_ts <- ts(data = dato_interior$`Temperatura (ºC)`[1:unidades], 
                              start = 0,#start_point,
                              frequency = 6*24)

humedad_interior_decomp <- decompose(humedad_interior_ts)
temperatura_interior_decomp <- decompose(temperatura_interior_ts)

humedad_interior_detrend <- humedad_interior_ts - humedad_interior_decomp$trend
temperatura_interior_detrend <- temperatura_interior_ts - temperatura_interior_decomp$trend

par(mar=c(1,1,1,1))
plot(humedad_interior_ts)
plot(humedad_interior_decomp)
plot(humedad_interior_detrend)
plot(temperatura_interior_ts)
plot(temperatura_interior_decomp)
plot(temperatura_interior_detrend)

library(forecast)

ggseasonplot(temperatura_interior_ts, year.labels = T, continuous = T)
ggseasonplot(temperatura_interior_ts, polar = T)

temperatura_df <- tibble(Fecha = dato_interior$Fecha,
                         segundos = 60 * 10 * (as.numeric(rownames(dato_interior))-1),
                         semana_muestreo = 1 + (segundos - (segundos %% (3600 *24*7)))/(3600 *24*7),
                         segundo_muestreo_semana = segundos - (semana_muestreo-1) * (3600 *24*7),
                         dia_muestreo = 1 + (segundos - (segundos %% (3600 *24)))/(3600 *24),
                         segundo_muestreo_dia = segundos - (dia_muestreo-1) * (3600 *24),
                         hora_muestreo_dia = floor(segundo_muestreo_dia/3600),
                         minuto_muestreo_dia = floor(segundo_muestreo_dia/60),
                         segundo_muestreo_hora = segundo_muestreo_dia - hora_muestreo_dia*3600,
                         Temperatura = dato_interior$`Temperatura (ºC)`)

temperatura_detrend_df <- tibble(Fecha = dato_interior$Fecha,
                                 segundos = 60 * 10 * (as.numeric(rownames(dato_interior))-1),
                                 semana_muestreo = 1 + (segundos - (segundos %% (3600 *24*7)))/(3600 *24*7),
                                 segundo_muestreo_semana = segundos - (semana_muestreo-1) * (3600 *24*7),
                                 dia_muestreo = 1 + (segundos - (segundos %% (3600 *24)))/(3600 *24),
                                 segundo_muestreo_dia = segundos - (dia_muestreo-1) * (3600 *24),
                                 hora_muestreo_dia = floor(segundo_muestreo_dia/3600),
                                 minuto_muestreo_dia = floor(segundo_muestreo_dia/60),
                                 segundo_muestreo_hora = segundo_muestreo_dia - hora_muestreo_dia*3600,
                                 Temperatura = as.matrix(temperatura_interior_detrend))

ggplot(temperatura_df)+
  geom_line(aes(x = segundo_muestreo_semana, y = Temperatura, 
                group = semana_muestreo, color = as.factor(semana_muestreo)))+
  theme_bw()+
  labs(x="Tiempo de muestreo [s]", y = "Temperatura [ºC]", color= "Semana", title = "Temperatura")

ggplot(temperatura_detrend_df)+
  geom_line(aes(x = segundo_muestreo_semana, y = Temperatura, 
                group = semana_muestreo, color = as.factor(semana_muestreo)))+
  theme_bw()+
  labs(x="Tiempo de muestreo [s]", y = "Temperatura [ºC]", color= "Semana", title = "Temperatura (sin tendencia)")

ggplot(temperatura_df)+
  geom_line(aes(x = segundo_muestreo_dia, y = Temperatura, 
                group = dia_muestreo, color = as.factor(dia_muestreo)))+
  theme_bw()+
  labs(x="Tiempo de muestreo [s]", y = "Temperatura [ºC]", color= "Día", title = "Temperatura")

ggplot(temperatura_detrend_df)+
  geom_line(aes(x = segundo_muestreo_dia, y = Temperatura, 
                group = dia_muestreo, color = as.factor(dia_muestreo)))+
  theme_bw()+
  labs(x="Tiempo de muestreo [s]", y = "Temperatura [ºC]", color= "Día", title = "Temperatura (sin tendencia)")

ggplot(temperatura_df)+
  geom_line(aes(x = dia_muestreo, y = Temperatura, 
                group = minuto_muestreo_dia, color = as.factor(hora_muestreo_dia)))+
  theme_bw()+
  labs(x="Día de muestreo", y = "Temperatura [ºC]", color= "Hora de\nmuestreo", title = "Temperatura")

ggplot(temperatura_detrend_df)+
  geom_line(aes(x = dia_muestreo, y = Temperatura, 
                group = minuto_muestreo_dia, color = as.factor(hora_muestreo_dia)))+
  theme_bw()+
  labs(x="Día de muestreo", y = "Temperatura [ºC]", color= "Hora de\nmuestreo", title = "Temperatura (sin tendencia)")

ggplot(temperatura_df)+
  geom_boxplot(aes(x = as.factor(hora_muestreo_dia), y = Temperatura, 
                   color = as.factor(hora_muestreo_dia)))+
  theme_bw()+
  labs(x="Tiempo de muestreo [h]", y = "Temperatura [ºC]", color= NULL, title = "Temperatura")+
  theme(legend.position="none")

ggplot(temperatura_detrend_df)+
  geom_boxplot(aes(x = as.factor(hora_muestreo_dia), y = Temperatura, 
                   color = as.factor(hora_muestreo_dia)))+
  theme_bw()+
  labs(x="Tiempo de muestreo [h]", y = "Temperatura [ºC]", color= NULL, title = "Temperatura (sin tendencia)")+
  theme(legend.position="none")

ggplot(temperatura_df, aes(x = segundo_muestreo_dia, y = dia_muestreo, fill = Temperatura)) + geom_tile()+
  theme_bw()+
  labs(x="Segundo de muestreo", y = "Día de muestreo", color= "Temperatura [ºC]", title = "Temperatura")

ggplot(temperatura_detrend_df, aes(x = segundo_muestreo_dia, y = dia_muestreo, fill = Temperatura)) + geom_tile()+
  theme_bw()+
  labs(x="Segundo de muestreo", y = "Día de muestreo", color= "Temperatura [ºC]", title = "Temperatura (sin tendencia)")


ggplot(temperatura_df %>% group_by(segundo_muestreo_dia) %>%
         summarise(median = median(Temperatura),quantile_25 = quantile(Temperatura,.25),
                   quantile_75 = quantile(Temperatura,.75)))+
  geom_ribbon(aes(x = segundo_muestreo_dia,ymin=quantile_25,ymax=quantile_75), fill="deepskyblue", alpha=1)+
  geom_line(aes(x = segundo_muestreo_dia, y = median),size = 1)+
  theme_bw()+
  labs(x="Tiempo de muestreo diario [s]", y = "Temperatura [ºC]", title = "Temperatura - Quantile plot")

ggplot(temperatura_detrend_df %>% group_by(segundo_muestreo_dia) %>%
         summarise(median = median(Temperatura, na.rm = T),quantile_25 = quantile(Temperatura,.25, na.rm = T),
                   quantile_75 = quantile(Temperatura,.75, na.rm = T)))+
  geom_ribbon(aes(x = segundo_muestreo_dia,ymin=quantile_25,ymax=quantile_75), fill="deepskyblue", alpha=1)+
  geom_line(aes(x = segundo_muestreo_dia, y = median),size = 1)+
  theme_bw()+
  labs(x="Tiempo de muestreo diario [s]", y = "Temperatura [ºC]", title = "Temperatura (sin tendencia) - Quantile plot")

ggplot(temperatura_df %>% group_by(segundo_muestreo_dia,hora_muestreo_dia,segundo_muestreo_hora) %>%
         summarise(median = median(Temperatura),quantile_25 = quantile(Temperatura,.25),
                   quantile_75 = quantile(Temperatura,.75)))+
  geom_ribbon(aes(x = segundo_muestreo_hora, ymin=quantile_25,ymax=quantile_75), fill="deepskyblue", alpha=1)+
  geom_line(aes(x = segundo_muestreo_hora, y = median),size = 1)+
  facet_wrap(~hora_muestreo_dia)+
  theme_bw()+
  labs(x="Tiempo de muestreo diario [s]", y = "Temperatura [ºC]", title = "Quantile plot")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(temperatura_detrend_df %>% group_by(segundo_muestreo_dia,hora_muestreo_dia,segundo_muestreo_hora) %>%
         summarise(median = median(Temperatura, na.rm = T),quantile_25 = quantile(Temperatura,.25, na.rm = T),
                   quantile_75 = quantile(Temperatura,.75, na.rm = T)))+
  geom_ribbon(aes(x = segundo_muestreo_hora, ymin=quantile_25,ymax=quantile_75), fill="deepskyblue", alpha=1)+
  geom_line(aes(x = segundo_muestreo_hora, y = median),size = 1)+
  facet_wrap(~hora_muestreo_dia)+
  theme_bw()+
  labs(x="Tiempo de muestreo diario [s]", y = "Temperatura [ºC]", title = "Temperatura (sin tendencia) - Quantile plot")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



# Correlation analysis--------

library(TSstudio)
library(tseries)

# Autocorrelation function
ts_cor(temperatura_interior_ts)
ts_cor(na.remove(temperatura_interior_detrend))

# Lag analysis
ts_lags(temperatura_interior_ts)
ts_lags(temperatura_interior_detrend)

# Cross-correlation analysis--------
ccf(x = temperatura_interior_ts, y = humedad_interior_ts, lag.max = 14)
ccf_plot(x = temperatura_interior_ts, y = humedad_interior_ts, lags = 0:14)

ccf(x = na.remove(temperatura_interior_detrend), y = na.remove(humedad_interior_detrend), lag.max = 14)
ccf_plot(x = na.remove(temperatura_interior_detrend), y = na.remove(humedad_interior_detrend), lags = 0:14)
