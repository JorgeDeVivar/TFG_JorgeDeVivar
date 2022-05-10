library(h2o)
library(TSstudio)
library(plotly)
library(lubridate)
library(forecast)
library(readxl)
library(stats)
library(tidyverse)

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

ts_decompose(temperatura_interior_ts)

ti_detrend <- temperatura_interior_ts - decompose(temperatura_interior_ts)$trend
temperatura_interior_decomp <- decompose(temperatura_interior_ts)
temperatura_interior_detrend <- temperatura_interior_ts - temperatura_interior_decomp$trend


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

ggplot(temperatura_detrend_df)+
  geom_boxplot(aes(x = as.factor(hora_muestreo_dia), y = Temperatura, 
                   color = as.factor(hora_muestreo_dia)))+
  theme_bw()+
  labs(x="Tiempo de muestreo [h]", y = "Temperatura [ºC]", color= NULL, title = "Temperatura_detrend")+
  theme(legend.position="none")

par(mar=c(1,1,1,1))
acf(temperatura_interior_ts)

ts_lags(temperatura_interior_ts, lags = c(12, 24, 36))

temint_df <- tibble(date = dato_interior$Fecha,
                    segundos = 60 * 10 * (as.numeric(rownames(dato_interior))-1),
                    semana_muestreo = 1 + (segundos - (segundos %% (3600 *24*7)))/(3600 *24*7),
                    segundo_muestreo_semana = segundos - (semana_muestreo-1) * (3600 *24*7),
                    day = 1 + (segundos - (segundos %% (3600 *24)))/(3600 *24),
                    y = dato_interior$`Temperatura (ºC)`)
head(temint_df)

temint_df$lag12 <- lag(temint_df$y, n = 12)

temint_df$trend <- decompose(temperatura_interior_ts)$trend
temint_df$trend_sqr <- temint_df$trend ^ 2

h <- 627
temint_train <- temint_df[1:(unidades - h), ]
temint_test <- temint_df[(unidades - h + 1):unidades, ]




lr <- lm(y ~ day + lag12 + trend + trend_sqr, data = temint_train)

temint_test$yhat <- predict(lr, newdata = temint_test)

mape_lr <- mean(abs(temint_test$y - temint_test$yhat)/temint_test$y)

h2o.init(max_mem_size = "16G")

train_h <- as.h2o(temint_train)
test_h <- as.h2o(temint_test)

x <- c("day", "lag12", "trend", "trend_sqr")
y <- "y"



gbm_md <- h2o.gbm(
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
h2o.varimp_plot(gbm_md)
test_h$pred_gbm <- h2o.predict(gbm_md, test_h)
test_1 <- as.data.frame(test_h)
mape_gbm <- mean(abs(test_1$y - test_1$pred_gbm) / test_1$y)
mape_gbm

plot_ly(data = test_1) %>%
  add_lines(x = ~ date, y = ~y, name = "Actual") %>%
  add_lines(x = ~ date, y = ~ yhat, name = "Linear Regression", line =
              list(dash = "dot")) %>%
  add_lines(x = ~ date, y = ~ pred_gbm, name = "Gradient Boosting Machine",
            line = list(dash = "dash")) %>%
  layout(title = "Temperatura interior - Actual vs. Prediction (Gradient
Boosting Machine)",
         yaxis = list(title = "Temperatura (ºC)"),
         xaxis = list(title = "Día"))
