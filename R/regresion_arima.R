temperatura_interior_ts <- ts(data = dato_interior$`Temperatura (ºC)`[1:unidades], start = 1, frequency = 3*24)

df <- tibble(y=dato_interior$`Temperatura (ºC)`[1:unidades])
df$measure <- dplyr::row_number(df)
df$lag72 <- dplyr::lag(df$y, n = 72)
df$trend <- decompose(temperatura_interior_ts)$trend
df$seasonal <- decompose(temperatura_interior_ts)$seasonal

library(lubridate)

h=627
par <- ts_split(ts.obj = temperatura_interior_ts, sample.out = h)
train <- par$train
test <- par$test
train_df <- df[1:(nrow(df) - h), ]
test_df <- df[(nrow(df) - h + 1):nrow(df), ]
md1 <- tslm(train ~ trend + seasonal, data = train_df)
checkresiduals(md1)

md2 <- auto.arima(train,
                  xreg = cbind(train_df$trend,
                               train_df$seasonal),
                  seasonal = TRUE,
                  stepwise = TRUE,
                  approximation = FALSE)
summary(md2)
checkresiduals(md2)
fc1 <- forecast(md1, newdata = test_df)
fc2 <- forecast(md2, xreg = cbind(test_df$trend,
                                  test_df$seasonal))
forecast::accuracy(fc1, test)
forecast::accuracy(fc2, test)
