
library(TSstudio)
data(USgas)
ts_plot(USgas,
        title = "US Monthly Natural Gas consumption",
        Ytitle = "Billion Cubic Feet",
        Xtitle = "Year")
ts_info(USgas)
ts_decompose(USgas)


h=12
USgas_split <- ts_split(USgas, sample.out = h)
train.ts <- USgas_split$train
test.ts <- USgas_split$test
ts_info(train.ts)
ts_info(test.ts)

ts_decompose(train.ts)
ts_decompose(test.ts)

USgas_df <- ts_to_prophet(USgas)
head(USgas_df)
USgas_df$trend <- 1:nrow(USgas_df)
library(lubridate)
USgas_df$seasonal <- factor(month(USgas_df$ds, label = T), ordered = FALSE)
head(USgas_df)

h=12
USgas_split <- ts_split(USgas, sample.out = h)
train.ts <- USgas_split$train

library(forecast)
md3 <- tslm(train.ts ~ season + trend + I(trend^2))
