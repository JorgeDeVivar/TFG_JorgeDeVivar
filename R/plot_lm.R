# Imagen de la regresión lineal
plot_lm <- function (data, train, test, title = NULL){
  p <- plot_ly(data = data,
               x = ~ ds,
               y = ~ y, 
               type = "scatter",
               mode = "line",
               name = "Actual") %>%
    add_lines(x = ~ train$ds,
              y = ~ train$yhat,
              line = list(color = "red"),
              name = "Fitted") %>%
    add_lines(x = ~ test$ds,
              y = ~ test$yhat,
              line = list(color = "green", dash = "dot", width = 3),
              name = "Forecasted") %>%
    layout(title = title,
           xaxis = list(title = "Días"),
           yaxis = list(title = "Temperatura (ºC)"),
           legend = list (x = 0.05, y = 0.95))
  return(p)
}
