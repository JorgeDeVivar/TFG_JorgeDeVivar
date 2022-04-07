################################################################################
###############################Cargar librer?as#################################

library (readxl)
library(tidyverse)
library(patchwork)

# Buscar la ruta del archivo del excel
#file.choose()

# Ruta de la consola y guardar en variable
ruta_excel <- "raw_data/DatosTFG.xlsx"

# Hojas del Excel
excel_sheets(ruta_excel)

# Importar los datos de la Hoja Exterior
dato_exterior <- read_excel(ruta_excel, sheet = "Exterior")

# Pasar a numerico la Temperatura exterior
dato_exterior$Temperatura <- suppressWarnings(as.numeric(dato_exterior$Temperatura))
dato_exterior$Humedad <- suppressWarnings(as.numeric(dato_exterior$Humedad))

#Obtener los datos de tiempo
x <- dato_exterior$FechaNum

################################################################################
# Plot raw series---------------------------------------------------------------

plotHumedad <- ggplot(dato_exterior, aes(x = FechaNum, y = Humedad))+
  #geom_point()+
  geom_line(size=1.0)+
  theme_bw()

  
plotTemperatura <- ggplot(dato_exterior, aes(x = FechaNum, y = Temperatura))+
  #geom_point()+
  geom_line(size=1.0)+
  theme_bw()
 
plotHumedad/plotTemperatura

ggplot(dato_exterior, aes(x = Humedad, y = Temperatura, color = FechaNum))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2))+
  theme_bw()
