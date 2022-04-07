# 1.- ImpHum

# 2.- Jorge De Vivar

# 3-. Fecha de finalizaci?n: 11/3/2022, 
#     Fecha de inicio: 9/3/2022

# 4.- R 4.1.2.

# 5.- Imputaci?n de datos Humedad

###############################Cargar librer?as#################################

library (readxl)
library(naniar)
library(visdat)
library(tidyverse)
library(simputation)
library(rio)
library(mice)
library(palmerpenguins)
library(missForest)

################################################################################

############################Load_functions######################################
source("R/SEPARA_muestras.R")
source("R/METODO_media.R")
source("R/METODO_cohen.R")
source("R/METODO_NNI.R")
source("R/METODO_razon.R")
source("R/METODO_regresion.R")
source("R/METODO_razon_aleatorio.R")
source("R/METODO_regresion_aleatorio.R")
source("R/METODO_RHD.R")
source("R/REALIZA_imputacion.R")

################################################################################

############################Introducci?n########################################

# Buscar la ruta del archivo del excel
#file.choose()

# Ruta de la consola y guardar en variable
ruta_excel <- "raw_data/DatosTFG.xlsx"

# Hojas del Excel
excel_sheets(ruta_excel)

# Importar los datos de la Hoja Exterior
dato_exterior <- read_excel(ruta_excel, sheet = "Exterior")

# Pasar a numerico la Humedad exterior
yna <- suppressWarnings(as.numeric(dato_exterior$Humedad))


#Obtener los datos de tiempo
x <- dato_exterior$FechaNum

# Saber los datos faltantes de la humedad exterior
sum(is.na(yna))

################################################################################


##############Lo necesario para empezar con la imputaci?n#######################

# Saber los datos que utilizo
#muestray <- yna
muestrax <- x
muestray <- yna

# Probabilidad de inclusi?n. 
# Es 2090 por el n?mero de datos que tenemos.
number_data_points <- length(muestrax)
Pi <- rep(1/number_data_points, number_data_points)

# N?mero de datos faltantes
m <- sum(is.na(yna))


## Separamos la muestra s n y obtenemos el resto de informaci?on necesaria:
SALIDA <- SEPARA_muestras(muestray, muestrax, Pi)

## Aplicamos el m?todo de imputaci?n media para obtener los donantes:
DONANTES.media <- METODO_media(SALIDA$datosy.r, SALIDA$Pi.r, SALIDA$m)

## Aplicamos el m?todo de cohen para obtener los donantes:
DONANTES.cohen <- METODO_cohen(SALIDA$datosy.r, SALIDA$Pi.r, SALIDA$m)

## Aplicamos el m?todo NNI para obtener los donantes:
DONANTES.NNI <- METODO_NNI(SALIDA$datosy.r, SALIDA$datosx.r, SALIDA$datosx.m, SALIDA$m)

## Aplicamos el m?todo de razon y regresion para obtener los donantes:
DONANTES.razon <- METODO_razon(SALIDA$datosy.r, SALIDA$datosx.r, SALIDA$Pi.r, SALIDA$datosx.m)
DONANTES.reg <- METODO_regresion(SALIDA$datosy.r, SALIDA$datosx.r, SALIDA$Pi.r, SALIDA$datosx.m)
DONANTES.razonA <- METODO_razon_aleatorio(SALIDA$datosy.r, SALIDA$datosx.r, SALIDA$Pi.r, SALIDA$datosx.m)
DONANTES.regA <- METODO_regresion_aleatorio(SALIDA$datosy.r, SALIDA$datosx.r, SALIDA$Pi.r, SALIDA$datosx.m)

## Aplicamos el m?todo de imputaci?n RHD para obtener los donantes:
DONANTES.RHD <- METODO_RHD(SALIDA$datosy.r, SALIDA$Pi.r, SALIDA$m)

## Reemplazamos los datos faltantes por los donantes obtenidos:

IMmedia <- REALIZA_imputacion(muestray, DONANTES.media, SALIDA$POS.faltantes)
IMcohen <- REALIZA_imputacion(muestray, DONANTES.cohen, SALIDA$POS.faltantes)
IMNNI <- REALIZA_imputacion(muestray, DONANTES.NNI, SALIDA$POS.faltantes)
IMrazon <- REALIZA_imputacion(muestray, DONANTES.razon, SALIDA$POS.faltantes)
IMreg <- REALIZA_imputacion(muestray, DONANTES.reg, SALIDA$POS.faltantes)
IMrazonA <- REALIZA_imputacion(muestray, DONANTES.razonA, SALIDA$POS.faltantes)
IMregA <- REALIZA_imputacion(muestray, DONANTES.regA, SALIDA$POS.faltantes)
IMRHD <- REALIZA_imputacion(muestray, DONANTES.RHD, SALIDA$POS.faltantes)


#################MICE###########################################################
datos <- data.frame (x,yna)

# M?todo simple sin MICE
datos$yna[which(is.na(datos$yna))] = mean(datos$yna,na.rm = TRUE)

# M?todo complejo con cart
my_imp = mice(datos, m = 5, method = c("", "cart"), maxit =20)
finalclean = complete(my_imp,5)


# M?todo complejo con pmm
my_imppmm = mice(datos, m = 5, method = c("", "pmm"), maxit =20)
finalcleanpmm = complete(my_imp,5)

################################################################################

# Data Frame de imputaci?n de datos 
ImputacionDatos <- data.frame(IMmedia, IMcohen, IMNNI, IMrazon, IMreg, IMrazonA, IMregA, IMRHD,finalclean,finalcleanpmm)

# Save data
export(ImputacionDatos,"processed_data/ImputacionDatosHum.xlsx")
write_csv(ImputacionDatos,"processed_data/ImputacionDatosHum.csv")
####FIN CODIGO##################################################################
