
library(tidyverse)
library(RColorBrewer)
library(readxl)
library(corrplot)


# Raw data ---------------------------
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


# Processed data ---------------------------
Hum_imp_aux <- read_csv("processed_data/ImputacionDatosHum.csv")
Temp_imp_aux <- read_csv("processed_data/ImputacionDatosTem.csv")
MF_imp <- read_csv("processed_data/ImputacionDatosMF.csv")

Temp_MF_imp <- MF_imp %>% select(fecha_num,Tem_missForest) %>% rename(missForest = Tem_missForest)
Hum_MF_imp <- MF_imp %>% select(fecha_num,Hum_missForest) %>% rename(missForest = Hum_missForest)

Hum_imp <- Hum_imp_aux %>% left_join(Hum_MF_imp, by = "fecha_num")
Temp_imp <- Temp_imp_aux %>% left_join(Temp_MF_imp, by = "fecha_num")

Hum_imp_final <- Hum_imp %>% gather("imputation_technique", "value", -fecha_num)
Temp_imp_final <- Temp_imp %>% gather("imputation_technique", "value", -fecha_num)


# Plot imputation results-------------


plot_imputation_hum <- ggplot(Hum_imp_final)+
  geom_line(aes(x = fecha_num, y = value, color = imputation_technique), size = 1)+
  scale_color_brewer(palette = "Paired")+
  facet_wrap(~imputation_technique,ncol=2)+
  geom_line(data = dato_exterior, aes(x = FechaNum, y = Humedad), size = 1)+
  theme_bw()+
  labs(x="Tiempo (s)", y="Porcentaje de humedad (%)",color="Técnica de\nimputación")+
  
plot_imputation_temp <- ggplot(Temp_imp_final)+
  geom_line(aes(x = fecha_num, y = value, color = imputation_technique), size = 1)+
  scale_color_brewer(palette = "Paired")+
  facet_wrap(~imputation_technique)+
  geom_line(data = dato_exterior, aes(x = FechaNum, y = Temperatura), size = 1)+
  theme_bw()
  

plot_imputation_hum_NN_RF <- ggplot(Hum_imp_final %>% 
                                      filter(imputation_technique %in% c("IMNNI","missForest")))+
  geom_line(aes(x = fecha_num, y = value, color = imputation_technique), size = 1)+
  scale_color_brewer(palette = "Paired")+
  facet_wrap(~imputation_technique, ncol = 1)+
  geom_line(data = dato_exterior, aes(x = FechaNum, y = Humedad), size = 1)+
  theme_bw()

plot_imputation_temp_NN_RF <- ggplot(Temp_imp_final%>% 
                                       filter(imputation_technique %in% c("IMNNI","missForest")))+
  geom_line(aes(x = fecha_num, y = value, color = imputation_technique), size = 1)+
  scale_color_brewer(palette = "Paired")+
  facet_wrap(~imputation_technique, ncol = 1)+
  geom_line(data = dato_exterior, aes(x = FechaNum, y = Temperatura), size = 1)+
  theme_bw()


# save plots

png("figures/imputation_temperature.png",
    width = 11.69, # The width of the plot in inches
    height = 11.69*0.5, units = "in", res=300*2)
plot_imputation_temp
# plot_annotation(tag_levels = c("I", "a"))
dev.off()

png("figures/imputation_humidity.png",
    width = 11.69, # The width of the plot in inches
    height = 11.69*0.5, units = "in", res=300*2)
plot_imputation_hum
# plot_annotation(tag_levels = c("I", "a"))
dev.off()

png("figures/imputation_temperature_NN_RF.png",
    width = 11.69, # The width of the plot in inches
    height = 11.69*0.5, units = "in", res=300*2)
plot_imputation_temp_NN_RF
# plot_annotation(tag_levels = c("I", "a"))
dev.off()

png("figures/imputation_humidity_NN_RF.png",
    width = 11.69, # The width of the plot in inches
    height = 11.69*0.5, units = "in", res=300*2)
plot_imputation_hum_NN_RF
# plot_annotation(tag_levels = c("I", "a"))
dev.off()


# Check correlation among external variables-------------

corr_data <- tibble(fecha_num = Hum_imp_final$fecha_num[Hum_imp_final$imputation_technique == "IMNNI"],
                    Hum_IMNNI = Hum_imp_final$value[Hum_imp_final$imputation_technique == "IMNNI"],
                    Hum_missForest = Hum_imp_final$value[Hum_imp_final$imputation_technique == "missForest"],
                    Temp_IMNNI = Temp_imp_final$value[Temp_imp_final$imputation_technique == "IMNNI"],
                    Temp_missForest = Temp_imp_final$value[Temp_imp_final$imputation_technique == "missForest"])

M <-cor(corr_data)
corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=5, name="RdYlBu"))

# As expected, external humidity and temperature are highly correlated variables: -0.6933242 (pearson)

ggplot(corr_data, aes(x = Hum_missForest, y = Temp_missForest, color = fecha_num))+
  geom_point()+
  geom_smooth(method = "lm", formula = y~x)+
  theme_bw()


cor.test(corr_data$Hum_missForest, corr_data$Temp_missForest, method=c("pearson"))
cor.test(corr_data$Hum_missForest, corr_data$Temp_missForest, method=c("spearman"))
