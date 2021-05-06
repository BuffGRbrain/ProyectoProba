#############################################################################################################################
#Importando los datos de excel a un dataset
#OJO se deben poner las direcciones donde tenga el archivo en su equipo
library(readxl)
COPUSD <- read_excel("C:/Users/USUARIO/Desktop/COPUSD prices.xlsx",
                     sheet = "COPUSD")
View(COPUSD)

library(readxl)
COPUSD_prices <- read_excel("C:/Users/USUARIO/Desktop/COPUSD prices.xlsx",
                            sheet = "brent", col_types = c("date",
                                                           "numeric"))
View(COPUSD_prices)


library(readxl)
Coffee <- read_excel("C:/Users/USUARIO/Desktop/COPUSD prices.xlsx",
                    sheet = "Coffe", col_types = c("date",
                                                   "numeric"))
View(Coffe)


#Fin de la importación
#############################################################################################################################


#############################################################################################################################
#Histograma

#Es necesario instalar install.packages("dplyr", dependencies = T)

library("dplyr")
hist(COPUSD_prices %>% pull (Close), main ="Histograma precios del dolar", xlab = "Precio de cierre", ylab = "Frecuencia",col = "darkgreen")
hist(brent %>% pull (Close), main ="Histograma precios del petroleo Brent", xlab = "Precio de cierre (en d�lares)", ylab = "Frecuencia",col = "darkgrey")
hist(coffee %>% pull (Close), main ="Histograma precios del Cafe", xlab = "Precio de cierre (en d�lares)", ylab = "Frecuencia",col = "darkgrey")

#############################################################################################################################
#Diagrama de Caja
COPUSD <- COPUSD_prices %>% pull (Close)
Coffee <- coffee %>% pull (Close)
brent <- brent %>% pull (Close)

preciosdol = boxplot(COPUSD)
coffeee = boxplot(Coffee)
brentt = boxplot(brent)


#############################################################################################################################
#Grafico de linea
COPUSD <- COPUSD_prices # %>% (Close)
Coffee <- coffee # %>% (Close)
brent <- brent # %>% (Close)

preciosdol = plot(COPUSD, type="l", main="Precios del dolar")
coffeee = plot(Coffee, type="l", main="Precios del cafe")
brentt = plot(brent, type="l", main="Precios del petroleo")


#############################################################################################################################
