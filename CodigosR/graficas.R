#############################################################################################################################
#Importando los datos de excel a un dataset
#OJO se deben poner las direcciones donde tenga el archivo en su equipo
library(readxl)
COPUSD_prices <- read_excel("probabilidadyestadistica/ProyectoProba/COPUSD prices.xlsx")
View(COPUSD_prices)


library(readxl)
brent <- read_excel("probabilidadyestadistica/ProyectoProba/COPUSD prices.xlsx",
                            sheet = "brent", col_types = c("date",
                                                           "numeric"))
View(brent)


library(readxl)
Coffee <- read_excel("probabilidadyestadistica/ProyectoProba/COPUSD prices.xlsx",
                    sheet = "Coffe", col_types = c("date",
                                                   "numeric"))
View(Coffee)
#Fin de la importación
#############################################################################################################################


#############################################################################################################################
#Histograma

#Es necesario instalar install.packages("dplyr", dependencies = T)

library("dplyr")
hist(COPUSD_prices %>% pull (Close), main ="Histograma precios del dolar", xlab = "Precio de cierre", ylab = "Frecuencia",col = "darkgreen")
hist(brent %>% pull (Close), main ="Histograma precios del petroleo Brent", xlab = "Precio de cierre (en dolares)", ylab = "Frecuencia",col = "darkgrey")
hist(Coffee %>% pull (Close), main ="Histograma precios del Cafe", xlab = "Precio de cierre (en dolares)", ylab = "Frecuencia",col = 623)


#############################################################################################################################
#Diagrama de Caja
COPUSD <- COPUSD_prices %>% pull (Close)
Coffee <- coffee %>% pull (Close)
brent <- brent %>% pull (Close)

preciosdol = boxplot(main = 'Precios Dolar', xlab = '', ylab = 'Precio de Cierre'  , COPUSD$Close, col = "darkgreen", horizontal = TRUE)
coffeee = boxplot(Coffee$Close,main = 'Precios Café', xlab = '', ylab = 'Precio de Cierre', col = 623)
brentt = boxplot(brent$Close,main = 'Precios Brent', xlab = '', ylab = 'Precio de Cierre', col="darkgrey")


#############################################################################################################################
#Grafico de linea
COPUSD <- COPUSD_prices # %>% (Close)
Coffee <- coffee # %>% (Close)
brent <- brent # %>% (Close)

preciosdol = plot(COPUSD, type="l", main="Precios del dolar", xlab = "Fecha", ylab = "Precio de cierre", col="darkgreen")
coffeee = plot(Coffee, type="l", main="Precios del cafe", xlab = "Fecha", ylab = "Precio de cierre", col=623)
brentt = plot(brent, type="l", main="Precios del petroleo", xlab = "Fecha", ylab = "Precio de cierre", col="darkgrey")


#############################################################################################################################
