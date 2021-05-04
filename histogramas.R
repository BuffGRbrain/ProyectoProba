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
Coffe <- read_excel("C:/Users/USUARIO/Desktop/COPUSD prices.xlsx",
                    sheet = "Coffe", col_types = c("date",
                                                   "numeric"))
View(Coffe)


#Fin de la importación
#############################################################################################################################


#############################################################################################################################
#Histograma

#Es necesario instalar install.packages("dplyr", dependencies = T)

library("dplyr")
hist(COPUSD_prices %>% pull (Close), main ="Histograma precios del d�lar", xlab = "Precio de cierre", ylab = "Frecuencia",col = "darkgreen")
hist(brent %>% pull (Close), main ="Histograma precios del petr�leo Brent", xlab = "Precio de cierre (en d�lares)", ylab = "Frecuencia",col = "darkgrey")
hist(coffee %>% pull (Close), main ="Histograma precios del Caf�", xlab = "Precio de cierre (en d�lares)", ylab = "Frecuencia",col = "darkgrey")

#############################################################################################################################
#Diagrama de Caja

preciosdol = boxplot(COPUSD)
coffeee = boxplot(Coffe)
brentt = boxplot(brent)


#############################################################################################################################
