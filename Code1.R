#############################################################################################################################
#Importando los datos de excel a un dataset
#
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





