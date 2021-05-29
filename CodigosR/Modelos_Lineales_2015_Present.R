############################################################
### Modelo de regresión lineal para Brent(x) y el dolar(y)
x = BRENT_INT2_ds$Close
y = COPUSD_INT2_ds$Close
n = length(x)
mod_brent = lm(y~x)
summary(mod_brent)

plot(x,y, xlab = "Precio Brent (USD)", ylab = "Precio Dolar (COP)")

abline(mod_brent, col = 2, lwd = 3)

#Datos muestrales (Tomados a partir del modelo)
# B0 = 3794.1930 Intercepto
# B1 = -12.2162 Pendiente
#r^2 = 0.1719
# Correlación: 0.1719 ed 17.19% Podemos decir que el precio del dolar se ve muy
#influenciado por el precio del petroleo brent que exporta colombia
# Pruebas de hipotesis


#Prueba de hippótesis para probar B1 = 0
alpha = 0.05
#H0: B1 = 0
#Ha: B1 != 0
#T = -17.84
T_alpha_medios = qt(alpha/2, lower.tail =  FALSE, df = n-2)
#T_alpha_medios = 1.96
#Dado que |T| > T_alpha_medios, rechazamos H0, por lo tanto, existe una
#Correlación entre el precio del petroleo Brent y el precio del dólar con
#respecto al peso col con un nivel de significación del 0.05.
#vp < 2e-16

###Intervalos de confianza y prediccion
#Intervalo de confianza del 90% cuando el precio del brent(x) es igual a 50
x_n = data.frame(x=50)
predict(mod_brent, x_n, level=0.9, interval="confidence")
# IC = [3168.238, 3198.527]

#Intervalo de prediccion del 90% cuando el precio del brent(x) es igual a 50
x_n = data.frame(x=50)
predict(mod_brent, x_n, level=0.9, interval="prediction")
#IP = [2642.292, 3724.473]


############################################################################################################################################################################################################################
#####Regresion lineal café

x_Coffee = COFFEE_INT2_ds$Close
y_Coffee = COPUSD_INT2_ds$Close
n = length(x_Coffee)
mod_coffee = lm(y_Coffee~x_Coffee)
summary(mod_coffee)

plot(x_Coffee,y_Coffee, xlab = "Precio Coffee (USD)", ylab = "Precio Dolar (COP)")

abline(mod_coffee, col = 2, lwd = 3)

#Datos muestrales (Tomados a partir del modelo)
# B0 = 4395.3425 Intercepto
# B1 = -10.5255 Pendiente
# r^2 = 0.255
# Correlación: Podemos decir que el precio del dolar se ve muy influenciado por el precio del petroleo brent que exporta colombia
# Pruebas de hipotesis

#Prueba de hippótesis para probar B1 = 0
alpha = 0.05
#H0: B1 = 0
#Ha: B1 != 0
#T = -22.94
T_alpha_medios = qt(alpha/2, lower.tail =  FALSE, df = n-2)
#T_alpha_medios = 1.96
#Dado que |T| > T_alpha_medios, rechazamos H0, por lo tanto, existe una
#Correlación entre el precio del Café y el precio del dólar con
#respecto al peso col con un nivel de significación del 0.05.
#vp < 2e-16

###Intervalos de confianza y prediccion

#Intervalo de confianza del 90% cuando el precio del brent(x) es igual a 50
x_Coffee_n = data.frame(x_Coffee=50)
predict(mod_coffee, x_Coffee_n, level=0.9, interval="confidence")
# IC = [3813.456, 3924.675]

#Intervalo de prediccion del 90% cuando el precio del brent(x) es igual a 50
x_Coffee_n = data.frame(x_Coffee=50)
predict(mod_coffee, x_Coffee_n, level=0.9, interval="prediction")
#IP = [3353.191, 4384.941]

############################################################################################################################################################################################################################
