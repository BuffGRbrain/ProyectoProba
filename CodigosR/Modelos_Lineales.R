############################################################
### Modelo de regresión lineal para Brent(x) y el dolar(y)
x = brent$Close
y = COPUSD$Close

mod_brent = lm(y~x)
summary(mod_brent)

plot(x,y, xlab = "Precio Brent", ylab = "Precio Dolar")

abline(mod_brent, col = 2, lwd = 3)

# B0 = 4019.1269 Intercepto
# B1 = -19.8993 Pendiente
# Correlación: 0.6631 ed 66.31% Podemos decir que el precio del dolar se ve muy influenciado por el precio del petroleo brent que exporta colombia
# Pruebas de hipotesis

###Intervalos de confianza y prediccion

#Intervalo de confianza del 90% cuando el precio del brent(x) es igual a 50
x_n = data.frame(x=50)
predict(mod_brent, x_n, level=0.9, interval="confidence")
# IC = [3008.497, 3039.823]

#Intervalo de prediccion del 90% cuando el precio del brent(x) es igual a 50
x_n = data.frame(x=50)
predict(mod_brent, x_n, level=0.9, interval="prediction")
#IP = [2394.5, 3653.82]


########################################################################################################################################################################################################
xb = mean(x)
yb= mean(y)

Sxx = sum((x-xb)^2)
Sxy = sum((x-xb)*(y-yb))
Syy = sum((y-yb)^2)

plot(x,y,main = 'ScatterPlot' )

B1 = Sxy/Sxx # pendiente de la recta
B0 = yb - B1*xb #ptos de corte con observaciones

#Prueba de hipótesis para E[Y] para saber como es el error de estimación Pendiente
#Prueba de hippótesis para probar B1 = 0
#H0: B1 = 0
#Ha: B1 != 0

n = length(x)
B1_0 = 0
SSE = Syy - B1*Sxy

#Desviación estandard
S = sqrt(SSE/(n-2))
S_1 = sqrt(S)
c11 = 1/Sxx

T = (B1 - B1_0/(S*sqrt(c11))) # Estadístico de prueba

alpha = 0.05

T_alpha = qt(alpha/2, lower.tail =  FALSE, df = n-2) # para el valor de Talfa
#luego T cae en la región de rechazo, es decir B1 es distinto de 0
valorp = pt(T, lower.tail =  FALSE, df = n-2) #pt es para valor p # Revisar pues no se si esta bien

#Intervalo de confianza para B1
#confint lifehack
limsup = B1+T_alpha *( S_1 * sqrt(c11))
liminf = B1-T_alpha *( S_1 * sqrt(c11))
#Luego concluimos que B1 tiene un valor negativo

############################################################################################################################################################################################################################
#####Regresion lineal café

x_Coffe = Coffee$Close
y_Coffe = COPUSD$Close
mod_coffee = lm(y~x)

mod_coffee = lm(y~x)
summary(mod_coffee)

plot(x_Coffe,y_Coffe, xlab = "Precio Brent", ylab = "Precio Dolar")

abline(mod_coffee, col = 2, lwd = 3)

# B0 = 4019.1269 Intercepto
# B1 = -19.8993 Pendiente
# Correlación: 0.6631 ed 66.31% Podemos decir que el precio del dolar se ve muy influenciado por el precio del petroleo brent que exporta colombia
# Pruebas de hipotesis

###Intervalos de confianza y prediccion

#Intervalo de confianza del 90% cuando el precio del brent(x) es igual a 50
x_Coffe_n = data.frame(x_Coffe=50)
predict(mod_coffee, x_Coffe_n, level=0.9, interval="confidence")
# IC = [3008.497, 3039.823]

#Intervalo de prediccion del 90% cuando el precio del brent(x) es igual a 50
x_n = data.frame(x=50)
predict(mod_coffee, x_n, level=0.9, interval="prediction")
#IP = [2394.5, 3653.82]


############################################################################################################################################################################################################################
x_Coffeb = mean(x)
y_Coffeb= mean(y)

Sxx = sum((x_Coffe-x_Coffeb)^2)
Sxy = sum((x_Coffe-x_Coffeb)*(y_Coffe-y_Coffeb))
Syy = sum((y_Coffe-y_Coffeb)^2)

plot(x_Coffe,y_Coffe,main = 'ScatterPlot' )

B1 = Sxy/Sxx # pendiente de la recta
B0 = y_Coffeb - B1*x_Coffeb #ptos de corte con observaciones

#Prueba de hipótesis para E[Y] para saber como es el error de estimación Pendiente
#Prueba de hippótesis para probar B1 = 0
#H0: B1 = 0
#Ha: B1 != 0

n = length(x_Coffe)
B1_0 = 0
SSE = Syy - B1*Sxy

#Desviación estandard
S = sqrt(SSE/(n-2))
S_1 = sqrt(S)
c11 = 1/Sxx

T = (B1 - B1_0/(S*sqrt(c11))) # Estadístico de prueba

alpha = 0.05

T_alpha = qt(alpha/2, lower.tail =  FALSE, df = n-2) # para el valor de Talfa
#luego T cae en la región de rechazo, es decir B1 es distinto de 0
valorp = pt(T, lower.tail =  FALSE, df = n-2) #pt es para valor p # Revisar pues no se si esta bien

#Intervalo de confianza para B1
#confint lifehack
limsup = B1+T_alpha *( S_1 * sqrt(c11))
liminf = B1-T_alpha *( S_1 * sqrt(c11))
#Luego concluimos que B1 tiene un valor negativo
