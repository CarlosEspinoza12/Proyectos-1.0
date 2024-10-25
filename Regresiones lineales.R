
# Regreciones lineales pasos ----------------------------------------------



library(datasets)
datos <- data(mtcars)

View(mtcars)

# cambiaremos los nombres de las columnas
colnames(mtcars) <- c("Millas_por_galon", "Numero_de_cilindros", "Desplazamiento",
                      "Potencia", "Relacion_eje_trasero", "Peso", "Tiempo_1_4_milla",
                      "Tipo_motor", "Tipo_transmision", "Numero_de_engranajes", "Numero_de_carburadores")


# Primero para poder entender la estructura de datos y los tipos de datos
# usaremos str() y summary()

str(mtcars)
summary(mtcars)

# Sabemos que pueden existir datos extremos en una base de datos para poder evitar
# que este afecte negativamente la formulacion del modelo tendremos que primero
# visualizar y luego eliminar

# el boxplot es una excelente erramienta para ver estos datos

boxplot(mtcars$Millas_por_galon)

boxplot(mtcars$Millas_por_galon, main="Boxplot de Peso con Puntos", ylab="Peso (en miles de libras)")


stripchart(mtcars$Millas_por_galon, method = "jitter", vertical = TRUE, add = TRUE, pch = 20, col = 'blue')




head(mtcars)

# Ahora sbemos algunos datos de entrada y que la estrutura es numerica

# Continuamos con escoger la relacion entre 2 variables que creemos
# que tienen cierta relacion en este caso
# escogemos peso y millas por galon
# puede ser otras varibales
# hacemos un plot()

plot(mtcars$Peso, mtcars$Millas_por_galon, 
     main = "Relación entre Peso y Millas por Galón",
     xlab = "Peso (en miles de libras)",
     ylab = "Millas por Galón")


# A partir del grafico podemos intuir una relacion negativa
# Para ello utilizaremos la funcion cor() que medira la correlacion 
# Entre dos variables


cor(mtcars$Peso, mtcars$Millas_por_galon)

# Podemos ver una correlacion 0,8 negativa por lo que es verdad
# La correlacion puede ir desde -1 a 1
# La correlacion no indica causalidad ya que este puede estar 
# causado por una tercera variable

# Por lo que al tratar con una variable dependiente tratamos de predecir 
# esta con una independiente que tiene cierta relacion
# pero el problema de eso es que esta puede estar influenciada con otra
# entonces para eliminar el sesgo (la dependiente explcia más o menos de lo que deberia)
# agregaremos otra variable explicativa

# Ahora tenemos que ver un par de cosas antes de continuar

mco1 <- lm(mtcars$Millas_por_galon ~ mtcars$Peso)
summary(mco1)

# Al evaluar el modelo vemos que
# el intercepto es 37,2, la presicion es 1,87 mientras más cercano a cero mejor
# 
# con ambos estadisticamente significativos


# Entonces, el valor t es una medida de cuán bueno es el coeficiente de la variable independiente en comparación con su error estándar.
# El error estándar representa la distancia promedio entre los valores originales y los valores predichos por el modelo.

# Por lo tanto hay dos formas de verlo, cuanto menor sea esta distancia de los errores estandar 
# en comparación con el coeficiente (beta), mejor.
# Así mismo por otro lado, a medida que el coeficiente (beta) aumenta, esta distancia se vuelve menos significativa.
# t puede ser nagativo



# Pr(>|t|) (Valor p)
#
# El valor p mide la probabilidad de que el coeficiente sea igual a cero. 
# Para que sea estadísticamente significativo, el valor p debe ser menor 
# al 10%, 5% o 1%. Dependiendo de esto, el coeficiente será más o menos significativo 
# y se rechazará la hipótesis nula.


# Ahora teniendo todo esto evaluaremos el modelo

# Interpretacion del modelo -----------------------------------------------



mco1 <- lm(mtcars$Millas_por_galon ~ mtcars$Peso)
summary(mco1)



# Esto no ayudara a verificar que los valores predichos sean los más parecidos
# A los datos reales
# y la homocedasticidad (que la varianza entre los datos es igual), 
# por lo tanto


par(mfrow = c(2, 2))
plot(mco1)


# Verificar si existen curvas en el modelo o datos no lineales en el modelo

# Para interpretar el primer gráfico:
# Residuales (errores del modelo) vs Valores Ajustados (valores predichos por el modelo)

# Debemos tener claros dos conceptos: linealidad y homocedasticidad.
# Linealidad: la relación entre la variable dependiente e independiente debe ser lineal.
# Homocedasticidad: la varianza de los residuos debe ser constante a lo largo de todos los valores ajustados.

# En el gráfico, los puntos ideales deben distribuirse de tal forma que la distancia entre ellos
# sea similar, indicando homocedasticidad.

# Para saber si la variable dependiente es lineal respecto a la independiente:
# No debe haber ningún patrón definido en el gráfico. Si hay un patrón, indica una relación no lineal.

# ¿Por qué debemos verificar esto?
# No debe haber un patrón entre los residuos y los valores ajustados.
# La presencia de un patrón sugiere que hay otro tipo de relación no lineal no capturada por el modelo.



# Segundo grafico
# Evaluación del gráfico Q-Q (Quantile-Quantile) de residuos:

# La normalidad de los residuos es un supuesto clave en la regresión lineal.
# Esto es importante para la validez de las inferencias estadísticas, como pruebas de hipótesis y intervalos de confianza
# a la hora de evaluar

# Interpretación del gráfico Q-Q:
# - Eje X (Theoretical Quantiles): Cuantiles teóricos de una distribución normal.
# - Eje Y (Standardized Residuals): Cuantiles observados de los residuos estandarizados.

# Los puntos deben alinearse cerca de la línea diagonal para indicar que los residuos son normalmente distribuidos.
# Desviaciones significativas de la línea diagonal sugieren que los residuos no siguen una distribución normal.

# Evaluación de la normalidad de los residuos:


# Lo mismo de antes pero solo es el grafico Q-Q
qqnorm(resid(mco1))
qqline(resid(mco1))

# Ahora la evaluacion con codigo


# Un valor p alto (por ejemplo, > 0.05) indica que los residuos pueden ser normalmente distribuidos.
# Un valor p bajo (por ejemplo, < 0.05) indica que los residuos no son normalmente distribuidos.
# W varía entre 0 y 1, donde un valor cercano a 1 indica que los datos se asemejan a una distribución normal.
# En este caso, W = 0.94508, lo cual está relativamente cerca de 1.

shapiro.test(resid(mco1))

# En caso de no tener esta distribucion de manera diagonal 
# podremos solucionarlo aplicando logaritmo, raiz cuadraga, inversa
# de esa forma normalizaremos la distribucion

# Podemos hacerlo de esta forma

mtcars$log_millas_por_galon <- log(mtcars$Millas_por_galon)
modelo_log <- lm(log_mpg ~ mtcars$Peso, data = mtcars)
qqnorm(resid(modelo_log))
qqline(resid(modelo_log))
shapiro.test(resid(modelo_log))

# En caso de no encontrar la nomralizacion correcta
# podemos usar rlm() que son menos sencibles a la normalizacion 
# son menos sensibles a estos supuestos 

library(MASS)
modelo_robusto <- rlm(mtcars$Millas_por_galon ~ mtcars$Peso)
summary(modelo_robusto)

# Existen otras formas más avanzadas que necesitan más profundizacion
# Lo nombraremos
# Ajustar un modelo lineal generalizado con una distribución de Poisson
# pueden tener muchos tipos de distribuciones

modelo_glm <- glm(mpg ~ wt, family = poisson(link = "log"), data = mtcars)
summary(modelo_glm)

# También los metodos no parametricos



# Ahora vamos con el tercer grafico - izquierda abajo


# Scale-Location Plot (Spread-Location Plot):
# Este gráfico se utiliza para verificar la homocedasticidad (constancia de la varianza) de los residuos.

# Interpretación del gráfico:
# - Eje X (Fitted Values): Representa los valores ajustados (predichos) por el modelo.
# - Eje Y (sqrt|Standardized Residuals|): Representa la raíz cuadrada de los residuos estandarizados absolutos.

# Qué buscar:
# - Distribución aleatoria y uniforme: A medida que los valores ajustados avanzan en el eje X,
#   los puntos en el eje Y deben estar dispersos de manera uniforme y aleatoria alrededor de la línea horizontal.
# - La línea roja suavizada debe ser horizontal y recta, indicando homocedasticidad.

# Patrones no deseados:
# - Patrón de Abanico (Fanning Out): Si la dispersión de los puntos aumenta a medida que los valores ajustados aumentan,
#   esto sugiere heterocedasticidad (varianza no constante).
# - Patrón de Trompeta (Fanning In): Si la dispersión de los puntos disminuye a medida que los valores ajustados aumentan,
#   esto también sugiere heterocedasticidad.


# Ultimo grafico


# Residuals vs Leverage Plot:
# Este gráfico se usa para identificar observaciones con mucha influencia en el modelo de regresión.

# Interpretación del gráfico:
# - Eje X (Leverage): Muestra cuánto leverage (influencia) tiene cada observación en el modelo.
# - Eje Y (Standardized Residuals): Muestra los residuos (errores) estandarizados de cada observación.

# Qué buscar:
# - Observaciones con alto leverage y grandes residuos: Estas observaciones tienen un gran impacto en el modelo.
# - Líneas de Cook's Distance: Observaciones fuera de estas líneas (valores mayores a 0.5 o 1) son especialmente influyentes.

# Qué hacer si encuentras observaciones influyentes:
# - Revisar los datos para detectar errores o casos especiales.
# - Usar modelos robustos que sean menos sensibles a observaciones influyentes.
# - Transformar los datos para reducir el impacto de las observaciones influyentes.



influence.measures(mco1)




