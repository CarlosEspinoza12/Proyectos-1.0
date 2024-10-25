

# Ley de los grandes numeros ----------------------------------------------

# En este primer ejercicio pondremos a prueba la ley de los grandes numeros
# Tenemos N numeros aleatorios donde la media es 0 y se distribuye
# su desviacion estandar es 1
# Ahora, sabemos que la E(x) = 68,2%


# COMENZAMOS

# PRIMER PASO

# rnorm()

# Esta funcion genera numeros aleatorios
# La cantidad de veces que seleccionamos dentro
# Por lo que tenemos un vector con 100 numeros aleatorios con media 0 y desv estandar 1


# SEGUNDO PASO

# for(i in 1:10 )

# De manera simple i tomara los valores del vector de la derecha en este caso de 1 a 10
# Eso es iterar
# Utilizaremos for para 
# i se iterara en el vector 1:10



# TERCER PASO

# if()

# UTILIZAREMOS if para generar valores entre -1 y 1
# estableceremos una condicion para ello

# CUARTO PASO

# Para aplicar el if y el for correctamente
# crearemmos un contador que esto nos permitira
# repetir el codigo una cierta cantidad de veces

# la variable contador tendra la cantidad de numeros generaros
# de manera aleatoria que cumplan con la condicion -1 y 1
# Eso significa que podremos generar % de probabilidad

contador <- 0 

for(i in rnorm(10000)){
  if(i > -1 & i < 1){
    contador <- contador + 1
  }
  
}

contador /10000


## PRACTICA 2


#Analisis de estados financieros -----------------------------------------
  
  #Datos
  ingresos <- c(14574.49, 7606.46, 8611.41, 9175.41, 8058.65, 8105.44, 11496.28, 9766.09, 10305.32, 14379.96, 10713.97, 15433.50)
gastos <- c(12051.82, 5695.07, 12319.20, 12089.72, 8658.57, 840.20, 3285.73, 5821.12, 6976.93, 16618.61, 10054.37, 3803.96)


# Instrucciones: 

# Construir un estado financiero con 30% de impuestos
# Analizar categorias de rendimeinto de la empresa
# con mejor mes, peor mes, meses buenos, meses malos


# Formato:

# Los resultado tienen que estar en % sin puntos desimales
# y en unidades de 1000k
# El impuesto puede ser negativo

#Respuesta

beneficio <- ingresos-gastos
round(beneficio,2)

impuestos <- beneficio*0.3
round(impuestos,2)


beneficio_neto <- beneficio-impuestos
round(beneficio_neto,2)

promedio <- mean(beneficio_neto)
round(promedio,2)

# Calcularemos el marguen de utilidad en porsentaje


marguen_utilidad <- round(beneficio_neto/ingresos,2)*100
marguen_utilidad

# Para calcular los meses buenos definiremos los meses que
# son mayor al promedio

promedio
beneficio_neto

# Hay que comparar cada mes con el promedio de los meses

rendimiento_positivo <- beneficio_neto > promedio
rendimiento_positivo

rendimiento_negativo <- !rendimiento_positivo
rendimiento_negativo

# Calcularemos cual es el mejor mes de beneficio neto
# Recordar que ">", "<" y "==" , son signos de comparacion

mejor_mes <- beneficio_neto == max(beneficio_neto)
mejor_mes

# Calcularemos cual es el peor mes de beneficio neto
peor_mes <- beneficio_neto == min(beneficio_neto)
peor_mes

# Tendremos que presentar los resultados de manera formal
# En una matrix, en este caso

# Dejaremos en unidades de 1000k

ingresos_1000 <- round(ingresos/1000)
gastos_1000 <- round(gastos/1000)
beneficio_1000 <- round(beneficio/1000)
beneficio_neto_1000 <- round(beneficio_neto/1000)
impuestos_1000 <- round(impuestos/1000)

ingresos_1000
gastos_1000
beneficio_1000
impuestos_1000
beneficio_neto_1000


# OUT PUT

m <- rbind(ingresos_1000,
           gastos_1000,
           beneficio_1000,
           impuestos_1000,
           beneficio_neto_1000,
           marguen_utilidad,
           rendimiento_positivo,
           rendimiento_negativo,
           mejor_mes,
           peor_mes)
m

# Agregando nombre a los meses

meses <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", 
           "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")


colnames(m) <- meses

m


#Practicando filter, select y indexacion ---------------------------------------------


# utilizando el select seleccionaremos las filas las cuales
# Decidiremos trabajar

# mtcars es una base de datos de R

data(mtcars)

# Instalando paquete con select y filter
library(dplyr)

# Utilizando el select seleccionaremos las filas las cuales
# Decidiremos trabajar

seleccionados <- mtcars %>%select(disp, drat, wt)
head(seleccionados)
# Ahora utilizando filter filtraremos por disp mayores a 150

filtro <- seleccionados %>% filter(disp > 150)
head(filtro)

# Utilizaremos ambas funciones al mismo tiempo

seleccionado_filtro <- mtcars %>% 
  select(disp, drat, wt) %>%
  filter(disp > 200)

head(seleccionado_filtro)



# Usando filter con funciones
# Filtramos las columnas por el maximo de disp

filtro_maximo <- mtcars %>% filter(disp == max(disp) )
filtro_maximo


# Ahora practicaremos la indexacion basica

data("mtcars")

# Seleccionaddo columanas expecificas
mtcars$mpg

# Seleccionando fila 1 y todas las columnas
mtcars[1,]

# Seleccionando columna 2 t todas la filas
mtcars[,2]

# Seleccionamos un valor en expecifico por fila y columna

mtcars[1,2]
