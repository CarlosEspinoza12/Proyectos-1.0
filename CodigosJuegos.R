
# Analisis de las ventas de PlayStation  ---------------------------------------------------


setwd("C:/Users/carlo/OneDrive/Escritorio/Base de datos/Video juegos")
library(readr)
vgsales <- read_csv("vgsales.csv")
View(vgsales)

nuevos_nombres <- c("Posicion", "Nombre", "Plataforma", "Año", "Genero", "Distribuidor", 
                    "Ventas_USA", "Ventas_EUROPA", "Ventas_JAPON", "Ventas_Otras", "Ventas_Globales")

colnames(vgsales) <- nuevos_nombres
head(vgsales)

str(vgsales)


vgsales$Plataforma <- factor(vgsales$Plataforma)
vgsales$Distribuidor <- factor(vgsales$Distribuidor) 


# Después de dejar los datos por categoria
# Veremos cuantas ventas totales se relizaron en USA, Europa y en japon
# En proporcion con el total

# Para esto filtraremos, veremos cuantas ventas realizadas
# PS3 y PS4 por separado 


library(dplyr)

filtro_PSX <- vgsales %>%
  filter(Plataforma %in% c("PS2","PS3","PS4"))

filtro_PSX  

# Total de ventas en millones
total_ventas_PSX <- sum(filtro_PSX$Ventas_Globales)
total_ventas_PSX


# Total de ventas en millones PS2

filtro_por_pais <- filtro_PSX %>%
  select(Ventas_USA,Ventas_EUROPA,Ventas_JAPON,Ventas_Otras,Ventas_Globales)

filtro_por_pais

# Pasos para un ciclo for

# Primero creamos vector numerico que tenga la misma proporcion que
# Los resultados que queremos en este caso 4
# numeric() <- reultados numericos
# ncol () <- numero de columnas
# Se crea un vector con n columnas almacenado en nombre_sumas

sumas <- numeric(ncol(filtro_por_pais))
sumas

# Creamos un ciclo for que va desde 1 a 4, tomando cada valor
# Se almacena en cada columna del vector 
# sumamos cada dato de manera vertical
# Queda todo en nombre_sumas

for(i in 1:5){
  sumas[i] <-  sum(filtro_por_pais[,i])
}

sumas

# Como extra agregaremos los nombres a los resultados
agregando_nombres <- c("Ventas_USA", "Ventas_EUROPA", "Ventas_JAPON", "Ventas_Otras", "Ventas_Globales")


names(sumas)<- agregando_nombres
sumas
# Creamos un ciclo for para agregar el posentaje correspondiente de las ventas totales

porsentaje_total <- numeric(5)
porsentaje_total

for(i in 1:5){
  porsentaje_total[i] <- sumas[i]/sumas[5]
}
# Agregaremos un porsentaje de las ventas totales a cada uno

Venta_totales <- round(sumas,2)
porsentaje <- round(porsentaje_total,2)

rbind(Venta_totales, porsentaje)

# buscaremos ahora cuales son los juegos más vendidos de playStation

filtro_PSX

summary(filtro_PSX$Ventas_Globales)

# Para asegurarnos que estan los datos de manera desendente en ventas globales

filtro_descendente <- filtro_PSX %>%
  arrange(desc(Ventas_Globales))
  
# Filtraremos los 10 juegos más vendidos 

filtro_juegos <- filtro_descendente %>%
  filter(Ventas_Globales > 10) %>%
  head(10)


# Tenemos los 10 juegos más vendidos 

filtro_juegos

# Ahora que pasa si queremos ver la probabilidad que un comprador
# Quiera un tipo de genero en expecifico en play station


# Utilizaremos variables categoricas asignando una probabilidad a cada uno
 conteo_genero <- filtro_PSX %>%
   count(Genero)
 
#Tenemos la cantidad de veces que existe un genero 
conteo_genero



# Podemos hacer esto de otra forma con group_by 

#library(tidyr)
library(magrittr)
library(ggplot2)

filtro_PSX %>% View()
# Ambos son lo mismo
filtro_PSX %>% group_by(Plataforma) %>% summarise(conteo = n())
filtro_PSX %>% group_by(Plataforma) %>% tally() 

# Estamos agrupando por categoria

# Ahora podremos realizar un calculo y este que dependa de
# Cada grupo de categoria
# Por ejemplo mean, sum(), var(), etc

filtro_PSX %>%
  group_by(Plataforma) %>%
  summarise(promedio = mean(Ventas_Globales))


generacion <- filtro_PSX %>%
  group_by(Plataforma) %>%
  summarise(sumas = sum(Ventas_Globales))

generacion

# Podemos asignar una probabilidad a cada uno para esto tenemos que sacar el total



total <- sum(conteo_genero$n)
total
# Ahora usaremos un ciclo for para asignar una probabilidad a cada uno

probabilidad_genero <- numeric(nrow(conteo_genero))
probabilidad_genero



for(i in 1:nrow(conteo_genero)){
  probabilidad_genero[i] <- conteo_genero$n[i]/total
  }

# Ahora agregaremos los nombres de nuevo por cada genero

names(probabilidad_genero) <- c("Acción", "Aventura", "Lucha", "Misceláneo", "Plataforma", 
                               "Rompecabezas", "Carreras", "Rol", "Disparos", "Simulación", 
                               "Deportes", "Estrategia")

# redondeamos las probabilidades
# Conocemos la distribucion de cantidad de juegos por genero

distribucion_generos <- round(probabilidad_genero,2)


# Tenemos la distribucion de juegos por genero
distribucion_generos
# Tenemos las ventas de juegos de play station por pais
sumas


# Crear un gráfico de barras apiladas para mostrar las ventas por región para PlayStation


library(ggplot2)

# Convertiremos los datos en un data frame

 m <- data.frame(region = names(sumas), ventas = as.numeric(sumas))
 
 filas <- 1:4
 columnas <- 1:2
 
 m <- m[filas,columnas]
 m
 
ggplot(data = m, aes(x = region , y = ventas, fill = region )) +
  geom_bar(stat = "identity") +
  labs(title = "Ventas por pais",
       x = "Pais",
       y = "Ventas en millones") + 
  theme(plot.title = element_text(hjust = 0.5))

# Ahora veremos la distribucion de ingresos de juegos dependiendo de la
# Generacion de play


generacion


ggplot(data = generacion, aes(x = Plataforma, y = sumas, fill = Plataforma)) +
  geom_bar(stat = "identity") +
  labs(title = "Ventas por generacion de play",
       x = "Generacion",
       y = "Ventas en millones") +
  theme(plot.title = element_text(hjust = 0.5))



filtro_PSX <- as.data.frame(filtro_PSX)

filtro_PSX$Año <- as.numeric(filtro_PSX$Año)

max(filtro_PSX$Año, na.rm = TRUE)
min(filtro_PSX$Año, na.rm = TRUE)


# Ahora podemos analisar cuanto es la diferencia de ventas
# Por plataforma para ver el exito de cada juego por plataforma
# Primero gta5 

# filtraremos el juego gta5

library(tidyr)
library(magrittr)
library(ggplot2)
library(dplyr)

# Ahora filtraremos en que plataforma tuvo mas exito gta5

filtro_gta5 <- vgsales %>%
  filter(Nombre == "Grand Theft Auto V")

filtro_gta5


ggplot(filtro_gta5, aes(x = Plataforma, y = Ventas_Globales, fill = Ventas_Globales)) +
  geom_bar(stat = "identity") +
  labs(title = "Ventas por plataforma",
       x = "Ventas totales",
       y = "Consola") +
  theme(plot.title = element_text(hjust = 0.5))
  
  


