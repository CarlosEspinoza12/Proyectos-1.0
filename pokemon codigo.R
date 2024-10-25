

# analisis pokemon --------------------------------------------------------



library(readr)
pokemon <- read_csv("pokemon.csv")


nombres <- c(
  "habilidades", "contra_insecto", "contra_siniestro", "contra_dragón", 
  "contra_eléctrico", "contra_hada", "contra_lucha", "contra_fuego", 
  "contra_volador", "contra_fantasma", "contra_planta", "contra_tierra", 
  "contra_hielo", "contra_normal", "contra_veneno", "contra_psíquico", 
  "contra_roca", "contra_acero", "contra_agua", "ataque", 
  "pasos_para_eclosionar", "felicidad_base", "total_base", "tasa_de_captura", 
  "clasificación", "defensa", "crecimiento_de_experiencia", "altura_m", "ps", 
  "nombre_japonés", "nombre", "porcentaje_macho", "número_pokedex", 
  "ataque_especial", "defensa_especial", "velocidad", "tipo1", "tipo2", 
  "peso_kg", "generación", "es_legendario"
)

colnames(pokemon) <- nombres

View(pokemon)


# Para hacer que los datos tengan alguna medida más simple
# juntaremos la ventaja de tipos y crearemos una medida para ver que tanta ventaja
# de tipo tiene con respecto al resto


# En segundo analizaremos el poder de ataque y defensa de manera general
# En pokemon usualmenrte los usuarios se especializan en uno de los ataques
# Siendo este ataque especial y ataque Por lo que se considerara el valor más alto de los dos
# En cuanto a la defensa analizaremos el conjunto de los dos




library(tidyr)
library(dplyr)


ventaja_tipo <- pokemon %>%
  select(2:19)
  
# Utilizando rowsum sumaremos las filas

ventaja <- rowSums(ventaja_tipo,na.rm = TRUE) 

# Agregamos la variable ventaja de nos dara una estimacion de una ventaja sobre
# El resto

pokemon$ventaja <- ventaja

pokemon


# Ahora analizaremos el ataque fisico y al ataque especial
# Dejaremos el más alto de los dos
# y sumaremos las dos defensas y lo llamaremos resistencia_ataques

pokemon <- pokemon %>%
  mutate(especialidad_ataque = pmax(ataque_especial,ataque)) %>%
  mutate(resistencia_ataques = defensa + defensa_especial)

pokemon


# Seleccionaremos las columnas de interes
# nombre,ventaja, especialidad_ataque, resistencia_ataques, velocidad

datos_interes <- pokemon %>%
  select(nombre,ventaja, especialidad_ataque, resistencia_ataques, velocidad, ps)


datos_interes

# Ahora analizaremos los pokemon que tienen estadisticas destacables
# Para eso estimaremos el promedio de cada dato numerico y destacaremos
# los que esten sobre este


vector <- numeric(5)
vector

for (i in 2:6){
  vector[i - 1] <- mean(datos_interes[[i]])
}

vector



# Ahora con todos los datos podemos empezar a sacar concluciones

# Primero analizaremos los datos para ver la distribucion de habilidades
# comenzaremos con los ofensivos
library(ggplot2)

ggplot(datos_interes, aes(x=velocidad, y = especialidad_ataque)) +
  geom_point() + 
  geom_smooth(method = "lm")


ggplot(datos_interes,aes(x = ventaja, y = especialidad_ataque)) +
  geom_point() + 
  geom_smooth(method = "lm")

# Analizando los graficos



