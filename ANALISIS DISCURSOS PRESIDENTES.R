#paquetes
install.packages("tidyverse")
install.packages("tokenizers")
library(tidyverse)
library(tokenizers)
library(ggplot2)
library(readr)

#iniciamos con un texto corto para probar la funcion tokenizers y otras basicas
#Este ejemplo pertenece al comienzo del último discurso sobre el Estado de la Unión de Barack Obama en 2016.
texto <- paste("También entiendo que como es temporada de elecciones, las expectativas para lo que lograremos este año son bajas. Aún así, señor Presidente de la Cámara de Representantes, aprecio el enfoque constructivo que usted y los otros líderes adoptaron a finales del año pasado para aprobar un presupuesto, y hacer permanentes los recortes de impuestos para las familias trabajadoras. Así que espero que este año podamos trabajar juntos en prioridades bipartidistas como la reforma de la justicia penal y ayudar a la gente que está luchando contra la adicción a fármacos de prescripción. Tal vez podamos sorprender de nuevo a los cínicos.")

#ahora vamos a segmentar el texto en palabras individuales con el paquete tokenizers
palabras <- tokenize_words(texto)
#visualizamos
palabras

#podemos visualizar cuantas palabras hay en el texto con la función length
#no podemos usar length(palabras) debido a que 
#La razón por la cual la longitud equivale a 1 es que la función tokenize_words
#devuelve una lista de objetos con una entrada por documento cargado. 
#Nuestro ingreso solo tiene un documento y, por tanto, la lista contiene solo un elemento. 

length(palabras[[1]])
#tenemos 101 palabras en el texto
#podemos ver cuantas veces se repite cada palabra 

#primero organizamos las palabras en una tabla y luego convertimos en un data frame 
tabla <- table(palabras[[1]])
#La funcion data_frame nos convierte en data frame
#palabra es el nombre de la primera columna que lleva los nombres de la tabla 
#recuento es la segunda columna y con as.numeric de tabla podemos sacar el numero
tabla <- data_frame(palabra = names(tabla), recuento = as.numeric(tabla))
tabla

#con la funcion arrange podemos organizar la tabla según la columna de interés
#en este caso, la columna de interés es el recuento 
#La función desc en el segundo argumento indica que queremos 
#clasificar en orden descendiente.
arrange(tabla, desc(recuento))

#ahora analicemos por oraciones 
#la funcion tokenize_sentences 
oraciones <- tokenize_sentences(texto)
oraciones

#Es posible conectar el resultado de la división de oraciones con el de la división por palabras. 
#Si ejecutamos la división de oraciones del párrafo con la función tokenize_words, 
#cada oración es tratada como un único documento y podemos saber cuantas palabras hay en  cada oración 

oraciones_palabras <- tokenize_words(oraciones[[1]])
oraciones_palabras

#con la función sapply podemos obtener las palabras por cada oración 
sapply(oraciones_palabras, length)

#Análisis de los discursos del Estado de la Unión desde 1790 a 2016
base_url <- "https://programminghistorian.org/assets/basic-text-processing-in-r"
url <- sprintf("%s/sotu_text/236.txt", base_url)
texto <- paste(readLines(url), collapse = "\n")
archivos <- sprintf("%s/sotu_text/%03d.txt", base_url, 1:236)
texto <- c()
for (f in archivos) {
  texto <- c(texto, paste(readLines(f), collapse = "\n"))
}
#tenemos una tabla con metadatos sobre cada uno de los discursos del Estado de la Unión. 
#Vamos a cargarla a R:
metadatos <- read_csv(sprintf("%s/%s", base_url, "metadata.csv"))


#miramos el numero de palabras
palabras <- tokenize_words(texto)
sapply(palabras, length)

#¿Existe un patrón temporal sobre la longitud de los discursos? 
#¿Cómo se compara la longitud de los discursos de otros presidentes a los de Franklin D. Roosevelt, Abraham Lincoln y George Washington?
#usamos un grafico para observar 
#eje x el año y eje y el numero de palabras
qplot(metadatos$year, sapply(palabras, length), color = metadatos$sotu_type) + labs(x = "Año", y = "Número de palabras", color = "Modalidad del discurso")
#observamos que los discursos escritos tienen mayor numero de palabras

#hagamos ahora un analisis estilometrico 
#la idea es analizar el estilo de escritura del autor teniendo en cuenta la longitud de las oraciones
oraciones <- tokenize_sentences(texto)

#miramos el numero de palabras en cada oracion 
oraciones_palabras <- sapply(oraciones, tokenize_words)

longitud_oraciones <- list()
for (i in 1:nrow(metadatos)) {
  longitud_oraciones[[i]] <- sapply(oraciones_palabras[[i]], length)
}
media_longitud_oraciones <- sapply(longitud_oraciones, median)
qplot(metadatos$year, media_longitud_oraciones) + geom_smooth() + labs(x = "Año", y = "Longitud media de las oraciones")



