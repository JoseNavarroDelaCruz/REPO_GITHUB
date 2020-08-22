#paquetes
install.packages("tidyverse")
install.packages("tokenizers")
library(tidyverse)
library(tokenizers)
library(ggplot2)
library(readr)


# Un comentario añadido por Jose
# Un comentario añadido por karen


#iniciamos con un texto corto para probar la funcion tokenizers y otras basicas
#Este ejemplo pertenece al comienzo del ?ltimo discurso sobre el Estado de la Uni?n de Barack Obama en 2016.
texto <- paste("Tambi?n entiendo que como es temporada de elecciones, las expectativas para lo que lograremos este a?o son bajas. A?n as?, se?or Presidente de la C?mara de Representantes, aprecio el enfoque constructivo que usted y los otros l?deres adoptaron a finales del a?o pasado para aprobar un presupuesto, y hacer permanentes los recortes de impuestos para las familias trabajadoras. As? que espero que este a?o podamos trabajar juntos en prioridades bipartidistas como la reforma de la justicia penal y ayudar a la gente que est? luchando contra la adicci?n a f?rmacos de prescripci?n. Tal vez podamos sorprender de nuevo a los c?nicos.")

#ahora vamos a segmentar el texto en palabras individuales con el paquete tokenizers
palabras <- tokenize_words(texto)
#visualizamos
palabras

#podemos visualizar cuantas palabras hay en el texto con la funci?n length
#no podemos usar length(palabras) debido a que 
#La raz?n por la cual la longitud equivale a 1 es que la funci?n tokenize_words
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

#con la funcion arrange podemos organizar la tabla seg?n la columna de inter?s
#en este caso, la columna de inter?s es el recuento 
#La funci?n desc en el segundo argumento indica que queremos 
#clasificar en orden descendiente.
arrange(tabla, desc(recuento))

#ahora analicemos por oraciones 
#la funcion tokenize_sentences 
oraciones <- tokenize_sentences(texto)
oraciones

#Es posible conectar el resultado de la divisi?n de oraciones con el de la divisi?n por palabras. 
#Si ejecutamos la divisi?n de oraciones del p?rrafo con la funci?n tokenize_words, 
#cada oraci?n es tratada como un ?nico documento y podemos saber cuantas palabras hay en  cada oraci?n 

oraciones_palabras <- tokenize_words(oraciones[[1]])
oraciones_palabras

#con la funci?n sapply podemos obtener las palabras por cada oraci?n 
sapply(oraciones_palabras, length)

#An?lisis de los discursos del Estado de la Uni?n desde 1790 a 2016
base_url <- "https://programminghistorian.org/assets/basic-text-processing-in-r"
url <- sprintf("%s/sotu_text/236.txt", base_url)
texto <- paste(readLines(url), collapse = "\n")
archivos <- sprintf("%s/sotu_text/%03d.txt", base_url, 1:236)
texto <- c()
for (f in archivos) {
  texto <- c(texto, paste(readLines(f), collapse = "\n"))
}
#tenemos una tabla con metadatos sobre cada uno de los discursos del Estado de la Uni?n. 
#Vamos a cargarla a R:
metadatos <- read_csv(sprintf("%s/%s", base_url, "metadata.csv"))


#miramos el numero de palabras
palabras <- tokenize_words(texto)
sapply(palabras, length)

#?Existe un patr?n temporal sobre la longitud de los discursos? 
#?C?mo se compara la longitud de los discursos de otros presidentes a los de Franklin D. Roosevelt, Abraham Lincoln y George Washington?
#usamos un grafico para observar 
#eje x el a?o y eje y el numero de palabras
qplot(metadatos$year, sapply(palabras, length), color = metadatos$sotu_type) + labs(x = "A?o", y = "N?mero de palabras", color = "Modalidad del discurso")
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
qplot(metadatos$year, media_longitud_oraciones) + geom_smooth() + labs(x = "A?o", y = "Longitud media de las oraciones")



