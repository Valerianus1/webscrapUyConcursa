# Accedo a cada uno de los llamados por su ID, extraigo el texto en su seccion principal y en base a eso armo una tabla separando la informacion en columnas
# Debido a que hay una gran cantidad de datos faltantes (algunos contenidos en los pdf adjuntados en cada llamado) y a que los datos mas antiguos estan organizados
# de forma distinta al resto, los resultados no fueron los deseados

library(tidyverse)
library(rvest)
library(zoo)
library(purrr)

llamados <- read.csv("llamados3.csv")[ , -c(1, 2)]  # Tabla con el listado de llamados laborales y sus IDs

#hacer lista de rotulos de la tabla, guardar todo el texto que se encuentre entre un rotulo y otro (descartando la info que ya tengo o no me interesa)

rotulos <- c("Período de postulación:\t", "Nombre del puesto:\t", "Tipo de Tarea:\t", "Estado:\t", "Tipo de Vínculo:\t", "Tiempo del Contrato:\t", "Retribución:\t", "Carga horaria:\t", "Lugar de desempeño:\t", "Tiempo de contratación:\t",
             "Descripción de Función:\t", "Requisitos Específicos:\t", "Lugar de Recepción de Postulaciones:\t", "Lugar de Recepción de Consultas:\t", 
             "Teléfono de Consultas:\t", "Recepción de Postulaciones a través de Uruguay Concursa:\t", "Organismo y Cantidad de Puestos\t", 
             "Comentario de Interés:\t", "Requisitos excluyentes\t", "Documentación Básica\t", "Incompatibilidades\t", "Otras condiciones de trabajo\t", "Etapas del proceso\t", "Medios de Postulación\t", "Documentos\t")
noimporta <- c("Período de postulación:\t", "Tipo de Tarea:\t", "Estado:\t", "Teléfono de Consultas:\t",  "Comentario de Interés:\t", "Etapas del proceso\t", "Medios de Postulación\t",
               "Requisitos excluyentes\t", "Incompatibilidades\t", "Requisitos Específicos:\t", "Documentación Básica\t", "Documentos\t")

rotulos <- c("Período de postulación:", "Nombre del puesto:", "Tipo de Tarea:", "Estado:", "Tipo de Vínculo:", "Tiempo del Contrato:", "Retribución:", "Carga horaria:", "Lugar de desempeño:", "Tiempo de contratación:",
             "Descripción de Función:", "Requisitos Específicos:", "Lugar de Recepción de Postulaciones:", "Lugar de Recepción de Consultas:", 
             "Teléfono de Consultas:", "Recepción de Postulaciones a través de Uruguay Concursa:", "Organismo y Cantidad de Puestos", 
             "Comentario de Interés:", "Requisitos excluyentes", "Documentación Básica", "Incompatibilidades", "Otras condiciones de trabajo", "Etapas del proceso", "Medios de Postulación", "Documentos")
noimporta <- c("Período de postulación:", "Tipo de Tarea:", "Estado:", "Teléfono de Consultas:",  "Comentario de Interés:", "Etapas del proceso", "Medios de Postulación",
               "Requisitos excluyentes", "Incompatibilidades", "Requisitos Específicos:", "Documentación Básica", "Documentos")

importa <- setdiff(rotulos, noimporta)
namesimporta <- c("Nombre del puesto:", "Tipo de Vínculo:", "Tiempo del Contrato:", "Retribución:", "Carga horaria:", "Lugar de desempeño:", "Tiempo de contratación:", "Descripción de Función:", "Lugar de Recepción de Postulaciones:", "Lugar de Recepción de Consultas:", "Recepción de Postulaciones a través de Uruguay Concursa:", "Organismo y Cantidad de Puestos", "Otras condiciones de trabajo")

varnames <- c("Id" ,"nombre", "vinculo", "retribucion", "cargaHoraria", "tContratacion", "descFuncion", "lugarRecepPost", "lugarRecepCons", "recepPostUyConcursa", "orgCantPuestos", "otrasConds", "tContrato")
#tiempo del contrato y tiempo de contratacion juntados en tContrato

concatenado <- paste(rotulos, collapse='|')
concatenado2 <- gsub(':', "", concatenado)

readText <-  function(id) {
  url <- "https://www.uruguayconcursa.gub.uy/Portal/servlet/com.si.recsel.verllamado?"
  texto <- html_session(paste0(url, id)) %>% html_node("#TABLE1")  %>% html_text()
  posiciones <- vector(mode="list", length=length(rotulos))
  names(posiciones) <- rotulos
  elementos <- vector(mode="character", length=length(posiciones))
  posiciones <- str_locate(texto, rotulos)[, 2]
  names(posiciones) <- rotulos
  posiciones <- posiciones[!is.na(posiciones)]
  acortado <- nchar(texto)-nchar(str_sub(texto, start=posiciones))
  elementos <- str_sub(texto, posiciones+1, acortado+str_locate(str_sub(texto, posiciones+1), concatenado)[, 2]+1)
  elementos <- str_sub(elementos, end=str_locate(elementos, concatenado)[, 1]-1)
  names(elementos) <- names(posiciones)
  elementos <- elementos[!is.na(elementos)]
  values <- vector(mode="character", length=13L)
  names(values) <- namesimporta
  values <-  c(id, ifelse(names(values)%in%names(elementos), elementos[names(values)], "NA"))
  names(values) <- c("Id", namesimporta)
  return(values)
}

ids <- llamados$Id
test <- map_dfr(ids, readText)

datosllamados <- test
datosllamados$tContrato <- NULL
tContrato <- rep(NA, 20136)
tContrato <- ifelse(datosllamados$`Tiempo del Contrato:`!="NA", datosllamados$`Tiempo del Contrato:`, ifelse(datosllamados$`Tiempo de contratación:`!="NA", datosllamados$`Tiempo de contratación:`, NA))

datosllamados <- cbind(datosllamados[, -c(4,8)], tContrato)
datosllamados[datosllamados=="NA"] <- NA

write.csv(datosllamados, "datospaginas.csv", fileEncoding="UTF-8")
