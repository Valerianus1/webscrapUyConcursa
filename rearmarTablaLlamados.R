# Leer los archivos txt y reconstruir la tabla de llamados 
# La base de datos tambien incluye informacion sobre el organismo que realiza el llamado, que no figura en la tabla
# recupero esa informacion que aparece en la mayoria de los titulos de los llamados y para aquellos que no pude resolver
# sustituyo manualmente fijandome en la pagina del llamado

library(dplyr)
library(rvest)

setwd("scraping") # Carpeta con los archivos de texto

files <- vector(mode="list", length = 168L) # cantidad de paginas de la tabla de llamados de UyConcursa, equivalente al total de archivos txt

for (i in 1:length(files)) files[[i]] <- read_html(paste0(i, ".txt"), encoding = "UTF-8")

rows <-  files[[1]] %>% html_nodes("tr") %>% html_text()
rows <- rows[-1] # el primer elemento contiene rotulos de la tabla y se descarta
rows <- trimws(rows, which="both")
for (i in 2:length(files)) {
  rows2 <- files[[i]] %>% html_nodes("tr") %>% html_text()
  rows2 <- rows2[-1]
  rows2 <- trimws(rows2, which="both")
  rows <- c(rows, rows2)
}

data <- data.frame(id=integer(0), codigo=character(0), titulo=character(0), tipoTarea=character(0), fechaInicio=character(0), fechaFin=character(0), estado=character(0))
for (i in 1:length(files)) {
  for (j in 1:str_count(as.character(files[[i]]), fixed("ContainerRow"))) {
    row <- files[[i]] %>% html_node(paste0("#Grid1ContainerRow_", str_pad(j, 4, "left", "0"))) %>% html_children() %>% html_text()
    data <-  data %>% add_row(id=as.integer(trimws(row[1], which="left")), codigo=row[2], titulo=row[4], tipoTarea=row[5], fechaInicio=row[6], fechaFin=row[7], estado=row[8])
  }
}

# agregar datos de organismo que hace el llamado
data$inciso <- ifelse(str_detect(data$titulo, "- Universidad de la República|UNAEJU"), "UdelaR", ifelse(str_detect(data$titulo, "- Administración de las Obras Sanitarias del Estado|- OSE"), "OSE", ifelse(str_detect(data$titulo, "- Administración de los Ferrocarriles del Estado|- AFE"), "AFE", 
                ifelse(str_detect(data$titulo, "- Administración de Servicios de Salud del Estado"), "ASSE", ifelse(str_detect(data$titulo, "- Administración Nacional de Combustibles, Alcohol y Portland|- ANCAP"), "ANCAP", ifelse(str_detect(data$titulo, "- Agencia Nacional de Vivienda|- ANP|- Agencia Nac. de Vivienda"), "ANP",
                ifelse(str_detect(data$titulo, "- Administración Nacional de Correos|- Adm.Nac.Correos"), "ANC", ifelse(str_detect(data$titulo, "- Administración Nacional de Educación Pública|- ANEP"), "ANEP", ifelse(str_detect(data$titulo, "- Administración Nacional de Puertos|- ANP"), "ANP",
                ifelse(str_detect(data$titulo, "- Administración Nacional de Telecomunicaciones|- ANTEL"), "ANTEL", ifelse(str_detect(data$titulo, "- Administración Nacional de Usinas y Trasmisiones Eléctricas|- UTE"), "UTE", ifelse(str_detect(data$titulo, "- Banco Central del Uruguay|- BCU"), "BCU",
                ifelse(str_detect(data$titulo, "- Banco de la República Oriental del Uruguay|- BROU"), "BROU", ifelse(str_detect(data$titulo, "- Banco de Previsión Social|- BPS"), "BPS", ifelse(str_detect(data$titulo, "- Banco de Seguros del Estado|- BSE"), "BSE", ifelse(str_detect(data$titulo, "- Banco Hipotecario del Uruguay|- BHU"), "BHU", 
                ifelse(str_detect(data$titulo, "- Comisión Nacional Honoraria de Zoonosis"), "Zoonosis", ifelse(str_detect(data$titulo, "- Fiscalía General de la Nación"), "Fiscalia General de la Nacion", ifelse(str_detect(data$titulo, "- INISA|- inisa"), "INISA", ifelse(str_detect(data$titulo, "- Instituto del Niño y Adolescente del Uruguay|- INAU"), "INAU", 
                ifelse(str_detect(data$titulo, "- Instituto Nacional de Colonización|- Inst.Nac.Colonización"), "INC", ifelse(str_detect(data$titulo, "- INUMET"), "INUMET", ifelse(str_detect(data$titulo, "- Ministerio de Ambiente"), "MA", ifelse(str_detect(data$titulo, "-  MDN|- MDN|- Control Integrado del Puente|- Ministerio de Defensa Nacional"), "MDN",
                ifelse(str_detect(data$titulo, "- Ministerio de Desarrollo Social|- mides|- MIDES|-  MIDES|- MIDES"), "MIDES", ifelse(str_detect(data$titulo, "-  MEF|-MEF|- MEF|- Ministerio de Economía y Finanzas|- Mrio. de Economía y Finanzas|MEF - CGN|MEF - DGI|MEF - DGC|- MEF|-  MEF|para MEF|para el MEF|para DGI|– MEF|–  MEF|Ministerio de Economía y Finanzas"), "MEF", ifelse(str_detect(data$titulo, "-  MEC|- MEC|- Ministerio de Educación y Cultura|- MEC|-  Ministerio de Educación y Cultura|- Ministerio de Educación|–  Ministerio de Educación y Cultura|– MEC|– Ministerio de Educación y Cultura"), "MEC", 
                ifelse(str_detect(data$titulo, "-  MGAP|- MGAP|- Ministerio de Ganadería, Agricultura y Pesca|- MGAP|MGAP.|para el MGAP"), "MGAP", ifelse(str_detect(data$titulo, "-MIEM|-  MIEM|- MIEM|- Ministerio de Industria, Energía y Minería|- Ministerio de Industria, Energía  y Minería|- Ministerio de Industría|MIEM"), "MIEM", ifelse(str_detect(data$titulo, "MRREE|- Ministerio de Relaciones Exteriores"), "MRREE", ifelse(str_detect(data$titulo, "- Ministerio de Salud Pública|- MSP|-  MSP|- Ministerio de Salud Pública|– Ministerio de Salud Pública"), "MSP", 
                ifelse(str_detect(data$titulo, "- Ministerio de Trabajo y Seguridad Social|-  MTSS|- MTSS|- MTSS|-MTSS|- Ministerio de Trabajo y Seguridad Social|MTSS - DGSE|del MTSS|– MTSS|– Ministerio de Trabajo y Seguridad Social|. MTSS"), "MTSS", ifelse(str_detect(data$titulo, "- MTOP|.MTOP"), "MTOP", ifelse(str_detect(data$titulo, "-  MT|- MT|- MINTUR|- Ministerio de Turismo|- MTyD|MTyD|- Ministerio de Turismo y Deporte|– Ministerio de Turismo y Deporte"), "MT", ifelse(str_detect(data$titulo, "-  MVOTMA|-MVOTMA|- MVOTMA|- Ministerio de Vivienda|. MVOTMA|.MVOTMA|DI.N.O.T.|DINOT"), "MVOTMA", 
                ifelse(str_detect(data$titulo, "- Ministerio del Interior|- MI|- Min.Interior|- MINTERIOR|- MInterior|- Penitenciario|- INR|Instituto Nacional de Rehabilitación|- I.N.R.|- INR.|en el I.N.R."), "MI", ifelse(str_detect(data$titulo, "- Intendencia|Int."), "Intendencia", 
                ifelse(str_detect(data$titulo, "- Poder Ejecutivo|Registro de Aspirantes \\(|Registro de Aspirantes -"), "Poder Ejecutivo", ifelse(str_detect(data$titulo, "- Poder Judicial"), "Poder Judicial", ifelse(str_detect(data$titulo, "- Poder Legislativo"), "Poder Legislativo",
                ifelse(str_detect(data$titulo, "Presidencia|-  Presidencia|- Presidencia|- ACCE| para ACCE|- ARCE|- ONSC|- INE|- Instituto Nacional de Estadística|- OPP|- OPP|– OPP|ONSC|INE|PRESIDENCIA"), "Presidencia", ifelse(str_detect(data$titulo, "PLUNA"), "PLUNA", ifelse(str_detect(data$titulo, "- Tribunal de Cuentas"), "Tribunal de Cuentas", ifelse(str_detect(data$titulo, "- Universidad Tecnológica|- UTEC"), "UTEC", ifelse(str_detect(data$titulo, "- Tribunal de lo Contencioso Administrativo"), "Tribunal de lo Contencioso Administrativo", ifelse(str_detect(data$titulo, "Consultor|MIDES"), "MIDES", NA))))))))))))))))))))))))))))))))))))))))))))))
data[match(c(7,76,77,94,100,105,158,182,199,11102,11604,11974,12013,12177,12206,12296,12602,12625,12627,12726,12949,13146,13201,13319,13888,13896,14224,14359,14581,15148,15394,15414,16850,17199,17215,17232,17280,17504,18318,18507,18655,18787,19207,19673,20266),data$id),8] <- c("MI","MVOTMA","MVOTMA","MVOTMA","Presidencia","MEF","MEF","MTSS","Presidencia","UdelaR","UdelaR","UdelaR","INAU","UdelaR","UdelaR","UdelaR","MVOTMA","UdelaR","UdelaR","AFE","ANEP","UdelaR","ANCAP","UdelaR","Fiscalia General de la Nacion","UdelaR","UdelaR","MIDES","UdelaR","UdelaR","UdelaR","UdelaR","UdelaR","UdelaR","UTEC","UdelaR","UdelaR","UdelaR","UdelaR","UTEC","UTEC","ASSE","ASSE","UdelaR","UdelaR")

write.csv(data, "tablallamados.csv")
