
# * Script03 *
# * Alvaro Tupayachi *
# FR2000 Estadistica forestal - Semestre 2021-I 
#
# ESTFOR 2021/08/10 - AED con R y RStudio
#  Trabajando con los datos de 713 árboles…
# 
#   El script tiene una parte previa y cuatro modulos de trabajo
#    + La parte previa sirve para acondicionar el ambiente de trabajo
#      haciéndolo más productivo: organiza la información bajo el
#      paradigma del proyecto
#    + Los módulos de trabajo se enfocan en el proceso de los datos en R.

#    >>Los equipos deben adecuar su proyecto al juego de datos que poseen
#      los objetivos de análisis que persigan<<

#
##  *Acondicionar el ambiente de trabajo*
#

#   Las siguientes líneas _hasta antes de "(a) leer archivo..."_ deben ser  
#   ejecutadas por única vez al iniciar el proyecto.
#   Escoja un nombre para su proyecto y nombre con él al script que pretende
#   desarrollar. En el presente caso el proyecto es: 'Escribir_Script111' 
#   
#   Rutina para acondicionar folders del proyecto
getwd()                                   # ¿ folder de trabajo actual?
#setwd("~/R-studio/Estfor20202/grG2") #'Working Directory'
#install.packages('ProjectTemplate')       # Instalar paq.'ProjectTemplate'
library('ProjectTemplate')                # Cargar paquete 'ProjectTemplate'
create.project('Script03', merge.strategy = 'allow.non.conflict')     # Crear árbol de folders del proy.

# Manualmente (via el sist. operativo)… colocar file CSV en el subdirectorio 
# /data del proyecto y asignarle nombre apropiado ...
# En adelante utilice las carpetas según la naturaleza de los archivos que  
# use o cree el proyecto (datos: /data, código R (scripts): /src, 
# documentos: docs, gráficos: /graphs, etcétera). 
# Lea el archivo READ.md de cada carpeta creada para saber qué debería
# contener.
# Preserve el file de datos original haciéndolo 'read.only' (solo lectura)
# usando el Explorer (Windows) o Finder (MacOS)

#   
#    a) acceso y descarga de datos del archivo CSV del aula virtual,
#    b) explorar e inspeccionar los datos, 
#    c) acondicionar los datos, 
#    d) hacer resúmenes cuantitativos y gráficos caracterizando las vars.

#
## (a) Leer archivo CSV y crear dataframe adaptando los tipos de variable 
library(readr)
datos_entidad_forestal_Eq1_Est20181110 <- read_csv("~/RStudio/Estfor20211/Proyectos/Script03/data/datos_entidad_forestal_Eq1_Est20181110.csv", col_types = cols(cat = col_skip()))
View(datos_entidad_forestal_Eq1_Est20181110)    # Visualizar nuevo dataframe

# Clonar tabla de datos para preservar los datos originales 
d713 <- datos_entidad_forestal_Eq1_Est20181110  # Nuevo dataframe con nombre corto
d713 <- d713[,-1]
# Revisar tipos de variable del nuevo dataframe
summary(d713)

# Asignar nombres mas convenientes a variables del nuevo dataframe 
names(d713) <- c("DAP","Abm","HT","rect","BFm")


d713$rect <- as.factor(d713$rect) # se convierte a factores la variable rect

# Crear vector de 713 etiquetas textuales para las rectitudes presentes
rectA <- as.character(labels(d713$rect))   

# Según valor de elemento i de vector rect, asignar título al elemento i de 
# vector asociado rectA
for(i in c(1:713)) {                            #revisar valor contenido en  
  if (d713$rect[i]==0) rectA[i]<-"Recto"        #cada registro i (de 1…120) 
  if (d713$rect[i]==1) rectA[i]<-"Semi sinuoso"        #asignar etiqueta correcta
  if (d713$rect[i]==2) rectA[i]<-"Sinuoso"
  if (d713$rect[i]==3) rectA[i]<-"Curvado"

}

# Chequear el vector de etiquetas de los geomorfones
rectA          # ahora vector 'rectA' contiene denominaciones (etiquetas) 
# que corresponden a los tipos

# Intercalar columna de datos de etiqueta de geomorfon junto a columna 
# de valores de geomorfon en el dataframe
dd713 <- cbind(d713[,1:3],rectA,d713[,4:5])   # new dataframe de 10 cols.
head(dd713)                                   # mostrar encabezamiento 

d713$BFm <- as.factor(d713$BFm) # se convierte a factores la variable BFm

# Crear vector de 713 etiquetas textuales para las rectitudes presentes
BFmA <- as.character(labels(d713$BFm))   

# Según valor de elemento i de vector BFm, asignar título al elemento i de 
# vector asociado BFmA
for(i in c(1:713)) {                            #revisar valor contenido en  
  if (d713$BFm[i]==0) BFmA[i]<-"Tablar"        #cada registro i (de 1…120) 
  if (d713$BFm[i]==1) BFmA[i]<-"Ligeramente tablar"        #asignar etiqueta correcta
  if (d713$BFm[i]==2) BFmA[i]<-"Semi circular"
  if (d713$BFm[i]==3) BFmA[i]<-"Circular"
  
}

# Chequear el vector de etiquetas de los geomorfones
BFmA          # ahora vector 'BFmA' contiene denominaciones (etiquetas) 
# que corresponden a los tipos

# Intercalar columna de datos de etiqueta de geomorfon junto a columna 
# de valores de geomorfon en el dataframe
dd713 <- cbind(d713[,1:3],rectA,d713[,4:5])   # new dataframe de 10 cols.
head(dd713) 

## (b) Explorando los datos

# Afirmaciones planteables como hipótesis sobre las variables y el terreno:
#  -los geomorfones comprenden rangos distintos de pendiente%
#  -distintos pisos altitudinales deben presentar distinto nivel de lluvia
#  -según sea mas alto el piso altitudinal debe haber una menor temperatura
#  -los geomorfones de "planura" solo existen en el piso altitudinal + bajo
#  -localidades del piso más elevado se orientan al SO (hacia el poniente)
#  -hay menos arcilla donde la pendiente es alta y hay mayor precipitación
#  -localidades mas bajas deben tener pendientes menores

# Un gráfico múltiple de dispersion XY de variables continuas puede ayudar
dxy713 <- cbind(dd713[,1:3]) # hacer dataframe de var. continuas
pairs(dxy713)                             # plot multipanel de scatterplots 

#...Volver a plotear omitiendo variables xLonG, yLatG 
pairs(dxy120[,1:6])

# ...¿que se puede visualizar?:
#   .relacion fuerte: 'tmpC' ~ 'elevM' (resultado esperable)
#   .relacion leve:  'elevM' ~ 'pendP'  ¡2 localidades aglutinan a lo demas!
#   .otras posibles relaciones: 'pendP'~'ppMM' 'pendP'~'clayP'
#   .¡necesario: revisar valor de 'pendP' de dos locs!  ¡posible descarte!

# Analizar graficamente valores de 'pendP'
plot(dd713$Abm)
plot(dd713$DAP)
plot(dd713$HT)

# ...los "outliers" tienen valores 'pendP' > 150.0, & locN: ±95, ±117
# ... al revisar dataframe se identifica recds: 92 (162.9) & 118 (165.4)

## (c) Acondicionando y explorando los datos 
# Reconstruir dataframe omitiendo recds de las localidades 92 y 118...
dd118a <- 
  rbind(dd120[1:91,],dd120[93:117,],dd120[119:120,]) # intercalar filas ok

# Purgados los registros inconsistentes...
# ...revisar plot multipanel de dispersion XY de las 6 variables continuas
dxy713 <- cbind(dd713[,1:3]) # generar dataframe de vars continuas
pairs(dxy713[,1:3])                       # plot multipanel XY de vars continuas

# Chequear las variables del dataframe logrado
summary(dxy6v)

# ...existen NA's...
dxy6v <- na.omit(dxy6v)   # omitir NA's en lo sucesivo

# Scatterplots de variables que parecen estar relaccionadas
plot(dxy6v$elevM,dxy6v$pendP)

# ...parece que… hasta los 400m, según aumenta elevM las pends son mayores
# Intento de prueba: vectores de valores elevM y pendP para elevaciones < 400

# crear vectores de dimension 118
ElevLT400 <- as.numeric(c(1:118))  
PendCond <- as.numeric(c(1:118))
ElevLT400 <- NaN; PendCond <- NaN  # valor inicial = NaN

# En caso elevacion sea menos de 400 llenar valor a vectores; si no, usar NaN
# NaN es valor-numérico-ausente. (En el ploteo se los ignora)

# Caracterizar los dos vectores obtenidos, de elevacion (m) y pendiente (%)

# ...hay NA's en los vectores 

# Omitir elementos NaN
ElevLT400 <- na.omit(ElevLT400)
PendCond <- na.omit(PendCond)

## (e) Obteniendo información, resultados

# Histogramas - distribución de los valores contenidos
hist(ElevLT400, main = "Distribución de elevaciones", labels = TRUE)
hist(PendCond, main = "Distribución de pendientes", labels = TRUE)

# Diagramas de tallo y hojas de distribuciones
stem(ElevLT400)
stem(PendCond)

# Scatterplot de los pares XY de locs < 400 m (75 ptos)
plot(ElevLT400,PendCond, main = "Nube XY de pares Elev|Pend hasta 400 m s.n.m.", xlab = "Elevación (m)", ylab = "Pendiente (%)")

# Medida de localizacion y de variación de variables continuas... 
" Promedios de las variables continuas para 118 localizaciones válidas "

# txt6proms <- as.character(c(1:6))
# txt6medians <- as.character(c(1:6))
# txt6varnz <- as.character(c(1:6))
# for(j in c(1:6)) {
#   txt6proms[j] 
#   <- paste("promedio de ", names(dxy6v[j]), " = ", mean(dxy6v[,j]))
#   txt6medians[j] 
#   <- paste("mediana de ", names(dxy6v[j]), " = ", median(dxy6v[,j]))
#   txt6varnz[j] 
#   <- paste("varianza de ", names(dxy6v[j]), " = ", var(dxy6v[j]))
# }
# 
# # Visualizar promedios, medianas y varianzas de  variables continuas
# txt6proms
# txt6medians
# txt6varnz

## Fin de Script
