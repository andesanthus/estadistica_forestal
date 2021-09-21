
# * Script03 *
# Alvaro Tupayachi
# FR2000 Estadistica forestal - Semestre 2021-I 
# 2021/09/07    

#### Propósito
#
# A. Caracterizar población de tamaño N = 120 constituida de los valores de 
# Temperatura media anual en 120 localidades donde ocurre una especie arbórea. 
# Considera los parámetros principales de: 
#   i. Localización: Media (µ), Mediana (Me), Moda (Mo)
#  ii. Dispersión: Desviación estándar (σ), Desv.media, Raíz media cuadrada 
#     (RMS), Rango. 
# iii. Igualmente, exhibe la forma como se distribuyen sus observaciones.
# 
# B. Extraer en forma aleatoria 120 muestras pequeñas (n=6) y obtener para cada 
#  muestra los estadísticos: media muestral (x̅) y desviación estándar muestral
#  (s). Determinar para cada muestra el IC.95 para µ y evaluar su eficacia en 
#  encerrar entre sus límites (LIC y LSC) el valor µ de la población. 
#  Determinar para cada muestra el IC.95 del valor σ y determinar su eficacia.
#  
# C. Determinar para cada muestra pequeña (n=6) los estadísticos t-calc y χ²-calc
# 
# D. Extraer en forma aleatoria 120 muestras grandes (n=60) y realizar 'B' 
#    aplicándolo a esta nueva 'población' de medias de muestras grandes.
#
# E. Determinar para cada muestra grande (n=60) los estadísticos t-calc y χ²-calc
# 
# F. Comparar gráficamente las distribuciones de las 3 'poblaciones': original,
#    de medias de muestras pequeñas (n=6) y de medias de muestras grandes (n=60)
#    
# G. Emplear prueba de normalidad de Ryan-Joiner u otra equivalente para evaluar 
#    la normalidad de las distribuciones de frecuencia de las tres 'poblaciones'


#### Acondicionar ambiente de trabajo
# 
## Cargar librerías necesarias y/o muy convenientes (preceder con '#' para  
#  paquetes ya instalados)

#install.packages("AdequacyModel")      # Estadísticas descriptivas a la mano 
library(AdequacyModel)

#install.packages("Analyze.stuff")      # Miscelánea de herramientas para 
library(analyze.stuff)                  # análisis de datos en filas y cols

#install.packages("Hmisc")
# "Contains many functions useful for data analysis, high-level graphics,
#  utility operations, functions for computing sample size and power..."
library(Hmisc)

#install.packages('ProjectTemplate')    # Administrar info del proyecto
library('ProjectTemplate')                    

#install.packages("tidyverse")          # Gestionar tablas planas complejas 
#  "dplyr is a grammar of data manipulation, providing 
#   a consistent set of verbs that help you solve common..."
#  Ver por ejemplo: 'https://michaeltoth.me/how-to-filter-in-r-a-detailed-introduction-to-the-dplyr-filter-function.html'
library(dplyr)                                  

# Potente libreria para graficos
library("ggplot2", lib.loc="~/Library/R/4.0/library") 

# Manipulación de Strings
library(stringr)                        

#install.packages("remotes")          # instala funcs desde otras fuentes 


## Definir estructura de folders del proyecto (#'s para saltar repetición)
#
# setwd("~/R-studio/Estfor20211/Proyectos")
# getwd()                                   # ¿'Working Directory' es…?
# ## Configurar el WD agregándole las carpetas necesarias para el proyecto
# install.packages('ProjectTemplate')       # Instale el paquete 'ProjectTemplate'
# library('ProjectTemplate')                # Cargue el paquete 'ProjectTemplate'
# create.project('Script03',                # Se agrega la carpeta del proyecto y
#                merge.strategy = 'allow.non.conflict') # carpetas-hijas
setwd("~/R-studio/Estfor20211/Proyectos/Script03")      # Definir WD


#### Obtener data que se procesará y acondicionar dataframe
#
library(readr)
Descr713inds <- datos_entidad_forestal_Eq1_Est20181110
View(Descr713inds)

# Renombrar variable 'cat', sin afectar nombres restantes 
names(Descr713inds) <- c("ind", names(Descr713inds)[2:6])

# Hacer vector suelto la variable de interés
DAPcm <- as.double(Descr713inds$DAP)

#### Desarrollar objetivos del Script
# 
### Trabajar con la "Población original" (x=gC)
#
## Caracterización de la población  
# 
# 1.- Visualizar una pequeña porción de esta
head(DAPcm)
# 2.- Describirla en distintas formas: número de obs, datos faltantes, valores
# diferentes, GMD (Generalized minimum distance of distribution), centiles: .05 
# .10 .25 .50 .75 .90 .95, 5mín, 5máx, medidas de localización, dispersión y su 
# distribución. Se utilizan acá varias funciones obtenidas de los paquetes
# -mencionados como {packages}- 
# 
summary(DAPcm)                      # {base}  ¡función usual, siempre disponible!
describe(DAPcm)                     # {Hmisc} 
descriptive(DAPcm)                  # {AdequacyModel} 
rms(DAPcm)                          # {analyze.stuff}

# 3.- Determinar parámetros individuales más importantes...
Mu     <- mean(DAPcm)                       # Media poblacional    
Fsm2sP <- length(DAPcm) / (length(DAPcm) - 1)  # Ajuste de Varianza (s² -> σ²)
varnz  <- var(DAPcm) * Fsm2sP               # Varianza poblacional σ²
DesvS  <- sd(DAPcm) * Fsm2sP                # Desv estandar poblac σ

# 4.- Visualizar distribución via histograma y plot de densidad
hist(DAPcm,labels=TRUE)    # Histograma frec. abs.:  #Obs c/ 0.5 ºC
plot(density(DAPcm))       # 'Histograma suavizado' (frec. relativa)
rug(DAPcm)                 # Sobre el eje X: marcas de casos ocurridos 

# 5.- Visualizar NParcial/temp (ºC): Histograma, densidad y curva N
hist(DAPcm,freq=FALSE,xlim=c(8,39),ylim=c(0,0.1),col="lightcyan",
     main="Individuos: Distribución según DAP", 
     xlab="(cm)", ylab="proporción de individuos")
# 
lines(density(DAPcm),col="blue",lwd=2)      # densidad
curve(dnorm(x,mean=Mu,sd=DesvS),from=8, # curva normal referencial
      to=39,add=TRUE,col="red",lwd=2)
legend("topleft",col=c("red","blue"),    # leyenda
       legend=c("Curva normal estimada",
                "Densidad"),lwd=2,bty="n")


## Pruebas para contratar la hipótesis de normalidad. Se compara
#     el p-value de la estadística de prueba con el valor de alfa usado
#     Si p-value > α : Normal | si p-value < α : no_Normal
#   

# Anderson-Darling test
#install.packages("nortest")  # Paquete 'nortest' contiene los tests 
library(nortest)              # Anderson-Darling, Shapiro-Francia, 
#                               Kolmogorov-Smirnov, Pearson

ad.test(DAPcm)                   # (Anderson-Darling)    
#|   
#|   Anderson-Darling normality test
#|   
#|   data:  DAP
#|   A = 0.33851, p-value = 0.5027

# Shapiro-Wilk test 
shapiro.test(DAPcm)               # Paquete {stats}, de instalación base
#|   
#|   Shapiro-Wilk normality test
#|   
#|   data:  DAP
#|   W = 0.99609, p-value = 0.07458


### Trabajar con población muestral MUESTRA PEQUEÑA
##  Repetir M=120 veces: Muestra aleatoria de n=6 con cálc. de estadísticos:
##  x_barra, s, tcalc, chi2calc, IC.95 de µ, EftvdIC, IC.95 de σ², EftvdICs2

# Definir vectores para contener los M resultados
szmues   <- 6
alfa     <- 0.05
grlib    <- szmues - 1
tTab95   <- abs(qt(alfa/2, grlib))
chi2tabL <- qchisq(alfa/2, grlib)
chi2tabR <- qchisq((1-alfa/2), grlib)
nVcs     <- 713
xBar     <- as.double(c(1:nVcs))
eSe      <- as.double(c(1:nVcs))
eSeSQ    <- as.double(c(1:nVcs))
tcalc    <- as.double(c(1:nVcs))
chi2calc <- as.double(c(1:nVcs))
LIC      <- as.double(c(1:nVcs))
LSC      <- as.double(c(1:nVcs))
EftvdIC  <- as.double(c(1:nVcs))
LICs2    <- as.double(c(1:nVcs))
LSCs2    <- as.double(c(1:nVcs))
EftvdICs2 <- as.double(c(1:nVcs))

# Iteración para calcular valores de estadisticos de 'nVcs' muestras
for(i in c(1:nVcs)) {
  mues <- sample(DAPcm, szmues,replace=FALSE) # muestra al azar
  
  # estadísticos media muestral y desvEstd muestral
  xBar[i] <- mean(mues)                    
  eSe[i] <- sd(mues)
  eSeSQ[i] <- (sd(mues))^2
  
  # estadisticos t-calc, chi2-calc
  tcalc[i] <- (mean(mues) - Mu) / (sd(mues) / sqrt(szmues))
  chi2calc[i] <- (grlib*sd(mues)*sd(mues)/varnz)
  
  # LIC.95 y LSC.95 de µ
  LIC[i] <- mean(mues) - tTab95 * (sd(mues) / sqrt(szmues))
  LSC[i] <- mean(mues) + tTab95 * (sd(mues) / sqrt(szmues))
  
  # efectividad del IC.95 de µ con datos de la muestra
  EftvdIC[i] <- ifelse((LIC[i] < Mu) & (Mu < LSC[i]), 1, 0)
  
  # LIC.95 y LSC.95 de σ²
  LICs2[i] <- grlib * sd(mues)^2 / chi2tabR
  LSCs2[i] <- grlib * sd(mues)^2 / chi2tabL
  
  # efectividad del IC.95 de σ² con datos de la muestra
  EftvdICs2[i] <- ifelse((LICs2[i] < varnz) & (varnz <LSCs2[i]), 1, 0)
}

# Convertir a categorias los valores 0 y 1 (facilitar ploteo)
EftvdICf <- as.factor(EftvdIC)
EftvdICs2f <- as.factor(EftvdICs2)


# Calc. promedios de: a) medias muestrales, y b) varianzas muestrales
mmP <- mean(xBar)
msP <- mean(eSeSQ)

## Presentar los resultados en Gráficos  
par(mfrow=c(2,2))                          # preparar... cuatro gráficos en quads

# ¿Es efectivo IC.95? Ver reparto de casos 'Sí/No' para µ y σ²
pie(table(EftvdICf),main="#Casos Sí/No ¿IC.95 contiene µ?",col=gray(0.6:0.9))
barplot(sort(table(EftvdICf),decreasing=TRUE), main="")
pie(table(EftvdICs2f),main="#Casos Sí/No ¿IC.95 contiene σ²?",col=gray(0.6:0.9))
barplot(sort(table(EftvdICs2f),decreasing=TRUE))
# , main="P: Casos de éxito (1) y fracaso (0) al probar si IC.95 contiene σ²"

# Efectividad de los dos IC.95 en números
summary(EftvdICf)                     # IC.95 de µ
summary(EftvdICs2f)                   # IC.95 de σ² 

# Acondicionar variables para reusarlas
# Renombrar vectores de estadísticas calculadas
xBarP  <-  xBar
eSeP   <-  eSe
tcalcP <-  tcalc
chi2calcP <- chi2calc
LICP   <-  LIC
LSCP   <-  LSC
LICs2P <-  LICs2
LSCs2P <-  LSCs2

# Construir dataframe juntando vectores de las poblaciones muestrales 
# obtenidas de muestras pequeñas
PM <- cbind(xBarP,eSeP,tcalcP,chi2calcP,LICP,LSCP,LICs2P,LSCs2P)


### Trabajar con población muestral MUESTRA GRANDE
#
##  Repetir M=120 veces: Muestra aleatoria de n=60 con cálculo de estadísticos
##  x_barra, s, tcalc, chi2calc, IC.95 de µ, EftvdIC, IC.95 de σ², EftvdICs2

# Definir vectores para contener los M resultados
szmues   <- 60
alfa     <- 0.05
grlib    <- szmues - 1
tTab95   <- abs(qt(alfa/2, grlib))
chi2tabL <- qchisq(alfa/2, grlib)
chi2tabR <- qchisq((1-alfa/2), grlib)
nVcs     <- 713
xBar     <- as.double(c(1:nVcs))
eSe      <- as.double(c(1:nVcs))
eSeSQ    <- as.double(c(1:nVcs))
tcalc    <- as.double(c(1:nVcs))
chi2calc <- as.double(c(1:nVcs))
LIC      <- as.double(c(1:nVcs))
LSC      <- as.double(c(1:nVcs))
EftvdIC  <- as.double(c(1:nVcs))
LICs2    <- as.double(c(1:nVcs))
LSCs2    <- as.double(c(1:nVcs))
EftvdICs2 <- as.double(c(1:nVcs))

# Iteración para calcular valores de estadisticos
for(i in c(1:nVcs)) {
  mues <- sample(DAPcm, szmues,replace=FALSE) # muestra al azar
  
  # estadísticos media muestral y desvEstd muestral
  xBar[i] <- mean(mues)                    
  eSe[i] <- sd(mues)
  eSeSQ[i] <- (sd(mues))^2
  
  # estadisticos t-calc, chi2-calc
  tcalc[i] <- (mean(mues) - Mu) / (sd(mues) / sqrt(szmues))
  chi2calc[i] <- (grlib*sd(mues)*sd(mues)/varnz)
  
  # LIC.95 y LSC.95 de µ
  LIC[i] <- mean(mues) - tTab95 * (sd(mues) / sqrt(szmues))
  LSC[i] <- mean(mues) + tTab95 * (sd(mues) / sqrt(szmues))
  
  # efectividad del IC.95 de µ con datos de la muestra
  EftvdIC[i] <- ifelse((LIC[i] < Mu) & (Mu < LSC[i]), 1, 0)
  
  # LIC.95 y LSC.95 de σ²
  LICs2[i] <- grlib * sd(mues)^2 / chi2tabR
  LSCs2[i] <- grlib * sd(mues)^2 / chi2tabL
  
  # efectividad del IC.95 de σ² con datos de la muestra
  EftvdICs2[i] <- ifelse((LICs2[i] < varnz) & (varnz <LSCs2[i]), 1, 0)
}

# Convertir a categorias los valores 0 y 1 (facilitar ploteo)
EftvdICf <- as.factor(EftvdIC)
EftvdICs2f <- as.factor(EftvdICs2)

# Calc. promedios de: a) medias muestrales, y b) varianzas muestrales
mmG <- mean(xBar)
msG <- mean(eSeSQ)

# Visualizar forma de distribuciones muestrales (variables continuas)
plot(density(xBar))
plot(density(eSe))
plot(density(tcalc))
plot(density(chi2calc))

# ¿Es efectivo IC.95? Ver reparto de casos 'Sí/No' para µ y σ²
pie(table(EftvdICf),main="#Casos Sí/No ¿IC.95 contiene µ?",col=gray(0.6:0.9))
barplot(sort(table(EftvdICf),decreasing=TRUE))
pie(table(EftvdICs2f),main="#Casos Sí/No ¿IC.95 contiene σ²?",col=gray(0.6:0.9))
barplot(sort(table(EftvdICs2f),decreasing=TRUE))

# Efectividad de los dos IC.95 en números
summary(EftvdICf)                     # IC.95 de µ
summary(EftvdICs2f)                   # IC.95 de σ² 

# Renombrar vectores de estadísticas calculadas para acoplarlos a tabla PM
xBarG  <- xBar
eSeG   <-  eSe
tcalcG <-  tcalc
chi2calcG <- chi2calc
LICG   <-  LIC
LSCG   <-  LSC
LICs2G <-  LICs2
LSCs2G <-  LSCs2

# Acoplar a tabla PM 8 'poblaciones muestrales' de muestras grandes 
PM <- cbind(PM[,],xBarG,eSeG,tcalcG,chi2calcG,LICG,LSCG,LICs2G,LSCs2G)


### Presentar en conjunto las poblaciones X, x̅ P, x̅ G y sus promedios µ
#
# Vectores para contener 'parámetros' µ, mmP, mmG
MuVcs  <- as.double(c(1:nVcs))
mmPvcs <- as.double(c(1:nVcs))
mmGvcs <- as.double(c(1:nVcs))

# Determinación de parámetros µ(x), µ(x̅ P),  µ(x̅ G)

for(i in c(1:nVcs)) {
  MuVcs[i] <- Mu                        # Media de población original
  mmPvcs[i] <- mmP                      # Media de pob. de medias de muestra P
  mmGvcs[i] <- mmG                      # Media de pob. de medias de muestra G
}

par(mfrow=c(1,1))                       # preparar... cuatro gráficos en quads

## Graficar la distribución de 120 obs. X, su promedio µ, 120 medias de
# muestras pequeñas P, 120 medias de muestras grandes G y los límites para 
# 0.95 proyectados con base en las localizaciones de los LCS y LCI de las
# muestras pequeñas y grandes

# Población de origen: Puntos x (negros), parámetro µ (trazo amarillo) 
plot(DAPcm, main="Distribución de valores Y, Ŷ{6} y Ŷ{60}")
abline(h=MuVcs,col="yellow",lwd=4)     # Línea del promedio µ

# Muestras pequeñas (P): Promedios x̅ y lím. de confianza ajustados: VERDE
points(xBarP, col="green")                # Ptos - medias muestrales P
z1 <- line(LSCP)                          # Línea ajustada a 120 LSCPs
z2 <- line(LICP)                          # Línea ajustada a 120 LICPs
abline(coef(z1),col="green")              # Traza línea f(coef(z1))
abline(coef(z2),col="green")              # Traza línea f(coef(z2))

abline(h=mmPvcs,col="darkgreen",lty=2)    # Linea ---- : promedio(x̅ P)

# Muestras grandes (G): Promedios x̅ y límites de conf. ajust.: NARANJA
points(xBarG, col="orange")               # Ptos - medias de muestra G
z4 <- line(LSCG)                          # Línea ajustada a 120 LSCGs
z5 <- line(LICG)                          # Línea ajustada a 120 LICGs
abline(coef(z4),col="orange")             # Traza línea f(coef(z4))
abline(coef(z5),col="orange")             # Traza línea f(coef(z5))

abline(h=mmGvcs,col="darkorange",lty=4)   # Linea promedio(x̅ G)

## Valores numéricos expuestos
txMu  <- "Promedio de la población original"
txMuP <-  "Promedio de la población de medias de muestras pequeñas"
txMuG <-  "Promedio de la población de medias de muestras grandes"

paste(txMu, Mu, sep = " = 21.6363")
paste(txMuP, mmP, sep = " = 21.5683")
paste(txMuG, mmG, sep = " = 21.6092")

# _____ fin de Script _________
