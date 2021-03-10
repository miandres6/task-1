#==============================================================================#
# Autores: María Vallejo, Andrea Cortes
# Colaboradores:
# Fecha elaboracion:08 de marzo de 2021
# Ultima modificacion: 
# Version de R: 4.0.3
#==============================================================================#
pacman::p_load(tidyverse,viridis,forcats,gapminder)

#1.Vectores
vector=c(1:100)
primos=c(1)

for(i in 1:100){
  contador=0
  for(j in 1:i){
    if(round(i/j,0)==(i/j)){
      contador=contador+1
    }
  }
  if(contador==2){
    primos=c(primos,i)
  }
}
#vector que contiene los numeros del 1 al 100
vector
#vector que contiene los numeros primos del 1 al 99
primos

#2. Limpiar una base de datos
#Cargar bases de datos
library(readxl)

cultivos= read_excel("data/input/cultivos.xlsx", range = "A9:Y362")
columnas=ncol(cultivos)
filas=nrow(cultivos)

#Eliminar las observaciones que no tienen código municipal
codigomunicipal=cultivos[,3]
filaseliminadas=c()
for(i in 1:filas){
  if(is.na(codigomunicipal[i,1])==TRUE){
    filaseliminadas=c(filaseliminadas,i)
  }
}
numfilaseliminadas=length(filaseliminadas)
cultivos=cultivos[-filaseliminadas,]

#Sustutuir NA por 0
cultivos[is.na(cultivos)]=0
View(cultivos)

#Pivotear a formato long
library(reshape2)

variables_0=colnames(cultivos[,1:4])
variables=colnames(cultivos[,5:columnas])

resultado=melt(data=cultivos, id.vars = c("CODDEPTO","DEPARTAMENTO","CODMPIO","MUNICIPIO"), measure.vars = variables, value.name = "Hectáreas")
View(resultado)

#3. GEIH
#3.1. Importar
pacman::p_load(tidyverse,readxl,haven)
caracteristicas_generales=readRDS(file='data/input/2019/Cabecera - Caracteristicas generales (Personas).rds')%>%
  dplyr::select(.,directorio , secuencia_p , orden)

ocupados=readRDS(file='data/input/2019/Cabecera - Ocupados.rds') %>%
  dplyr::select(.,directorio , secuencia_p , orden)
View(ocupados)
