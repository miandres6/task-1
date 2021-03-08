#Taller A
#Elaborado por: Andrea Yohana, Miguel Andrés Castillo y María Vallejo 
#Fecha de elaboracion: 08/03/2021
#Ultima modificacion: 08/03/2021

# intial configuration
rm(list = ls()) # limpia el entorno de R
pacman::p_load(tidyverse,readxl,haven) # cargar y/o instalar paquetes a usar


#1. Vectores
##Vector del 1 al 100
vector_1_100 = c(1:100)

##Vector del 1 al 99 impares. Ayuda obtenida de OpenCourseWare
n = 99
vector_impares=c()
for(i in 1:n){
  if(i%%2!=0) vector_impares<-c(vector_impares,i)
}

##Vector 1 al 100 pares. Ayuda de Nube de Datos
vector_pares=c()
setdiff(vector_1_100, vector_impares)
vector_pares=c(vector_1_100[!vector_1_100%in%vector_impares])


#2.Limpiar una base de datos
library("readxl")
cultivos=read_excel("data/input/cultivos.xlsx")


