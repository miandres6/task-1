#==============================================================================#
# Autores: Maria Vallejo, Andrea Cortes, Miguel Castillo
# # Fecha elaboracion:08 de marzo de 2021
# Ultima modificacion: 12 de marzo de 2021
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

variables=colnames(cultivos[,5:columnas])

resultado=melt(data=cultivos, id.vars = c("CODDEPTO","DEPARTAMENTO","CODMPIO","MUNICIPIO"), measure.vars = variables, value.name = "Hectareas")
View(resultado)

#3. GEIH
#3.1. Importar
pacman::p_load(tidyverse,readxl,haven)
caracteristicas_generales=readRDS(file='data/input/2019/Cabecera - Caracteristicas generales (Personas).rds')
ocupados=readRDS(file='data/input/2019/Cabecera - Ocupados.rds') 

duplicated(caracteristicas_generales$directorio)%>% table()
duplicated(paste0(caracteristicas_generales$directorio, caracteristicas_generales$secuencia_p))%>% table()
duplicated(paste0(caracteristicas_generales$directorio, caracteristicas_generales$secuencia_p, caracteristicas_generales$orden))%>% table(
  
) #No existen individuos duplicados en caracteristicas_generales

duplicated(paste0(ocupados$directorio,ocupados$secuencia_p,ocupados$orden)) %>% table() #Ocupados son identificadores unicos


#Unir base de datos en una sola
nueva_base=full_join(caracteristicas_generales,ocupados,by=c('directorio','secuencia_p','orden'))
View(nueva_base)

nueva_base$Ocupados=ifelse(is.na(nueva_base$mes.y), 0, 1) #desempleados=0

#Guardar la base
saveRDS(object = nueva_base , file = "data/output/NuevaBase.rds")

#3.2 Descriptivas

summary(nueva_base)

#Número de ocupados:
#-Por edad
nueva_base %>%  group_by(P6040)%>% summarise(media=mean(Ocupados), varz=var(Ocupados),dsvest=sd(Ocupados), total=sum(Ocupados))
#-Por sexo
nueva_base %>% group_by(P6020) %>%  summarise(media=mean(Ocupados), varz=var(Ocupados),dsvest=sd(Ocupados), total=sum(Ocupados))
#-Por tipo de contrato
nueva_base %>% group_by(P6430) %>%  summarise(media=mean(Ocupados), varz=var(Ocupados),dsvest=sd(Ocupados), total=sum(Ocupados))
#-Urbano/Rural  
nueva_base %>% group_by(area.x) %>% summarise(media=mean(Ocupados), varz=var(Ocupados),dsvest=sd(Ocupados), total=sum(Ocupados))

#Ingresos laborales:
a=subset(nueva_base,is.na(P6750)==F)

#-Por edad
a %>%  group_by(P6040)%>% summarise(media=mean(P6750), varz=var(P6750), dsvest=sd(P6750),total=sum(P6750))
#-Por sexo
a %>% group_by(P6020) %>% summarise(media=mean(P6750), varz=var(P6750), dsvest=sd(P6750),total=sum(P6750))
#-Por tipo de contrato
a %>% group_by(P6430) %>% summarise(media=mean(P6750), varz=var(P6750), dsvest=sd(P6750),total=sum(P6750))
#-Urbano/Rural  
a %>% group_by(area.x) %>% summarise(media=mean(P6750), varz=var(P6750), dsvest=sd(P6750),total=sum(P6750))


Library(ggplot2)

#Gráfica edad 

n3=ggplot(data=nueva_base, aes(x=P6040)) + geom_bar()
n3
n4=nueva_base %>% filter() %>% ggplot(aes(x=P6040))+geom_density(fill="#330066", color="#E1AF00", alpha=0.65)

#Gráfica empleados

n1=ggplot(data=nueva_base, aes(x=as.factor(Ocupados), fill=as.factor(Ocupados)))

n2= n1+ geom_bar() +scale_fill_hue(c=45)+theme(legend.position = "rigth")+labs(title="Empleados y desempleados", x="Empleados vs. Desempleados")

n2

#Gráfica empleados por genero


n5= nueva_base %>% group_by(P6020) %>% summarise(total=sum(Ocupados)) %>% ggplot(data=., aes(x=P6020,y=total))+geom_bar(stat="identity", fill="#E1AF00", alpha=0.7, width=0.75)+ coord_flip()+xlab("genero")+ylab("Cantidad personas empleadas")+theme_bw()


#Gráfica empleados por edad

n6=nueva_base %>% group_by(P6040) %>% summarise(total=sum(Ocupados)) %>% ggplot(data=., aes(x=P6040,y=total))+geom_bar(stat="identity", fill="#E1AF00", alpha=0.7, width=0.75)+ coord_flip()+xlab("edad")+ylab("Cantidad personas empleadas")+theme_bw()







