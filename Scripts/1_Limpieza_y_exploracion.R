#===================== Limpieza y exploración de datos =========================


#Alistar el ambiente de trabajo ------------------------------------------------

##Limpiar el ambiente ---------####
cat("\014")
rm(list = ls())


##Cargar paquetes necesarios --------####

#Cargar pacman 
if(!require(pacman)) install.packages("pacman") ; require(pacman)

#Cargar los demas paquetes 

p_load(
  tidyverse, #Análisis de datos
  skimr, #Resumir información datos
  rio, #Importar/Exportar datos
  openxlsx, #Abrir archivos de excel 
  here, #Referenciar archivos fácil 
  stringi #Remover acentos gramaticales
) 

##Definir el directorio de trabajo -------####
wd <- here()
setwd(wd)


##Cargar datos ----------####

#Area del Municipio
mun_area_db <- read.csv("Data/mun_area.csv")

#Población del municipio
pob_censal_db <- read.xlsx("Data/pob_censal.xlsx")


#Entendimiento de los datos ----------------------------------------------------

##Estadísticas descriptivas y exploración inicial -------####
skim(mun_area_db)
skim(pob_censal_db)


##Poner tipos de variable correctos --------####

mun_area_db <- mun_area_db %>% 
               mutate(across(c(DP, DPMP), as.factor)) #Volver categóricas los números de los departamentos y municipios

pob_censal_db <- pob_censal_db %>% 
                 mutate(across(c(DPNOM, MPIO, DP, DPMP), as.factor)) #Volver categóricas los números y nombres de los departamentos y municipios



#Preparación de los datos ------------------------------------------------------

##Construir llave única ---------####

pob_censal_db <- pob_censal_db %>% 
                 mutate(ID = paste0(DPMP, "_", AÑO))

##Identificar duplicados ---------####

#Área municipio 
sum(duplicated(mun_area_db))
sum(duplicated(mun_area_db$DPMP))

#Población por municipio 
sum(duplicated(pob_censal_db))
sum(duplicated(pob_censal_db$ID))

#No hay duplicados en general, ni unidades de observación duplicadas

##Asegurar tipos de datos correctos ---------####


#Verificación de años donde se hizo un censo para cada municipio 
janitor::tabyl(pob_censal_db$AÑO)

#Los años disponible son 1985, 1993, 2005 y 2018 todos años donde el DANE hizo
#censo y para cada uno tengo el mismo número de observaciones. 


#Verificar población de los municipios 
pob_censal_db %>% 
              mutate(flagged = Población < 0) %>% 
              count(flagged)

#Verificar área de los municipios 
mun_area_db %>% 
          mutate(flagged = MPIO_NAREA < 15) %>% 
          count(flagged)


#Mismo número de municipios en ambas tablas
n_distinct(mun_area_db$DPMP)
n_distinct(pob_censal_db$DPMP)

#La base de censos poblacionales tiene un municipio más. En siguiente punto 
#hagamos la unión veremos que municipio es. 


##Indentificar y tratar valores faltantes ----------####
skim(mun_area_db)
skim(pob_censal_db)

#Ninguna de las variables de las dos bases tiene valores faltantes 


##Estandarizar nombres de los municipios y departamentos---------####


pob_censal_db <- pob_censal_db %>% 
                 mutate(DPNOM = stri_trans_general(DPNOM, "Latin-ASCII"), #Remover acentos
                        MPIO = stri_trans_general(MPIO, "Latin-ASCII")) %>% #Remover acentos
                  mutate(across(c(DPNOM, MPIO), toupper)) #Poner nombres en mayúscula
  
  
##Pob censal como base ancha ---------####


#Pasar base de larga a ancha
pob_censal_wide_db <- pob_censal_db %>% 
                      select(-ID) %>% #Remover el identificador para que la base de pueda colapsar a nivel de municipio
                      pivot_wider(names_from = AÑO, #Nombres del año 
                                  values_from = Población) #Valores de la población 

#Renombrar variables
pob_censal_wide_db <- pob_censal_wide_db %>% 
                      rename(
                        Pob_1985 = `1985`, 
                        Pob_1993 = `1993`,
                        Pob_2005 = `2005`,
                        Pob_2018 = `2018`
                      )

#Guardar base
write.xlsx(pob_censal_wide_db, "Data/pob_censal_wide.xlsx", overwrite = T)

  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  



