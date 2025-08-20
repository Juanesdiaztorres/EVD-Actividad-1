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
wd <- here("C:/Users/mario/Documents/EVDS")
setwd(wd)


##Cargar datos ----------####

#Area del Municipio
mun_area_db <- read.csv("mun_area.csv")

#Población del municipio
pob_censal_db <- read.xlsx("pob_censal.xlsx")


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
write.xlsx(pob_censal_wide_db, "pob_censal_wide.xlsx", overwrite = T)

  
#Integración de tablas, validaciones y cálculos --------------------------------


##Unir población wide con mun area ------####

#Conservar solo el área del departamento 
mun_area_db_sub <- mun_area_db %>% 
                   select(DPMP, MPIO_NAREA)

#Agregar el área del departamento
db <- left_join(pob_censal_wide_db, mun_area_db_sub, by = "DPMP")

#Hay un municipio sin área 
skim(db)

db <- db %>% 
      mutate(no_area = ifelse(is.na(MPIO_NAREA)== T, 1, 0))
#El municipio de Mapiripana en el departamento de Guainía no tiene un área registrada
  
  
#Cálculos de interés - medición --------------------------------

#Siguiendo Lora y Prada (2021)

#Densidad poblacional
db <- mutate(db, densidad_pob85 = Pob_1985/MPIO_NAREA)
db <- mutate(db, densidad_pob93 = Pob_1993/MPIO_NAREA)
db <- mutate(db, densidad_pob05 = Pob_2005/MPIO_NAREA)
db <- mutate(db, densidad_pob18 = Pob_2018/MPIO_NAREA) 

#Crecimiento absoluto

db <- mutate(db, CA = Pob_2018 - Pob_1985)

#Crecimiento porcentual
db <- mutate(db, CP = CA/Pob_1985*100)

#Agregación por departamento
agregdepart <- db %>%
  group_by(DPNOM) %>%
  summarise(
    Densidad_Promedio = mean(densidad_pob85, na.rm = TRUE),
    Crecimiento_Absoluto = sum(CA, na.rm = TRUE),
    Crecimiento_Porcentual = mean(CP, na.rm = TRUE)
  )


#Preguntas de indagación - Storytelling --------------------------------

#1
CA_top <- db %>%
  arrange(desc(CA)) %>%
  head(20)

CP_top <- db %>%
  arrange(desc(CP)) %>%
  head(20)
#2  
densidad_top_93 <- db %>%
  arrange(desc(densidad_pob93)) %>%
  head(10)

densidad_top_18  <- db %>%
  arrange(desc(densidad_pob18)) %>%
  head(10)

#3
densidad_top_93_dpto <- db %>%
  arrange(DPNOM, densidad_pob93)

densidad_top_93_dpto %>% top_n(5, densidad_pob93)

densidad_top_18_dpto <- db %>%
  arrange(DPNOM, densidad_pob18)

c <- densidad_top_18_dpto %>% top_n(5, densidad_pob18)

#4 
#Gráfico Q-Q
ggplot(db, aes(sample = scale(Pob_1985))) +
  geom_qq() +
  geom_abline()







  
  
  
  
  
  
  




  
  
  
  
  
  
  
  
  
  
  
  
  



