# LIBRERÍAS REQUERIDAS


library(readr) # PARA IMPORTAR REGISTROS
library(dplyr) # PARA MANIPULAR ARCHIVOS
library(tidyverse) # PARA MANIPULAR ARCHIVOS
library(stringr)
library(tidyr)

# IMPORTAR ARCHIVOS


# PERIODO 20132
SB11_20132I <- read_delim("~/ONP/Oficina Nacional de Estadística/Repositorios GitHub/Contexto/Saber11/Fuentes/SB11-20132.txt", 
                         "|", escape_double = FALSE, trim_ws = TRUE)
# PERIODO 20142
SB11_20142I <- read_delim("~/ONP/Oficina Nacional de Estadística/Repositorios GitHub/Contexto/Saber11/Fuentes/SB11-20142.txt", 
                         "|", escape_double = FALSE, trim_ws = TRUE)
# PERIODO 20152
SB11_20152I <- read_delim("~/ONP/Oficina Nacional de Estadística/Repositorios GitHub/Contexto/Saber11/Fuentes/SB11-20152.txt", 
                         "|", escape_double = FALSE, trim_ws = TRUE)
# PERIODO 20162
SB11_20162I <- read_delim("~/ONP/Oficina Nacional de Estadística/Repositorios GitHub/Contexto/Saber11/Fuentes/SB11-20162.txt", 
                         "|", escape_double = FALSE, trim_ws = TRUE)


# SELECCIONAR VARIABLES COMUNES 

inter1 <- intersect(c(names(SB11_20142I)), c(names(SB11_20152I)))
interseccion <- intersect(c(inter1),c(names(SB11_20162I)))


# SE CREAN BASES CON VARIABLES COMUNES

## SE CREA UN VECTOR NUMÉRICO CON LAS POSICIONES DE LAS VARIABLES COMUNES 

Nombres1 <- match(interseccion, names(SB11_20142I))
Nombres2 <- match(interseccion, names(SB11_20152I))
Nombres3 <- match(interseccion, names(SB11_20162I))

## SE SELECCIONAN LAS BASES CON LAS VARIABLES COMUNES

SB11_20142I <- select(SB11_20142I, Nombres1)
SB11_20152I <- select(SB11_20152I, Nombres2)
SB11_20162I <- select(SB11_20162I, Nombres3)


## SE ELIMINA LA VARIABLE "ESTU_VECES_ESTADO_ESTUDIANTIL" POR PROBLEMAS DE CONSISTENCIA

SB11_20142I <- select(SB11_20142I, -c(ESTU_VECES_ESTADO_ESTUDIANTIL))
SB11_20152I <- select(SB11_20152I, -c(ESTU_VECES_ESTADO_ESTUDIANTIL))
SB11_20162I <- select(SB11_20162I, -c(ESTU_VECES_ESTADO_ESTUDIANTIL))


# BASE DE DATOS CONSOLIDADA CON VARIABLES COMUNES

SABER11 <- bind_rows(SB11_20142I, SB11_20152I, SB11_20162I)

# SABER11 %>% group_by(ESTU_COD_RESIDE_MCPIO, ESTU_RESIDE_MCPIO) %>% 
  # summarise(Total = n()) %>% arrange(desc(Total))

# CREAR PUNTO DE CORTE POR PUNTAJE TOTAL

# 310 PARA LOS QUE PRESENTARON EL SABER PRO EN 20142 (PRIMERA CONVOCATORIA)
# 318 PARA LOS QUE PRESENTARON EL SABER PRO EN 20152 (SEGUNDO CONVOCATORIA)
# 342 PARA LOS QUE PRESENTARON EL SABER PRO EN 20162 (TERCERA CONVOCATORIA)
# 348 PARA LOS QUE PRESENTARON EL SABER PRO EN 20172 (CUARTA CONVOCATORIA)

SABER11 <- SABER11 %>% mutate(ASPIRANTE = if_else(PERIODO == 20142 & PUNT_GLOBAL >= 310, "Sí",
                                                  if_else(PERIODO == 20152 & PUNT_GLOBAL >= 318, "Sí",
                                                          if_else(PERIODO == 20162 & PUNT_GLOBAL >= 342, "Sí", "No"))))

# SE CREA VARIABLE SIMULADA PARA LA CUARTA CONVOCATORIA (PILOTO 2016-2)

SABER11 <- SABER11 %>% mutate(ASPIRANTEN = if_else(PERIODO == 20162 & PUNT_GLOBAL >= 348, "Sí","No"))


## SE CREA BASE SABER 11 NIVELES 1 Y 2

SISBEN12 <- SABER11 %>% filter(FAMI_NIVEL_SISBEN %in% c(1,2))

# 94% estrato 1 o 2
# Nivel 1 y 2 del SISBEN existe en el "100 %" de los municipios

## BASES PARA MAPAS MUNICIPIOS

  SISBEN12 %>% group_by(PERIODO,ESTU_COD_RESIDE_MCPIO, ESTU_RESIDE_MCPIO, ESTU_RESIDE_DEPTO, ASPIRANTE) %>% 
  summarise(Total = n()) %>% ungroup() %>% spread(ASPIRANTE, Total) %>% 
  mutate_all(funs(replace(., is.na(.), 0))) %>% 
  mutate(Probabilidad = round((Sí/(No + Sí))*100, 2))



  
  
  
  
names(SISBEN12)

  
    stocks <- data.frame(
    time = as.Date('2009-01-01') + 0:9,
    X = rnorm(10, 0, 1),
    Y = rnorm(10, 0, 2),
    Z = rnorm(10, 0, 4)
  )

stocksm <- stocks %>% gather(stock, price, -time)
stocksm %>% spread(stock, price)
  
  
  
  

%>% summarise(Cuenta = count(ESTU_COD_RESIDE_MCPIO))





summarise(iris, n = sum(Petal.Length))

%>% group_by(PERIODO, ASPIRANTE) %>% 
  summarise(Total = n(Petal.Length))

names(iris)


# INICIO FASE EXPLORATORIA

SABER11 %>% group_by(ESTU_GENERO) %>% tally()
SISBEN12 %>% group_by(ESTU_GENERO) %>% tally()

SISBEN12 %>% group_by(ESTU_GENERO, ASPIRANTE) %>% tally()


SISBEN12 %>% group_by(PERIODO, ASPIRANTE,ESTU_COD_RESIDE_MCPIO) %>% 
  summarise(Total = n()) %>% group_by(PERIODO, ASPIRANTE) %>% 
  summarise(Total = n())


SISBEN12 %>% filter(PERIODO==20162) %>% group_by(PERIODO, ASPIRANTE,ESTU_COD_RESIDE_MCPIO) %>% 
  summarise(Total = n()) %>% group_by(PERIODO, ASPIRANTE) %>% 
  summarise(Total = n())





 View(SISBEN12 %>% group_by(PERIODO, ESTU_ETNIA, ASPIRANTE) %>% 
  summarise(Total = n())) 
  
  
  
  %>% group_by(PERIODO, ESTU_ETNIA, ASPIRANTE) %>% 
  summarise(Total = n()))





nrow(SISBEN12 %>%  filter(PERIODO==20162) %>% 
       group_by(ESTU_COD_RESIDE_MCPIO) %>% tally())








str_detect(names(SISBEN12),"ESTRA")



names(SABER11)


# INICIO FASE EXPLORATORIA


## POR MUNICIPIOS DE RESIDENCIA DEL ESTUDIANTE

SABER11 %>% filter(PERIODO==20162) %>% group_by(ESTU_COD_RESIDE_MCPIO) %>% count()


SABER11 %>% group_by(PERIODO, FAMI_NIVEL_SISBEN, ASPIRANTE,ESTU_COD_RESIDE_MCPIO) %>% 
  summarise(Total = n()) %>% group_by(PERIODO, FAMI_NIVEL_SISBEN, ASPIRANTE) %>% 
  summarise(Total = n())



names(SABER11)



ESTU_RESIDE_MCPIO



glimpse(SABER11)


SABER11 %>% group_by(PERIODO) %>% 
  summarise(Total = n()) %>% arrange(desc(Total))






ASPIRANTE <- 




SABER11 %>% group_by(FAMI_NIVEL_SISBEN) %>% 
  summarise(Total = n()) %>% arrange(desc(Total))



SABER11 %>% group_by(PERIODO, ASPIRANTE) %>% summarise(TOTAL = n(),
                                                       MIN = min(PUNT_GLOBAL),
                                                       MAX = max(PUNT_GLOBAL), 
                                                       MEDIA= mean(PUNT_GLOBAL))






# 


summary(SABER11$PUNT_GLOBAL)

ESTU_COD_RESIDE_MCPIO 


glimpse(SABER11)


write.csv

A <- as.matrix.data.frame(is.na(iris))

margin.table(A)


library(dplyr)
h <- data.frame(is.na(iris))

h %>% mutate(Total = Sepal.Length+Sepal.Width+Petal.Length+Petal.Width+Species) %>% 
  arrange(desc(Total))




h %>% mutate_all(.funs = funs(Total=.+2))


glimpse(h)

addmargins(h)

as.matrix.data.frame(A)


stocks <- data.frame(
  time = as.Date('2009-01-01') + 0:9,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4)
)

stocksm <- stocks %>% gather(stock, price, -time)
stocksm %>% spread(stock, price)








