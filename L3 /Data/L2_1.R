# Modelo de Conteo
#-------------------------------------------
rm(list = ls())
# Ruta de carpeta
setwd("D:/Dropbox/Docencia/UNI/L3/Aplicacion")

# Librerias
library(tidyverse)
library(dplyr)
require(openxlsx)
library(readr)

# Carga de datos
data <- read_csv("E0.csv")
data %>%  head()
data %>%  dim()
data %>%  names()

# Enlace de la descripccion de la data
# https://www.football-data.co.uk/notes.txt

# descripccion de la variable target
table(data$FTHG)
barplot(table(data$FTHG))
boxplot(data$FTHG)
mean(data$FTHG)

# Podemos obsevar que el equipo local tiene una alta concentracion
# en goles de entre 0 y 3 (valores)

# Visitante
table(data$FTAG)
barplot(table(data$FTAG))
boxplot(data$FTAG)
mean(data$FTAG)

# Analisis descriptivo
summary(data$FTHG)
mean(data$FTHG)
var(data$FTHG)

# Mapa de calor de correlacion
#library(ggplot2)
#library(reshape2)



# Separamos las variables de la matriz
df <- data %>% 
  select(FTHG, HS, AS, HY)

#install.packages("psych")
library(psych)
corPlot(df, cex = 1.2, main = "Matriz de correlación")

# Modelando el pronostico de GOL
# FTHG ~ HS+ HST + HC + HC +HF + HO+ HY + HR
#?lm()

# separmos la data en train y test
n = nrow(data)
m = ceiling(n*0.7)
set.seed(138)
train_id = sort(sample(1:n, m , replace=F))

data_train = data[train_id,]
data_test = data[-train_id,]

# Dimensiones de mi data trian y test
data_train %>%  dim()
data_test %>%  dim()

#--------------------------------------------------------
# Modelo poisson
#--------------------------------------------------------
# HS = Home Team Shots
# HST = Home Team Shots on Target
# HC = Home Team Corners
# HF = Home Team Fouls Committed
# HY = Home Team Yellow Cards 
# HR = Home Team Red Cards

mode1 = glm(FTHG ~ HS+ HST + HC +HF + HY + HR, 
            data=data_train, family = poisson(link="log"))
summary(mode1)
y1_train = predict(mode1 , data_train, type = "response")
y1_test = predict(mode1 , data_test, type = "response")

# Correacion
rho_1_train <- cor(data_train$HTHG, y1_train)^2
rho_1_test <- cor(data_test$HTHG, y1_test)^2
rho_1_train
rho_1_test

# Modelo 2
#---------------------------------------

mode2 = glm(FTHG ~ HST + HY+AST+I(HST^2)+I(HST^3)+I(HST^4), 
            data=data_train, family = poisson(link="log"))
summary(mode2)
y2_train = predict(mode2 , data_train, type = "response")
y2_test = predict(mode2 , data_test, type = "response")

# Correacion
rho_2_train <- cor(data_train$HTHG, y2_train)^2
rho_2_test <- cor(data_test$HTHG, y2_test)^2

rho_1_train
rho_1_test

rho_2_train
rho_2_test

# Presentaicon
library(stargazer)
stargazer(mode2, type = "html", out="Modelo_Poisson.html")
modelo2 <- stargazer(mode2,type="text")

# Combinando los modelos M1  Y M2
stargazer(mode1, mode2,    
          #se=list(cse(mco),NULL,NULL), 
          title="Modelo de conteos (Poisson)", type="text", 
          df=FALSE, digits=4)

#--------------------------------------------------------
# Modelo Binomial Negativa
#--------------------------------------------------------
library(MASS)
mode3 = glm.nb(FTHG ~ HS+ HST + HC +HF + HY + HR, 
               data=data_train)
summary(mode3)
y3_train = predict(mode3 , data_train, type = "response")

# Correacion
rho_3_train <- cor(data_train$HTHG, y3_train)^2

stargazer(mode1, mode2, mode3,   
          #se=list(cse(mco),NULL,NULL), 
          title="Modelo de conteos (Poisson)", type="text", 
          df=FALSE, digits=4)
