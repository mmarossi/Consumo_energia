install.packages("VIM")
library(VIM)
install.packages("ggplot2")
library(ggplot2)
install.packages("lubridate")
library(lubridate)
install.packages("devtools")
library(devtools)
install.packages("tidyverse")
library(tidyverse)
install.packages("dplyr")
library(dplyr)
#verificando dados missing
matrixplot(energy_consumption)
aggr(energy_consumption)
#comportamento da variável "electricy_consumption"
summary(energy_consumption$electricity_consumption)
boxplot(energy_consumption$electricity_consumption)
hist(energy_consumption$electricity_consumption)
#separando as informações da coluna "datatime" em novas colunas (separação de dia, mês, dia da semana e ano)
base1 <- mutate(energy_consumption, year = as.numeric(year(energy_consumption$datetime)), 
                month = as.numeric(month(energy_consumption$datetime)), 
                day = as.numeric(mday(energy_consumption$datetime)),
                weekdays = wday(energy_consumption$datetime, label=TRUE),
                hour = as.numeric(hour(energy_consumption$datetime)))
#removemos a variável "datetime" e a coluna de consumo da nova base          
base2 <- base1[,-c(2,8)]
#definimos o ponto de corte - ponto de corte: menor que a mediana="0", maior="1"
corte <- energy_consumption$electricity_consumption
base2 <- mutate(base2, corte = as.numeric(ifelse(corte < 267, '0', '1')))
#verificamos a proporção entre os consumidores através do ponto de corte estabelecido.
prop.table(table(base2$corte))
#visualização das primeiras linhas da tabela alterada
head(base2)

# ANALISE BIVARIADA
# Variáveis quantitativas 
boxplot(base2$temperature            ~ base2$corte)
boxplot(base2$pressure               ~ base2$corte)
boxplot(base2$windspeed              ~ base2$corte)
boxplot(base2$year                   ~ base2$corte)
boxplot(base2$month                  ~ base2$corte)
boxplot(base2$day                    ~ base2$corte)
boxplot(base2$hour                   ~ base2$corte)

#Variáveis quantitativas e quali
prop.table(table(base2$corte))
prop.table(table(base2$weekdays,       base2$corte),1)
prop.table(table(base2$temperature,    base2$corte),1)
prop.table(table(base2$pressure,       base2$corte),1)
prop.table(table(base2$windspeed,      base2$corte),1)
prop.table(table(base2$year,           base2$corte),1)
prop.table(table(base2$month,          base2$corte),1)
prop.table(table(base2$day,            base2$corte),1)
plot(prop.table(table(base2$day,       base2$corte),1), main="Consumo por dia")
prop.table(table(base2$hour,           base2$corte),1)
plot(prop.table(table(base2$hour,      base2$corte),1), main="Consumo por hora")



install.packages("caret")
library(caret)

set.seed(12345)
#solicitamos que seja criada um partição contendo 70% dos dados
index <- createDataPartition(base2$corte, p= 0.7,list = F)
#divisão entre base treino e teste, onde 70% ficou em treino e 30% para teste
data.train <- base2[index, ] # base de desenvolvimento: 70%
data.test  <- base2[-index,] # base de teste: 30%

# Testando as proporções das amostras em relação base original
prop.table(table(base2$corte))
prop.table(table(data.train$corte))
prop.table(table(data.test$corte))

----------------------------------------------------------------------------------------------------------------
 #modelagem
 
  names  <- names(data.train) 
f_full <- as.formula(paste("corte ~",
                           paste(names[!names %in% "corte"], collapse = " + ")))

glm.full <- glm(f_full, data= data.train, family= binomial(link='logit'))
summary(glm.full)


#multicolinearidade
install.packages("car")
library(car)

#verficando VIF do modelo 
vif(glm.full)
#verficando a correlação das var quantitativas
library(ppcor)
vars.quant <- data.train[,c(2,4,5,7,8,9,11)]
pcor(vars.quant, method = "pearson")$estimate
#retirando a variável "pressure"
vars.quant <- data.train[,c(2,5,7,8,9,11)]
pcor(vars.quant, method = "pearson")$estimate

names2  <- names(data.train[,c(2,5,7,8,9,10,11)]) 
f_full2 <- as.formula(paste("corte ~",
                           paste(names2[!names2 %in% "corte"], collapse = " + ")))

glm.full2 <- glm(f_full2, data= data.train, family= binomial(link='logit'))
summary(glm.full2)

vif(glm.full2)

names3  <- names(data.train[,c(2,5,7,8,9,10)]) 
f_full3 <- as.formula(paste("corte ~",
                            paste(names3[!names3 %in% "corte"], collapse = " + ")))

glm.full3 <- glm(f_full3, data= data.train, family= binomial(link='logit'))
summary(glm.full3)

#seleção automatica de variaveis
glm.step <- stepAIC(glm.full3,direction = 'both', trace = TRUE)
summary(glm.step)

# Aplicando o modelo nas amostras  e determinando as probabilidades

glm.prob.train <- predict(glm.step,type = "response")
glm.prob.test <- predict(glm.step, newdata = data.test, type= "response")

# aderência do ajuste logístico
install.packages("rms")
library(rms)
val.prob(glm.prob.train, data.train$corte, smooth = F)[18]

# Comportamento da saida do modelo
hist(glm.prob.test, breaks = 25, col = "lightblue",xlab= "Probabilidades",
     ylab= "Frequência",main= "Regressão Logística")

boxplot(glm.prob.test ~ data.test$corte,col= c("red", "green"), horizontal= T)

--------------------------------------------------------------------------------------------------------------
#avaliando a performance do modelo
  install.packages("hmeasure")
  library(hmeasure) 

glm.train <- HMeasure(data.train$corte,glm.prob.train)
glm.test  <- HMeasure(data.test$corte, glm.prob.test)
glm.train$metrics
glm.test$metrics

install.packages("pROC")
library(pROC)

roc1 <- roc(data.test$corte,glm.prob.test)
y1 <- roc1$sensitivities
x1 <- 1-roc1$specificities

plot(x1,y1, type="n",
     xlab = "1 - Especificidade", 
     ylab= "Sensitividade")
lines(x1, y1,lwd=3,lty=1, col="purple") 
-----------------------------------------------------------------------------------------------------------------

