
library(tsibble)
library(ggplot2)
library(dplyr)
library(tidyr)
library(zoo)
library(survival)
library(ggfortify)
library(tidyverse)
library(fpp3)
library(GGally)
library(normtest)
library(lmtest)
library(urca)

library(RSNNS)
library(quantmod)

# install.packages("urca")
# install.packages("lmtest")

# setwd("C:/Users/susi_/Desktop/Machine Learning2/Series_Temporales/practica_local")
# setwd("C:/Users/Beatriz/Desktop/Máster/3er trimestre/Machine Learning II/Práctica")
# setwd("C:/Users/natal/OneDrive/Documentos/0_Universidad/10-Master/2_Curso_2019-2020/Tercer_Trimestre/0_Machine_Learning_2/REPO DEF")

Sys.setlocale ("LC_TIME", 'English')
data <- read.csv("amazon.csv")


###################################################
# AJUSTE MANUAL CON FIRES ACUMULADOS
###################################################


#### Análisis exploratorio de la señal ####

data_all <- data %>%
  select(year, month, number) %>% 
  mutate(month = if_else(month == "Abril", "Apr", 
                 if_else(month == "Agosto", "Aug", 
                 if_else(month == "Dezembro", "Dec", 
                 if_else(month == "Fevereiro", "Feb", 
                 if_else(month == "Janeiro", "Jan", 
                 if_else(month == "Julho", "Jul", 
                 if_else(month == "Junho", "Jun", 
                 if_else(month == "Maio", "May", 
                 if_else(month == "Setembro", "Sep", 
                 if_else(month == "Novembro", "Nov",  
                 if_else(month == "Outubro", "Oct", "Mar")))))))))))) %>% 

  mutate(new_date = as.yearmon(paste(month, "-", year), "%B - %Y")) %>% 
  group_by(new_date) %>%
  summarise(total = sum(as.integer(number))) %>% 
  mutate(new_quarter = quarter(new_date, with_year = TRUE)) %>%
  group_by(new_quarter) %>%
  summarise(total_quarter = sum(as.integer(total)))


ts_data_all<- ts(data_all$total_quarter, start = c(1998,1), frequency = 4)

# Normalizamos:
stsDataN <- (ts_data_all-min(ts_data_all))/(max(ts_data_all)-min(ts_data_all))
plot(stsDataN)


set.seed(1) 

tamano_total <- length(stsDataN) - 4
tamano_total

tamano_train <- tamano_total - 8
tamano_train

train <- 1:(tamano_train)
train
length(train)

test<-(tamano_train+1):(tamano_total)
test
length(test)


y<-as.zoo(stsDataN) 
class(y)
y


x1<-Lag(y,k=1) 
x2<-Lag(y,k=2) 
x3<-Lag(y,k=3) 
x4<-Lag(y,k=4) 

slogN<-cbind(y,x1,x2,x3,x4)

stsData_lag<-slogN[-c(1:4),] # quitar los NA: se quita 1998

inputs<-stsData_lag[,2:5] # los inputs son los retardos, variables
length(inputs)

outputs<-stsData_lag[,1] # el output es la variable respuesta
length(outputs)

fit<-elman(inputs[train],
           outputs[train], 
           size=c(15, 3), # número de capas y de neuronas (dos capas, una de 9 neuronas y otra de 2)
           learnFuncParams=c(0.1), # ritmo de aprendizaje
           maxit=5000) # máximo número de iteraciones

y<-as.vector(outputs[,-test]) 
plot(y,type="l")

plotIterativeError(fit, main = "Iterative Error for 3,2 Neuron elman Model")

pred_train<-predict(fit,inputs[-test])
pred_train

pred_test <- predict(fit,inputs[-train])
pred_test

# Estas dos líneas van juntas
plot(y,type="l")
lines(pred,col="red")

# Desnormalizas 

valuesPred_test <- pred_test*(max(ts_data_all)-min(ts_data_all)) + min(ts_data_all) 
valuesPred_test

valuesPred_train <- pred_train*(max(ts_data_all)-min(ts_data_all)) + min(ts_data_all) 
valuesPred_train

x <- 1:(tamano_total+length(valuesPred_test)+4) 
length(x)

y <- c(as.vector(ts_data_all), valuesPred_test) 
length(y)

plot(x, y)

plot(x[1:tamano_total], 
     y[1:tamano_total],
     col = "blue", 
     type="l")
lines(x[(tamano_total):length(x)], y[(tamano_total):length(x)],col="red")


getPerformance = function(pred, val) {
  res = pred - val
  MAE = sum(abs(res))/length(val)
  MAE_std = sd(abs(res))
  MAPE = mean(100*abs(res)/val)
  MAPE_std = sd(100*abs(res)/val)
  MSE = sum(res^2)/length(val)
  MSE_std = sd((res)**2)
  RMSE = sqrt(MSE)
  RMSE_std = sqrt(MSE_std)
  perf = data.frame(MAE,MAE_std,MAPE,MAPE_std,RMSE,RMSE_std)
}



# valor real y prediccion

accuracy_train_elman <- getPerformance(valuesPred_train, ts_data_all[train+4]) %>% 
  mutate(Evaluation='Train Red Elman') 
accuracy_train_elman

accuracy_test_elman <- getPerformance(valuesPred_test, ts_data_all[test+4]) %>% 
  mutate(Evaluation='Test Red Elman')
accuracy_test_elman

bind_rows(accuracy_train_elman, accuracy_test_elman) %>% select(Evaluation, everything())






