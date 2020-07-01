

library(RSNNS) 
library(quantmod)


rawData=scan('https://www.diegocalvo.es/wp-content/uploads/2016/09/datos-serie-temporal.txt')
plot(rawData)
tsData = ts(rawData, start = c(1966,1), frequency = 12)
print(tsData)
plot(tsData)

stsData<-as.ts(tsData, F)
class(stsData)
class(tsData)

stsDataN<- (stsData-min(stsData))/(max(stsData)-min(stsData))
plot(stsDataN)



#definimos un conjunto de datos para train 90% y 10% para test 
set.seed(1) 
tamano_total <- length(stsDataN) 
tamano_total

tamano_train <- round(tamano_total*9/12, digits = 0) 
tamano_train

train <- 0:(tamano_train-1)
train

test<-(tamano_train):tamano_total
test


y<-as.zoo(stsDataN) 
class(y)

x1<-Lag(y,k=1) 
x2<-Lag(y,k=2) 
x3<-Lag(y,k=3) 
x4<-Lag(y,k=4) 
x5<-Lag(y,k=5) 
x6<-Lag(y,k=6) 
x7<-Lag(y,k=7) 
x8<-Lag(y,k=8) 
x9<-Lag(y,k=9) 
x10<-Lag(y,k=10) 
x11<-Lag(y,k=11) 
x12<-Lag(y,k=12) 
slogN<-cbind(y,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12)



stsDataN<-slogN[-c(1:12),]

inputs<-stsDataN[,2:13]
outputs<-stsDataN[,1]


fit<-elman(inputs[train],
           outputs[train], 
           size=c(9,2), # número de capas y de neuronas (dos capas, una de 9 neuronas y otra de 2)
           learnFuncParams=c(0.1), # ritmo de aprendizaje
           maxit=5000) # máximo número de iteraciones

y<-as.vector(outputs[-test]) 
plot(y,type="l")

plotIterativeError(fit, main = "Iterative Error for 3,2 Neuron elman Model")

pred<-predict(fit,inputs[-test]) 

# Estas dos líneas van juntas
plot(y,type="l")
lines(pred,col="red")

predictions<-predict(fit,inputs[-train])

valuesPred <- predictions*(max(stsData)-min(stsData)) + min(stsData) 
valuesPred

x <- 1:(tamano_total+length(valuesPred)) 
length(x)

y <- c(as.vector(tsData),valuesPred) 
length(y)

plot(x, y) 
plot(x[1:tamano_total], 
     y[1:tamano_total],
     col = "blue", 
     type="l") 
lines(x[(tamano_total):length(x)], y[(tamano_total):length(x)],col="red")

