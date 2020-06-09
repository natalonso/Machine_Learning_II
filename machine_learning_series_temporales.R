
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

# install.packages("lmtest")

data <- read.csv("amazon.csv")

data_rio <- data %>%
  filter(state == "Rio") %>% 
  select(year, month, number) %>% 
  mutate(month = if_else(month == "Abril", "April", 
                 if_else(month == "Agosto", "August", 
                 if_else(month == "Dezembro", "December", 
                 if_else(month == "Fevereiro", "February", 
                 if_else(month == "Janeiro", "January", 
                 if_else(month == "Julho", "July", 
                 if_else(month == "Junho", "June", 
                 if_else(month == "Maio", "May", 
                 if_else(month == "Setembro", "September", 
                 if_else(month == "Novembro", "November", 
                 if_else(month == "Outubro", "October", "March")))))))))))) %>% 
  
  group_by(year, month) %>% 
  summarise(total = sum(as.integer(number)))


ts_data_rio <- ts(data_rio$total, start = c(1998,1), frequency = 12)
fires_rio <- as_tsibble(ts_data_rio, index = new_date)

fires_rio %>%
  autoplot(value) +
  labs(title = "Fires per year") +
  xlab("Year") + ylab("Fires")


fires_train <- fires_rio %>% filter_index(. ~ "2015-12")
fires_test <- fires_rio %>% filter_index(2016 ~ .)

fires_train %>%
  gg_season(value, labels = "right")

fires_train %>%
  model(seats = feasts:::SEATS(value)) %>%
  components() %>%
  autoplot()

#### Estacionariedad en varianza: para estabilizar la varianza. ####

lambda <- fires_train %>%
  features(value, features = guerrero) %>% pull(lambda_guerrero)
lambda # lambda próximo a cero -> transformacion log

fires_train %>% autoplot(log(value)) +
  labs(y = "Log transformed")

#### Estacionariedad en media: tb hay que analizar si aplicar una diferencia para estabilizar la media ####

# Transformación logarítmica no válida: da NA, probamos tb sin log()

fires_train %>% features(log(value), unitroot_kpss)
fires_train %>% features(difference(log(value), 1), unitroot_kpss)
fires_train %>% autoplot(difference(log(value), 1)) 

# Evalúa el estadístico fuerza estacional FS y sugiere una diferencia estacional si esta es mayor que 0.64.
fires_train %>%
  mutate(log_turnover = difference(log(value),1)) %>%
  features(log_turnover, unitroot_nsdiffs)

# Probamos sin log:
fires_train %>% features((value), unitroot_kpss)
fires_train %>% features(difference((value), 1), unitroot_kpss)
fires_train %>% autoplot(difference(value, 1)) 
fires_train %>%
  mutate(log_turnover = difference((value),1)) %>%
  features(log_turnover, unitroot_nsdiffs) # como es menor que 0.64 no indica diferencia estacional

#### Autocorrelación ####

# estimate of the autocorrelation function of a (possibly multivariate) time series: ACF
acf(fires_train,lag.max=140)

# estimate of the partial autocorrelation function of a (possibly multivariate) time series: PACF
pacf(fires_train, lag.max=140)

#### Determinación del modelo ####

fitARIMA <- arima(fires_train, 
                  order=c(1,1,1), 
                  seasonal = list(order = c(1,0,0), period = 12), 
                  method="ML")



coeftest(fitARIMA)
