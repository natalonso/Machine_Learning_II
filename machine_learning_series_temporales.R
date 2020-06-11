
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

# install.packages("lmtest")
# install.packages("urca")
setwd("C:/Users/susi_/Desktop/Machine Learning2/Series_Temporales/practica_local")
Sys.setlocale ("LC_TIME", 'English')
data <- read.csv("amazon.csv")


data_rio <- data %>%
  filter(state == "Rio") %>% 
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


ts_data_rio <- ts(data_rio$total_quarter, start = c(1998,1), frequency = 4)
fires_rio <- as_tsibble(ts_data_rio, index = new_quarter)

fires_rio %>%
  autoplot(value) +
  labs(title = "Fires per year") +
  xlab("Year") + ylab("Fires")


fires_train <- fires_rio %>% filter_index(. ~ "2015-12")
fires_test <- fires_rio %>% filter_index(2016 ~ .)

fires_train %>%
  gg_season(value, labels = "right")

#Gráfica por trimestres:
fires_train %>% gg_subseries(value, labels = "right")

#Gráficos de retardo:

fires_train %>% gg_lag(value, geom="point")

fires_train %>%
  model(seats = feasts:::STL(value)) %>%
  components() %>%
  autoplot()

#### Estacionariedad en varianza: para estabilizar la varianza. ####

lambda <- fires_train %>%
  features(value, features = guerrero) %>% 
  pull(lambda_guerrero)
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
fires_train %>% ACF(value, lag_max = 140) %>% autoplot()

# estimate of the partial autocorrelation function of a (possibly multivariate) time series: PACF
pacf(fires_train, lag.max=140)
fires_train %>% PACF(value, lag_max = 140) %>% autoplot()

#### Determinación del modelo ####

fitARIMA <- arima(fires_train$value, 
                  order=c(1,1,1), 
                  seasonal = list(order = c(1,0,0), period = 4), 
                  method="ML")



coeftest(fitARIMA) 

### Fase de estimación y contraste ###

my_tsresiduals <- function (data, ...) {
  if (!fabletools::is_mable(data)) {
    abort("gg_tsresiduals() must be used with a mable containing only one model.")
  }
  data <- stats::residuals(data)
  if (n_keys(data) > 1) {
    abort("gg_tsresiduals() must be used with a mable containing only one model.")
  }
  gg_tsdisplay(data, !!sym(".resid"), plot_type = "partial",  
               ...)
}

fit2 <- fires_train %>%
  model(arima = ARIMA(value ~ pdq(0,1,0) + PDQ(1,1,0, period= 7)))
fit2 %>% my_tsresiduals(lag_max =36)
report(fit2)

fit3 <- fires_train %>%
  model(arima = ARIMA(value ~ pdq(0,1,0) + PDQ(1,1,1)))
fit3 %>% my_tsresiduals(lag_max =36)
report(fit3)


fit4 <- fires_train %>%
  model(arima = ARIMA(value ~ pdq(0,1,2) + PDQ(1,1,1)))
fit4 %>% my_tsresiduals(lag_max =36)
report(fit4)

