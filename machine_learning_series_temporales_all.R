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
# setwd("C:/Users/susi_/Desktop/Machine Learning2/Series_Temporales/practica_local")
# setwd("C:/Users/Beatriz/Desktop/Máster/3er trimestre/Machine Learning II/Práctica")
# setwd("C:/Users/natal/OneDrive/Documentos/0_Universidad/10-Máster/2_Curso_2019-2020/Tercer_Trimestre/0_Machine_Learning_2/REPO DEF")

Sys.setlocale ("LC_TIME", 'English')
data <- read.csv("amazon.csv")

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
fires <- as_tsibble(ts_data_all, index = new_quarter)

fires %>%
  autoplot(value) +
  labs(title = "Fires per year") +
  xlab("Year") + ylab("Fires")

fires <- fires %>% filter_index("1999 Q1" ~ "2014 Q4")

# Dividir en train y test:

fires_train <- fires %>% filter_index("1999 Q1" ~ "2012 Q4")
fires_test <- fires %>% filter_index("2013 Q1" ~ "2014 Q4")

# Gráfica con todos los años:

fires_train %>% gg_season(value, labels = "right")

# Gráfica por trimestres:

fires_train %>% gg_subseries(value)

# Gráficos de retardo:

fires_train %>% gg_lag(value, geom="point")

# Descomponer la señal en tendencia + estacionalidad + aleatorio:

fires_train %>%
  model(seats = feasts:::STL(value)) %>%
  components() %>%
  autoplot()

# Comprobar estacionariedad en varianza: para estabilizar la varianza.

lambda <- fires_train %>%
  features(value, features = guerrero) %>% 
  pull(lambda_guerrero)
lambda # lambda próximo a cero -> transformacion log
#lambda 1.61 -> no transformacion ???

fires_train %>% autoplot(value)

# Estacionariedad en media: tb hay que analizar si aplicar una diferencia para estabilizar la media

fires_train %>% features(value, unitroot_kpss)
fires_train %>% features(difference(value, 1), unitroot_kpss)
fires_train %>% autoplot(difference(value, 1)) 

# Evalúa el estadístico fuerza estacional FS y sugiere una diferencia estacional si ésta es mayor que 0.64.

fires_train %>%
  mutate(log_turnover = difference(value,1)) %>%
  features(log_turnover, unitroot_nsdiffs)

# Análisis de autocorrelación

# Autocorrelacion de la serie temporal: ACF

fires_train %>% 
  ACF(value, lag_max = 100) %>% 
  autoplot()

# Autocorrelación parcial de la serie temporal: PACF
fires_train %>% 
  PACF(value, lag_max = 100) %>% 
  autoplot()

#### Determinación del modelo ####

# Modelo manual: SAR
fit_manual <- fires_train %>% model(arima = ARIMA(value ~ pdq(1,0,0) + PDQ(1,0,0, period = 4)))

fit_automatico <- fires_train %>% model(arima = ARIMA(value))
report(fit_automatico) # coef +/- 2*es no solape el uno

kk <- Arima(fires_train$value, order = c(1,0,0), seasonal=list(order=c(1,0,0),period=4), lambda = 1.6)
kk


fit_automatico <- fires_train %>% model(arima = ARIMA(value) + lambda) # ajuste automático
report(fit_automatico)

#### Estimacion y contraste ####

# Si el p_valor es muy bajo -> los coeficientes son significativos:

t_stat <- tidy(fit_manual)$statistic
t_stat

p_value <- tidy(fit_manual)$p.value # h0 es que el coef es 0. como p es 0, se rechaza, entonces es significativo
p_value

#### Análisis de residuos ####

aug <-fit_manual %>% augment()

aug %>%
  ggplot(aes(x = .resid)) +
  geom_histogram(bins = 50) +
  ggtitle("Histogram of residuals")

residual <- stats::residuals(fit_manual)
gg_tsdisplay(residual,!!sym(".resid"), plot_type = "partial", lag_max = 90)

# Test para la media: la media debería ser cero, para cumplir el test de normalidad: h0: media cero
# Conclusión: como p_value es "alto", se acepta que la media de los residuos es cero.

t.test(aug$.resid) 

# Test de autocorrelaciones: los residuos tienen que ser incorrelados: h0: autocorrelacion de los residuos es 0
# Conclusión: p_valor "significativo", por tanto aceptamos que los residuos son incorrelados
# dof: grados de libertad: número de variables

aug %>% features(.resid, ljung_box, lag=8, dof=3)

# Test homocedasticidad: h0: varianza cero-residuos homocedásticos
# Conclusión: p_valor "grande", aceptamos que la varianza es cero, por tanto, son homocedástico

log_log <- aug %>% as_tibble() %>% 
  group_by(quarter(index)) %>% 
  summarize(mean_resid = (mean(.resid+1)), std_resid = (sd(.resid+1)))

summary(lm(std_resid~mean_resid, log_log))

# Test de normalidad: Jarque Bera test: h0: hay normalidad
# Conclusión: p_value "grande" -> aceptamos que hay normalidad

jb.norm.test(na.omit(aug$.resid))
qqnorm(aug$.resid, sub="Gráfico Q para evaluar normalidad");qqline(aug$.resid)

shapiro.test(aug$.resid)

#### Predicción ####

# Residual accuracy
resids <- fit_manual %>% 
  accuracy() %>% 
  select(-c(.model, .type, ME, MPE, ACF1 )) %>% 
  mutate(Evaluation='Training') 

# Forecasting
fc <- fit_manual %>%
  forecast(h=8) 

fc %>%
  autoplot(fires, level = NULL, col="red", lwd = 2) +
  ggtitle("Forecasts for fires in Amazonas") +
  xlab("Year") + ylab("Number of fires")

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


accuracy_train <-getPerformance(aug$.fitted,fires_train$value) %>% 
  mutate(Evaluation='Train') 
accuracy_train

accuracy_test <-getPerformance(fc$value,fires_test$value) %>% 
  mutate(Evaluation='Test')
accuracy_test

# Show errors together
bind_rows(accuracy_train, accuracy_test) %>% select(Evaluation, everything())

#exp(fit_manual$arima[[1]]$fit$est$.fitted)

aug %>%  ggplot() +
  geom_line(aes(x = index, y = .fitted), color="navy") +
  geom_line(aes(x = index, y = value), color="gray24") +
  # geom_line(data=air_forecast, aes(x = index, y = value), color="red4") +
  ggtitle("SARIMA train fitted values") +
  xlab('Dates') +
  ylab('Fires') + facet_wrap(vars(year(index)), scales = 'free')



