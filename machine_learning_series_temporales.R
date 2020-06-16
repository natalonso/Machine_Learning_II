
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

Sys.setlocale ("LC_TIME", 'English')
data <- read.csv("amazon.csv")

#### Análisis exploratorio de la señal ####

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


# Dividir en train y test:

fires_train <- fires_rio %>% filter_index("1999 Q1" ~ "2013 Q4")
fires_test <- fires_rio %>% filter_index("2014 Q1" ~ "2015 Q4")

# Gráfica con todos los años:

fires_train %>% gg_season(value, labels = "right")

# Gráfica por trimestres:

fires_train %>% gg_subseries(value, labels = "right")

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

fires_train %>% autoplot(log(value)) +
  labs(y = "Log transformed")

# Lambda -0.33 -> transformacion logaritmica

# Estacionariedad en media: tb hay que analizar si aplicar una diferencia para estabilizar la media

fires_train %>% features(log(value), unitroot_kpss)
fires_train %>% features(difference(log(value), 1), unitroot_kpss)
fires_train %>% autoplot(difference(log(value), 1)) 

# Evalúa el estadístico fuerza estacional FS y sugiere una diferencia estacional si ésta es mayor que 0.64.

fires_train %>%
  mutate(log_turnover = difference(log(value),1)) %>%
  features(log_turnover, unitroot_nsdiffs)

# Análisis de autocorrelación

# Autocorrelacion de la serie temporal: ACF

fires_train %>% 
  ACF(log(value), lag_max = 100) %>% 
  autoplot()

# Autocorrelación parcial de la serie temporal: PACF
fires_train %>% 
  PACF(log(value), lag_max = 100) %>% 
  autoplot()

#### Determinación del modelo ####

# Modelo manual: SAR
fit_manual <- fires_train %>% model(arima = ARIMA(log(value) ~ pdq(1,0,0) + PDQ(1,0,0, period=4)+1))
report(fit_manual) # coef +/- 2*es no solape el cero

fit_automatico <- fires_train %>% model(arima = ARIMA(log(value))) # ajuste automático
report(fit_automatico)

#### Estimacion y contraste ####

# Si el p_valor es muy bajo -> los coeficientes son significativos:

t_stat <- tidy(fit_manual)$statistic
t_stat

p_value <- tidy(fit_manual)$p.value # h0 es que el coef es 0. como p es 0, se rechaza, entonces es significativo
p_value

#### Análisis de residuos ####

# Test para la media: la media debería ser cero, para cumplir el test de normalidad: h0: media cero
# Conclusión: como p_value es "alto", se acepta que la media de los residuos es cero.

aug <-fit_manual %>% augment()

aug %>%
  ggplot(aes(x = .resid)) +
  geom_histogram(bins = 50) +
  ggtitle("Histogram of residuals")

aug %>% 
  ACF(.resid, lag_max = 100) %>% 
  autoplot()

aug %>% 
  PACF(.resid, lag_max = 100) %>% 
  autoplot()


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

fit_manual %>% my_tsresiduals()

t.test(aug$.resid) 

# Test de autocorrelaciones: los residuos tienen que ser incorrelados: h0: autocorrelacion de los residuos es 0
# Conclusión: p_valor "significativo", por tanto aceptamos que los residuos son incorrelados
# dof: grados de libertad: número de variables

aug %>% features(.resid, ljung_box, lag=8, dof=4)

# Test homocedasticidad: h0: varianza cero-residuos homocedásticos
# Conclusión: p_valor "grande", aceptamos que la varianza es cero, por tanto, son homocedástico

log_log <- aug %>% as_tibble() %>% 
  group_by(week(index)) %>% 
  summarize(mean_resid = log(mean(.resid+1)), std_resid = log(sd(.resid+1))) 

summary(lm(std_resid~mean_resid, log_log))

# Test de normalidad: Jarque Bera test: h0: hay normalidad
# Conclusión: p_value "grande" -> aceptamos que hay normalidad

jb.norm.test(na.omit(aug$.resid))
qqnorm(aug$.resid, sub="Figura 09: Gráfico Q para evaluar normalidad");qqline(aug$.resid)

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
  autoplot() +
  ggtitle("Forecasts for fires in Rio") +
  xlab("Year") +
  guides(colour = guide_legend(title = "Forecast"))

plot(fc$value, type='l', col="red")
plot(fires_test$value, type='l', col="blue")

fires <- rbind(as_data_frame(fires_train), as_data_frame(fires_test))
fires_rio <- fires_rio %>% filter_index("1999 Q1" ~ "2015 Q4")
  
class(fires_rio)

test_err <- fc %>% 
  accuracy(fires_rio) %>% 
  select(-c(.model, .type, ME, MPE, ACF1 )) %>% 
  mutate(Evaluation='Test')

# Show errors together
bind_rows(test_err, resids) %>% select(Evaluation, everything())



