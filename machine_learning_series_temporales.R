

library(tsibble)
library(ggplot2)
library(dplyr)
library(tidyr)
library(zoo)
library(survival)
library(ggfortify)

data <- read.csv("amazon.csv")
Sys.setlocale ("LC_TIME", 'English')



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

  mutate(new_date = as.yearmon(paste(month, "-", year), "%B - %Y")) %>% 
  group_by(new_date) %>%
  summarise(total = sum(as.integer(number))) 

data_rio_ts <- ts(data_rio$total, start = c(1998,1), frequency = 12)
autoplot(data_rio_ts) +
labs(title = "Number of fires in Rio") +
xlab("Year") + ylab("Fires")


###################### SEGUIR POR AQUI ######################

fires_rio <- as_tsibble(data_rio, index = new_date)
fires_rio$new_date



