

AirPassengers
class(AirPassengers)

air = as_tsibble(AirPassengers, index = date)
air$index

class(air$value)


autoplot(AirPassengers)


air %>%
  autoplot(AirPassengers) +
  labs(title = "Monthly totals of international airline passengers") +
  xlab("Year") + ylab("passengers")



### Fase de estimación y contraste ###
# 
# my_tsresiduals <- function (data, ...) {
#   if (!fabletools::is_mable(data)) {
#     abort("gg_tsresiduals() must be used with a mable containing only one model.")
#   }
#   data <- stats::residuals(data)
#   if (n_keys(data) > 1) {
#     abort("gg_tsresiduals() must be used with a mable containing only one model.")
#   }
#   gg_tsdisplay(data, !!sym(".resid"), plot_type = "partial",
#                ...)
# }

# fit2 <- fires_train %>%
#   model(arima = ARIMA(value ~ pdq(0,1,0) + PDQ(1,1,0, period= 7)))
# fit2 %>% my_tsresiduals(lag_max =36)
# report(fit2)
# 
# fit3 <- fires_train %>%
#   model(arima = ARIMA(value ~ pdq(0,1,0) + PDQ(1,1,1)))
# fit3 %>% my_tsresiduals(lag_max =36)
# report(fit3)
# 
# 
# fit4 <- fires_train %>%
#   model(arima = ARIMA(value ~ pdq(0,1,2) + PDQ(1,1,1)))
# fit4 %>% my_tsresiduals(lag_max =36)
# report(fit4)