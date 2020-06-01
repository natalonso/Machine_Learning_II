

AirPassengers

air = as_tsibble(AirPassengers, index = date)
air$index

class(air$value)

air %>%
  autoplot(value) +
  labs(title = "Monthly totals of international airline passengers") +
  xlab("Year") + ylab("passengers")