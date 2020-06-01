

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