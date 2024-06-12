############# Script to generate pandemic shocks and their impact on Co2 emissions
################################################################################

# Load the necessary libraries
library(forecast)

####### Load WEO GDP data: -----

growth <- NGDP_R %>% 
  filter(ifscode == 1) %>% 
  select(-ifscode) %>% 
  slice(-nrow(.))


# Convert the data into a time series object and fit ARIMA:

fit <- ts(growth$NGDPR, start=c(1950), frequency=1) %>% 
  auto.arima()

# Forecast future values:

future_years <- 2029:2100

forecasted_values <- forecast(fit, h=length(future_years)) %>% 
  as_tibble() %>% 
  select(`Point Forecast`) %>%
  mutate(Year = 2029:2100) %>% 
  setNames(c("NGDPR","Year"))

# Bind with historical data: 
  
gdp_forecast <- rbind(growth,forecasted_values) %>% 
  mutate(SCENARIO = "Historical") 
    


    