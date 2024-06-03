# Script to compare graphically the evolution of the scenarios variables (kaya identity)
# with the constructed historical to make sure they match


# Plot all scenarios: -----

df <- World %>% 
  filter(str_detect(Period,"Baseline|Historical")) %>% 
  summarise(`CarbonIntensity`= mean(`CarbonIntensity`,na.rm=T),
            `EnergyIntensity`= mean(`EnergyIntensity`,na.rm=T),
            GDPperCapita = mean(GDPperCapita,na.rm=T),
            `CO2OWID (kt)` = mean(`CO2OWID (kt)`,na.rm=T),
            PrimaryEnergyUse = mean(PrimaryEnergyUse,na.rm=T),
            Pop = mean(Pop, na.rm=T),
            .by = c(Year,Period)) %>%
  pivot_longer(cols = CarbonIntensity:Pop,names_to = "var",values_to = 
                 "values") 

df %>% 
  ggplot(aes(x=Year,y=values,col=Period)) +
  geom_line() +
  facet_wrap(~ var, scales = "free_y") +
  theme_minimal()

# Plot: scenario 1 and 5 components of Kaya identity ----

df %>% 
  ggplot(aes(x=Year)) +
  geom_line(data=. %>% filter(str_detect(Period,"Historical|1|5")),aes(y=values,col=Period), size=1.5) +
  geom_line(data=. %>% filter(str_detect(Period,"2|3|4")),aes(y=values,group=Period),col="grey",alpha=0.5,size=1.5) +
  facet_wrap(~ var, scales = "free_y") +
  theme_minimal()

# Plot: scenario 2,3,4 - components of Kaya identity ----

df %>% 
  ggplot(aes(x=Year)) +
  geom_line(data=. %>% filter(str_detect(Period,"Historical|2|3|4")),aes(y=values,col=Period), size=1.5) +
  geom_line(data=. %>% filter(str_detect(Period,"1|5")),aes(y=values,group=Period),col="grey",alpha=0.5,size=1.5) +
  facet_wrap(~ var, scales = "free_y") +
  theme_minimal()