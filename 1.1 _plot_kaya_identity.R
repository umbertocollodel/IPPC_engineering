# Code to generate overall graph (problems with the rbind - does not work)

ShapleyWorld %>% 
  select(Year,Period,CO2Growth,contains("Contribution")) %>%
  filter(!(Year %in% c(2010, 2020) & Period != "Historical")) %>% 
  filter(str_detect(Period,"Baseline")) %>% 
  split(.$Period) %>% 
  map(~ .x %>% group_by(Year)) %>%
  map(~ .x %>% summarise(CO2Growth = mean(CO2Growth),
                  PopContribution = mean(PopContribution),
                  IncContribution = mean(IncContribution),
                  EnergryContribution = mean(EnergryContribution),
                  CarbonContribution = mean(CarbonContribution),) %>% 
        ungroup()) %>% 
  map(~ .x %>% pivot_longer(cols = PopContribution:CarbonContribution, names_to = "Var", values_to = "Value")) %>% 
  map(~ .x %>% mutate(Var = case_when(Var =="PopContribution" ~ "Pop",
            Var == "IncContribution" ~ "Income",
            Var == "EnergryContribution" ~ "Energy Intensity",
            T ~ "Carbon Intensity"))) %>% 
  map(~ .x %>% rbind(.x$Historical)) %>% 
  map(~ .x %>% 
    ggplot() +
    geom_col(aes(x = Year, y = Value, fill = Var), size = 1, position = "stack") +
    geom_point(aes(x = Year, y = CO2Growth), col = "black", size = 2) +
    theme_bw() + 
    theme(axis.title = element_blank(),
          text = element_text(size = 14),
          legend.position = "bottom",
          panel.grid.minor = element_blank(), 
          legend.title = element_blank()) +
    # scale_fill_manual(name=NULL,
    #                    values = c("CO2 Emission" = IMF.Black,
    #                               "GDP Per Capita" = IMF.Blue,
    #                               "Energy Intensity" = IMF.Orange,
    #                               "Population" = IMF.Purple,
    #                               "Carbon Intensity" = IMF.Green)) +
    ggtitle("Kaya identity: drivers of CO2 emissions", subtitle = "(Contribution to CO2 Emission Growth)") +
    scale_x_continuous(n.breaks = 10))