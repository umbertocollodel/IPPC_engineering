######## Script to generate comparison of Shapley decomp. for OECD vs. non-OECD 
######## countries




#read SSP data OECD level - forecasts under scenarios
################################################################################################

#add in SSP data

SSP <- read_csv("Data/SSP_IAM_V2_201811.csv")
str(SSP)
unique(SSP$VARIABLE)

#test
unique(SSP$MODEL)
#Test <- SSP[SSP$MODEL == "AIM/CGE",]
#only keep world
unique(SSP$REGION)
OECDSSP <- SSP[SSP$REGION %in% c("World", "R5.2OECD"),]
# #keep only SSP2
# Test <- Test[grep("SSP2", Test$SCENARIO),]

rm(SSP)

#pivot longer years
names(OECDSSP)
OECDSSP <- pivot_longer(OECDSSP, "2005":"2100", names_to = "Year", values_to = "Value")
class(OECDSSP$Year)
OECDSSP$Year <- as.numeric(OECDSSP$Year)
OECDSSP$UNIT <- NULL

#pivot wider years
OECDSSP <- pivot_wider(OECDSSP, names_from = "VARIABLE", values_from = "Value")

#keep relevant variables
names(OECDSSP)
OECDSSP$FertilityRate <- NA
OECDSSP$TotalDeaths <- NA
OECDSSP$Period <- "SSP"
OECDSSP <- OECDSSP[, c("Year", "REGION", "GDP|PPP", "Emissions|CO2", "Population", "FertilityRate", "TotalDeaths", "Primary Energy", "Period", "MODEL", "SCENARIO")]

#modify emission and population unit
OECDSSP$`Emissions|CO2` <- OECDSSP$`Emissions|CO2` * 1000
OECDSSP$Population <- OECDSSP$Population * 1000
OECDSSP$`GDP|PPP` <- OECDSSP$`GDP|PPP` * 1000
OECDSSP$`Primary Energy` <- OECDSSP$`Primary Energy` * 1000

#create world SSP
WorldSSP <- OECDSSP[OECDSSP$REGION == "World",]
OECDSSP <- OECDSSP[OECDSSP$REGION == "R5.2OECD",]

#create nonOECDSSP
nonOECDSSP <- WorldSSP
nonOECDSSP$REGION <- "nonOECD"
nonOECDSSP$`GDP|PPP` <- nonOECDSSP$`GDP|PPP` - OECDSSP$`GDP|PPP`
nonOECDSSP$`Emissions|CO2` <- nonOECDSSP$`Emissions|CO2` - OECDSSP$`Emissions|CO2`
nonOECDSSP$Population <- nonOECDSSP$Population - OECDSSP$Population
nonOECDSSP$`Primary Energy` <- nonOECDSSP$`Primary Energy` - OECDSSP$`Primary Energy`

#GDP per capita and carbon intensity
names(OECDSSP)
OECDSSP$GDPperCapita <- OECDSSP$`GDP|PPP`/OECDSSP$Population
OECDSSP$EnergyIntensity <- OECDSSP$`Primary Energy` / OECDSSP$`GDP|PPP`
OECDSSP$CarbonIntensity <- OECDSSP$`Emissions|CO2` / OECDSSP$`Primary Energy`

nonOECDSSP$GDPperCapita <- nonOECDSSP$`GDP|PPP`/nonOECDSSP$Population
nonOECDSSP$EnergyIntensity <- nonOECDSSP$`Primary Energy` / nonOECDSSP$`GDP|PPP`
nonOECDSSP$CarbonIntensity <- nonOECDSSP$`Emissions|CO2` / nonOECDSSP$`Primary Energy`

#merge OECD and non-OECD together
OECDSSP$REGION <- "OECD"
WorldSSP <- rbind(OECDSSP, nonOECDSSP)

names(OECDSSP)

################################################################################################


#shapley decomposition - OECD vs. non-OECD
################################################################################################

#shapley function
Shapley <- function(a0, b0, c0, d0, at, bt, ct, dt) {
  deltaa <- at - a0
  abar <- deltaa * ((b0*c0*d0 + bt * ct *dt) / 4 + (bt * c0 * d0 + b0 * ct * d0 + b0 * c0 * dt + bt * ct *d0 +  + bt * c0 *dt +  + b0 * ct *dt) / 12)
  
  deltab <- bt - b0
  bbar <- deltab * ((a0*c0*d0 + at * ct *dt) / 4 + (at * c0 * d0 + a0 * ct * d0 + a0 * c0 * dt + at * ct *d0 +  + at * c0 *dt +  + a0 * ct *dt) / 12)
  
  deltac <- ct - c0
  cbar <- deltac * ((b0*a0*d0 + bt * at *dt) / 4 + (bt * a0 * d0 + b0 * at * d0 + b0 * a0 * dt + bt * at *d0 +  + bt * a0 *dt +  + b0 * at *dt) / 12)
  
  deltad <- dt - d0
  dbar <- deltad * ((b0*c0*a0 + bt * ct *at) / 4 + (bt * c0 * a0 + b0 * ct * a0 + b0 * c0 * at + bt * ct *a0 +  + bt * c0 *at +  + b0 * ct *at) / 12)
  
  c(abar, bbar, cbar, dbar, use.names = T)
  
}

#chart
WorldSSP$Pop <- WorldSSP$Population
ShapleyWorld <- WorldSSP[WorldSSP$Year %in% seq(1900, 2100, 10),]

ShapleyWorld <- group_by(ShapleyWorld, REGION, MODEL) %>% 
  mutate(PopGrowth = Pop / lag(Pop) * 100 - 100,
         IncomeGrowth = GDPperCapita / lag(GDPperCapita) * 100 - 100,
         CO2Growth = `Emissions|CO2` / lag(`Emissions|CO2`) * 100 - 100,
         EnergyIntensityGrowth = EnergyIntensity / lag(EnergyIntensity) * 100 - 100,
         CarbonIntensityGrowth = CarbonIntensity / lag(CarbonIntensity) * 100 - 100) %>% 
  ungroup()

#ShapleyWorld$`CO2OWID (kt)` <- round(ShapleyWorld$`CO2OWID (kt)`)


rm(ShapleyValue)

for (i in 1:nrow(ShapleyWorld)) {
  
  if (exists("ShapleyValue")) {
    temp <- Shapley(ShapleyWorld$Pop[i], ShapleyWorld$GDPperCapita[i], ShapleyWorld$EnergyIntensity[i], ShapleyWorld$CarbonIntensity[i],
                    ShapleyWorld$Pop[i+1], ShapleyWorld$GDPperCapita[i+1], ShapleyWorld$EnergyIntensity[i+1], ShapleyWorld$CarbonIntensity[i+1])
    temp <- data.frame(abar = temp[1], bbar = temp[2], cbar = temp[3], dbar = temp[4])
    
    ShapleyValue <- rbind(ShapleyValue,
                          temp)
  } else {
    ShapleyValue <- Shapley(ShapleyWorld$Pop[i], ShapleyWorld$GDPperCapita[i], ShapleyWorld$EnergyIntensity[i], ShapleyWorld$CarbonIntensity[i],
                            ShapleyWorld$Pop[i+1], ShapleyWorld$GDPperCapita[i+1], ShapleyWorld$EnergyIntensity[i+1], ShapleyWorld$CarbonIntensity[i+1])
    ShapleyValue <- data.frame(abar = ShapleyValue[1], bbar = ShapleyValue[2], cbar = ShapleyValue[3], dbar = ShapleyValue[4])
  }
}

ShapleyValue <- ShapleyValue[-nrow(ShapleyValue),]
ShapleyWorld$LaggedEmission <- lag(ShapleyWorld$`Emissions|CO2`)
ShapleyWorld <- ShapleyWorld[-1,]

#merge 
ShapleyWorld <- cbind(ShapleyWorld, ShapleyValue)
ShapleyWorld$Emission <- ShapleyWorld$`Emissions|CO2`

#calculate contribution
ShapleyWorld$PopContribution <- ShapleyWorld$abar / ShapleyWorld$LaggedEmission * 100
ShapleyWorld$IncContribution <- ShapleyWorld$bbar / ShapleyWorld$LaggedEmission * 100
ShapleyWorld$EnergryContribution <- ShapleyWorld$cbar / ShapleyWorld$LaggedEmission * 100
ShapleyWorld$CarbonContribution <- ShapleyWorld$dbar / ShapleyWorld$LaggedEmission * 100

#remove 2010 or 2020 for SSP models
ShapleyWorld <- ShapleyWorld[!ShapleyWorld$Year %in% c(2010, 2020),]

rm(temp)

ShapleyWorld <- group_by(ShapleyWorld, REGION, SCENARIO, Year) %>% 
  summarise(CO2Growth = mean(CO2Growth),
            PopContribution = mean(PopContribution),
            IncContribution = mean(IncContribution),
            EnergryContribution = mean(EnergryContribution),
            CarbonContribution = mean(CarbonContribution),) %>% 
  ungroup()

# From wide to long

ShapleyWorld <- pivot_longer(ShapleyWorld, PopContribution:CarbonContribution, names_to = "Var", values_to = "Value")

ShapleyWorld$Var <- case_when(ShapleyWorld$Var == "PopContribution" ~ "Pop",
                              ShapleyWorld$Var == "IncContribution" ~ "Income",
                              ShapleyWorld$Var == "EnergryContribution" ~ "Energy Intensity",
                              T ~ "Carbon Intensity")

############ Plot Shapley decomposition: OECD vs non-OECD (historical + SSP scenario) by scenario -----

dir.create("output/figures/OECD-non")

#ShapleyWorld_hist <- ShapleyWorld %>% 
#  filter(Period == "Historical")



oecd_kaya_plots <- ShapleyWorld %>%
  split(.$SCENARIO) %>% 
  #  map(~ rbind(ShapleyWorld_hist,.x)) %>% 
  imap(~ ggplot(.x) +
         geom_col(aes(x = Year, y = Value, fill = Var), width = 7, position = "stack") +
         geom_line(aes(x = Year, y = CO2Growth,col = "CO2 Growth"), size = 0.8) +
         geom_point(aes(x = Year, y = CO2Growth),col = "#a37c00", size = 2.5, alpha=0.3) +
         facet_wrap(~ REGION) +
         theme_bw() + 
         theme(axis.title = element_blank(),
               text = element_text(size = 14),
               legend.position = "bottom",
               panel.grid.minor = element_blank(), 
               legend.title = element_blank()) +
         geom_hline(yintercept=0, size=0.5) +
         geom_vline(xintercept = 2025, size=0.5) +
         geom_text(y=100,x=2080, label=.y) +
         # scale_fill_manual(name=NULL,
         #                    values = c("CO2 Emission" = IMF.Black,
         #                               "GDP Per Capita" = IMF.Blue,
         #                               "Energy Intensity" = IMF.Orange,
         #                               "Population" = IMF.Purple,
         #                               "Carbon Intensity" = IMF.Green)) +
         ggtitle("Kaya identity: drivers of CO2 emissions", 
                 subtitle = "(Contribution to CO2 Emission Growth, %)") +
         scale_x_continuous(n.breaks = 10) +
         scale_fill_manual(values = c("#099ec8","#84bc41","#f9c416","#9cd8e9")) +
         scale_color_manual(values = "#a37c00")
  )


#Export: 

oecd_kaya_plots %>% 
  iwalk(~ ggsave(paste0("output/figures/OECD-non/",.y,".png"),
                 .x,
                 width = 10,
                 height = 6,
                 dpi = "retina")
  )


############ Plot Shapley decomposition: OECD vs non-OECD (historical + SSP scenario)  -----
############ all scenarios together
#chart
ShapleyWorldSnapshot <- WorldSSP[WorldSSP$Year %in% seq(1970, 2100, 10),]
#keep 1950, 2020 from historical and 2020 and 2100 from SSP
ShapleyWorldSnapshot <- ShapleyWorldSnapshot[ShapleyWorldSnapshot$Year %in% c(2020, 2100),]

names(ShapleyWorldSnapshot)
ShapleyWorldSnapshot <- group_by(ShapleyWorldSnapshot, Period, REGION) %>% 
  mutate(PopGrowth = Pop / lag(Pop) * 100 - 100,
         IncomeGrowth = GDPperCapita / lag(GDPperCapita) * 100 - 100,
         CO2Growth = `Emissions|CO2` / lag(`Emissions|CO2`) * 100 - 100,
         EnergyIntensityGrowth = EnergyIntensity / lag(EnergyIntensity) * 100 - 100,
         CarbonIntensityGrowth = CarbonIntensity / lag(CarbonIntensity) * 100 - 100) %>% 
  ungroup()

#ShapleyWorld$`CO2OWID (kt)` <- round(ShapleyWorld$`CO2OWID (kt)`)


rm(ShapleyValue)

for (i in 1:nrow(ShapleyWorldSnapshot)) {
  
  if (exists("ShapleyValue")) {
    temp <- Shapley(ShapleyWorldSnapshot$Pop[i], ShapleyWorldSnapshot$GDPperCapita[i], ShapleyWorldSnapshot$EnergyIntensity[i], ShapleyWorldSnapshot$CarbonIntensity[i],
                    ShapleyWorldSnapshot$Pop[i+1], ShapleyWorldSnapshot$GDPperCapita[i+1], ShapleyWorldSnapshot$EnergyIntensity[i+1], ShapleyWorldSnapshot$CarbonIntensity[i+1])
    temp <- data.frame(abar = temp[1], bbar = temp[2], cbar = temp[3], dbar = temp[4])
    
    ShapleyValue <- rbind(ShapleyValue,
                          temp)
  } else {
    ShapleyValue <- Shapley(ShapleyWorldSnapshot$Pop[i], ShapleyWorldSnapshot$GDPperCapita[i], ShapleyWorldSnapshot$EnergyIntensity[i], ShapleyWorldSnapshot$CarbonIntensity[i],
                            ShapleyWorldSnapshot$Pop[i+1], ShapleyWorldSnapshot$GDPperCapita[i+1], ShapleyWorldSnapshot$EnergyIntensity[i+1], ShapleyWorldSnapshot$CarbonIntensity[i+1])
    ShapleyValue <- data.frame(abar = ShapleyValue[1], bbar = ShapleyValue[2], cbar = ShapleyValue[3], dbar = ShapleyValue[4])
  }
}

ShapleyValue <- ShapleyValue[-nrow(ShapleyValue),]
ShapleyWorldSnapshot$LaggedEmission <- lag(ShapleyWorldSnapshot$`Emissions|CO2`)
ShapleyWorldSnapshot <- ShapleyWorldSnapshot[-1,]

#merge 
ShapleyWorldSnapshot <- cbind(ShapleyWorldSnapshot, ShapleyValue)
ShapleyWorldSnapshot$Emission <- ShapleyWorldSnapshot$`Emissions|CO2`

#calculate contribution
ShapleyWorldSnapshot$PopContribution <- ShapleyWorldSnapshot$abar / ShapleyWorldSnapshot$LaggedEmission * 100
ShapleyWorldSnapshot$IncContribution <- ShapleyWorldSnapshot$bbar / ShapleyWorldSnapshot$LaggedEmission * 100
ShapleyWorldSnapshot$EnergryContribution <- ShapleyWorldSnapshot$cbar / ShapleyWorldSnapshot$LaggedEmission * 100
ShapleyWorldSnapshot$CarbonContribution <- ShapleyWorldSnapshot$dbar / ShapleyWorldSnapshot$LaggedEmission * 100

#remove 2010 or 2020 for SSP models
ShapleyWorldSnapshot <- ShapleyWorldSnapshot[!(ShapleyWorldSnapshot$Year %in% c(2020)),]

rm(temp)

ShapleyWorldSnapshot <- group_by(ShapleyWorldSnapshot, REGION, SCENARIO, Year) %>% 
  summarise(CO2Growth = mean(CO2Growth),
            PopContribution = mean(PopContribution),
            IncContribution = mean(IncContribution),
            EnergryContribution = mean(EnergryContribution),
            CarbonContribution = mean(CarbonContribution),) %>% 
  ungroup()

# From wide to long

ShapleyWorldSnapshot <- pivot_longer(ShapleyWorldSnapshot, PopContribution:CarbonContribution, names_to = "Var", values_to = "Value")

ShapleyWorldSnapshot$Var <- case_when(ShapleyWorldSnapshot$Var == "PopContribution" ~ "Pop",
                                      ShapleyWorldSnapshot$Var == "IncContribution" ~ "Income",
                                      ShapleyWorldSnapshot$Var == "EnergryContribution" ~ "Energy Intensity",
                                      T ~ "Carbon Intensity")


# Plot:

ShapleyWorldSnapshot %>%
  mutate(REGION = case_when(REGION == "nonOECD" ~ "Non-OECD",
                            T ~ REGION)) %>% 
  mutate(REGION = factor(REGION,levels = c("OECD","Non-OECD"))) %>% 
  ggplot() +
  geom_col(aes(x = SCENARIO, y = Value, fill = Var), width = 0.5, position = "stack") +
  facet_wrap(~ REGION) +
  theme_bw() + 
  theme(axis.title = element_blank(),
        text = element_text(size = 14),
        legend.position = "bottom",
        panel.grid.minor = element_blank(), 
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  # scale_fill_manual(name=NULL,
  #                    values = c("CO2 Emission" = IMF.Black,
  #                               "GDP Per Capita" = IMF.Blue,
  #                               "Energy Intensity" = IMF.Orange,
  #                               "Population" = IMF.Purple,
  #                               "Carbon Intensity" = IMF.Green)) +
  ggtitle("Kaya identity: drivers of CO2 emissions", 
          subtitle = "(Contribution to CO2 Emission Growth, %)") +
  scale_fill_manual(values = c("#099ec8","#84bc41","#f9c416","#9cd8e9"))

ggsave("output/figures/OECD-non/summary.png",
       width = 10,
       height = 6,
       dpi = "retina")

#change Year to period
# ShapleyWorldSnapshot$Year <- case_when(ShapleyWorldSnapshot$Year == 2020 ~ "1970-2020",
#                                        T ~ "2020-2100")
#export data for table and charts for the WP
write.csv(ShapleyWorldSnapshot, "Output/Shapley Decomposition by OECD and nonOECD.csv")


