################################################################################################
####################################### WP with Nicoletta ######################################
########################### Written by Li, Jiakun (Jli5@imf.org) ###############################
############## For any question, please contact Li, Jiakun (Jli5@imf.org) ######################
################################################################################################

#detach packages
detachAllPackages <- function() {
  basic.packages.blank <-  c("stats", 
                             "graphics", 
                             "grDevices", 
                             "utils", 
                             "datasets", 
                             "methods", 
                             "base")
  basic.packages <- paste("package:", basic.packages.blank, sep = "")
  
  package.list <- search()[ifelse(unlist(gregexpr("package:", search())) == 1, 
                                  TRUE, 
                                  FALSE)]
  
  package.list <- setdiff(package.list, basic.packages)
  
  if (length(package.list) > 0)  for (package in package.list) {
    detach(package, character.only = TRUE)
    print(paste("package ", package, " detached", sep = ""))
  }
}

detachAllPackages()

#automatically download required packages
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) {
    sapply(new.pkg, install.packages, dependencies = T)
  }
  sapply(pkg, require, character.only = T)
}

#enter package names
package <- c("relaimpo", "readxl", "tidyverse", "haven", "lubridate", "rJava", "countrycode", "xlsx", "devtools", "shapley")

#load packages (if not installed, they're installed first and then loaded into the environment)
check.packages(package)

#download shapley value decomposition package
#devtools::install_github("elbersb/shapley")

#housekeeping
rm(list = ls())

#set working directory
path <- getwd()
setwd(path)


#some functions to be used later
#source("C:/Users/JLi5/Desktop/Tools/Functions to be used later.R")


#read and clean data
################################################################################################
#read data NGDP_R
excel_sheets("Data/NGDP_R.xlsx")
NGDP_R <- read_excel("Data/NGDP_R.xlsx", sheet = "a")
str(NGDP_R)
#pivot longer
NGDP_R <- pivot_longer(NGDP_R, "1950A1":"2028A1", names_to = "Year", values_to = "NGDPR")
#keep relevant variables
names(NGDP_R)
NGDP_R <- NGDP_R[, c("country", "Year", "NGDPR")]
names(NGDP_R)[1] <- "ifscode"
class(NGDP_R$ifscode)
NGDP_R$ifscode <- as.numeric(NGDP_R$ifscode)
#remove A1 from year
NGDP_R$Year <- gsub("A1", "", NGDP_R$Year)
NGDP_R$Year <- as.numeric(NGDP_R$Year)
class(NGDP_R$ifscode)
NGDP_R$ifscode <- as.numeric(NGDP_R$ifscode)

#read country dummies
CountryGroups <- read_excel("Data/country groups_final.xlsx", sheet = "Full")
#merge FundArrangementsSince1952 with CountryGroups
#names(CountryGroups)
VarToKeep <- c("country", "ifscode", "iso", "Advanced Economies", "Emerging and developing economies",
               "Emerging Market and Middle-Income Economies", "Emerging Market and Developing Economies: Low Income Developing Countries",
               "Countries Eligible for IMF Concessional Lending (PRGT)", "fragile", "small", "AFR", "APD", "EUR", "MCD", "WHD")
CountryGroups <- CountryGroups[, VarToKeep]
names(CountryGroups) <- c("country", "ifscode", "iso", "AEs", "EMDEs", "EMs", "LIDCs", "LICs", "FCS", "SDS", "AFR", "APD", "EUR", "MCD", "WHD")
rm(VarToKeep)

#read population, fertility rate, and mortality rate data
UNData <- read_excel("Data/Population.xlsx", sheet = "Cleaned")
names(UNData)
#keep relevant variables
UNData <- UNData[, c("ISO3 Alpha-code", "Year", "Total Population, as of 1 July (thousands)", "Total Fertility Rate (live births per woman)", "Total Deaths (thousands)")]
str(UNData)
UNData <- UNData[!is.na(UNData$`ISO3 Alpha-code`),]
UNData$`Total Population, as of 1 July (thousands)` <- as.numeric(UNData$`Total Population, as of 1 July (thousands)`)
UNData$`Total Fertility Rate (live births per woman)` <- as.numeric(UNData$`Total Fertility Rate (live births per woman)`)
UNData$`Total Deaths (thousands)` <- as.numeric(UNData$`Total Deaths (thousands)`)

#read CO2 from the WB
CO2WB <- read_excel("Data/CO2_WB.xlsx", sheet = "Data")
#pivot longer
names(CO2WB)
CO2WB <- pivot_longer(CO2WB, "1960":"2021", names_to = "Year", values_to = "CO2WB (kt)")
str(CO2WB)
CO2WB$Year <- as.numeric(CO2WB$Year)
#keep relevant variables
CO2WB <- CO2WB[, c("Country Code", "Year", "CO2WB (kt)")]
#read CO2 from OWID
CO2OWID <- read_excel("Data/owid-co2-data.xlsx")
names(CO2OWID)
#keep relevant variables
CO2OWID <- CO2OWID[, c("year", "iso_code", "co2")]
str(CO2OWID)
names(CO2OWID)[3] <- "CO2OWID (kt)"
CO2OWID$`CO2OWID (kt)` <- CO2OWID$`CO2OWID (kt)` * 1000

#left join NGDPR to country groups
Data <- left_join(CountryGroups, NGDP_R, "ifscode")
#merge UNData with Data
Data <- left_join(Data, UNData, c("iso" = "ISO3 Alpha-code", "Year"))
#merge with CO2WB data
Data <- left_join(Data, CO2WB, c("iso" = "Country Code", "Year"))
#merge with CO2OWID data
Data <- left_join(Data, CO2OWID, c("iso" = "iso_code", "Year" = "year"))
#create an income indicator based on AEs and LICs
Data$EMs <- ifelse(Data$AEs != 1 & Data$LICs != 1, 1, 0)
Data <- pivot_longer(Data, c("AEs", "EMs", "LICs"), names_to = "Income", values_to = "Dummy")
Data <- Data[Data$Dummy == 1,]
Data$Dummy <- NULL

#create world data
unique(NGDP_R$ifscode)
WorldData <- NGDP_R[NGDP_R$ifscode == 1 & !is.na(NGDP_R$ifscode),]
WorldData$ifscode <- NULL
#join CO2OWID data
#read CO2 from OWID
CO2OWID <- read_excel("Data/owid-co2-data.xlsx")
names(CO2OWID)
#keep relevant variables
CO2OWID <- CO2OWID[, c("country", "year", "iso_code", "co2")]
str(CO2OWID)
names(CO2OWID)[4] <- "CO2OWID (kt)"
CO2OWID$`CO2OWID (kt)` <- CO2OWID$`CO2OWID (kt)` * 1000
#keep only world data
CO2OWID <- CO2OWID[CO2OWID$country == "World",]
CO2OWID$country <- NULL
CO2OWID$iso_code <- NULL
WorldData <- full_join(WorldData, CO2OWID, c("Year" = "year"))

#join in population
#read population, fertility rate, and mortality rate data
UNData <- read_excel("Data/Population.xlsx", sheet = "World")
names(UNData)
#keep relevant variables
UNData <- UNData[, c("ISO3 Alpha-code", "Year", "Total Population, as of 1 July (thousands)", "Total Fertility Rate (live births per woman)", "Total Deaths (thousands)")]
str(UNData)
UNData$`Total Population, as of 1 July (thousands)` <- as.numeric(UNData$`Total Population, as of 1 July (thousands)`)
UNData$`Total Fertility Rate (live births per woman)` <- as.numeric(UNData$`Total Fertility Rate (live births per woman)`)
UNData$`Total Deaths (thousands)` <- as.numeric(UNData$`Total Deaths (thousands)`)
UNData$`ISO3 Alpha-code` <- NULL
WorldData <- full_join(WorldData, UNData, c("Year"))

WorldData <- WorldData[order(WorldData$Year),]

################################################################################################


#create historical world dataset
################################################################################################

#for world dataset - includes primary energy consumption
WorldPrimaryEnergy <- read.csv("Data/global-energy-substitution.csv")
names(WorldPrimaryEnergy)
str(WorldPrimaryEnergy)

#calculate the total primary energy use by year
WorldPrimaryEnergy <- pivot_longer(WorldPrimaryEnergy, 
                                   "Other.renewables..TWh..substituted.energy.":"Traditional.biomass..TWh..substituted.energy.", 
                                   names_to = "Variable", values_to = "Value")

WorldPrimaryEnergy <- group_by(WorldPrimaryEnergy, Year) %>% 
  summarise(PrimaryEnergyUse = sum(Value)) %>% 
  mutate(PrimaryEnergyUse = PrimaryEnergyUse*3.6) %>% #convert to EJ
  ungroup()

#merge data to World data
WorldData <- left_join(WorldData, WorldPrimaryEnergy, "Year")

#create an indicator for historical data
WorldData$Period <- "Historical"

################################################################################################


#read SSP data - forecasts under scenarios
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
WorldSSP <- SSP[SSP$REGION == "World",]
# #keep only SSP2
# Test <- Test[grep("SSP2", Test$SCENARIO),]

rm(SSP)

#pivot longer years
names(WorldSSP)
WorldSSP <- pivot_longer(WorldSSP, "2005":"2100", names_to = "Year", values_to = "Value")
class(WorldSSP$Year)
WorldSSP$Year <- as.numeric(WorldSSP$Year)
WorldSSP$UNIT <- NULL

#pivot wider years
WorldSSP <- pivot_wider(WorldSSP, names_from = "VARIABLE", values_from = "Value")

#keep relevant variables
names(WorldSSP)
WorldSSP$FertilityRate <- NA
WorldSSP$TotalDeaths <- NA
WorldSSP$Period <- "SSP"
WorldSSP <- WorldSSP[, c("Year", "GDP|PPP", "Emissions|CO2|Fossil Fuels and Industry", "Population", "FertilityRate", "TotalDeaths", "Primary Energy", "Period", "MODEL", "SCENARIO")]

#modify emission and population unit
WorldSSP$`Emissions|CO2|Fossil Fuels and Industry` <- WorldSSP$`Emissions|CO2|Fossil Fuels and Industry` * 1000
WorldSSP$Population <- WorldSSP$Population * 1000
WorldSSP$`GDP|PPP` <- WorldSSP$`GDP|PPP` * 1000
WorldSSP$`Primary Energy` <- WorldSSP$`Primary Energy` * 1000

names(WorldSSP)

################################################################################################


#combined historical and SSP dataset
################################################################################################

#to make consistent the names of historical dataset and SSP dataset
WorldData$MODEL <- NA
WorldData$SCENARIO <- NA
names(WorldSSP) <- names(WorldData)

#keep historical data form 1950 to 2021
WorldData <- WorldData[WorldData$Year %in% 1950:2021,]

#join historical dataset and SSP dataset
World <- rbind(WorldData, WorldSSP)
World$Period[!is.na(World$SCENARIO)] <- World$SCENARIO[!is.na(World$SCENARIO)]
World$SCENARIO <- NULL
unique(World$MODEL)
World$MODEL[is.na(World$MODEL)] <- "Historical"

rm(WorldData, WorldSSP)


#GDP per capita and carbon intensity
World$GDPperCapita <- World$NGDPR/World$`Total Population, as of 1 July (thousands)`
World$EnergyIntensity <- World$PrimaryEnergyUse / World$NGDPR
World$CarbonIntensity <- World$`CO2OWID (kt)` / World$PrimaryEnergyUse

# #calculation contribution to growth of emission
# World <- mutate(World,
#                 lgIncomeGrowth = log(IncomeGrowth / 100 + 1),
#                 lgPopGrowth = log(PopGrowth / 100 + 1),
#                 lgEnergyIntensityGrowth = log(EnergyIntensityGrowth / 100 + 1),
#                 lgCarbonIntensityGrowth = log(CarbonIntensityGrowth / 100 + 1),
#                 lgCO2Growth = log(CO2Growth / 100 + 1)) %>% 
#   ungroup()

# #World Data time index
# World$timeidx <- World$Year - 1950
# 

# 
# #keep years between 2021 and 1963
# World <- World[World$Year >= 1965 & World$Year <= 2021,]

################################################################################################


#shapley decomposition - world
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
World$Pop <- World$`Total Population, as of 1 July (thousands)`
ShapleyWorld <- World[World$Year %in% seq(1900, 2100, 10),]

ShapleyWorld <- group_by(ShapleyWorld, Period, MODEL) %>% 
  mutate(PopGrowth = `Total Population, as of 1 July (thousands)` / lag(`Total Population, as of 1 July (thousands)`) * 100 - 100,
         IncomeGrowth = GDPperCapita / lag(GDPperCapita) * 100 - 100,
         CO2Growth = `CO2OWID (kt)` / lag(`CO2OWID (kt)`) * 100 - 100,
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
ShapleyWorld$LaggedEmission <- lag(ShapleyWorld$`CO2OWID (kt)`)
ShapleyWorld <- ShapleyWorld[-1,]

#merge 
ShapleyWorld <- cbind(ShapleyWorld, ShapleyValue)
ShapleyWorld$Emission <- ShapleyWorld$`CO2OWID (kt)`

#calculate contribution
ShapleyWorld$PopContribution <- ShapleyWorld$abar / ShapleyWorld$LaggedEmission * 100
ShapleyWorld$IncContribution <- ShapleyWorld$bbar / ShapleyWorld$LaggedEmission * 100
ShapleyWorld$EnergryContribution <- ShapleyWorld$cbar / ShapleyWorld$LaggedEmission * 100
ShapleyWorld$CarbonContribution <- ShapleyWorld$dbar / ShapleyWorld$LaggedEmission * 100

#remove 2010 or 2020 for SSP models
ShapleyWorld <- ShapleyWorld[!(ShapleyWorld$Year %in% c(2010, 2020) & ShapleyWorld$Period != "Historical"),]

rm(temp)

ShapleyWorld <- group_by(ShapleyWorld, Period, Year) %>% 
  summarise(CO2Growth = mean(CO2Growth),
            PopContribution = mean(PopContribution),
            IncContribution = mean(IncContribution),
            EnergryContribution = mean(EnergryContribution),
            CarbonContribution = mean(CarbonContribution),) %>% 
  ungroup()

#export data for table and charts for the WP
# write.csv(ShapleyWorld, "Output/Shapley Decomposition.csv")

ShapleyWorld <- pivot_longer(ShapleyWorld, PopContribution:CarbonContribution, names_to = "Var", values_to = "Value")

unique(ShapleyWorld$Var)
ShapleyWorld$Var <- case_when(ShapleyWorld$Var == "PopContribution" ~ "Pop",
                              ShapleyWorld$Var == "IncContribution" ~ "Income",
                              ShapleyWorld$Var == "EnergryContribution" ~ "Energy Intensity",
                              T ~ "Carbon Intensity")

# Plot Shapley decomposition: historical chart + SSP scenario (by scenario) ------

dir.create("output/figures/World")

ShapleyWorld_hist <- ShapleyWorld %>% 
  filter(Period == "Historical")



world_kaya_plots <- ShapleyWorld %>% 
  split(.$Period) %>% 
  map(~ rbind(ShapleyWorld_hist,.x)) %>% 
  imap(~ ggplot(.x[.x$Period %in% c("Historical", .y),]) +
  geom_col(aes(x = Year, y = Value, fill = Var), width = 7, position = "stack") +
  geom_line(aes(x = Year, y = CO2Growth,col = "CO2 Growth"), size = 0.8) +
  geom_point(aes(x = Year, y = CO2Growth),col = "#a37c00", size = 2.5, alpha=0.3) +
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

world_kaya_plots %>% 
  iwalk(~ ggsave(paste0("output/figures/World/",.y,".png"),
                .x,
                width = 10,
                height = 6,
                dpi = "retina")
  )


#do the same as a snap shot
#chart
ShapleyWorldSnapshot <- World[World$Year %in% seq(1970, 2100, 10),]
#keep 1950, 2020 from historical and 2020 and 2100 from SSP
ShapleyWorldSnapshot <- ShapleyWorldSnapshot[ShapleyWorldSnapshot$Year %in% c(1970, 2020) & ShapleyWorldSnapshot$Period == "Historical" | ShapleyWorldSnapshot$Year %in% c(2020, 2100) & ShapleyWorldSnapshot$Period != "Historical",]

ShapleyWorldSnapshot <- group_by(ShapleyWorldSnapshot, Period, MODEL) %>% 
  mutate(PopGrowth = `Total Population, as of 1 July (thousands)` / lag(`Total Population, as of 1 July (thousands)`) * 100 - 100,
         IncomeGrowth = GDPperCapita / lag(GDPperCapita) * 100 - 100,
         CO2Growth = `CO2OWID (kt)` / lag(`CO2OWID (kt)`) * 100 - 100,
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
ShapleyWorldSnapshot$LaggedEmission <- lag(ShapleyWorldSnapshot$`CO2OWID (kt)`)
ShapleyWorldSnapshot <- ShapleyWorldSnapshot[-1,]

#merge 
ShapleyWorldSnapshot <- cbind(ShapleyWorldSnapshot, ShapleyValue)
ShapleyWorldSnapshot$Emission <- ShapleyWorldSnapshot$`CO2OWID (kt)`

#calculate contribution
ShapleyWorldSnapshot$PopContribution <- ShapleyWorldSnapshot$abar / ShapleyWorldSnapshot$LaggedEmission * 100
ShapleyWorldSnapshot$IncContribution <- ShapleyWorldSnapshot$bbar / ShapleyWorldSnapshot$LaggedEmission * 100
ShapleyWorldSnapshot$EnergryContribution <- ShapleyWorldSnapshot$cbar / ShapleyWorldSnapshot$LaggedEmission * 100
ShapleyWorldSnapshot$CarbonContribution <- ShapleyWorldSnapshot$dbar / ShapleyWorldSnapshot$LaggedEmission * 100

#remove 2010 or 2020 for SSP models
ShapleyWorldSnapshot <- ShapleyWorldSnapshot[!(ShapleyWorldSnapshot$Year %in% c(2020) & ShapleyWorldSnapshot$Period != "Historical"),]

rm(temp)

ShapleyWorldSnapshot <- group_by(ShapleyWorldSnapshot, Period, Year) %>% 
  summarise(CO2Growth = mean(CO2Growth),
            PopContribution = mean(PopContribution),
            IncContribution = mean(IncContribution),
            EnergryContribution = mean(EnergryContribution),
            CarbonContribution = mean(CarbonContribution),) %>% 
  ungroup()

#change Year to period
ShapleyWorldSnapshot$Year <- case_when(ShapleyWorldSnapshot$Year == 2020 ~ "1970-2020",
                                       T ~ "2020-2100")

# From wide to long

ShapleyWorldSnapshot <- pivot_longer(ShapleyWorldSnapshot, PopContribution:CarbonContribution, names_to = "Var", values_to = "Value")

ShapleyWorldSnapshot$Var <- case_when(ShapleyWorldSnapshot$Var == "PopContribution" ~ "Pop",
                                      ShapleyWorldSnapshot$Var == "IncContribution" ~ "Income",
                                      ShapleyWorldSnapshot$Var == "EnergryContribution" ~ "Energy Intensity",
                                      T ~ "Carbon Intensity")

# Plot:

ShapleyWorldSnapshot %>% 
  ggplot() +
  geom_col(aes(x = Period, y = Value, fill = Var), width = 0.5, position = "stack") +
  geom_vline(xintercept = 1.5, size=0.5) +
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

# Export:

ggsave("output/figures/World/summary.png",
       width = 10,
       height = 6,
       dpi = "retina")
#export data for table and charts for the WP
write.csv(ShapleyWorldSnapshot %>% pivot_wider(names_from =Var, values_from= Value),
          "output/tables/world/historical_vs_ssp.csv")





