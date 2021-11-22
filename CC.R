#!/usr/bin/env Rscript
#
# Code to write out excel using the CC Schema.
#

# Load packages  - Without java dependency 
library(openxlsx)
library(lubridate)


# Initiate variables
group <- "Edinburgh"
model <-  "WSS"
scenario <- "NowCast"
modeltype <- "Cases"
version <- 0.1
today <- today()
ageband <-  "All"
# Region should be inherited from most recent compartment run, e.g. region <- "Scotland"
valuetype <- "R"

# If you need multiple repeated values you can use the rep command so
# rep(value, number of repetitions), e.g. rep(group, length(SomeVector)).
CC <- data.frame(
  Group = group,
  Model = model,
  Scenario = scenario,
  ModelType = modeltype,
  Version = version,
  "Creation Day" = day(today),
  "Creation Month" = month(today),
  "Creation Year" = year(today),
  "Day of Value" = day(enddate-2),
  "Month of Value" = month(enddate-2),
  "Year of Value" = year(enddate-2),
  AgeBand = ageband,
  Geography = "Scotland",
  ValueType = valuetype,
  Value = R_BestGuess$Scotland,
  "Quantile 0.05" = R_Quant$Scotland[1],
  "Quantile 0.1" = "",
  "Quantile 0.15" = "",
  "Quantile 0.2" = "",
  "Quantile 0.25" = R_Quant$Scotland[2],
  "Quantile 0.3" = "",
  "Quantile 0.35" = "",
  "Quantile 0.4" = "",
  "Quantile 0.45" = "",
  "Quantile 0.5" = R_Quant$Scotland[3],
  "Quantile 0.55" = "",
  "Quantile 0.6" = "",
  "Quantile 0.65" = "",
  "Quantile 0.7" = "",
  "Quantile 0.75" = R_Quant$Scotland[4],
  "Quantile 0.8" = "",
  "Quantile 0.85" = "",
  "Quantile 0.9" = "",
  "Quantile 0.95" = R_Quant$Scotland[5],
  check.names = FALSE
)
# Need to create a list to add more rows to the CrystalCast df
CCtmp <- CC
CCtmp$Geography="England"
CCtmp$"Quantile 0.05"=R_Quant$England[1]
CCtmp$"Quantile 0.25"=R_Quant$England[2]
CCtmp$"Quantile 0.5"=R_Quant$England[3]
CCtmp$"Quantile 0.75"=R_Quant$England[4]
CCtmp$"Quantile 0.95"=R_Quant$England[5]
CCtmp$Value = R_BestGuess$England
# Add the new row
CC <- rbind(CC, CCtmp)


CCtmp$Geography="Wales"
CCtmp$"Quantile 0.05"=R_Quant$Wales[1]
CCtmp$"Quantile 0.25"=R_Quant$Wales[2]
CCtmp$"Quantile 0.5"=R_Quant$Wales[3]
CCtmp$"Quantile 0.75"=R_Quant$Wales[4]
CCtmp$"Quantile 0.95"=R_Quant$Wales[5]
CCtmp$Value = R_BestGuess$Wales
# Add the new row
CC <- rbind(CC, CCtmp)


CCtmp$Geography="Northern Ireland"
CCtmp$"Quantile 0.05"=R_Quant$NI[1]
CCtmp$"Quantile 0.25"=R_Quant$NI[2]
CCtmp$"Quantile 0.5"=R_Quant$NI[3]
CCtmp$"Quantile 0.75"=R_Quant$NI[4]
CCtmp$"Quantile 0.95"=R_Quant$NI[5]
CCtmp$Value = R_BestGuess$NI
# Add the new row
CC <- rbind(CC, CCtmp)

# Growth rate
CCtmp$ValueType <- "growth_rate"
CCtmp$Geography="England"
CCtmp$"Quantile 0.05"=exp((R_Quant$England[1]-1.0)/genTime)-1.0
CCtmp$"Quantile 0.25"=exp((R_Quant$England[2]-1.0)/genTime)-1.0
CCtmp$"Quantile 0.5"=exp((R_Quant$England[3]-1.0)/genTime)-1.0
CCtmp$"Quantile 0.75"=exp((R_Quant$England[4]-1.0)/genTime)-1.0
CCtmp$"Quantile 0.95"=exp((R_Quant$England[5]-1.0)/genTime)-1.0
CCtmp$Value = exp((R_BestGuess$England-1.0)/genTime)-1.0
# Add the new row
CC <- rbind(CC, CCtmp)
CCtmp$ValueType <- "growth_rate"
CCtmp$Geography="Scotland"
CCtmp$"Quantile 0.05"=exp((R_Quant$Scotland[1]-1.0)/genTime)-1.0
CCtmp$"Quantile 0.25"=exp((R_Quant$Scotland[2]-1.0)/genTime)-1.0
CCtmp$"Quantile 0.5"=exp((R_Quant$Scotland[3]-1.0)/genTime)-1.0
CCtmp$"Quantile 0.75"=exp((R_Quant$Scotland[4]-1.0)/genTime)-1.0
CCtmp$"Quantile 0.95"=exp((R_Quant$Scotland[5]-1.0)/genTime)-1.0
CCtmp$Value = exp((R_BestGuess$Scotland-1.0)/genTime)-1.0
# Add the new row
CC <- rbind(CC, CCtmp)

CCage <- CCtmp
CCage$Geography="England"
CCage$AgeBand="0-4"
CCage$"Quantile 0.05"=exp((R_Quant$x00[1]-1.0)/genTime)-1.0
CCage$"Quantile 0.25"=exp((R_Quant$x00[2]-1.0)/genTime)-1.0
CCage$"Quantile 0.5"=exp((R_Quant$x00[3]-1.0)/genTime)-1.0
CCage$"Quantile 0.75"=exp((R_Quant$x00[4]-1.0)/genTime)-1.0
CCage$"Quantile 0.95"=exp((R_Quant$x00[5]-1.0)/genTime)-1.0
CCage$Value = exp((R_BestGuess$x00-1.0)/genTime)-1.0
# Add the new row
CC <- rbind(CC, CCage)

CCage$Geography="England"
CCage$AgeBand="5-14"
CCage$"Quantile 0.05"=exp((R_Quant$x05[1]-1.0)/genTime)-1.0
CCage$"Quantile 0.25"=exp((R_Quant$x05[2]-1.0)/genTime)-1.0
CCage$"Quantile 0.5"=exp((R_Quant$x05[3]-1.0)/genTime)-1.0
CCage$"Quantile 0.75"=exp((R_Quant$x05[4]-1.0)/genTime)-1.0
CCage$"Quantile 0.95"=exp((R_Quant$x05[5]-1.0)/genTime)-1.0
CCage$Value = exp((R_BestGuess$x05-1.0)/genTime)-1.0
# Add the new row
CC <- rbind(CC, CCage)

CCage <- CCtmp
CCage$Geography="England"
CCage$AgeBand="15-24"
CCage$"Quantile 0.05"=exp((R_Quant$x15[1]-1.0)/genTime)-1.0
CCage$"Quantile 0.25"=exp((R_Quant$x15[2]-1.0)/genTime)-1.0
CCage$"Quantile 0.5"=exp((R_Quant$x15[3]-1.0)/genTime)-1.0
CCage$"Quantile 0.75"=exp((R_Quant$x15[4]-1.0)/genTime)-1.0
CCage$"Quantile 0.95"=exp((R_Quant$x15[5]-1.0)/genTime)-1.0
CCage$Value = exp((R_BestGuess$x15-1.0)/genTime)-1.0
# Add the new row
CC <- rbind(CC, CCage)


CCage <- CCtmp
CCage$Geography="England"
CCage$AgeBand="25-44"
CCage$"Quantile 0.05"=exp((R_Quant$x25[1]-1.0)/genTime)-1.0
CCage$"Quantile 0.25"=exp((R_Quant$x25[2]-1.0)/genTime)-1.0
CCage$"Quantile 0.5"=exp((R_Quant$x25[3]-1.0)/genTime)-1.0
CCage$"Quantile 0.75"=exp((R_Quant$x25[4]-1.0)/genTime)-1.0
CCage$"Quantile 0.95"=exp((R_Quant$x25[5]-1.0)/genTime)-1.0
CCage$Value = exp((R_BestGuess$x25-1.0)/genTime)-1.0
# Add the new row
CC <- rbind(CC, CCage)


CCage <- CCtmp
CCage$Geography="England"
CCage$AgeBand="45-64"
CCage$"Quantile 0.05"=exp((R_Quant$x45[1]-1.0)/genTime)-1.0
CCage$"Quantile 0.25"=exp((R_Quant$x45[2]-1.0)/genTime)-1.0
CCage$"Quantile 0.5"=exp((R_Quant$x45[3]-1.0)/genTime)-1.0
CCage$"Quantile 0.75"=exp((R_Quant$x45[4]-1.0)/genTime)-1.0
CCage$"Quantile 0.95"=exp((R_Quant$x45[5]-1.0)/genTime)-1.0
CCage$Value = exp((R_BestGuess$x45-1.0)/genTime)-1.0
# Add the new row
CC <- rbind(CC, CCage)

CCage <- CCtmp
CCage$Geography="England"
CCage$AgeBand="65-74"
CCage$"Quantile 0.05"=exp((R_Quant$x65[1]-1.0)/genTime)-1.0
CCage$"Quantile 0.25"=exp((R_Quant$x65[2]-1.0)/genTime)-1.0
CCage$"Quantile 0.5"=exp((R_Quant$x65[3]-1.0)/genTime)-1.0
CCage$"Quantile 0.75"=exp((R_Quant$x65[4]-1.0)/genTime)-1.0
CCage$"Quantile 0.95"=exp((R_Quant$x65[5]-1.0)/genTime)-1.0
CCage$Value = exp((R_BestGuess$x65-1.0)/genTime)-1.0
# Add the new row
CC <- rbind(CC, CCage)


CCage <- CCtmp
CCage$Geography="England"
CCage$AgeBand="75+"
CCage$"Quantile 0.05"=exp((R_Quant$x75[1]-1.0)/genTime)-1.0
CCage$"Quantile 0.25"=exp((R_Quant$x75[2]-1.0)/genTime)-1.0
CCage$"Quantile 0.5"=exp((R_Quant$x75[3]-1.0)/genTime)-1.0
CCage$"Quantile 0.75"=exp((R_Quant$x75[4]-1.0)/genTime)-1.0
CCage$"Quantile 0.95"=exp((R_Quant$x75[5]-1.0)/genTime)-1.0
CCage$Value = exp((R_BestGuess$x75-1.0)/genTime)-1.0
# Add the new row
CC <- rbind(CC, CCage)

CCmid <-CCtmp

CCtmp$ValueType <-"R"
CCage <- CCtmp
CCage$Geography="England"
CCage$AgeBand="0-4"
CCage$"Quantile 0.05"=R_Quant$x00[1]
CCage$"Quantile 0.25"=R_Quant$x00[2]
CCage$"Quantile 0.5"=R_Quant$x00[3]
CCage$"Quantile 0.75"=R_Quant$x00[4]
CCage$"Quantile 0.95"=R_Quant$x00[5]
CCage$Value = R_BestGuess$x00
# Add the new row
CC <- rbind(CC, CCage)

CCage$Geography="England"
CCage$AgeBand="5-14"
CCage$"Quantile 0.05"=R_Quant$x05[1]
CCage$"Quantile 0.25"=R_Quant$x05[2]
CCage$"Quantile 0.5"=R_Quant$x05[3]
CCage$"Quantile 0.75"=R_Quant$x05[4]
CCage$"Quantile 0.95"=R_Quant$x05[5]
CCage$Value = R_BestGuess$x05
# Add the new row
CC <- rbind(CC, CCage)

CCage <- CCtmp
CCage$Geography="England"
CCage$AgeBand="15-24"
CCage$"Quantile 0.05"=R_Quant$x15[1]
CCage$"Quantile 0.25"=R_Quant$x15[2]
CCage$"Quantile 0.5"=R_Quant$x15[3]
CCage$"Quantile 0.75"=R_Quant$x15[4]
CCage$"Quantile 0.95"=R_Quant$x15[5]
CCage$Value = R_BestGuess$x15
# Add the new row
CC <- rbind(CC, CCage)


CCage <- CCtmp
CCage$Geography="England"
CCage$AgeBand="25-44"
CCage$"Quantile 0.05"=R_Quant$x25[1]
CCage$"Quantile 0.25"=R_Quant$x25[2]
CCage$"Quantile 0.5"=R_Quant$x25[3]
CCage$"Quantile 0.75"=R_Quant$x25[4]
CCage$"Quantile 0.95"=R_Quant$x25[5]
CCage$Value = R_BestGuess$x25
# Add the new row
CC <- rbind(CC, CCage)


CCage <- CCtmp
CCage$Geography="England"
CCage$AgeBand="45-64"
CCage$"Quantile 0.05"=R_Quant$x45[1]
CCage$"Quantile 0.25"=R_Quant$x45[2]
CCage$"Quantile 0.5"=R_Quant$x45[3]
CCage$"Quantile 0.75"=R_Quant$x45[4]
CCage$"Quantile 0.95"=R_Quant$x45[5]
CCage$Value = R_BestGuess$x45
# Add the new row
CC <- rbind(CC, CCage)

CCage <- CCtmp
CCage$Geography="England"
CCage$AgeBand="65-74"
CCage$"Quantile 0.05"=R_Quant$x65[1]
CCage$"Quantile 0.25"=R_Quant$x65[2]
CCage$"Quantile 0.5"=R_Quant$x65[3]
CCage$"Quantile 0.75"=R_Quant$x65[4]
CCage$"Quantile 0.95"=R_Quant$x65[5]
CCage$Value = R_BestGuess$x65
# Add the new row
CC <- rbind(CC, CCage)


CCage <- CCtmp
CCage$Geography="England"
CCage$AgeBand="75+"
CCage$"Quantile 0.05"=R_Quant$x75[1]
CCage$"Quantile 0.25"=R_Quant$x75[2]
CCage$"Quantile 0.5"=R_Quant$x75[3]
CCage$"Quantile 0.75"=R_Quant$x75[4]
CCage$"Quantile 0.95"=R_Quant$x75[5]
CCage$Value = R_BestGuess$x75
# Add the new row
CC <- rbind(CC, CCage)

CCmid <-CCtmp

CCmid$Geography="North East"
CCmid$Value = R_BestGuess$NEY
Quant = R_Quant$NEY
CCmid$"Quantile 0.05"=Quant[1]
CCmid$"Quantile 0.25"=Quant[2]
CCmid$"Quantile 0.5"=Quant[3]
CCmid$"Quantile 0.75"=Quant[4]
CCmid$"Quantile 0.95"=Quant[5]
CC <- rbind(CC, CCmid)

CCmid <-CCtmp
CCmid$Geography="North West"
CCmid$Value = R_BestGuess$NW
Quant = R_Quant$NW
CCmid$"Quantile 0.05"=Quant[1]
CCmid$"Quantile 0.25"=Quant[2]
CCmid$"Quantile 0.5"=Quant[3]
CCmid$"Quantile 0.75"=Quant[4]
CCmid$"Quantile 0.95"=Quant[5]
CC <- rbind(CC, CCmid)

CCmid <-CCtmp
CCmid$Geography="London"
CCmid$Value = R_BestGuess$NW
Quant = R_Quant$London
CCmid$"Quantile 0.05"=Quant[1]
CCmid$"Quantile 0.25"=Quant[2]
CCmid$"Quantile 0.5"=Quant[3]
CCmid$"Quantile 0.75"=Quant[4]
CCmid$"Quantile 0.95"=Quant[5]
CC <- rbind(CC, CCmid)


CCmid <-CCtmp
CCmid$Geography="East of England"
CCmid$Value = R_BestGuess$EE
Quant = R_Quant$EE
CCmid$"Quantile 0.05"=Quant[1]
CCmid$"Quantile 0.25"=Quant[2]
CCmid$"Quantile 0.5"=Quant[3]
CCmid$"Quantile 0.75"=Quant[4]
CCmid$"Quantile 0.95"=Quant[5]
CC <- rbind(CC, CCmid)



# Add the new row
CC <- rbind(CC, CCmid)

CCmid <-CCtmp
CCmid$Geography="South East"
CCmid$Value = R_BestGuess$SE
Quant = R_Quant$SE
CCmid$"Quantile 0.05"=Quant[1]
CCmid$"Quantile 0.25"=Quant[2]
CCmid$"Quantile 0.5"=Quant[3]
CCmid$"Quantile 0.75"=Quant[4]
CCmid$"Quantile 0.95"=Quant[5]
CC <- rbind(CC, CCmid)


CCmid <-CCtmp
CCmid$Geography="South West"
CCmid$Value = R_BestGuess$SW
Quant = R_Quant$SW
CCmid$"Quantile 0.05"=Quant[1]
CCmid$"Quantile 0.25"=Quant[2]
CCmid$"Quantile 0.5"=Quant[3]
CCmid$"Quantile 0.75"=Quant[4]
CCmid$"Quantile 0.95"=Quant[5]
CC <- rbind(CC, CCmid)


CCmid <-CCtmp


CCmid$Geography="Midlands"
CCmid$Value = R_BestGuess$Midlands
Quant = R_Quant$Midlands
CCmid$"Quantile 0.05"=Quant[1]
CCmid$"Quantile 0.25"=Quant[2]
CCmid$"Quantile 0.5"=Quant[3]
CCmid$"Quantile 0.75"=Quant[4]
CCmid$"Quantile 0.95"=Quant[5]
# Add the new row
CC <- rbind(CC, CCmid)


for (d in 4:(length(smoothweightR$date)-3)){
  CCdate <-CCtmp
  CCdate$Geography="England"
  CCdate$Value = smoothweightR$y[d]
  CCdate$"Quantile 0.05"=min(smoothweightR$y[(d-3):(d+3)])-0.2
  CCdate$"Quantile 0.25"=min(smoothweightR$y[(d-3):(d+3)])-0.1
  CCdate$"Quantile 0.5"=smoothweightR$y[d]
  CCdate$"Quantile 0.75"=max(smoothweightR$y[(d-3):(d+3)])+0.1
  CCdate$"Quantile 0.95"=max(smoothweightR$y[(d-3):(d+3)])+0.2
  CCdate$"Day of Value" = day(smoothweightR$date[d])
  CCdate$"Month of Value" = month(smoothweightR$date[d])
  CCdate$"Year of Value" = year(smoothweightR$date[d])
  # Add the new row
  CC <- rbind(CC, CCdate)
}

for (d in 4:(length(rat$date)-3)){
  CCdate$Geography="Scotland"
  CCdate$Value =rat$smoothScotland[d]
  CCdate$"Quantile 0.05"=min(rat$smoothScotland[(d-3):(d+3)])-0.2
  CCdate$"Quantile 0.25"=min(rat$smoothScotland[(d-3):(d+3)])-0.1
  CCdate$"Quantile 0.5"=rat$smoothScotland[d]
  CCdate$"Quantile 0.75"=max(rat$smoothScotland[(d-3):(d+3)])+0.1
  CCdate$"Quantile 0.95"=max(rat$smoothScotland[(d-3):(d+3)])+0.2
  CCdate$"Day of Value" = day(rat$date[d])
  CCdate$"Month of Value" = month(rat$date[d])
  CCdate$"Year of Value" = year(rat$date[d])
  # Add the new row
  CC <- rbind(CC, CCdate)
}
for (d in 4:(length(rat$date)-3)){
  CCdate$Geography="North East"
  CCdate$Value = rat$smoothNEY[d]
  CCdate$"Quantile 0.05"=min(rat$smoothNEY[(d-3):(d+3)])-0.2
  CCdate$"Quantile 0.25"=min(rat$smoothNEY[(d-3):(d+3)])-0.1
  CCdate$"Quantile 0.5"=rat$smoothNEY[d]
  CCdate$"Quantile 0.75"=max(rat$smoothNEY[(d-3):(d+3)])+0.1
  CCdate$"Quantile 0.95"=max(rat$smoothNEY[(d-3):(d+3)])+0.2
  CCdate$"Day of Value" = day(rat$date[d])
  CCdate$"Month of Value" = month(rat$date[d])
  CCdate$"Year of Value" = year(rat$date[d])
  # Add the new row
  CC <- rbind(CC, CCdate)
}
for (d in 4:(length(rat$date)-3)){
  CCdate$Geography="North West"
  CCdate$Value = rat$smoothNW[d]
  CCdate$"Quantile 0.05"=min(rat$smoothNW[(d-3):(d+3)])-0.2
  CCdate$"Quantile 0.25"=min(rat$smoothNW[(d-3):(d+3)])-0.1
  CCdate$"Quantile 0.5"=rat$smoothNW[d]
  CCdate$"Quantile 0.75"=max(rat$smoothNW[(d-3):(d+3)])+0.1
  CCdate$"Quantile 0.95"=max(rat$smoothNW[(d-3):(d+3)])+0.2
  CCdate$"Day of Value" = day(rat$date[d])
  CCdate$"Month of Value" = month(rat$date[d])
  CCdate$"Year of Value" = year(rat$date[d])
  # Add the new row
  CC <- rbind(CC, CCdate)
}
for (d in 4:(length(rat$date)-3)){
  CCdate$Geography="East of England"
  CCdate$Value = rat$smoothEE[d]
  CCdate$"Quantile 0.05"=min(rat$smoothEE[(d-3):(d+3)])-0.2
  CCdate$"Quantile 0.25"=min(rat$smoothEE[(d-3):(d+3)])-0.1
  CCdate$"Quantile 0.5"=rat$smoothEE[d]
  CCdate$"Quantile 0.75"=max(rat$smoothEE[(d-3):(d+3)])+0.1
  CCdate$"Quantile 0.95"=max(rat$smoothEE[(d-3):(d+3)])+0.2
  CCdate$"Day of Value" = day(rat$date[d])
  CCdate$"Month of Value" = month(rat$date[d])
  CCdate$"Year of Value" = year(rat$date[d])
  # Add the new row
  CC <- rbind(CC, CCdate)
}
for (d in 4:(length(rat$date)-3)){
  CCdate$Geography="South West"
  CCdate$Value = rat$smoothSW[d]
  CCdate$"Quantile 0.05"=min(rat$smoothSW[(d-3):(d+3)])-0.2
  CCdate$"Quantile 0.25"=min(rat$smoothSW[(d-3):(d+3)])-0.1
  CCdate$"Quantile 0.5"=rat$smoothSW[d]
  CCdate$"Quantile 0.75"=max(rat$smoothSW[(d-3):(d+3)])+0.1
  CCdate$"Quantile 0.95"=max(rat$smoothSW[(d-3):(d+3)])+0.2
  CCdate$"Day of Value" = day(rat$date[d])
  CCdate$"Month of Value" = month(rat$date[d])
  CCdate$"Year of Value" = year(rat$date[d])
  # Add the new row
  CC <- rbind(CC, CCdate)
}

for (d in 4:(length(rat$date)-3)){
  CCdate$Geography="South East"
  CCdate$Value = rat$smoothSE[d]
  CCdate$"Quantile 0.05"=min(rat$smoothSE[(d-3):(d+3)])-0.2
  CCdate$"Quantile 0.25"=min(rat$smoothSE[(d-3):(d+3)])-0.1
  CCdate$"Quantile 0.5"=rat$smoothSE[d]
  CCdate$"Quantile 0.75"=max(rat$smoothSE[(d-3):(d+3)])+0.1
  CCdate$"Quantile 0.95"=max(rat$smoothSE[(d-3):(d+3)])+0.2
  CCdate$"Day of Value" = day(rat$date[d])
  CCdate$"Month of Value" = month(rat$date[d])
  CCdate$"Year of Value" = year(rat$date[d])
  # Add the new row
  CC <- rbind(CC, CCdate)
}
for (d in 4:(length(rat$date)-3)){
  CCdate$Geography="London"
  CCdate$Value = rat$smoothLondon[d]
  CCdate$"Quantile 0.05"=min(rat$smoothLondon[(d-3):(d+3)])-0.2
  CCdate$"Quantile 0.25"=min(rat$smoothLondon[(d-3):(d+3)])-0.1
  CCdate$"Quantile 0.5"=rat$smoothLondon[d]
  CCdate$"Quantile 0.75"=max(rat$smoothLondon[(d-3):(d+3)])+0.1
  CCdate$"Quantile 0.95"=max(rat$smoothLondon[(d-3):(d+3)])+0.2
  CCdate$"Day of Value" = day(rat$date[d])
  CCdate$"Month of Value" = month(rat$date[d])
  CCdate$"Year of Value" = year(rat$date[d])
  # Add the new row
  CC <- rbind(CC, CCdate)
}
for (d in 4:(length(rat$date)-3)){
  CCdate$Geography="Midlands"
  CCdate$Value = rat$smoothMid[d]
  CCdate$"Quantile 0.05"=min(rat$smoothMid[(d-3):(d+3)])-0.2
  CCdate$"Quantile 0.25"=min(rat$smoothMid[(d-3):(d+3)])-0.1
  CCdate$"Quantile 0.5"=rat$smoothMid[d]
  CCdate$"Quantile 0.75"=max(rat$smoothMid[(d-3):(d+3)])+0.1
  CCdate$"Quantile 0.95"=max(rat$smoothMid[(d-3):(d+3)])+0.2
  CCdate$"Day of Value" = day(rat$date[d])
  CCdate$"Month of Value" = month(rat$date[d])
  CCdate$"Year of Value" = year(rat$date[d])
  # Add the new row
  CC <- rbind(CC, CCdate)
}
for (d in 4:(length(rat$date)-3)){
  CCdate$Geography="Wales"
  CCdate$Value = rat$smoothWales [d]
  CCdate$"Quantile 0.05"=min(rat$smoothWales[(d-3):(d+3)])-0.2
  CCdate$"Quantile 0.25"=min(rat$smoothWales[(d-3):(d+3)])-0.1
  CCdate$"Quantile 0.5"=rat$smoothWales[d]
  CCdate$"Quantile 0.75"=max(rat$smoothWales[(d-3):(d+3)])+0.1
  CCdate$"Quantile 0.95"=max(rat$smoothWales[(d-3):(d+3)])+0.2
  CCdate$"Day of Value" = day(rat$date[d])
  CCdate$"Month of Value" = month(rat$date[d])
  CCdate$"Year of Value" = year(rat$date[d])
  # Add the new row
  CC <- rbind(CC, CCdate)
}
for (d in 4:(length(rat$date)-3)){
  CCdate$Geography="Northern Ireland"
  CCdate$Value = rat$smoothNI[d]
  CCdate$"Quantile 0.05"=min(rat$smoothNI[(d-3):(d+3)])-0.2
  CCdate$"Quantile 0.25"=min(rat$smoothNI[(d-3):(d+3)])-0.1
  CCdate$"Quantile 0.5"=rat$smoothNI[d]
  CCdate$"Quantile 0.75"=max(rat$smoothNI[(d-3):(d+3)])+0.1
  CCdate$"Quantile 0.95"=max(rat$smoothNI[(d-3):(d+3)])+0.2
  CCdate$"Day of Value" = day(rat$date[d])
  CCdate$"Month of Value" = month(rat$date[d])
  CCdate$"Year of Value" = year(rat$date[d])
  # Add the new row
  CC <- rbind(CC, CCdate)
}
# Write to excel


#  Medium term projections


today <- today()
ageband <-  "All"
CCdate$Scenario="NowCast"
CCdate$Geography=region
CCdate$ValueType="hospital_inc"
#  Log. Errors from fluctuations times 2 for methodological uncertainty
#  adjust for recent discrepancy

for (d in 8:(nrow(comp$newSARI)-22)){
  if(comp$DEATH$date[d]<(today-reporting_delay)){ CCdate$Scenario="MTP"}
  CCdate$Value = sum(comp$newSARI[d,2:20])
  CCdate$"Quantile 0.05"=max(0,CCdate$Value*(1-6*sqrt(sum(comp$newSARI[(d-6):d,2:20])/7)/CCdate$Value))
  CCdate$"Quantile 0.25"=max(0,CCdate$Value*(1-2*sqrt(sum(comp$newSARI[(d-6):d,2:20])/7)/CCdate$Value))
  CCdate$"Quantile 0.5"=CCdate$Value
  CCdate$"Quantile 0.75"=CCdate$Value*(1+2*sqrt(sum(comp$newSARI[(d-6):d,2:20])/7)/CCdate$Value)
  CCdate$"Quantile 0.95"=CCdate$Value*(1+6*sqrt(sum(comp$newSARI[(d-7):d,2:20])/7)/CCdate$Value)
  CCdate$"Day of Value" = day(comp$newSARI$date[d])
  CCdate$"Month of Value" = month(comp$newSARI$date[d])
  CCdate$"Year of Value" = year(comp$newSARI$date[d])
  # Add the new row
  CC <- rbind(CC, CCdate)
}
CCdate$ValueType="death_inc_line"
CCdate$Scenario="NowCast"
for (d in 8:(nrow(comp$DEATH)-22)){
  if(comp$DEATH$date[d]<(today-reporting_delay)){ CCdate$Scenario="MTP"}
  CCdate$Value = sum(comp$DEATH[d,2:20])
  CCdate$"Quantile 0.05"=max(0,CCdate$Value*(1-3*sqrt(sum(comp$DEATH[(d-6):d,2:20])/7)/CCdate$Value))
  CCdate$"Quantile 0.25"=max(0,CCdate$Value*(1-sqrt(sum(comp$DEATH[(d-6):d,2:20])/7)/CCdate$Value))
  CCdate$"Quantile 0.5"=CCdate$Value
  CCdate$"Quantile 0.75"=CCdate$Value*(1+sqrt(sum(comp$DEATH[(d-6):d,2:20])/7)/CCdate$Value)
  CCdate$"Quantile 0.95"=CCdate$Value*(1+3*sqrt(sum(comp$DEATH[(d-7):d,2:20])/7)/CCdate$Value)
  CCdate$"Day of Value" = day(comp$DEATH$date[d])
  CCdate$"Month of Value" = month(comp$DEATH$date[d])
  CCdate$"Year of Value" = year(comp$DEATH$date[d])
  # Add the new row
  CC <- rbind(CC, CCdate)
}

#  Check with ONS CCdate$ValueType="prevalence"
CCdate$ValueType="incidence"
CCdate$Scenario="NowCast"
for (d in 8:(nrow(comp$CASE)-22)){
  if(comp$CASE$date[d]<(today-reporting_delay)){ CCdate$Scenario="MTP"}
  CCdate$Value = sum(comp$CASE[d,2:20])
  CCdate$"Quantile 0.05"=CCdate$Value*0.5
  CCdate$"Quantile 0.25"=CCdate$Value*0.75
  CCdate$"Quantile 0.5"=CCdate$Value
  CCdate$"Quantile 0.75"=CCdate$Value*1.5
  CCdate$"Quantile 0.95"=CCdate$Value*2
  CCdate$"Day of Value" = day(comp$CASE$date[d])
  CCdate$"Month of Value" = month(comp$CASE$date[d])
  CCdate$"Year of Value" = year(comp$CASE$date[d])
  # Add the new row
  CC <- rbind(CC, CCdate)
}


#  Crystalcast format output  
write.xlsx(CC, file = paste("Data/CCcompartment",today,".xlsx"), sheetName = "WSS", rowNames = FALSE, overwrite=TRUE)
