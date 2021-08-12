#!/usr/bin/env Rscript
#
# Code to write out excel using the CC Schema.
#

# Load packages
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
region <- "Scotland"
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
  Geography = region,
  ValueType = valuetype,
  Value = R_Scotland_BestGuess,
  "Quantile 0.05" = R_Scotland_Quant[1],
  "Quantile 0.1" = "",
  "Quantile 0.15" = "",
  "Quantile 0.2" = "",
  "Quantile 0.25" = R_Scotland_Quant[2],
  "Quantile 0.3" = "",
  "Quantile 0.35" = "",
  "Quantile 0.4" = "",
  "Quantile 0.45" = "",
  "Quantile 0.5" = R_Scotland_Quant[3],
  "Quantile 0.55" = "",
  "Quantile 0.6" = "",
  "Quantile 0.65" = "",
  "Quantile 0.7" = "",
  "Quantile 0.75" = R_Scotland_Quant[4],
  "Quantile 0.8" = "",
  "Quantile 0.85" = "",
  "Quantile 0.9" = "",
  "Quantile 0.95" = R_Scotland_Quant[5],
  check.names = FALSE
)
# Need to create a list to add more rows to the CrystalCast df
CCEng <- CC
CCEng$Geography="England"
CCEng$"Quantile 0.05"=R_England_Quant[1]
CCEng$"Quantile 0.25"=R_England_Quant[2]
CCEng$"Quantile 0.5"=R_England_Quant[3]
CCEng$"Quantile 0.75"=R_England_Quant[4]
CCEng$"Quantile 0.95"=R_England_Quant[5]
CCEng$Value = R_England_BestGuess
# Add the new row
CC <- rbind(CC, CCEng)

CCage <- CCEng
CCage$Geography="England"
CCage$AgeBand="0-4"
CCage$"Quantile 0.05"=Growth_00_Quant[1]
CCage$"Quantile 0.25"=Growth_00_Quant[2]
CCage$"Quantile 0.5"=Growth_00_Quant[3]
CCage$"Quantile 0.75"=Growth_00_Quant[4]
CCage$"Quantile 0.95"=Growth_00_Quant[5]
CCage$Value = Growth_00_BestGuess
# Add the new row
CC <- rbind(CC, CCage)

CCage$Geography="England"
CCage$AgeBand="5-14"
CCage$"Quantile 0.05"=Growth_05_Quant[1]
CCage$"Quantile 0.25"=Growth_05_Quant[2]
CCage$"Quantile 0.5"=Growth_05_Quant[3]
CCage$"Quantile 0.75"=Growth_05_Quant[4]
CCage$"Quantile 0.95"=Growth_05_Quant[5]
CCage$Value = Growth_05_BestGuess
# Add the new row
CC <- rbind(CC, CCage)

CCage <- CCEng
CCage$Geography="England"
CCage$AgeBand="15-24"
CCage$"Quantile 0.05"=Growth_15_Quant[1]
CCage$"Quantile 0.25"=Growth_15_Quant[2]
CCage$"Quantile 0.5"=Growth_15_Quant[3]
CCage$"Quantile 0.75"=Growth_15_Quant[4]
CCage$"Quantile 0.95"=Growth_15_Quant[5]
CCage$Value = Growth_15_BestGuess
# Add the new row
CC <- rbind(CC, CCage)


CCage <- CCEng
CCage$Geography="England"
CCage$AgeBand="25-44"
CCage$"Quantile 0.05"=Growth_25_Quant[1]
CCage$"Quantile 0.25"=Growth_25_Quant[2]
CCage$"Quantile 0.5"=Growth_25_Quant[3]
CCage$"Quantile 0.75"=Growth_25_Quant[4]
CCage$"Quantile 0.95"=Growth_25_Quant[5]
CCage$Value = Growth_25_BestGuess
# Add the new row
CC <- rbind(CC, CCage)


CCage <- CCEng
CCage$Geography="England"
CCage$AgeBand="45-64"
CCage$"Quantile 0.05"=Growth_45_Quant[1]
CCage$"Quantile 0.25"=Growth_45_Quant[2]
CCage$"Quantile 0.5"=Growth_45_Quant[3]
CCage$"Quantile 0.75"=Growth_45_Quant[4]
CCage$"Quantile 0.95"=Growth_45_Quant[5]
CCage$Value = Growth_45_BestGuess
# Add the new row
CC <- rbind(CC, CCage)

CCage <- CCEng
CCage$Geography="England"
CCage$AgeBand="65-74"
CCage$"Quantile 0.05"=Growth_65_Quant[1]
CCage$"Quantile 0.25"=Growth_65_Quant[2]
CCage$"Quantile 0.5"=Growth_65_Quant[3]
CCage$"Quantile 0.75"=Growth_65_Quant[4]
CCage$"Quantile 0.95"=Growth_65_Quant[5]
CCage$Value = Growth_65_BestGuess
# Add the new row
CC <- rbind(CC, CCage)


CCage <- CCEng
CCage$Geography="England"
CCage$AgeBand="75+"
CCage$"Quantile 0.05"=Growth_75_Quant[1]
CCage$"Quantile 0.25"=Growth_75_Quant[2]
CCage$"Quantile 0.5"=Growth_75_Quant[3]
CCage$"Quantile 0.75"=Growth_75_Quant[4]
CCage$"Quantile 0.95"=Growth_75_Quant[5]
CCage$Value = Growth_75_BestGuess
# Add the new row
CC <- rbind(CC, CCage)

CCmid <-CCEng
CCmid$Geography="North East"
CCmid$Value = R_NEY_BestGuess
Quant = R_NEY_Quant
CCmid$"Quantile 0.05"=Quant[1]
CCmid$"Quantile 0.25"=Quant[2]
CCmid$"Quantile 0.5"=Quant[3]
CCmid$"Quantile 0.75"=Quant[4]
CCmid$"Quantile 0.95"=Quant[5]
CC <- rbind(CC, CCmid)

CCmid <-CCEng
CCmid$Geography="North West"
CCmid$Value = R_NW_BestGuess
Quant = R_NW_Quant
CCmid$"Quantile 0.05"=Quant[1]
CCmid$"Quantile 0.25"=Quant[2]
CCmid$"Quantile 0.5"=Quant[3]
CCmid$"Quantile 0.75"=Quant[4]
CCmid$"Quantile 0.95"=Quant[5]
CC <- rbind(CC, CCmid)


CCmid <-CCEng
CCmid$Geography="East of England"
CCmid$Value = R_EE_BestGuess
Quant = R_EE_Quant
CCmid$"Quantile 0.05"=Quant[1]
CCmid$"Quantile 0.25"=Quant[2]
CCmid$"Quantile 0.5"=Quant[3]
CCmid$"Quantile 0.75"=Quant[4]
CCmid$"Quantile 0.95"=Quant[5]
CC <- rbind(CC, CCmid)


CCmid <-CCEng
CCmid$Geography="London"
CCmid$Value = R_London_BestGuess
Quant = unname(quantile(tail(rat$London), probs=c(0.05,0.25,0.5,0.75,0.95)))
CCmid$"Quantile 0.05"=R_London_Quant[1]
CCmid$"Quantile 0.25"=R_London_Quant[2]
CCmid$"Quantile 0.5"=R_London_Quant[3]
CCmid$"Quantile 0.75"=R_London_Quant[4]
CCmid$"Quantile 0.95"=R_London_Quant[5]
# Add the new row
CC <- rbind(CC, CCmid)

CCmid <-CCEng
CCmid$Geography="South East"
CCmid$Value = R_SE_BestGuess
Quant = R_SE_Quant
CCmid$"Quantile 0.05"=Quant[1]
CCmid$"Quantile 0.25"=Quant[2]
CCmid$"Quantile 0.5"=Quant[3]
CCmid$"Quantile 0.75"=Quant[4]
CCmid$"Quantile 0.95"=Quant[5]
CC <- rbind(CC, CCmid)


CCmid <-CCEng
CCmid$Geography="South West"
CCmid$Value = R_SW_BestGuess
Quant = R_SW_Quant
CCmid$"Quantile 0.05"=Quant[1]
CCmid$"Quantile 0.25"=Quant[2]
CCmid$"Quantile 0.5"=Quant[3]
CCmid$"Quantile 0.75"=Quant[4]
CCmid$"Quantile 0.95"=Quant[5]
CC <- rbind(CC, CCmid)


CCmid <-CCEng
CCmid$Geography="Midlands"
CCmid$Value = R_Midlands_BestGuess
Quant = R_Midlands_Quant
CCmid$"Quantile 0.05"=Quant[1]
CCmid$"Quantile 0.25"=Quant[2]
CCmid$"Quantile 0.5"=Quant[3]
CCmid$"Quantile 0.75"=Quant[4]
CCmid$"Quantile 0.95"=Quant[5]
# Add the new row
CC <- rbind(CC, CCmid)

for (d in 4:(length(smoothweightR$date)-3)){
CCdate <-CCEng
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


# Write to excel
write.xlsx(CC, file = "Data/WSS_CC.xlsx", sheetName = "WSS", row.names = FALSE, overwrite = TRUE)

