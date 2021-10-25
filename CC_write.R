#!/usr/bin/env Rscript
#
# Code to write out excel using the CC Schema.
#

CC_write <- function(CCcomp,region){
# Initiate variables

startwrite=200  

group <- "Edinburgh"
model <-  "WSS"
scenario <- "NowCast"
modeltype <- "Cases"
version <- 0.1
today <- today()
ageband <-  "All"
# Region should be inherited from most recent CCcompartment run, e.g. region <- "Scotland"
valuetype <- "R"

# Load packages  - Without java dependency 
library(openxlsx)
library(lubridate)

#  Initiate CC with Scotland default - overwrite later

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
CCtmp<-CC  
#  Medium term projections


today <- today()
ageband <-  "All"
CCtmp$Scenario="MTP"
CCtmp$Geography=region
CCtmp$ValueType="hospital_inc"
#  Log. Errors from fluctuations time 4 for methodological uncertainty
#  adjust for recent discrepancy
for (d in startwrite:(nrow(CCcomp$newSARI)-22)){
  CCtmp$Value = sum(CCcomp$newSARI[d,2:20])
  CCtmp$"Quantile 0.05"=max(0,CCtmp$Value*(1-12*sqrt(sum(CCcomp$newSARI[(d-6):d,2:20])/7)/CCtmp$Value))
  CCtmp$"Quantile 0.25"=max(0,CCtmp$Value*(1-4*sqrt(sum(CCcomp$newSARI[(d-6):d,2:20])/7)/CCtmp$Value))
  CCtmp$"Quantile 0.5"=CCtmp$Value
  CCtmp$"Quantile 0.75"=CCtmp$Value*(1+4*sqrt(sum(CCcomp$newSARI[(d-6):d,2:20])/7)/CCtmp$Value)
  CCtmp$"Quantile 0.95"=CCtmp$Value*(1+12*sqrt(sum(CCcomp$newSARI[(d-7):d,2:20])/7)/CCtmp$Value)
  CCtmp$"Day of Value" = day(CCcomp$newSARI$date[d])
  CCtmp$"Month of Value" = month(CCcomp$newSARI$date[d])
  CCtmp$"Year of Value" = year(CCcomp$newSARI$date[d])
  # Add the new row
  CC <- rbind(CC, CCtmp)
}
CCtmp$ValueType="death_inc_line"

for (d in startwrite:(nrow(CCcomp$DEATH)-22)){
  CCtmp$Value = sum(CCcomp$DEATH[d,2:20])
  CCtmp$"Quantile 0.05"=max(0,CCtmp$Value*(1-12*sqrt(sum(CCcomp$DEATH[(d-6):d,2:20])/7)/CCtmp$Value))
  CCtmp$"Quantile 0.25"=max(0,CCtmp$Value*(1-4*sqrt(sum(CCcomp$DEATH[(d-6):d,2:20])/7)/CCtmp$Value))
  CCtmp$"Quantile 0.5"=CCtmp$Value
  CCtmp$"Quantile 0.75"=CCtmp$Value*(1+4*sqrt(sum(CCcomp$DEATH[(d-6):d,2:20])/7)/CCtmp$Value)
  CCtmp$"Quantile 0.95"=CCtmp$Value*(1+12*sqrt(sum(CCcomp$DEATH[(d-7):d,2:20])/7)/CCtmp$Value)
  CCtmp$"Day of Value" = day(CCcomp$DEATH$date[d])
  CCtmp$"Month of Value" = month(CCcomp$DEATH$date[d])
  CCtmp$"Year of Value" = year(CCcomp$DEATH$date[d])
  # Add the new row
  CC <- rbind(CC, CCtmp)
}
#  Check with ONS 
CCtmp$ValueType="incidence"
for (d in startwrite:(nrow(CCcomp$CASE)-22)){
  CCtmp$Value = sum(CCcomp$CASE[d,2:20])
  CCtmp$"Quantile 0.05"=max(0,CCtmp$Value*(1-12*sqrt(sum(CCcomp$CASE[(d-6):d,2:20])/7)/CCtmp$Value))
  CCtmp$"Quantile 0.25"=max(0,CCtmp$Value*(1-4*sqrt(sum(CCcomp$CASE[(d-6):d,2:20])/7)/CCtmp$Value))
  CCtmp$"Quantile 0.5"=CCtmp$Value
  CCtmp$"Quantile 0.75"=CCtmp$Value*(1+4*sqrt(sum(CCcomp$CASE[(d-6):d,2:20])/7)/CCtmp$Value)
  CCtmp$"Quantile 0.95"=CCtmp$Value*(1+12*sqrt(sum(CCcomp$CASE[(d-7):d,2:20])/7)/CCtmp$Value)
  CCtmp$"Day of Value" = day(CCcomp$CASE$date[d])
  CCtmp$"Month of Value" = month(CCcomp$CASE$date[d])
  CCtmp$"Year of Value" = year(CCcomp$CASE$date[d])
  # Add the new row
  CC <- rbind(CC, CCtmp)
}
#  Missing prevalence covers discrepancy between ONS and case data
Missing_prevalence=1.1
CCtmp$ValueType="prevalence"
PREV<-CCcomp$ILI[2:20]+CCcomp$SARI[2:20]+CCcomp$CRIT[2:20]+CCcomp$MILD[2:20]
PREV=PREV*Missing_prevalence
for (d in startwrite:(nrow(PREV)-22)){
  CCtmp$Value = sum(PREV[d,])
  CCtmp$"Quantile 0.05"=max(0,CCtmp$Value*(1-12*sqrt(sum(PREV[(d-6):d,])/7)/CCtmp$Value))
  CCtmp$"Quantile 0.25"=max(0,CCtmp$Value*(1-4*sqrt(sum(PREV[(d-6):d,])/7)/CCtmp$Value))
  CCtmp$"Quantile 0.5"=CCtmp$Value
  CCtmp$"Quantile 0.75"=CCtmp$Value*(1+4*sqrt(sum(PREV[(d-6):d,])/7)/CCtmp$Value)
  CCtmp$"Quantile 0.95"=CCtmp$Value*(1+12*sqrt(sum(PREV[(d-7):d,])/7)/CCtmp$Value)
  CCtmp$"Day of Value" = day(CCcomp$ILI$date[d])
  CCtmp$"Month of Value" = month(CCcomp$ILI$date[d]) 
  CCtmp$"Year of Value" = year(CCcomp$ILI$date[d])
  # Add the new row
  CC <- rbind(CC, CCtmp)
}
#  Crystalcast format output  

write.xlsx(CC, file = paste("data/CCcompartment",today(),region,".xlsx"), 
         overwrite = TRUE,  sheetName = region, rowNames = FALSE)

}

