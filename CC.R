#!/usr/bin/env Rscript
#
# Code to write out excel using the CC Schema.
#

# Load packages  - Without java dependency 
library(openxlsx)
library(lubridate)

NIWal_write <- function(){
  startwrite=startdate+400
  # Initiate variables
  group <- "Edinburgh"
  model <-  "WSS"
  scenario <- "Nowcast"
  modeltype <- "Cases"
  version <- 0.1
  today <- today()
  ageband <-  "All"
  # Region should be inherited from most recent compartment run, e.g. region <- "Scotland"
  valuetype <- "R"
  
  # If you need multiple repeated values you can use the rep command so
  # rep(value, number of repetitions), e.g. rep(group, length(SomeVector)).
  CCx <- data.frame(
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
    "Quantile 0.05" = "",
    "Quantile 0.1" = "",
    "Quantile 0.15" = "",
    "Quantile 0.2" = "",
    "Quantile 0.25" ="",
    "Quantile 0.3" = "",
    "Quantile 0.35" = "",
    "Quantile 0.4" = "",
    "Quantile 0.45" = "",
    "Quantile 0.5" = "",
    "Quantile 0.55" = "",
    "Quantile 0.6" = "",
    "Quantile 0.65" = "",
    "Quantile 0.7" = "",
    "Quantile 0.75" ="",
    "Quantile 0.8" = "",
    "Quantile 0.85" = "",
    "Quantile 0.9" = "",
    "Quantile 0.95"="" ,
    check.names = FALSE
  )
  
  CCage <- CCx
  CCtmp <- CCx

  
  for (d in 400:(length(rat$date)-3)){
    CCtmp$Geography="Wales"
    CCtmp$Value = rat$smoothWales[d]
    CCtmp$"Quantile 0.05"=min(rat$smoothWales[(d-3):(d+3)])-0.2
    CCtmp$"Quantile 0.25"=min(rat$smoothWales[(d-3):(d+3)])-0.1
    CCtmp$"Quantile 0.5"=rat$smoothWales[d]
    CCtmp$"Quantile 0.75"=max(rat$smoothWales[(d-3):(d+3)])+0.1
    CCtmp$"Quantile 0.95"=max(rat$smoothWales[(d-3):(d+3)])+0.2
    CCtmp$"Day of Value" = day(rat$date[d])
    CCtmp$"Month of Value" = month(rat$date[d])
    CCtmp$"Year of Value" = year(rat$date[d])
    # Add the new row
    CCx <- rbind(CCx, CCtmp)
  }
  
  for (d in 400:(length(rat$date)-3)){
    CCtmp$Geography="Northern Ireland"
    CCtmp$Value = rat$smoothNI[d]
    CCtmp$"Quantile 0.05"=min(rat$smoothNI[(d-3):(d+3)])-0.2
    CCtmp$"Quantile 0.25"=min(rat$smoothNI[(d-3):(d+3)])-0.1
    CCtmp$"Quantile 0.5"=rat$smoothNI[d]
    CCtmp$"Quantile 0.75"=max(rat$smoothNI[(d-3):(d+3)])+0.1
    CCtmp$"Quantile 0.95"=max(rat$smoothNI[(d-3):(d+3)])+0.2
    CCtmp$"Day of Value" = day(rat$date[d])
    CCtmp$"Month of Value" = month(rat$date[d])
    CCtmp$"Year of Value" = year(rat$date[d])
    # Add the new row
    CCx <- rbind(CCx, CCtmp)
  }
  # MTPs moved to CC_write Write to excel
CCx <- CCx[-c(1),]
return(CCx)
}
#  Crystalcast format output  - now returned to write in regional.R along with everything else
# write.xlsx(CC, file = paste("Data/CCcompartment",today,".xlsx"), sheetName = "WSS", rowNames = FALSE, overwrite=TRUE)
