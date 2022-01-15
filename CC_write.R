#!/usr/bin/env Rscript
#
# Code to write out excel using the CC Schema.
#

# Load packages  - Without java dependency
library(openxlsx)
library(lubridate)

CC_write <- function(CCcomp,region,pop,R_region,Q_region,Rseries,ratio,filename){
# write from arbitrary start point to six weeks time
startwrite=470
endwrite=nrow(regcases)+reporting_delay+44
group <- "Edinburgh"
model <-  "WSS"
scenario <- "Nowcast"
modeltype <- "Cases"
version <- 0.1
today <- today()
ageband <-  "All"
# Region should be inherited from most recent CCcompartment run, e.g. region <- "Scotland"
valuetype <- "R"

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
  "Day of Value" = day(enddate),
  "Month of Value" = month(enddate),
  "Year of Value" = year(enddate),
  AgeBand = ageband,
  Geography = region,
  ValueType = valuetype,
  Value = R_region,
  "Quantile 0.05" = Q_region[1],
  "Quantile 0.1" = "",
  "Quantile 0.15" = "",
  "Quantile 0.2" = "",
  "Quantile 0.25" = Q_region[2],
  "Quantile 0.3" = "",
  "Quantile 0.35" = "",
  "Quantile 0.4" = "",
  "Quantile 0.45" = "",
  "Quantile 0.5" = Q_region[3],
  "Quantile 0.55" = "",
  "Quantile 0.6" = "",
  "Quantile 0.65" = "",
  "Quantile 0.7" = "",
  "Quantile 0.75" = Q_region[4],
  "Quantile 0.8" = "",
  "Quantile 0.85" = "",
  "Quantile 0.9" = "",
  "Quantile 0.95" = Q_region[5],
  check.names = FALSE
)
  CCtmp<-CC
  #  R number Nowcast.  Error mainly comes from uncertainty in GenTime
  CCtmp$Scenario="Nowcast"
  CCtmp$Geography=region
  CCtmp$ValueType="R"
  for (d in startwrite:(length(Rseries)-3)){
    CCtmp$Value = Rseries[d]
    CCtmp$"Quantile 0.05"=min(Rseries[(d-3):(d+3)])-0.15
    CCtmp$"Quantile 0.25"=min(Rseries[(d-3):(d+3)])-0.075
    CCtmp$"Quantile 0.5"=Rseries[d]
    CCtmp$"Quantile 0.75"=max(Rseries[(d-3):(d+3)])+0.075
    CCtmp$"Quantile 0.95"=max(Rseries[(d-3):(d+3)])+0.15
    CCtmp$"Day of Value" = day(rat$date[d])
    CCtmp$"Month of Value" = month(rat$date[d])
    CCtmp$"Year of Value" = year(rat$date[d])
    # Add the new row
    CC <- rbind(CC, CCtmp)
  }
  d#  Delete the first row - Crystal cast requires numerical order
  CC <- CC[-c(1),]

  #  Hindcast for growth rate
  CCtmp$Scenario="Nowcast"
  CCtmp$Geography=region
  CCtmp$ValueType="growth_rate"
  for (d in startwrite:(length(Rseries)-3)){
    CCtmp$Value = exp((Rseries[d]-1.0)/genTime)-1.0
    CCtmp$"Quantile 0.05"=exp((min(Rseries[(d-3):(d+3)])-0.1-1.0)/genTime)-1.0
    CCtmp$"Quantile 0.25"=exp((min(Rseries[(d-3):(d+3)])-0.05-1.0)/genTime)-1.0
    CCtmp$"Quantile 0.5"=exp((Rseries[d]-1.0)/genTime)-1.0
    CCtmp$"Quantile 0.75"=exp((max(Rseries[(d-3):(d+3)])+0.05-1.0)/genTime)-1.0
    CCtmp$"Quantile 0.95"=exp((max(Rseries[(d-3):(d+3)])+0.1-1.0)/genTime)-1.0
    CCtmp$Value = exp((Rseries[d]-1.0)/genTime)-1.0
    CCtmp$"Day of Value" = day(rat$date[d])
    CCtmp$"Month of Value" = month(rat$date[d])
    CCtmp$"Year of Value" = year(rat$date[d])
    # Add the new row
    CC <- rbind(CC, CCtmp)
  }

  if(region!="Wales"){
   if(region!="Northern Ireland"){
today <- Sys.Date()
ageband <-  "All"
CCtmp$Scenario="MTP"
CCtmp$Geography=region
CCtmp$ValueType="hospital_inc"
#  Log. Errors from fluctuations time 4 for methodological uncertainty
#  adjust for recent discrepancies
#  Would like regional admissions data, only have totals, but use them anyway
#  Omicron uncertainty *10 on R, from 21/12/21
R_error=1
for (d in startwrite:endwrite){
  if(CCcomp$CASE$date[d]>(today-reporting_delay)){ R_error=R_error+0.2/genTime
                                                   CCtmp$Scenario="MTP"}
  CCtmp$Value = sum(CCcomp$newSARI[d,2:20])/ratio$newhosp
  NStoday = sum(CCcomp$newSARI[d,2:20])
  CCtmp$"Quantile 0.05"=max(0,NStoday*(1-6*sqrt(sum(CCcomp$newSARI[(d-6):d,2:20])/7)*R_error/NStoday))/ratio$newhosp
  CCtmp$"Quantile 0.25"=max(0,NStoday*(1-2*sqrt(sum(CCcomp$newSARI[(d-6):d,2:20])/7)*R_error/NStoday))/ratio$newhosp
  CCtmp$"Quantile 0.5"=CCtmp$Value
  CCtmp$"Quantile 0.75"=NStoday*(1+4*sqrt(sum(CCcomp$newSARI[(d-6):d,2:20])/7)*R_error/NStoday)/ratio$newhosp
  CCtmp$"Quantile 0.95"=NStoday*(1+12*sqrt(sum(CCcomp$newSARI[(d-6):d,2:20])/7)*R_error/NStoday)/ratio$newhosp
  CCtmp$"Day of Value" = day(CCcomp$newSARI$date[d])
  CCtmp$"Month of Value" = month(CCcomp$newSARI$date[d])
  CCtmp$"Year of Value" = year(CCcomp$newSARI$date[d])
  # Add the new row
  CC <- rbind(CC, CCtmp)
}
CCtmp$ValueType="type28_death_inc_line"
CCtmp$Scenario="MTP"
R_error=1
for (d in startwrite:endwrite){
  if(CCcomp$DEATH$date[d]>(today-reporting_delay)){ CCtmp$Scenario="MTP"
                                                    R_error=R_error+0.2/genTime}  
  CCtmp$Value = sum(CCcomp$DEATH[d,2:20])/ratio$death
  CCtmp$"Quantile 0.05"=max(0,CCtmp$Value*(1-sqrt(sum(CCcomp$DEATH[(d-6):d,2:20])/7)*3*R_error/CCtmp$Value))
  CCtmp$"Quantile 0.25"=max(0,CCtmp$Value*(1-sqrt(sum(CCcomp$DEATH[(d-6):d,2:20])/7)*R_error/CCtmp$Value))
  CCtmp$"Quantile 0.5"=CCtmp$Value
  CCtmp$"Quantile 0.75"=CCtmp$Value*(1+sqrt(sum(CCcomp$DEATH[(d-6):d,2:20])/7)*R_error/CCtmp$Value)
  CCtmp$"Quantile 0.95"=CCtmp$Value*(1+sqrt(sum(CCcomp$DEATH[(d-6):d,2:20])/7)*3*R_error/CCtmp$Value)
  CCtmp$"Day of Value" = day(CCcomp$DEATH$date[d])
  CCtmp$"Month of Value" = month(CCcomp$DEATH$date[d])
  CCtmp$"Year of Value" = year(CCcomp$DEATH$date[d])
  # Add the new row
  CC <- rbind(CC, CCtmp)
}
#  Incidence is per 100000
# Missing_prevalence, deduced from ONS, is the undetected ongoing cases
# Missing incidence is the undetected infections. primarily short-lived
#  Quantiles for the prevalence & incidence represent perceived methodological not statistical errors.
#  These need more detailed study!
Missing_prevalence=1.0
Missing_incidence=2.2
#scalefac=(100000/pop)*Missing_incidence convert to total numbers
scalefac=Missing_incidence
CCtmp$ValueType="incidence"
CCtmp$Scenario="Nowcast"
R_error=1.0
for (d in startwrite:endwrite){
  if(CCcomp$CASE$date[d]>(today-reporting_delay)){R_error=R_error+0.3/genTime
    CCtmp$Scenario="MTP"
    CCtmp$ValueType="infections_inc"
  }
  CASEtoday=sum(CCcomp$CASE[d,2:20])
  CCtmp$Value =   CASEtoday*scalefac
  CCtmp$"Quantile 0.05"=max(0,CASEtoday*(1-0.3*R_error))*scalefac
  CCtmp$"Quantile 0.25"=max(0,CASEtoday*(1-0.1*R_error))*scalefac
  CCtmp$"Quantile 0.5"=CASEtoday*scalefac
  CCtmp$"Quantile 0.75"=CASEtoday*(1+0.1*R_error)*scalefac
  CCtmp$"Quantile 0.95"=CASEtoday*(1+0.3*R_error)*scalefac
  CCtmp$"Day of Value" = day(CCcomp$CASE$date[d])
  CCtmp$"Month of Value" = month(CCcomp$CASE$date[d])
  CCtmp$"Year of Value" = year(CCcomp$CASE$date[d])
  # Add the new row
  CC <- rbind(CC, CCtmp)
}
#  Missing prevalence covers discrepancy between ONS and case data
#  Also assume a five day delay between infection and test.
#  Prevalence is a percentage who would test positive
# There is some difficulty about the ONS data c/f e.g. https://www.medrxiv.org/content/10.1101/2021.02.09.21251411v1.full.pdf
CCtmp$ValueType="prevalence"
CCtmp$Scenario="Nowcast"
R_error=1.0
for (d in startwrite:(endwrite)){
  if(CCcomp$CASE$date[d]>(today-reporting_delay)){R_error=R_error+0.2/genTime 
    CCtmp$Scenario="MTP"
    CCtmp$ValueType="prevalence_mtp"
  }
  PREV= sum(CCcomp$ILI[d,2:20]+CCcomp$SARI[d,2:20])+sum(CCcomp$CASE[d:(d+4),2:20])*Missing_incidence
  CCtmp$Value=PREV*Missing_prevalence/pop*100
  CCtmp$"Quantile 0.05"=max(0,CCtmp$Value*(1.0-0.3*R_error))
  CCtmp$"Quantile 0.25"=max(0,CCtmp$Value*(1.0-0.1*R_error))
  CCtmp$"Quantile 0.5"=CCtmp$Value
  CCtmp$"Quantile 0.75"=CCtmp$Value*(1.0+0.1*R_error)
  CCtmp$"Quantile 0.95"=CCtmp$Value*(1.0+0.3*R_error)
  CCtmp$"Day of Value" = day(CCcomp$ILI$date[d])
  CCtmp$"Month of Value" = month(CCcomp$ILI$date[d])
  CCtmp$"Year of Value" = year(CCcomp$ILI$date[d])
  # Add the new row
  CC <- rbind(CC, CCtmp)
}

# No need to do this provides Pred extends beyond endwrite Extend for the last three days assuming derivative of CASE[d] is zero 


#  Crystalcast format output  

#End Wales NI exclusion ifs
}}

try(
if(file.exists(filename)){
wb<-loadWorkbook(filename)
addWorksheet(wb,region)
writeData(wb,region,CC)
saveWorkbook(wb,filename,overwrite = TRUE)
} else {}
)

return(CC)
}
