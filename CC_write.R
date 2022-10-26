#!/usr/bin/env Rscript
#
# Code to write out excel using the CC Schema.
#

# Load packages  - Without java dependency
library(openxlsx)
library(lubridate)

CC_write <- function(CCcomp,region,pop,R_region,Q_region,Rseries,ratio,Missing_incidence){
# write from arbitrary start point to six weeks time


startwrite=length(CCcomp$CASE$date)-120
endwrite=length(CCcomp$CASE$date)-40
group <- "Edinburgh"
model <-  "WSS"
scenario <- "Nowcast"
modeltype <- "Cases"
version <- 1.0
as.integer(endwrite-enddate) 
ageband <-  "All"
# Region should be inherited from most recent CCcompartment run, e.g. region <- "Scotland"
Valuetype <- "R"

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
  ValueType = Valuetype,
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
 
#  systemic uncertainty estimate (fractional)  
  sigma=0.15
  #  R number Nowcast.  Error mainly comes from uncertainty in GenTime
  CCtmp$Scenario="Nowcast"
  CCtmp$Geography=region
  CCtmp$ValueType="R"
  for (d in startwrite:(length(Rseries)-3)){
    CCtmp$Value = Rseries[d]
    CCtmp$"Quantile 0.05"=max(min(Rseries[(d-3):(d+3)])*0.85,0)
    CCtmp$"Quantile 0.25"=max(min(Rseries[(d-3):(d+3)])*0.925,0)

    CCtmp$"Quantile 0.5"=Rseries[d]
    CCtmp$"Quantile 0.75"=max(Rseries[(d-3):(d+3)])*1.075
    CCtmp$"Quantile 0.95"=max(Rseries[(d-3):(d+3)])*1.15
    CCtmp$"Day of Value" = day(rat$date[d])
    CCtmp$"Month of Value" = month(rat$date[d])
    CCtmp$"Year of Value" = year(rat$date[d])
    # Add the new row
    CC <- rbind(CC, CCtmp)
  }
  #  Delete the first row - Crystal cast requires numerical order
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

#  in addition to R-uncertainty, add uncertainty in missing_incidence from Feb2022 due to
#  removal of confirmatory PCR tests
#
#  Uncertainty in two parts - in data collection +-10%, R_number cumulative projection.  sqrt capped at 0 - assuming random walk.
# 0 would mean the infection dies out faster than the generation time, which might happen from large R uncertainty.  
#
R_error=  exp(  (sqrt(1.0+Q_region-R_region)-1.0)  /genTime)
cum_error=R_error

for (d in startwrite:endwrite){
  if(CCcomp$CASE$date[d]>(today-reporting_delay)){
  CCtmp$Scenario="MTP"
  cum_error=cum_error*R_error}
  CCtmp$Value = sum(CCcomp$newSARI[d,2:20])/ratio$newhosp
  CCtmp$"Quantile 0.05"=max(0,CCtmp$Value*cum_error[1]*(1.0-2.5*sigma))
  CCtmp$"Quantile 0.25"=max(0,CCtmp$Value*cum_error[2]*(1.0-sigma))
  CCtmp$"Quantile 0.5"=CCtmp$Value
  CCtmp$"Quantile 0.75"=CCtmp$Value*cum_error[4]*(1.0+sigma)
  CCtmp$"Quantile 0.95"=CCtmp$Value*cum_error[5]*(1.0+2.5*sigma)
  CCtmp$"Day of Value" = day(CCcomp$newSARI$date[d])
  CCtmp$"Month of Value" = month(CCcomp$newSARI$date[d])
  CCtmp$"Year of Value" = year(CCcomp$newSARI$date[d])
  # Add the new row
  CC <- rbind(CC, CCtmp)
}

CCtmp$ValueType="type28_death_inc_line"
CCtmp$Scenario="MTP"

cum_error=R_error
for (d in startwrite:endwrite){
  if(CCcomp$DEATH$date[d]>(today-reporting_delay)){ 
    CCtmp$Scenario="MTP"
    cum_error=cum_error*R_error}
  CCtmp$Value = sum(CCcomp$DEATH[d,2:20])/ratio$death
  CCtmp$"Quantile 0.05"=max(0,CCtmp$Value*cum_error[1]*(1.0-3*sigma))
  CCtmp$"Quantile 0.25"=max(0,CCtmp$Value*cum_error[2]*(1.0-sigma))
  CCtmp$"Quantile 0.5"=CCtmp$Value
  CCtmp$"Quantile 0.75"=CCtmp$Value*cum_error[4]*(1.0+sigma)
  CCtmp$"Quantile 0.95"=CCtmp$Value*cum_error[5]*(1.0+3*sigma)
  CCtmp$"Day of Value" = day(CCcomp$DEATH$date[d])
  CCtmp$"Month of Value" = month(CCcomp$DEATH$date[d])
  CCtmp$"Year of Value" = year(CCcomp$DEATH$date[d])
  # Add the new row
  CC <- rbind(CC, CCtmp)
}

#  Incidence is per 100000
# Missing_prevalence, deduced weekly by hand from ONS, is the undetected ongoing cases
# Missing incidence is the undetected infections. primarily short-lived
#  Quantiles for the prevalence & incidence represent perceived methodological not statistical errors.
#  These need more detailed study!
#  4/2/22 Prevalence has been running below ONS levels for several weeks, suspect the sensitivity is improved
#  with delat/omicron having *much* higher viral loads
#  Missing Prevalence increases sharply with the withdrawal of free testing
#  Change by factor of 2 in England & Wales fitted to ONS (DJW offline) from startwrite in 2022.  
#Missing_prevalence=1.3*7.5
#Missing_incidence=Missing_prevalence
# 31/5/2022 Missing incidence estimated from ONS prevalence data in covid_trimmed

CCtmp$ValueType="incidence"
CCtmp$Scenario="Nowcast"
cum_error=R_error
for (d in startwrite:endwrite){
  if(CCcomp$CASE$date[d]>(today-reporting_delay-2)){
    CCtmp$Scenario="MTP"
    CCtmp$ValueType="infections_inc"
    cum_error=cum_error*R_error
  }

  CCtmp$Value = sum(CCcomp$CASE[d,2:20])
  CCtmp$"Quantile 0.05"=max(0,CCtmp$Value*cum_error[1]*(1.0-3*sigma))
  CCtmp$"Quantile 0.25"=max(0,CCtmp$Value*cum_error[2]*(1.0-sigma))
  CCtmp$"Quantile 0.5"=CCtmp$Value
  CCtmp$"Quantile 0.75"=CCtmp$Value*cum_error[4]*(1.0+sigma)
  CCtmp$"Quantile 0.95"=CCtmp$Value*cum_error[5]*(1.0+3*sigma)
  CCtmp$"Day of Value" = day(CCcomp$CASE$date[d])
  CCtmp$"Month of Value" = month(CCcomp$CASE$date[d])
  CCtmp$"Year of Value" = year(CCcomp$CASE$date[d])
  # Add the new row
  CC <- rbind(CC, CCtmp)
}
#  Missing prevalence covers discrepancy between ONS and case data
#  Also assume a five day delay between infection and test.
#  Prevalence is a percentage who would test positive
#From 22/06 CASEs alerady contain missing incidence
# There is some difficulty about the ONS data c/f e.g. https://www.medrxiv.org/content/10.1101/2021.02.09.21251411v1.full.pdf
CCtmp$ValueType="prevalence"
CCtmp$Scenario="Nowcast"
cum_error=R_error
for (d in (startwrite+20):(endwrite)){
  if(CCcomp$CASE$date[d]>(today-reporting_delay)){
  CCtmp$Scenario="MTP"
  CCtmp$ValueType="prevalence_mtp"
  cum_error=cum_error*R_error
  }
  PREV= sum(CCcomp$CASE[(d-12):(d),2:20])

  CCtmp$Value=PREV/pop*100
  CCtmp$"Quantile 0.05"=max(0,CCtmp$Value*cum_error[1]*(1.0-3*sigma))
  CCtmp$"Quantile 0.25"=max(0,CCtmp$Value*cum_error[2]*(1.0-sigma))
  CCtmp$"Quantile 0.5"=CCtmp$Value
  CCtmp$"Quantile 0.75"=CCtmp$Value*cum_error[4]*(1.0+sigma)
  CCtmp$"Quantile 0.95"=CCtmp$Value*cum_error[5]*(1.0+3*sigma)
  CCtmp$"Day of Value" = day(CCcomp$ILI$date[d])
  CCtmp$"Month of Value" = month(CCcomp$ILI$date[d])
  CCtmp$"Year of Value" = year(CCcomp$ILI$date[d])
  # Add the new row
  CC <- rbind(CC, CCtmp)
}

# No need to do this provides Pred extends beyond endwrite Extend for the last three days assuming derivative of CASE[d] is zero 

CCtmp$ValueType="hospital_prev"
CCtmp$Scenario="MTP"
cum_error=R_error
for (d in startwrite:(endwrite)){
  if(CCcomp$CASE$date[d]>(today-reporting_delay)){
  CCtmp$Scenario="MTP"
  CCtmp$ValueType="hospital_prev"
  # Prevalence is stochastically much more stable than incidence, but cumulative error is not
  #  Reduce sigma by sqrt(length of stay)
  #  increase cum_error twice as fast
  
  cum_error=cum_error*R_error*R_error
  }
  OCC = sum(CCcomp$SARI[d,2:20]+CCcomp$CRIT[d,2:20]+CCcomp$CRITREC[d,2:20])/ratio$hosp
  CCtmp$Value=OCC

  CCtmp$"Quantile 0.05"=max(0,CCtmp$Value*cum_error[1]*(1.0-sigma))
  CCtmp$"Quantile 0.25"=max(0,CCtmp$Value*cum_error[2]*(1.0-0.3*sigma))
  CCtmp$"Quantile 0.5"=CCtmp$Value
  CCtmp$"Quantile 0.75"=CCtmp$Value*cum_error[4]*(1.0+0.3*sigma)
  CCtmp$"Quantile 0.95"=CCtmp$Value*cum_error[5]*(1.0+sigma)
  CCtmp$"Day of Value" = day(CCcomp$SARI$date[d])
  CCtmp$"Month of Value" = month(CCcomp$SARI$date[d])
  CCtmp$"Year of Value" = year(CCcomp$SARI$date[d])
  # Add the new row
  CC <- rbind(CC, CCtmp)
}

#  Crystalcast format output  

#End Wales NI exclusion ifs
}}
return(CC)
}

