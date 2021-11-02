source("CC_write.R")
source("CompartmentFunction.R")
source("Predictions.R")


getData <- function(dftmp) {
  out <- dftmp %>%
    select(date = date, age = age, values = cases) %>%
    pivot_wider(id_cols = date, names_from = age, values_from = values) %>%
    select(-unassigned, -"60+", -"00_59") %>%
    filter(date >= startdate & date <= enddate) %>%
    arrange(date)
  return(out)
}


gethData <- function(dftmp) {
  out <- dftmp %>%
    select(date = date, saridat = hospitalCases, newsaridat = newAdmissions) %>%
    filter(date >= startdate & date <= enddate) %>%
    arrange(date)
  return(out)
}

#  Do the regions, after a full run of covid_trimmed
NEurl="https://api.coronavirus.data.gov.uk/v2/data?areaType=region&areaCode=E12000001&metric=newCasesBySpecimenDateAgeDemographics&format=csv"
NWurl="https://api.coronavirus.data.gov.uk/v2/data?areaType=region&areaCode=E12000002&metric=newCasesBySpecimenDateAgeDemographics&format=csv"
YHurl="https://api.coronavirus.data.gov.uk/v2/data?areaType=region&areaCode=E12000003&metric=newCasesBySpecimenDateAgeDemographics&format=csv"
EMurl="https://api.coronavirus.data.gov.uk/v2/data?areaType=region&areaCode=E12000004&metric=newCasesBySpecimenDateAgeDemographics&format=csv"
WMurl="https://api.coronavirus.data.gov.uk/v2/data?areaType=region&areaCode=E12000005&metric=newCasesBySpecimenDateAgeDemographics&format=csv"
EEurl="https://api.coronavirus.data.gov.uk/v2/data?areaType=region&areaCode=E12000006&metric=newCasesBySpecimenDateAgeDemographics&format=csv"
londonurl="https://api.coronavirus.data.gov.uk/v2/data?areaType=region&areaCode=E12000007&metric=newCasesBySpecimenDateAgeDemographics&format=csv"
SEurl="https://api.coronavirus.data.gov.uk/v2/data?areaType=region&areaCode=E12000008&metric=newCasesBySpecimenDateAgeDemographics&format=csv"
SWurl="https://api.coronavirus.data.gov.uk/v2/data?areaType=region&areaCode=E12000009&metric=newCasesBySpecimenDateAgeDemographics&format=csv"



coltypes <- cols(col_character(), col_character(),col_character(),
                 col_date(format="%Y-%m-%d"), col_character(),
                 col_integer(), col_integer(), col_number())
# Remap the ages column to be the header rows, remove the unassigned,
# 60+ and 00_59 columns, filter dates to be between the start and end
# dates and order the output by date


NE <-  read.csv(file=NEurl)
NW <-  read.csv(file=NWurl)
YH <-  read.csv(file=YHurl)
EM <-  read.csv(file=EMurl)
WM <-  read.csv(file=WMurl)
EE <-  read.csv(file=EEurl)
london <-  read.csv(file=londonurl)
SE <-  read.csv(file=SEurl)
SW <-  read.csv(file=SWurl)


NE<-getData(NE)
NW<-getData(NW)
YH<-getData(YH)
EM<-getData(EM)
WM<-getData(WM)
EE<-getData(EE)
london<-getData(london)
SE<-getData(SE)
SW<-getData(SW)

MD=EM
NEY=YH
MD[2:20]=EM[2:20]+WM[2:20]
NEY[2:20]=NE[2:20]+YH[2:20]

#  Hospital data only available by 7 NHS regions.  obvs.

SWhospurl="https://api.coronavirus.data.gov.uk/v2/data?areaType=nhsRegion&areaCode=E40000006&metric=hospitalCases&metric=newAdmissions&format=csv"
EEhospurl="https://api.coronavirus.data.gov.uk/v2/data?areaType=nhsRegion&areaCode=E40000007&metric=hospitalCases&metric=newAdmissions&format=csv"
londonhospurl="https://api.coronavirus.data.gov.uk/v2/data?areaType=nhsRegion&areaCode=E40000003&metric=hospitalCases&metric=newAdmissions&format=csv"
MDhospurl="https://api.coronavirus.data.gov.uk/v2/data?areaType=nhsRegion&areaCode=E40000008&metric=hospitalCases&metric=newAdmissions&format=csv"
SEhospurl="https://api.coronavirus.data.gov.uk/v2/data?areaType=nhsRegion&areaCode=E40000005&metric=hospitalCases&metric=newAdmissions&format=csv"
NEYhospurl="https://api.coronavirus.data.gov.uk/v2/data?areaType=nhsRegion&areaCode=E40000009&metric=hospitalCases&metric=newAdmissions&format=csv"
NWhospurl="https://api.coronavirus.data.gov.uk/v2/data?areaType=nhsRegion&areaCode=E40000010&metric=hospitalCases&metric=newAdmissions&format=csv"
Scothospurl="https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=S92000003&metric=hospitalCases&metric=newAdmissions&format=csv"
Waleshospurl="https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=W92000004&metric=hospitalCases&metric=newAdmissions&format=csv"
NIhospurl="https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=N92000002&metric=hospitalCases&metric=newAdmissions&format=csv"
hNEY <-  read.csv(file=NEYhospurl)
hNW <-  read.csv(file=NWhospurl)
hMD <-  read.csv(file=MDhospurl)
hEE <-  read.csv(file=EEhospurl)
hlondon <-  read.csv(file=londonhospurl)
hSE <-  read.csv(file=SEhospurl)
hSW <-  read.csv(file=SWhospurl)
hScot <- read.csv(file=Scothospurl)
hWal <- read.csv(file=Waleshospurl)
hNI <- read.csv(file=NIhospurl)
Hospital$NEY<-gethData(hNEY)
Hospital$NW<-gethData(hNW)
Hospital$MD<-gethData(hMD)
Hospital$EE<-gethData(hEE)
Hospital$london<-gethData(hlondon)
Hospital$SE<-gethData(hSE)
Hospital$SW<-gethData(hSW)
Hospital$Scot<-gethData(hScot)
Hospital$Wal<-gethData(hWal)
Hospital$NI<-gethData(hNI)

for (iage in 2:length(london)){
  london[iage]<-Weekend(london %>% pull(iage)) 
}
for (iage in 2:length(NE)){
  NE[iage]<-Weekend(NE %>% pull(iage)) 
}
for (iage in 2:length(NW)){
  NW[iage]<-Weekend(NW %>% pull(iage)) 
}
for (iage in 2:length(YH)){
  YH[iage]<-Weekend(YH %>% pull(iage)) 
}
for (iage in 2:length(WM)){
  WM[iage]<-Weekend(WM %>% pull(iage)) 
}
for (iage in 2:length(EM)){
  EM[iage]<-Weekend(EM %>% pull(iage)) 
}
for (iage in 2:length(EE)){
  EE[iage]<-Weekend(EE %>% pull(iage)) 
}
for (iage in 2:length(SE)){
  SE[iage]<-Weekend(SE %>% pull(iage)) 
}
for (iage in 2:length(SW)){
  SW[iage]<-Weekend(SW %>% pull(iage)) 
}
for (iage in 2:length(MD)){
  MD[iage]<-Weekend(MD %>% pull(iage)) 
}
for (iage in 2:length(NEY)){
  NEY[iage]<-Weekend(NEY %>% pull(iage)) 
}

london$date<-as.Date(london$date)
NE$date<-as.Date(NE$date)
NW$date<-as.Date(NW$date)
YH$date<-as.Date(YH$date)
EM$date<-as.Date(EM$date)
WM$date<-as.Date(WM$date)
EE$date<-as.Date(EE$date)
SE$date<-as.Date(SE$date)
SW$date<-as.Date(SW$date)
MD$date<-as.Date(MD$date)
NEY$date<-as.Date(NEY$date)

#  Still use R from 7 regions...  CFR and vaccinations are assumed from National stats

complondon<- Compartment(london,  covidsimAge, RawCFR, comdat,3,nrow(london))
predlondon<-Predictions(complondon,R_BestGuess$London)

compNW<- Compartment(NW,  covidsimAge, RawCFR, comdat,3,nrow(NW))
predNW<-Predictions(compNW,R_BestGuess$NW)

compEE<- Compartment(EE,  covidsimAge, RawCFR, comdat,3,nrow(EE))
predEE<-Predictions(compEE,R_BestGuess$EE)

compSE<- Compartment(SE,  covidsimAge, RawCFR, comdat,3,nrow(SE))
predSE<-Predictions(compSE,R_BestGuess$SE)

compSW<- Compartment(SW,  covidsimAge, RawCFR, comdat,3,nrow(SW))
predSW<-Predictions(compSW,R_BestGuess$SW)

compMD<- Compartment(MD,  covidsimAge, RawCFR, comdat,3,nrow(MD))
predMD<-Predictions(compMD,R_BestGuess$Midlands)

compNEY<- Compartment(NEY,  covidsimAge, RawCFR, comdat,3,nrow(NEY))
predNEY<-Predictions(compNEY,R_BestGuess$NEY)

rm(compSE,compSW,compMD,compNEY,compNW,compEE,complondon, 
   SE, SW, EE, NEY, MD, london, NW, hSE, hSW, hEE, hNEY, hMD, hlondon, hNW)

CC_write(predNW,"North West",population$NW[1],R_BestGuess$NW,R_Quant$NW)
CC_write(predNEY,"North East",population$NEY[1],R_BestGuess$NEY,R_Quant$NEY)
CC_write(predMD,"Midlands",population$MD[1],R_BestGuess$Midlands,R_Quant$Midlands)
CC_write(predlondon,"London",population$London[1],R_BestGuess$London,R_Quant$London)
CC_write(predSW,"South West",population$SW[1],R_BestGuess$SW,R_Quant$SW)
CC_write(predSE,"South East",population$SE[1],R_BestGuess$SE,R_Quant$SE)
CC_write(predEE,"East of England",population$EE[1],R_BestGuess$EE,R_Quant$EE)

#  Monitoring plots for MTP deaths

plot_date<-c(plotdate[1],plotdate[2])
ymax = max(tail(rowSums(predMD$DEATH[2:20]),n=100))*1.1
plot(rowSums(predMD$DEATH[2:20]),x=predMD$DEATH$date,xlim=plot_date,ylim=c(0,ymax),cex.axis=0.7,ylab="Regional Death",xlab="Date") 
lines(rowSums(predNEY$DEATH[2:20]),x=predNEY$DEATH$date,xlim=plot_date,col="red") 
lines(rowSums(predNW$DEATH[2:20]),x=predNW$DEATH$date,xlim=plot_date,col="blue")  
lines(rowSums(predSW$DEATH[2:20]),x=predSW$DEATH$date,xlim=plot_date,col="green")  
lines(rowSums(predSE$DEATH[2:20]),x=predSE$DEATH$date,xlim=plot_date,col="orange")  
lines(rowSums(predEE$DEATH[2:20]),x=predEE$DEATH$date,xlim=plot_date,col="violet")  
lines(rowSums(predlondon$DEATH[2:20]),x=predlondon$DEATH$date,xlim=plot_date,col="yellow")  





#

