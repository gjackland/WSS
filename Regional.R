source("CC_write.R")
source("CompartmentFunction.R")
source("Predictions.R")

getData <- function(dftmp) {for(reg in 1:9){
  out <- dftmp %>%
    select(date = date, age = age, values = cases) %>%
    pivot_wider(id_cols = date, names_from = age, values_from = values) %>%
    select(-unassigned, -"60+", -"00_59") %>%
    filter(date >= startdate & date <= enddate) %>%
    arrange(date)
}
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

#  Still use R from 7 regions...

complondon<- Compartment(london,  covidsimAge, RawCFR, comdat,3,nrow(london))
predlondon<-Predictions(complondon,R_London_BestGuess)

#compNE<- Compartment(NE,  covidsimAge, RawCFR, comdat,3,nrow(NE))
#predNE<-Predictions(compNE,R_NEY_BestGuess)

compNW<- Compartment(NW,  covidsimAge, RawCFR, comdat,3,nrow(NW))
predNW<-Predictions(compNW,R_NW_BestGuess)

#compYH<- Compartment(YH,  covidsimAge, RawCFR, comdat,3,nrow(YH))
#predYH<-Predictions(compYH,R_NEY_BestGuess)

#compEM<- Compartment(EM,  covidsimAge, RawCFR, comdat,3,nrow(EM))
#predEM<-Predictions(compEM,R_Midlands_BestGuess)

#compWM<- Compartment(WM,  covidsimAge, RawCFR, comdat,3,nrow(WM))
#predWM<-Predictions(compWM,R_Midlands_BestGuess)

compEE<- Compartment(EE,  covidsimAge, RawCFR, comdat,3,nrow(EE))
predEE<-Predictions(compEE,R_EE_BestGuess)

compSE<- Compartment(SE,  covidsimAge, RawCFR, comdat,3,nrow(SE))
predSE<-Predictions(compSE,R_SE_BestGuess)

compSW<- Compartment(SW,  covidsimAge, RawCFR, comdat,3,nrow(SW))
predSW<-Predictions(compSW,R_SW_BestGuess)

compMD<- Compartment(MD,  covidsimAge, RawCFR, comdat,3,nrow(MD))
predMD<-Predictions(compMD,R_Midlands_BestGuess)

compNEY<- Compartment(NEY,  covidsimAge, RawCFR, comdat,3,nrow(NEY))
predNEY<-Predictions(compNEY,R_NEY_BestGuess)

#rm(compSE,compSW,compMD,compNEY,compNW,compEE,complondon)

CC_write(predNW,"North West")
CC_write(predNEY,"North East")
CC_write(predMD,"Midlands")
CC_write(predlondon,"London")
CC_write(predSW,"South West")
CC_write(predSE,"South East")
CC_write(predEE,"East of England")
