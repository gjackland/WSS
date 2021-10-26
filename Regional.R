source("CC_write.R")
source("CompartmentFunction.R")
source("Predictions.R")
#  ONS population estimates per region by age
#https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationprojections/datasets/regionsinenglandtable1
population <- data.frame(
"England"=c(56989570,3199741,3537022,3508137,3177717,
3412241,3738878,3873273,3766573,3563040,3542921,3871312,3827564,
3302144,2818833,2832803,2119239,1453202,909572,535358),
"NE"=c(2681149,137097,154195,156681,148629,170939,173895,
172727,163340,151146,154343,181209,193111,176408,151087,
148964,105013,74505,44484,23379),
"NW"=c(7395093,416449,458725,454524,
414109,452192,483195,496081,471598,433582,443823,508030,
509524,444826,377061,382122,277200,192637,114829,64587),
"Yorks"=c(5548941,308644,340930,
341248,322455,360851,366596,364471,345539,321312,331519,
378151,374542,331591,284620,286100,208605,144814,88607,48348),
"EM"=c(4917711,264339,296895,297062,
279881,310193,307943,310511,301307,287345,301274,343517,
343502,298132,260088,265897,197209,129095,78728,44794),
"WM"=c(6024811,348437,382020,378605,
350855,383156,407210,405697,379330,354065,360907,404454,
397278,339529,296082,294805,232009,158862,96415,55097),
"EE"=c(6312979,355635,398437,395598,
339798,325756,366991,399596,407771,399357,402382,438271,
434586,374081,325178,339205,256233,173570,112856,67679),
"London"=c(9095459,582490,605204,561365,
485920,560277,763720,819377,781076,695268,602324,571429,
520757,419558,327896,283856,207035,150959,96744,60205),
"SE"=c(9282330,501455,574647,590311,
525005,519183,536663,559528,582547,595850,603652,651177,
644634,549839,468026,486072,371793,253386,165101,103462),
"SW"=c(5731097,285196,325970,332744,
311065,329696,332666,345285,334066,325116,342698,395074,
409631,368181,328796,345781,264143,175374,111810,67807),
"Scotland"=c(5475660,261674,292754,303417,
280757,333740,370972,381258,357430,330569,337259,389238,
400834,360684,305248,289590,204947,143858,86413,45018)
)
population$MD<-population$EM+population$WM
population$NEY<-population$Yorks+population$NE


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

hNEY <-  read.csv(file=NEYhospurl)
hNW <-  read.csv(file=NWhospurl)
hMD <-  read.csv(file=MDhospurl)
hEE <-  read.csv(file=EEhospurl)
hlondon <-  read.csv(file=londonhospurl)
hSE <-  read.csv(file=SEhospurl)
hSW <-  read.csv(file=SWhospurl)

hNEY<-gethData(hNEY)
hNW<-gethData(hNW)
hMD<-gethData(hMD)
hEE<-gethData(hEE)
hlondon<-gethData(hlondon)
hSE<-gethData(hSE)
hSW<-gethData(hSW)


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
predlondon<-Predictions(complondon,R_BestGuess$London)

#compNE<- Compartment(NE,  covidsimAge, RawCFR, comdat,3,nrow(NE))
#predNE<-Predictions(compNE,R_NEY_BestGuess)

compNW<- Compartment(NW,  covidsimAge, RawCFR, comdat,3,nrow(NW))
predNW<-Predictions(compNW,R_BestGuess$NW)

#compYH<- Compartment(YH,  covidsimAge, RawCFR, comdat,3,nrow(YH))
#predYH<-Predictions(compYH,R_NEY_BestGuess)

#compEM<- Compartment(EM,  covidsimAge, RawCFR, comdat,3,nrow(EM))
#predEM<-Predictions(compEM,R_Midlands_BestGuess)

#compWM<- Compartment(WM,  covidsimAge, RawCFR, comdat,3,nrow(WM))
#predWM<-Predictions(compWM,R_Midlands_BestGuess)

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

rm(compSE,compSW,compMD,compNEY,compNW,compEE,complondon, SE, SW, EE, NEY, MD, london, NW)

CC_write(predNW,"North West",population$NW[1],R_BestGuess$NW,R_Quant$NW)
CC_write(predNEY,"North East",population$NEY[1],R_BestGuess$NEY,R_Quant$NEY)
CC_write(predMD,"Midlands",population$MD[1],R_BestGuess$MD,R_Quant$MD)
CC_write(predlondon,"London",population$London[1],R_BestGuess$London,R_Quant$London)
CC_write(predSW,"South West",population$SW[1],R_BestGuess$SW,R_Quant$SW)
CC_write(predSE,"South East",population$SE[1],R_BestGuess$SE,R_Quant$SE)
CC_write(predEE,"East of England",population$EE[1],R_BestGuess$EE,R_Quant$EE)

