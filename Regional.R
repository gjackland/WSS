source("CC_write.R")
source("CC.R")
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

rm( SE, SW, EE, NEY, MD, london, NW, hSE, hSW, hEE, hNEY, hMD, hlondon, hNW)

# recent scaling factors for MTPs
#Ratios
Hospital$Eng$newsaridat=Hospital$NEY$newsaridat+Hospital$NW$newsaridat+
  Hospital$MD$newsaridat+Hospital$EE$newsaridat+
  Hospital$SE$newsaridat+Hospital$SW$newsaridat+
  Hospital$london$newsaridat
Hospital$Eng$saridat=Hospital$NEY$saridat+Hospital$NW$saridat+
  Hospital$MD$saridat+Hospital$EE$saridat+
  Hospital$SE$saridat+Hospital$SW$saridat+
  Hospital$london$saridat
total_deaths=sum(deathdat[2:20])
total_cases=sum(casedat[2:20])
total_admissions=sum(Hospital$Eng$newsaridat)
total_crit=sum(Hospital$UK$critdat)
total_time_death=nrow(deathdat)
total_time_case=nrow(casedat)
total_time=length(Hospital$UK$date)
ratio <-list()
ratio$death=total_deaths/sum(compEng$DEATH[1:total_time,2:20])
ratio$death=1.0
ratio$case=total_cases/sum(compEng$CASE[1:total_time,2:20])
ratio$newhosp=total_admissions/sum(compEng$newSARI[1:total_time,2:20])
ratio$hosp=sum(Hospital$Eng$saridat)/sum(compEng$SARI[1:total_time,2:20])
ratio$crit=total_crit/sum(compEng$CRIT[1:total_time,2:20])

#  Rescale big regional differences in hospitalization times.
filename=paste("data/CCcompartment",Sys.Date(),"regions.xlsx")

CCEng=CC_write(predEng,"England",population$England[1],R_BestGuess$England,R_Quant$England,rat$smoothEngland,ratio,filename)
ratio$death=sum(compScot$DEATH[(total_time-51):(total_time-1),2:20])/sum(scotdeath[(total_time-51):(total_time-1),2:20])
ratio$hosp=sum(rowSums(predScot$SARI[2:20]+predScot$CRIT[2:20]+predScot$CRITREC[2:20]))/sum(Hospital$Scot$saridat)
CCScot=CC_write(predScot,"Scotland",population$Scotland[1],R_BestGuess$Scotland,R_Quant$NW,rat$smoothScotland,ratio,filename)
ratio$death=sum(compNW$DEATH[(total_time-50):total_time,2:20])/sum(regdeaths$`North West`[(total_time-50):total_time])
ratio$hosp=sum(rowSums(predNW$SARI[2:20]+predNW$CRIT[2:20]+predNW$CRITREC[2:20]))/sum(Hospital$NW$saridat)
CCNW=CC_write(predNW,"North West",population$NW[1],R_BestGuess$NW,R_Quant$NW,rat$smoothNW,ratio,filename)
ratio$death=sum(compNEY$DEATH[(total_time-50):total_time,2:20])/sum(regdeaths$NEY[(total_time-50):total_time])
ratio$hosp=sum(rowSums(predNEY$SARI[2:20]+predNEY$CRIT[2:20]+predNEY$CRITREC[2:20]))/sum(Hospital$NEY$saridat)
CCNEY=CC_write(predNEY,"North East and Yorkshire",population$NEY[1],R_BestGuess$NEY,R_Quant$NEY,rat$smoothNEY,ratio,filename)
ratio$death=sum(compMD$DEATH[(total_time-50):total_time,2:20])/sum(regdeaths$MD[(total_time-50):total_time])
ratio$hosp=sum(rowSums(predMD$SARI[2:20]+predMD$CRIT[2:20]+predMD$CRITREC[2:20]))/sum(Hospital$MD$saridat)
CCMD=CC_write(predMD,"Midlands",population$MD[1],R_BestGuess$Midlands,R_Quant$Midlands,rat$smoothMD,ratio,filename)
ratio$death=sum(complondon$DEATH[(total_time-50):total_time,2:20])/sum(regdeaths$London[(total_time-50):total_time])
ratio$hosp=sum(rowSums(predlondon$SARI[2:20]+predlondon$CRIT[2:20]+predlondon$CRITREC[2:20]))/sum(Hospital$london$saridat)
CCLon=CC_write(predlondon,"London",population$London[1],R_BestGuess$London,R_Quant$London,rat$smoothLondon,ratio,filename)
ratio$death=sum(compSW$DEATH[(total_time-50):total_time,2:20])/sum(regdeaths$`South West`[(total_time-50):total_time])
ratio$hosp=sum(rowSums(predSW$SARI[2:20]+predSW$CRIT[2:20]+predSW$CRITREC[2:20]))/sum(Hospital$SW$saridat)
CCSW=CC_write(predSW,"South West",population$SW[1],R_BestGuess$SW,R_Quant$SW,rat$smoothSW,ratio,filename)
ratio$hosp=sum(rowSums(predSE$SARI[2:20]+predSE$CRIT[2:20]+predSE$CRITREC[2:20]))/sum(Hospital$SE$saridat)
ratio$death=sum(compSE$DEATH[(total_time-50):total_time,2:20])/sum(regdeaths$`South East`[(total_time-50):total_time])
CCSE=CC_write(predSE,"South East",population$SE[1],R_BestGuess$SE,R_Quant$SE,rat$smoothSE,ratio,filename)
ratio$hosp=sum(rowSums(predEE$SARI[2:20]+predEE$CRIT[2:20]+predEE$CRITREC[2:20]))/sum(Hospital$EE$saridat)
ratio$death=sum(compEE$DEATH[(total_time-50):total_time,2:20])/sum(regdeaths$`East of England`[(total_time-50):total_time])
CCEE=CC_write(predEE,"East of England",population$EE[1],R_BestGuess$EE,R_Quant$EE,rat$smoothEE,ratio,filename)

#Now combine all the sheets into one

NIWal_write()

CC<-rbind(CCEng,CCScot,CCNW,CCNEY,CCMD,CCLon,CCSW,CCSE,CCEE)

write.xlsx(CC, file = "allx.xlsx", 
           overwrite = TRUE,  sheetName = region, rowNames = FALSE)

#  Monitoring plots for MTP deaths

plot_date<-c(plotdate[1],plotdate[2])
ymax = max(tail(rowSums(predMD$DEATH[2:20]),n=100))*1.1
plot(rowSums(predMD$DEATH[2:20]),x=predMD$DEATH$date,xlim=plot_date,cex.axis=0.7,ylab="Regional Death",xlab="Date") 
lines(rowSums(predNEY$DEATH[2:20]),x=predNEY$DEATH$date,xlim=plot_date,col="red") 
lines(rowSums(predNW$DEATH[2:20]),x=predNW$DEATH$date,xlim=plot_date,col="blue")  
lines(rowSums(predSW$DEATH[2:20]),x=predSW$DEATH$date,xlim=plot_date,col="green")  
lines(rowSums(predSE$DEATH[2:20]),x=predSE$DEATH$date,xlim=plot_date,col="orange")  
lines(rowSums(predEE$DEATH[2:20]),x=predEE$DEATH$date,xlim=plot_date,col="violet")  
lines(rowSums(predlondon$DEATH[2:20]),x=predlondon$DEATH$date,xlim=plot_date,col="yellow")  


plot(y=Hospital$MD$newsaridat,x=Hospital$UK$date,ylab="MD Hospital Admissions",xlab="Date",xlim=plot_date)
lines(rowSums(predMD$newSARI[2:20])/ratio$newhosp,x=predMD$newSARI$date)
plot(y=Hospital$NW$newsaridat,x=Hospital$UK$date,ylab="NW Hospital Admissions",xlab="Date",xlim=plot_date)
lines(rowSums(predNW$newSARI[2:20])/ratio$newhosp,x=predNW$newSARI$date)
plot(y=Hospital$NEY$newsaridat,x=Hospital$UK$date,ylab="NEY Hospital Admissions",xlab="Date",xlim=plot_date)
lines(rowSums(predNEY$newSARI[2:20])/ratio$newhosp,x=predNEY$newSARI$date)
plot(y=Hospital$EE$newsaridat,x=Hospital$UK$date,ylab="EE Hospital Admissions",xlab="Date",xlim=plot_date)
lines(rowSums(predEE$newSARI[2:20])/ratio$newhosp,x=predEE$newSARI$date)
plot(y=Hospital$SE$newsaridat,x=Hospital$UK$date,ylab="SE Hospital Admissions",xlab="Date",xlim=plot_date)
lines(rowSums(predSE$newSARI[2:20])/ratio$newhosp,x=predSE$newSARI$date)
plot(y=Hospital$SW$newsaridat,x=Hospital$UK$date,ylab="SW Hospital Admissions",xlab="Date",xlim=plot_date)
lines(rowSums(predSW$newSARI[2:20])/ratio$newhosp,x=predSW$newSARI$date)
plot(y=Hospital$london$newsaridat,x=Hospital$UK$date,ylab="London Hospital Admissions",xlab="Date",xlim=plot_date)
lines(rowSums(predlondon$newSARI[2:20])/ratio$newhosp,x=predlondon$newSARI$date)
plot(y=Hospital$Scot$newsaridat,x=Hospital$Scot$date,ylab="Scotland Hospital Admissions",xlab="Date",xlim=plot_date)
lines(rowSums(predScot$newSARI[2:20])/ratio$newhosp,x=predScot$newSARI$date)


plot(y=Hospital$Eng$saridat,x=Hospital$UK$date,ylab="England Hospital Cases",xlab="Date",xlim=plot_date)
lines(rowSums(predEng$SARI[2:20]+predEng$CRIT[2:20]+predEng$CRITREC[2:20])/ratio$hosp,x=predEng$newSARI$date)
plot(y=Hospital$MD$saridat,x=Hospital$UK$date,ylab="MD Hospital Cases",xlab="Date",xlim=plot_date)
lines(rowSums(predMD$SARI[2:20]+predMD$CRIT[2:20]+predMD$CRITREC[2:20]),x=predMD$newSARI$date)
plot(y=Hospital$NW$saridat,x=Hospital$UK$date,ylab="NW Hospital Cases",xlab="Date",xlim=plot_date)
lines(rowSums(predNW$SARI[2:20]+predNW$CRIT[2:20]+predNW$CRITREC[2:20]),x=predNW$newSARI$date)
plot(y=Hospital$NEY$saridat,x=Hospital$UK$date,ylab="NEY Hospital Cases",xlab="Date",xlim=plot_date)
lines(rowSums(predNEY$SARI[2:20]+predNEY$CRIT[2:20]+predNEY$CRITREC[2:20]),x=predNEY$newSARI$date)
plot(y=Hospital$EE$saridat,x=Hospital$UK$date,ylab="EE Hospital Cases",xlab="Date",xlim=plot_date)
lines(rowSums(predEE$SARI[2:20]+predEE$CRIT[2:20]+predEE$CRITREC[2:20]),x=predEE$newSARI$date)
plot(y=Hospital$SE$saridat,x=Hospital$UK$date,ylab="SE Hospital Cases",xlab="Date",xlim=plot_date)
lines(rowSums(predSE$SARI[2:20]+predSE$CRIT[2:20]+predSE$CRITREC[2:20]),x=predSE$newSARI$date)
plot(y=Hospital$SW$saridat,x=Hospital$UK$date,ylab="SW Hospital Cases",xlab="Date",xlim=plot_date)
lines(rowSums(predSW$SARI[2:20]+predSW$CRIT[2:20]+predSW$CRITREC[2:20]),x=predSW$newSARI$date)
plot(y=Hospital$london$saridat,x=Hospital$UK$date,ylab="London Hospital Cases",xlab="Date",xlim=plot_date)
lines(rowSums(predlondon$SARI[2:20]+predlondon$CRIT[2:20]+predlondon$CRITREC[2:20]),x=predlondon$newSARI$date)



#Admissions Uk total and by region
sum(na.locf(Hospital$Eng$saridat))
sum(na.locf(Hospital$UK$saridat))
sum(na.locf(Hospital$NEY$saridat)+Hospital$NW$saridat+Hospital$EE$saridat+Hospital$MD$saridat+Hospital$london$saridat+Hospital$SE$saridat+Hospital$SW$saridat+Hospital$Scot$saridat+na.locf(Hospital$Wal$saridat)+na.locf(Hospital$NI$saridat))
sum(rowSums(compEng$SARI[2:20]+compEng$CRIT[2:20]+compEng$CRITREC[2:20]))
sum(na.locf(Hospital$Eng$newsaridat))
sum(na.locf(compEng$newSARI[2:20]))
