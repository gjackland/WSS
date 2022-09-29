

region="Scotland"
##  CFR gets entangled with vaccine and variant effect.  Use pre-vaccination values
##  wild-type inherited from England and will adjust CFR later for vaccine/variant

#  Full Epidemic model.
compScot <- Compartment(scotage, covidsimAge, RawCFR, comdat,2,nrow(scotage))
#  Medium Term Projections
predScot<-Predictions(compScot,R_BestGuess$Scotland,predtime,population$Scotland)


try(CC_write(predScot,"Scotland2",population$Scotland[1],R_BestGuess$Scotland,R_Quant$Scotland),rat$smoothScotland)
#  Crystalcast format output  
#write.xlsx(CC, file = paste("Data/compartment",today,"all.xlsx"), sheetName = "WSS", rowNames = FALSE)

#Remove NA 's 
Hospital$Scot$newsaridat <- na.locf(Hospital$Scot$newsaridat)
Hospital$Scot$newcritdat <- na.locf(Hospital$Scot$newcritdat)
if(interactive()){
#Ratios
total_deaths=sum(scotdeath[2:20])
total_cases=sum(scotage[2:20])
total_admissions=sum(Hospital$Scot$newsaridat)
total_crit=sum(Hospital$Scot$newcritdat)
total_time_death=nrow(scotdeath)
total_time_case=nrow(scotage)
total_time=length(Hospital$Scot$date)
ratio <-list()
ratio$death=total_deaths/sum(compScot$DEATH[1:total_time_death,2:20])
ratio$case=total_cases/sum(compScot$CASE[1:total_time_case,2:20])
ratio$hosp=total_admissions/sum(compScot$newSARI[1:total_time,2:20])
ratio$crit=total_crit/sum(compScot$newCRIT[1:total_time,2:20])

rbind(compScot$CASE,predScot$CASE)->plotCASE
plot(rowSums(plotCASE[2:20]),x=plotCASE$date)
#Monitoring plots

plot(rowSums(predScot$SARI[2:20]+predScot$CRIT[2:20]+predScot$CRITREC[2:20]),col="blue", 
     type='l',ylab="Scottish Hospital Beds",xlab="Date")
lines(Hospital$Scot$saridat,ylab="Hospital",xlab="Date",las=2)

plot(Hospital$Scot$newsaridat,x=Hospital$Scot$date,ylab="Scottish Hospital Admissions",xlab="Date")
lines(rowSums(predScot$newSARI[2:20]),x=predScot$SARI$date,col='red')
plot(rowSums(predScot$CASE[2:20]),x=predScot$CASE$date,ylab="Cases",xlab="Date")
lines(rowSums(predScot$newMILD[2:20]+predScot$newILI[2:20]),col="red",x=predScot$newMILD$date)

plot(Hospital$Scot$saridat,x=Hospital$Scot$date,ylab="Hospital",xlab="Date",las=2)
plot(rowSums(predScot$SARI[2:20]),col="blue",x=predScot$newCRIT$date)

plot(rowSums(predScot$DEATH[2:20]),col="blue",x=predScot$DEATH$date,type="l", ylab="Deaths",xlab="Date",las=2)
lines(rowSums(scotdeath[2:20]),x=scotdeath$date,ylab="Deaths",xlab="Date")
lines(rowSums(predScot$DEATH[2:20]),x=predScot$DEATH$date,ylab="Deaths",col='red',xlab="Date")
}

