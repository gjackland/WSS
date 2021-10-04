#  Compartment section from WSS.
#  Set CASE to the appropriate region
#  CASE is the input cases which get WSS'ed.  
# CASE=casedat produces estimates for UK, this already happens at the end of the main code.  CASE=scotage is for Scotland
source("SampleCompartmentFunction.R")
casedat=scotage
deathdat=scotdeath
R_BestGuess=R_Scotland_BestGuess
region="Scotland"
Hospital<-scotHospital
RawCFR=colSums(deathdat[2:20])/colSums(CASE[2:20])



out <- Compartment(scotage, covidsimAge, RawCFR, comdat)

# Unpack the values returned
DEATH <- out$DEATH
RECOV <- out$RECOV
MILD <- out$MILD
oldMILD <- out$oldMILD
newMILD <- out$newMILD
ILI <- out$ILI
oldILI <- out$oldILI
newILI <- out$newILI
SARI <- out$SARI
oldSARI <-  out$oldSARI
newSARI <- out$newSARI
CRIT <- out$CRIT
oldCRIT <- out$oldCRIT
newCRIT <- out$newCRIT
CRITREC <- out$CRITREC
newCRITREC <- out$newCRITREC
oldCRITREC <- out$oldCRITREC
CASE <- out$CASE
pCtoD <- out$pCtoD
pItoS <- out$pItoS
pStoC <-  out$pStoC
pStoD <- out$pStoD
pTtoI <-  out$pTtoI
MildToRecovery <-  out$MildToRecovery
xday <- out$xday
vacCFR <- out$vacCFR
ILIToSARI <-  out$ILIToSARI
ILIToRecovery <- out$ILIToRecovery
SARIToCritical <- out$SARIToCritical
SARIToDeath <- out$SARIToDeath
SARIToRecovery <-  out$SARIToRecovery
CriticalToDeath <- out$CriticalToDeath
CriticalToCritRecov <- out$CriticalToCritRecov
CritRecovToRecov <- out$CritRecovToRecov


# End of historical compartment section

predtime = 28

#  For loop over time, predCASE using R numbers
predCASE<-ILI[lengthofdata,(1:20)]
predCASE[1,(2:20)]<-CASE[lengthofdata,(2:20)] #  Growth rate by age group
predCASE[1,1]=CASE$date[nrow(CASE)]

ipred=1 #  Counter for date prediction
startdate=CASE$date[nrow(CASE)]
for (iday in ((lengthofdata+1):(lengthofdata+predtime))){
  
  # R decays back to 1 with growth rate down 5% a day or back up at 5%
  # R is the same in all age groups
  if(R_BestGuess > 1.0){R_BestGuess=(R_BestGuess-1)*0.95+1.0}
  if(R_BestGuess < 1.0){R_BestGuess=(R_BestGuess-1)*0.95+1.0}
  #  Proportions become variant dependent.  ILI is case driven, so extra infectivity is automatic
  # from the data. ILI->SARI increases with variant.  CRIT is an NHS decision, not favoured for very old
  #  Need to increase CFR without exceeding 1.  Note inverse lethality isnt a simple % as CFR cant be >1
  #  Will have negative people  trouble if CFR>1
  
  newMILD[iday,agerange]=predCASE[ipred,agerange]*(1.0-pTtoI)+newMILD[iday,agerange]
  newILI[iday,agerange]=predCASE[ipred,agerange]*  pTtoI    +newILI[iday,agerange]
  
  
  #  vectorize
  MtoR=outer(as.numeric(newMILD[iday,agerange]),MildToRecovery,FUN="*")
  oldMILD[(iday:xday),agerange]=oldMILD[(iday:xday),agerange]+MtoR
  for (iage in agerange){
    # All todays new MILDs will all leave to REC across distribution
    
    
    # ILI will go to SA/RI and REC   Vaccination frozen on last day, not predicted
    ItoS = as.numeric(newILI[iday,iage] * pItoS[iage-1] * (1.0-vacdat[lengthofdata,iage]*vacCFR)) *ILIToSARI  #  ItoS = as.numeric(newILI[iday,iage] *  pItoS[iage-1])     *ILIToSARI
    ItoR = as.numeric(newILI[iday,iage] *(1.0-pItoS[iage-1])) *ILIToRecovery
    newSARI[(iday:xday),iage]=newSARI[(iday:xday),iage]+ItoS
    oldILI[(iday:xday),iage]=oldILI[(iday:xday),iage]+ItoR+ItoS
    # SARI will go to REC, DEATH, CRIT
    StoC = as.numeric(newSARI[iday,iage] *pStoC[iage-1]) *SARIToCritical
    StoD = as.numeric(newSARI[iday,iage] *pStoD[iage-1])      *SARIToDeath
    StoR = as.numeric(newSARI[iday,iage] *(1.0-pStoC[iage-1]-pStoD[iage-1]) )*SARIToRecovery
    newCRIT[(iday:xday),iage]=newCRIT[(iday:xday),iage]+StoC
    oldSARI[(iday:xday),iage]=oldSARI[(iday:xday),iage]+StoR+StoC+StoD
    
    # CRIT  goes to CRITREC DEATH
    CtoD = as.numeric(newCRIT[iday,iage]*pCtoD[(iage-1)]) *CriticalToDeath
    CtoCR = as.numeric(newCRIT[iday,iage]*(1.0-pCtoD[(iage-1)])) *CriticalToCritRecov
    newCRITREC[(iday:xday),iage]=newCRITREC[(iday:xday),iage]+CtoCR
    oldCRIT[(iday:xday),iage]=oldCRIT[(iday:xday),iage]+CtoD+CtoCR
    
    # CRITREC goes to RECOV
    CRtoR = as.numeric(newCRITREC[iday,iage]) *CritRecovToRecov
    oldCRITREC[(iday:xday),iage]=oldCRITREC[(iday:xday),iage]+CRtoR
    # DEATH and RECOV are cumulative, again anticipating where "new" will end up.
    DEATH[(iday:xday),iage]=DEATH[(iday:xday),iage]+CtoD+StoD
    RECOV[(iday:xday),iage]=RECOV[(iday:xday),iage]+StoR+ItoR+MtoR+CRtoR
  }
  # Finally, vectorize & update todays totals: New cases + transfers from other compartments -
  # transfers to other compartments + leftover from yesterday
  MILD[iday,agerange]=MILD[iday,agerange]+newMILD[iday,agerange]-oldMILD[iday,agerange]+MILD[(iday-1),agerange]
  ILI[iday,agerange]=ILI[iday,agerange]+newILI[iday,agerange]-oldILI[iday,agerange]+ILI[(iday-1),agerange]
  SARI[iday,agerange]=SARI[iday,agerange]+newSARI[iday,agerange]-oldSARI[iday,agerange]+SARI[(iday-1),agerange]
  CRIT[iday,agerange]=CRIT[iday,agerange]+newCRIT[iday,agerange]-oldCRIT[iday,agerange]+CRIT[(iday-1),agerange]
  CRITREC[iday,agerange]=CRITREC[iday,agerange]+newCRITREC[iday,agerange]-oldCRITREC[iday,agerange]+CRITREC[(iday-1),agerange]
  #
  ##  Finally, estimate cases for tomorrow.  This uses an R value calculated above, but for CrystalCast purposes from
  ##  we can use MLP Rx.x as an input here
  predCASE[(ipred+1),(2:20)]<-predCASE[ipred,(2:20)]*exp((R_BestGuess-1.0)/genTime)
  predCASE[ipred+1,1]<-startdate+ipred
  ipred=ipred+1
  # End of compartment section
}

#CrystalCast output - use CC.R


#  Medium term projections


today <- today()
ageband <-  "All"
CCdate$Scenario="MTP"
CCdate$Geography=region
CCdate$ValueType="hospital_inc"
#  Log. Errors from fluctuations time 4 for methodological uncertainty
#  adjust for recent discrepancy


for (d in 8:(nrow(newSARI)-22)){
  CCdate$Value = sum(newSARI[d,2:20])
  CCdate$"Quantile 0.05"=max(0,CCdate$Value*(1-12*sqrt(sum(newSARI[(d-6):d,2:20])/7)/CCdate$Value))
  CCdate$"Quantile 0.25"=max(0,CCdate$Value*(1-4*sqrt(sum(newSARI[(d-6):d,2:20])/7)/CCdate$Value))
  CCdate$"Quantile 0.5"=CCdate$Value
  CCdate$"Quantile 0.75"=CCdate$Value*(1+4*sqrt(sum(newSARI[(d-6):d,2:20])/7)/CCdate$Value)
  CCdate$"Quantile 0.95"=CCdate$Value*(1+12*sqrt(sum(newSARI[(d-7):d,2:20])/7)/CCdate$Value)
  CCdate$"Day of Value" = day(newSARI$date[d])
  CCdate$"Month of Value" = month(newSARI$date[d])
  CCdate$"Year of Value" = year(newSARI$date[d])
  # Add the new row
  CC <- rbind(CC, CCdate)
}
CCdate$ValueType="death_inc_line"
for (d in 8:(nrow(DEATH)-22)){
  CCdate$Value = sum(DEATH[d,2:20])
  CCdate$"Quantile 0.05"=max(0,CCdate$Value*(1-12*sqrt(sum(DEATH[(d-6):d,2:20])/7)/CCdate$Value))
  CCdate$"Quantile 0.25"=max(0,CCdate$Value*(1-4*sqrt(sum(DEATH[(d-6):d,2:20])/7)/CCdate$Value))
  CCdate$"Quantile 0.5"=CCdate$Value
  CCdate$"Quantile 0.75"=CCdate$Value*(1+4*sqrt(sum(DEATH[(d-6):d,2:20])/7)/CCdate$Value)
  CCdate$"Quantile 0.95"=CCdate$Value*(1+12*sqrt(sum(DEATH[(d-7):d,2:20])/7)/CCdate$Value)
  CCdate$"Day of Value" = day(DEATH$date[d])
  CCdate$"Month of Value" = month(DEATH$date[d])
  CCdate$"Year of Value" = year(DEATH$date[d])
  # Add the new row
  CC <- rbind(CC, CCdate)
}
#  Crystalcast format output  
write.xlsx(CC, file = "Data/compartment.xlsx", sheetName = "WSS", rowNames = FALSE)




rbind(CASE,predCASE)->plotCASE
plot(rowSums(plotCASE[2:20]),x=plotCASE$date)
#Monitoring plots

plot(rowSums(newSARI[2:20]),col="blue",x=SARI$date, type='l',xlim=c(Hospital$Date[1],(enddate+predtime)))
points(Hospital$newAdmissions,x=Hospital$Date,ylab="Scottish Hospital Cases",xlab="Date")

plot(Hospital$newAdmissions,x=Hospital$Date,ylab="Scottish Hospital Cases",xlab="Date",xlim=c(Hospital$Date[1],(Hospital$Date[350]+48)))
lines(rowSums(newSARI[2:20]),x=SARI$date,col='red')
plot(rowSums(CASE[2:20]),x=deathdat$date,ylab="Cases",xlab="Date")
lines(rowSums(newMILD[2:20]+newILI[2:20]),col="red",x=newMILD$date)

plot(Hospital$ICUAdmissions,x=deathdat$date,ylab="ICU Occupation",xlab="Date")
lines(rowSums(newCRIT[2:20]),col="blue",x=CRIT$date)

plot(rowSums(DEATH[2:20]),col="blue",x=DEATH$date,type="l",
     ylab="Deaths",xlab="Date")
points(rowSums(deathdat[2:20]),x=deathdat$date,ylab="Deaths",xlab="Date")
