#  Compartment section from WSS.
#  Set CASE to the appropriate region
  #  CASE is the input cases which get WSS'ed.  
# CASE=casedat produces estimates for UK.  CASE=scotage is for Scotland
  
CASE=scotage
  deathdat=scotdeath
  R_BestGuess=R_Scotland_BestGuess
  region="Scotland"
  
  RawCFR=colSums(deathdat[2:20])/colSums(CASE[2:20])
  cdflength =50
  #  Time dependences of transitions - assumed age independent
  #  Make cdflength day cdfs.  these are same for all age groups, but fractions Prop/CFR vary
  #  Choose to use lognormal with logsd=logmean/4.0.  Data not available to do better
  #  Mean stay in Hospital = Sum(Cases)/Sum(admissions) = 10 days
  #  In model  sum(SARI[2:20]+CRIT[2:20]+CRITREC[2:20])/sum(newSARI[2:20])
  logmean = log(12.6)
  MildToRecovery=dlnorm(1:cdflength, logmean,  logmean/4.0) # These "Milds" are never recorded
  logmean=log(10.6)
  ILIToRecovery=dlnorm(1:cdflength, logmean,  logmean/4.0)
  #  Fit  shift & scale from ILI to SARI
  logmean=log(5.0)
  ILIToSARI=dlnorm(1:cdflength, logmean,  logmean/2.0)
  logmean=log(9.0)
  SARIToRecovery=dlnorm(1:cdflength, logmean,  logmean/2.0)
  logmean=log(6.0)
  SARIToDeath=dlnorm(1:cdflength, logmean,  logmean/2.0)
  logmean=log(4.0)
  SARIToCritical=dlnorm(1:cdflength, logmean,  logmean/2.0)
  logmean=log(7.5) # legman time spent on ICU, 7.5 days from Faes, note mean!=logmean
  CriticalToCritRecov=dlnorm(1:cdflength, logmean,  logmean/4.0)
  CriticalToDeath=dlnorm(1:cdflength, logmean,  logmean/4.0)
  logmean=log(8.0) #  Stay in hospital post ICU - needs evidence
  CritRecovToRecov=dlnorm(1:cdflength, logmean,  logmean/4.0)
  
  #  Normalise these time distributions
  MildToRecovery=MildToRecovery/sum(MildToRecovery)
  ILIToRecovery=ILIToRecovery/sum(ILIToRecovery)
  ILIToSARI=ILIToSARI/sum(ILIToSARI)
  SARIToRecovery=SARIToRecovery/sum(SARIToRecovery)
  SARIToDeath=SARIToDeath/sum(SARIToDeath)
  SARIToCritical=SARIToCritical/sum(SARIToCritical)
  CriticalToCritRecov=CriticalToCritRecov/sum(CriticalToCritRecov)
  CriticalToDeath=CriticalToDeath/sum(CriticalToDeath)
  CritRecovToRecov=CritRecovToRecov/sum(CritRecovToRecov)
  #  Follow infections through ILI (Case) - SARI (Hospital) - Crit (ICU) - CritRecov (Hospital)- Deaths
  
  #  Zero dataframes.
  #  Follow these cases to the end of the CDFs]
  lengthofdata=  length(CASE$date)#
  lengthofspread = length(ILIToRecovery)
  
  #extend ILI longer than deathdat to allow for predictions (eventually)
  ILI<-deathdat
  for (i in lengthofdata:(lengthofdata+lengthofspread) ){
    ILI[i,(2:20)] = 0.0
    ILI[i,1] =ILI$date[1]+i-1
  }
  cols <- names(ILI)[2:ncol(ILI)]
  ILI[cols] <-  0.0
  MILD <- ILI
  SARI <- ILI
  CRIT <- ILI
  CRITREC <- ILI
  RECOV <- ILI
  DEATH <- ILI
  
  # These are the new arrivals in each category.  NOT the increase.  Recov and death just increase
  # Initialize with day 1 in place
  newMILD <- MILD
  newILI <- ILI
  newSARI <- SARI
  newCRIT <- CRIT
  newCRITREC <- CRITREC
  oldMILD <- MILD
  oldILI <- ILI
  oldSARI <- SARI
  oldCRIT <- CRIT
  oldCRITREC <- CRITREC
  
  #  Set day 1.  This assumes - wrongly - that there were zero cases before,
  #              but should autocorrect as those cases get resolved
  
  #  covidsimAge has no date row, so need to use iage-1
  
  MILD[1,(2:ncol(MILD))]=CASE[1,(2:ncol(CASE))]*covidsimAge$Prop_Mild_ByAge
  ILI[1,(2:ncol(ILI))]=CASE[1,(2:ncol(CASE))]*covidsimAge$Prop_ILI_ByAge
  SARI[1,(2:ncol(SARI))]=CASE[1,(2:ncol(CASE))]*covidsimAge$Prop_SARI_ByAge
  CRIT[1,(2:ncol(CRIT))]=CASE[1,(2:ncol(CASE))]*covidsimAge$Prop_Critical_ByAge
  
  # Add new cases to Mild, ILI, SARI and CRIT people in each  age group.
  # Bring forward cases from yesterday
  # Current values will typically be negative, as they are sums of people leaving the compartment
  # Nobody changes age band.  Vectorize over distributions
  
  #Age dependent transition probabilities a->ILI b->SARI c->Death
  # apow+bpow+cpow=1 gives a fit to death data, not accounting for variant & vaccination effect
  # bpow/bfac conditions the hospital admissions by age Distribution is Approx U65=65-85=2 * 85+
  apow = 0.15
  bpow = 0.4
  cpow = 1.0-apow-bpow
  afac=1.0
  bfac=1.1
  cfac=1.0/afac/bfac
  for (iday in (2:lengthofdata)){
    pTtoI<-afac*RawCFR^apow*sqrt(comdat$lethality[iday])
    pItoS<-bfac*RawCFR^bpow*sqrt(comdat$lethality[iday])
    pStoD<-cfac*RawCFR^cpow*sqrt(comdat$lethality[iday])
    #  Entry to ventilation still from covidsim
    pStoC= covidsimAge$Prop_Critical_ByAge /
      ( covidsimAge$Prop_Critical_ByAge + covidsimAge$Prop_SARI_ByAge )#*comdat$lethality[iday]
    # All routes to death are the same, vary by age
    pCtoD <- pStoD
    pCRtoD <- pStoD
    #REscale pStoD to allow for CRIT->CRITREC route
    pStoD <- pStoD - pStoC*(pCtoD+(1-pCtoD)*pCRtoD)
    
    #  For loop over time
    
    
    #  Proportions become variant dependent.  ILI is case driven, so extra infectivity is automatic
    # from the data. ILI->SARI increases with variant.  CRIT is an NHS decision, not favoured for very old
    #  Need to increase CFR without exceeding 1.  Note inverse lethality isnt a simple % as CFR cant be >1
    #  Will have negative people  trouble if CFR>1
    
    
    # Inter-compartment probability differs from covidsim's idea of totals ending their illness
    #in that compartment  prior to RECOV/DEATH
    
    #    pItoS= (Prop_Critical_ByAge+Prop_SARI_ByAge )*comdat$lethality[iday]  /
    #        (  (Prop_Critical_ByAge+Prop_SARI_ByAge )*comdat$lethality[iday] +Prop_ILI_ByAge )
    
    xday=iday+length(SARIToCritical)-1
    agerange=(2:ncol(ILI))
    ageminus=agerange-1
    
    newMILD[iday,agerange]=CASE[iday,agerange]*(1.0-pTtoI)+newMILD[iday,agerange]
    newILI[iday,agerange]=CASE[iday,agerange]*  pTtoI    +newILI[iday,agerange]
    
    
    #  vectorize
    MtoR=outer(as.numeric(newMILD[iday,agerange]),MildToRecovery,FUN="*")
    oldMILD[(iday:xday),agerange]=oldMILD[(iday:xday),agerange]+MtoR
    vacCFR=0.75 #Vaccine reduction in ILI-> SARI
    for (iage in agerange){
      # All todays new MILDs will all leave to REC across distribution
      # multiple by vaccination and its CFR reduction
      # ILI will go to SA/RI and REC
      ItoS = as.numeric(newILI[iday,iage] * pItoS[iage-1] * (1.0-vacdat[iday,iage]*vacCFR)) *ILIToSARI
      # Replace with vaccine effect
      #    ItoS = as.numeric(newILI[iday,iage] * pItoS[iage-1])  *ILIToSARI
      ItoR = as.numeric(newILI[iday,iage] *(1.0-pItoS[iage-1])) *ILIToRecovery
      newSARI[(iday:xday),iage]=newSARI[(iday:xday),iage]+ItoS
      oldILI[(iday:xday),iage]=oldILI[(iday:xday),iage]+ItoR+ItoS
      # SARI will go to REC, DEATH, CRIT
      #  Assume vaccination only reduces ILI-> SARI  CFR is th StoD/StoC death rate by 0%
      StoC = as.numeric(newSARI[iday,iage] *pStoC[iage-1] * (1.0-vacdat[iday,iage]*0.0) )*SARIToCritical
      StoD = as.numeric(newSARI[iday,iage] *pStoD[iage-1] * (1.0-vacdat[iday,iage]*0.0) )*SARIToDeath
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
  }
# End of historical compartment section

  predtime = 28
  
  #  For loop over time, predCASE using R numbers
  predCASE<-ILI[lengthofdata,(1:20)]
  predCASE[1,(2:20)]<-CASE[lengthofdata,(2:20)] #  Growth rate by age group
  predCASE[1,1]=enddate
  
  ipred=1
  startdate=CASE$date[nrow(CASE)]
  for (iday in ((lengthofdata+1):(lengthofdata+predtime))){
    
    # R decays back to 1 with growth rate down 10% a day
    # R is the same in all age groups
    R_BestGuess=(R_BestGuess-1)*0.9+1.0
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
rbind(CASE,predCASE)->plotCASE
plot(rowSums(plotCASE[2:20]),x=plotCASE$date)
#Monitoring plots
plot(HospitalData$newAdmissions)
lines(rowSums(newSARI[2:20]),col="blue")
plot(HospitalData$hospitalCases,x=HospitalData$date,ylab="Hospital Cases",xlab="Date")
lines(rowSums(SARI[2:20]+CRIT[2:20]+CRITREC[2:20]),x=SARI$date,col='red')
plot(rowSums(CASE[2:20]),x=deathdat$date,ylab="Cases",xlab="Date")
lines(rowSums(newMILD[2:20]+newILI[2:20]),col="red",x=newMILD$date)

plot(HospitalData$covidOccupiedMVBeds,x=deathdat$date,ylab="ICU Occupation",xlab="Date")
lines(rowSums(CRIT[2:20]),col="blue",x=CRIT$date)

plot(rowSums(DEATH[2:20]),col="blue",x=DEATH$date)
lines(rowSums(deathdat[2:20]),x=deathdat$date,ylab="Deaths",xlab="Date")
