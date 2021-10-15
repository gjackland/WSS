##Assume that R and lethality are constants

Predictions <- function(input,R_BestGuess){
  #Unpack input
  
  DEATH <- input$DEATH
  RECOV <- input$RECOV
  MILD <- input$MILD
  oldMILD <- input$oldMILD
  newMILD <- input$newMILD
  ILI <- input$ILI
  oldILI <- input$oldILI
  newILI <- input$newILI
  SARI <- input$SARI
  oldSARI <-  input$oldSARI
  newSARI <- input$newSARI
  CRIT <- input$CRIT
  oldCRIT <- input$oldCRIT
  newCRIT <- input$newCRIT
  CRITREC <- input$CRITREC
  newCRITREC <- input$newCRITREC
  oldCRITREC <- input$oldCRITREC
  CASE <- input$CASE
  pCtoD <- input$pCtoD
  pItoS <- input$pItoS
  pStoC <-  input$pStoC
  pStoD <- input$pStoD
  pTtoI <-  input$pTtoI
  MildToRecovery <-  input$MildToRecovery
  xday <- input$xday
  vacCFR <- input$vacCFR
  ILIToSARI <-  input$ILIToSARI
  ILIToRecovery <- input$ILIToRecovery
  SARIToCritical <- input$SARIToCritical
  SARIToDeath <- input$SARIToDeath
  SARIToRecovery <-  input$SARIToRecovery
  CriticalToDeath <- input$CriticalToDeath
  CriticalToCritRecov <- input$CriticalToCritRecov
  CritRecovToRecov <- input$CritRecovToRecov
  # enddateP is end of actual data - sometimes earlier than asked for

  predtime = 28
  #  For loop over time, predCASE using R numbers
  lengthofdata <- nrow(CASE)
  enddateP<-CASE$date[lengthofdata]
  agerange <- (2:ncol(ILI))
  #  Initialise predCASE, the predicted values. These will be added to CASE, with the
  #   actual data retained in casedat
  predCASE<-CASE[lengthofdata,(1:20)]
  predCASE[1,(2:20)]<-CASE[lengthofdata,(2:20)] #  Growth rate by age group
  predCASE[1,1]=enddateP+1
  ipred=1
  for (iday in ((lengthofdata+1):(lengthofdata+predtime))){
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
    
    # R decays back to 1 with growth rate down 5% a day, faster if larger
    # R is the same in all age groups
    
    if(R_BestGuess > 1.4) {R_BestGuess=(R_BestGuess-1)*0.95+1.0}
    R_BestGuess=(R_BestGuess-1)*0.95+1.0
    predCASE[(ipred+1),(2:20)]<-predCASE[ipred,(2:20)]*exp((R_BestGuess-1)/genTime)
    predCASE[ipred+1,1]<-enddateP+1+ipred 
    ipred=ipred+1
  }
  

  # Pack anything that you want to use - anything not returned will not have a
  # value in the calling space.
  
  DEATH -> input$DEATH
  RECOV -> input$RECOV
  MILD -> input$MILD
  oldMILD -> input$oldMILD
  newMILD -> input$newMILD
  ILI -> input$ILI
  oldILI -> input$oldILI
  newILI -> input$newILI
  SARI -> input$SARI
  oldSARI ->  input$oldSARI
  newSARI -> input$newSARI
  CRIT -> input$CRIT
  oldCRIT -> input$oldCRIT
  newCRIT -> input$newCRIT
  CRITREC -> input$CRITREC
  newCRITREC -> input$newCRITREC
  oldCRITREC -> input$oldCRITREC
  rbind(CASE,predCASE) -> input$CASE
  pCtoD -> input$pCtoD
  pItoS -> input$pItoS
  pStoC ->  input$pStoC
  pStoD -> input$pStoD
  pTtoI ->  input$pTtoI
  MildToRecovery ->  input$MildToRecovery
  xday -> input$xday
  vacCFR -> input$vacCFR
  ILIToSARI ->  input$ILIToSARI
  ILIToRecovery -> input$ILIToRecovery
  SARIToCritical -> input$SARIToCritical
  SARIToDeath -> input$SARIToDeath
  SARIToRecovery ->  input$SARIToRecovery
  CriticalToDeath -> input$CriticalToDeath
  CriticalToCritRecov -> input$CriticalToCritRecov
  CritRecovToRecov -> input$CritRecovToRecov
  return(input)
}
