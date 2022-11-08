##Assume that R and lethality are constants

Predictions <- function(input,R_input,predtime,pop){
  #Unpack input  Pred
  R_input0=R_input
  input[is.na(input)]<-0
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
 
 
  lengthofdata <- nrow(CASE)
  enddateP<-CASE$date[lengthofdata]

  #  Maximum pre-existing immunity from old infections only 20% as per IC report 49, by age group
  NotS0=min(1.0,colSums(CASE[2:20])/pop[2:20])*0.2
  S=1.0-NotS0

#  Boost R_input by the already susceptible.
#  This is taken out again when incrementing cases using current notS  
  #  Removed 12/4 with omicron complete R_input<-R_input/(1.0-NotS0)
  #  For 12/4, given issues with case data, drive predictions using ONS R_value
  #  R_input=rat$smoothONS[length(rat$smoothONS)]
  #  Current prevalence of omicron
  #x= (lengthofdata-Omicrondate)*1.0/genTime
  #today_Omicron=1.0/(1.0+exp(-x))#  For loop over time, predCASE using R numbers
  # enddateP is end of actual data - sometimes earlier than asked for
  
  agerange <- (2:ncol(ILI))
  #  Initialise predCASE, the predicted values. These will be added to CASE, with the
  #   actual data retained in casedat
  #  For CASE predictions 
  predCASE<-CASE[lengthofdata,(1:20)]

  ipred=1

  for (iday in ((lengthofdata+1):(lengthofdata+predtime+1))){
   xday=xday+1
    #  Proportions become variant dependent.  ILI is case driven, so extra infectivity is automatic
    # from the data. ILI->SARI increases with variant.  CRIT is an NHS decision, not favoured for very old
    #  Need to increase CFR without exceeding 1.  Note inverse lethality isnt a simple % as CFR cant be >1
    #  Will have negative people  trouble if CFR>1
    
    newMILD[iday,agerange]=predCASE[ipred,agerange]*(1.0-pTtoI)+newMILD[iday,agerange]
    newILI[iday,agerange]=predCASE[ipred,agerange]*  pTtoI    +newILI[iday,agerange]

    #  vectorize 
#  MtoR=outer(as.numeric(newMILD[iday,agerange]),MildToRecovery,FUN="*")
#  oldMILD[(iday:xday),agerange]=oldMILD[(iday:xday),agerange]+MtoR
    for (iage in agerange){
      MtoR = as.numeric(newMILD[iday-1,iage]) * MildToRecovery 
      oldMILD[(iday:xday),iage]=oldMILD[(iday:xday),iage]+MtoR
      # All todays new MILDs will all leave to REC across distribution
      # ILI will go to SARI and REC   Vaccination frozen on last day, not predicted   Assumption is that boosters compensate for waning immunity
   
      ItoS = as.numeric(newILI[iday-1,iage] * pItoS[iage-1] * (1.0-vacdat[nrow(vacdat),iage]*vacCFR)) *ILIToSARI  #  ItoS = as.numeric(newILI[iday,iage] *  pItoS[iage-1])     *ILIToSARI
      ItoR = as.numeric(newILI[iday-1,iage] *(1.0-pItoS[iage-1]*(1.0-vacdat[nrow(vacdat),iage]*vacCFR))) *ILIToRecovery
      newSARI[(iday:xday),iage]=newSARI[(iday:xday),iage]+ItoS
      oldILI[(iday:xday),iage]=oldILI[(iday:xday),iage]+ItoR+ItoS
      # SARI will go to REC, DEATH, CRIT
      StoC = as.numeric(newSARI[iday-1,iage] *pStoC[iage-1]) *SARIToCritical
      StoD = as.numeric(newSARI[iday-1,iage] *pStoD[iage-1])      *SARIToDeath
      StoR = as.numeric(newSARI[iday-1,iage] *(1.0-pStoC[iage-1]-pStoD[iage-1]) )*SARIToRecovery
      newCRIT[(iday:xday),iage]=newCRIT[(iday:xday),iage]+StoC
      oldSARI[(iday:xday),iage]=oldSARI[(iday:xday),iage]+StoR+StoC+StoD
      
      # CRIT  goes to CRITREC DEATH
      CtoD = as.numeric(newCRIT[iday-1,iage]*pCtoD[(iage-1)]) *CriticalToDeath
      CtoCR = as.numeric(newCRIT[iday-1,iage]*(1.0-pCtoD[(iage-1)])) *CriticalToCritRecov
      newCRITREC[(iday:xday),iage]=newCRITREC[(iday:xday),iage]+CtoCR
      oldCRIT[(iday:xday),iage]=oldCRIT[(iday:xday),iage]+CtoD+CtoCR
      
      # CRITREC goes to RECOV
      CRtoR = as.numeric(newCRITREC[iday-1,iage]) *CritRecovToRecov
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
    ##  Finally, estimate cases for tomorrow.  This uses an R value calculated above, but for CrystalCast purposes
    ##  we can use MLP Rx.x as an input here. Previous code assuming burgeoning omicron-induced immunity removed
    

    # R decays back to 1 with growth rate down 5% a day, faster if larger
    # R is the same in all age groups
    # This come from the network model as the epidemic behaviour becomes wavelike
    # 5% is probably too slow, but more importantly the decay should (probably) 
    # depend on the number of cases as that measures "breakthrough" into new regions
    # 
    #Assume third omicron wave will follow pattern of previous two
    R_input=  ((R_input-1.0)*R_decay+1.0)  #((R_input-1)*R_decay+1.0)  
    #  Infections not confined by age group - use an average
    #  Assume Omicron established (12/4/22) - immunity built into R_input, reverts slowly to endemic
    #  this doesn't fit data too well, trend is for oscillations but cant predict that with simple model
    R_eff=sum(R_input)/19
    predCASE[(ipred+1),(2:20)]<-predCASE[ipred,(2:20)]*exp((R_input-1)/genTime)
    predCASE[ipred+1,1]<-enddateP+ipred
    ipred=ipred+1 
  }
  # Remove the redundant first column
  predCASE <- predCASE[-c(1),]
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
  S -> input$S
  return(input)
}
