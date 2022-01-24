#   Scenarios

#  Still use R from 7 regions...  CFR and vaXXinations are assumed from National stats
#  Requested Scenarios 0.7, 0.9, 1.1 and 1.3
# Edit xlsx file to change MTP to MTP R0.7
R_Scenario=0.7
XX=data.frame()
# Runup to Jan 25th
midLon<-Predictions(compLon,R_BestGuess$Lon,10,population$Lon)
midNW<-Predictions(compNW,R_BestGuess$NW,10,population$NW)
midScot<-Predictions(compScot,R_BestGuess$Scotland,10,population$Scotland)
midEng<-Predictions(compEng,R_BestGuess$England,10,population$England)
midEE<-Predictions(compEE,R_BestGuess$EE,10,population$EE)
midSE<-Predictions(compSE,R_BestGuess$SE,10,population$SE)
midSW<-Predictions(compSW,R_BestGuess$SW,10,population$SW)
midMD<-Predictions(compMD,R_BestGuess$MD,10,population$MD)
midNEY<-Predictions(compNEY,R_BestGuess$NEY,10,population$NEY)
Rs=c(0.7, 0.9, 1.1, 1.3)
for (R_Scenario in Rs ){
  spredLon<-Predictions(midLon,R_Scenario,predtime,population$Lon)
  spredNW<-Predictions(midNW,R_Scenario,predtime,population$NW)
  spredScot<-Predictions(midScot,R_Scenario,predtime,population$Scotland)
  spredEng<-Predictions(midEng,R_Scenario,predtime,population$England)
  spredEE<-Predictions(midEE,R_Scenario,predtime,population$EE)
  spredSE<-Predictions(midSE,R_Scenario,predtime,population$SE)
  spredSW<-Predictions(midSW,R_Scenario,predtime,population$SW)
  spredMD<-Predictions(midMD,R_Scenario,predtime,population$MD)
  spredNEY<-Predictions(midNEY,R_Scenario,predtime,population$NEY)

# recent scaling factors for MTPs, 
total_time = min(nrow(deathdat),nrow(casedat),length(Hospital$UK$date))
recent_time<-(total_time-50):total_time-reporting_delay
#Ratios
Hospital$Eng$newsaridat=Hospital$NEY$newsaridat+Hospital$NW$newsaridat+
  Hospital$MD$newsaridat+Hospital$EE$newsaridat+
  Hospital$SE$newsaridat+Hospital$SW$newsaridat+
  Hospital$Lon$newsaridat
Hospital$Eng$saridat=Hospital$NEY$saridat+Hospital$NW$saridat+
  Hospital$MD$saridat+Hospital$EE$saridat+
  Hospital$SE$saridat+Hospital$SW$saridat+
  Hospital$Lon$saridat
total_deaths=sum(deathdat[recent_time,2:20])
total_cases=sum(casedat[recent_time,2:20])

total_admissions=sum(Hospital$Eng$newsaridat[recent_time])
total_crit=sum(Hospital$UK$critdat[recent_time])
ratio <-list()
ratio$Eng$death=sum(predEng$DEATH[recent_time,2:20])/sum(regdeaths[recent_time,2:8])
ratio$Eng$case=sum(predEng$CASE[recent_time,2:20])/total_cases
ratio$Eng$newhosp=sum(compEng$newSARI[recent_time,2:20])/total_admissions
ratio$Eng$hosp=sum(rowSums(predEng$SARI[recent_time,2:20]+predEng$CRIT[recent_time,2:20]+predEng$CRITREC[recent_time,2:20]))/sum(Hospital$Eng$saridat[recent_time])
ratio$Eng$crit=sum(compEng$CRIT[recent_time,2:20])/total_crit

#  Rescale big regional differences in hospitalization times.
filename=paste("data/Scenario",R_Scenario,"regions.xlsx")

XXEng=CC_write(spredEng,"England",population$England[1],R_Scenario,R_Quant$England,rat$smoothEngland,ratio$Eng,filename)

ratio$Scot$death=sum(compScot$DEATH[recent_time,2:20])/sum(scotdeath[recent_time,2:20])
ratio$Scot$hosp=sum(rowSums(predScot$SARI[recent_time,2:20]+predScot$CRIT[recent_time,2:20]+predScot$CRITREC[recent_time,2:20]))/sum(Hospital$Scot$saridat[recent_time])
ratio$Scot$newhosp=sum(rowSums(predScot$newSARI[recent_time,2:20]))/sum(Hospital$Scot$newsaridat[recent_time])
XXScot=CC_write(spredScot,"Scotland",population$Scotland[1],R_Scenario,R_Quant$NW,rat$smoothScotland,ratio$Scot,filename)

ratio$NW$death=sum(compNW$DEATH[recent_time,2:20])/sum(regdeaths$`North West`[recent_time])
ratio$NW$hosp=sum(rowSums(predNW$SARI[recent_time,2:20]+predNW$CRIT[recent_time,2:20]+predNW$CRITREC[recent_time,2:20]))/sum(Hospital$NW$saridat[recent_time])
ratio$NW$newhosp=sum(rowSums(predNW$newSARI[recent_time,2:20]))/sum(Hospital$NW$newsaridat[recent_time])
XXNW=CC_write(spredNW,"North West",population$NW[1],R_Scenario,R_Quant$NW,rat$smoothNW,ratio$NW,filename)

ratio$NEY$death=sum(compNEY$DEATH[recent_time,2:20])/sum(regdeaths$NEY[recent_time])
ratio$NEY$hosp=sum(rowSums(predNEY$SARI[recent_time,2:20]+predNEY$CRIT[recent_time,2:20]+predNEY$CRITREC[recent_time,2:20]))/sum(Hospital$NEY$saridat[recent_time])
ratio$NEY$newhosp=sum(rowSums(predNEY$newSARI[recent_time,2:20]))/sum(Hospital$NEY$newsaridat[recent_time])
XXNEY=CC_write(spredNEY,"North East and Yorkshire",population$NEY[1],R_Scenario,R_Quant$NEY,rat$smoothNEY,ratio$NEY,filename)

ratio$MD$death=sum(compMD$DEATH[recent_time,2:20])/sum(regdeaths$MD[recent_time])
ratio$MD$hosp=sum(rowSums(predMD$SARI[recent_time,2:20]+predMD$CRIT[recent_time,2:20]+predMD$CRITREC[recent_time,2:20]))/sum(Hospital$MD$saridat[recent_time])
ratio$MD$newhosp=sum(rowSums(predMD$newSARI[recent_time,2:20]))/sum(Hospital$MD$newsaridat[recent_time])
XXMD=CC_write(spredMD,"Midlands",population$MD[1],R_Scenario,R_Quant$Midlands,rat$smoothMD,ratio$MD,filename)

ratio$Lon$death=sum(compLon$DEATH[recent_time,2:20])/sum(regdeaths$London[recent_time])
ratio$Lon$hosp=sum(rowSums(predLon$SARI[recent_time,2:20]+predLon$CRIT[recent_time,2:20]+predLon$CRITREC[recent_time,2:20]))/sum(Hospital$Lon$saridat[recent_time])
ratio$Lon$newhosp=sum(rowSums(predLon$newSARI[recent_time,2:20]))/sum(Hospital$Lon$newsaridat[recent_time])
XXLon=CC_write(spredLon,"London",population$Lon[1],R_Scenario,R_Quant$Lon,rat$smoothLon,ratio$Lon,filename)

ratio$SW$death=sum(compSW$DEATH[recent_time,2:20])/sum(regdeaths$`South West`[recent_time])
ratio$SW$hosp=sum(rowSums(predSW$SARI[recent_time,2:20]+predSW$CRIT[recent_time,2:20]+predSW$CRITREC[recent_time,2:20]))/sum(Hospital$SW$saridat[recent_time])
ratio$SW$newhosp=sum(rowSums(predSW$newSARI[recent_time,2:20]))/sum(Hospital$SW$newsaridat[recent_time])
XXSW=CC_write(spredSW,"South West",population$SW[1],R_Scenario,R_Quant$SW,rat$smoothSW,ratio$SW,filename)

ratio$SE$hosp=sum(rowSums(predSE$SARI[recent_time,2:20]+predSE$CRIT[recent_time,2:20]+predSE$CRITREC[recent_time,2:20]))/sum(Hospital$SE$saridat[recent_time])
ratio$SE$death=sum(compSE$DEATH[recent_time,2:20])/sum(regdeaths$`South East`[recent_time])
ratio$SE$newhosp=sum(rowSums(predSE$newSARI[recent_time,2:20]))/sum(Hospital$SE$newsaridat[recent_time])
XXSE=CC_write(spredSE,"South East",population$SE[1],R_Scenario,R_Quant$SE,rat$smoothSE,ratio$SE,filename)

ratio$EE$hosp=sum(rowSums(predEE$SARI[recent_time,2:20]+predEE$CRIT[recent_time,2:20]+predEE$CRITREC[recent_time,2:20]))/sum(Hospital$EE$saridat[recent_time])
ratio$EE$death=sum(compEE$DEATH[recent_time,2:20])/sum(regdeaths$`East of England`[recent_time])
ratio$EE$newhosp=sum(rowSums(predEE$newSARI[recent_time,2:20]))/sum(Hospital$EE$newsaridat[recent_time])
XXEE=CC_write(spredEE,"East of England",population$EE[1],R_Scenario,R_Quant$EE,rat$smoothEE,ratio$EE,filename)


#Now combine all the sheets into one


XX<-rbind(XX,XXEng,XXScot,XXNW,XXNEY,XXMD,XXLon,XXSW,XXSE,XXEE)


stringMTPR=paste("MTP R",R_Scenario,sep="")
XX[XX=="prevalence_mtp"]<-"HelloMum"
XX[XX=="MTP"]<-stringMTPR
XX[XX=="HelloMum"]<-"prevalence_mtp"
}
oldXX<-XX
XX %>% filter(across(everything(), ~ !grepl("Nowcast", .))) -> X4
XXX<-rbind(CC,X4)
write.xlsx(XXX, file = "exscenario.xlsx", 
           overwrite = TRUE,   rowNames = FALSE)
