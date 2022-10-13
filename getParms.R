
# Start and end date - the date to collect data from
# First month or so will be equilibration, especially if started at a time of high caseload
#startdate <- as.Date("2020/04/08") #as.Date("2020/08/09")
startdate <- as.Date("2021/10/09")

# Lose only the last day of data - use tail correction for reporting delay
# Weekend data can be sketchy Extend the enddate if run on Monday morning
#  May need to rerun with longer delay for Scottish reporting
reporting_delay=9
enddate <-  Sys.Date()-reporting_delay
#  Predtime is the length of medium term predictuions after enddate
predtime = 100
  #  Time dependences of transitions - assumed age independent
  #  Make cdflength day cdfs.  these are same for all age groups, but fractions Prop/CFR vary
  #  Choose to use lognormal with logsd=logmean/4.0.  Data not available to do better
  #  Mean stay in Hospital = Sum(Cases)/Sum(admissions) = 10 days
# Set the generation time in days (coverts growth rate to R)
# Omicron Gen time and R_decay much lower
genTime <- 4.0
R_decay=0.9
#   Lethality of variants
Kentfac <- 0.4
Indiafac <- 0.9
Omicronfac <- 0.0
Kenttrans <- 0.3
Indiatrans<- 0.4
# Done bespoke  Omicrontrans <- 0.0 

cdflength <- 50  

logmean <- log(6.0)
  MildToRecovery <- dlnorm(1:cdflength, logmean,  logmean/8.0) # These "Milds" are never recorded
  logmean <- log(7.0)
  ILIToRecovery <- dlnorm(1:cdflength, logmean,  logmean/4.0)
  #  Fit  shift & scale from ILI to SARI
  logmean <- log(5.0)
  ILIToSARI <- dlnorm(1:cdflength, logmean,  logmean/2.0)
  logmean <- log(9.0)
  SARIToRecovery <- dlnorm(1:cdflength, logmean,  logmean/2.0)
  logmean <- log(6.0)
  SARIToDeath <- dlnorm(1:cdflength, logmean,  logmean/2.0)
  logmean <- log(4.0)
  SARIToCritical <- dlnorm(1:cdflength, logmean,  logmean/2.0)
  logmean <- log(7.5) # legman time spent on ICU, 7.5 days from Faes, note mean!=logmean
  CriticalToCritRecov <- dlnorm(1:cdflength, logmean,  logmean/4.0)
  CriticalToDeath <- dlnorm(1:cdflength, logmean,  logmean/4.0)
  logmean <- log(8.0) #  Stay in hospital post ICU - needs evidence
  CritRecovToRecov <- dlnorm(1:cdflength, logmean,  logmean/4.0)
  
  #  Normalise these time distributions
  MildToRecovery <- MildToRecovery/sum(MildToRecovery)
  ILIToRecovery <- ILIToRecovery/sum(ILIToRecovery)
  ILIToSARI <- ILIToSARI/sum(ILIToSARI)
  SARIToRecovery <- SARIToRecovery/sum(SARIToRecovery)
  SARIToDeath <- SARIToDeath/sum(SARIToDeath)
  SARIToCritical <- SARIToCritical/sum(SARIToCritical)
  CriticalToCritRecov <- CriticalToCritRecov/sum(CriticalToCritRecov)
  CriticalToDeath <- CriticalToDeath/sum(CriticalToDeath)
  CritRecovToRecov <- CritRecovToRecov/sum(CritRecovToRecov)
  #  Follow infections through ILI (Case) - SARI (Hospital) - Crit (ICU) - CritRecov (Hospital)- Deaths
  # Age dependent transition probabilities a->ILI b->SARI c->Death
  # apow+bpow+cpow=1 gives a fit to death data, not accounting for variant & vaccination effect
  # bpow/bfac conditions the hospital admissions by age Distribution is Approx U65=65-85=2 * 85+
  apow <- 0.15
  bpow <- 0.4
  cpow <- 1.0-apow-bpow
  afac <- 1.0
  bfac <- 1.2
  #  Adjust for omicron data - more hospitalisation, fewer deaths  
  afac <- 1.1
  bfac <- 1.5
  cfac <- 1.0/afac/bfac
  ### Smoothing Filters for R calculation in Estimate_R
  s1 <- 0.05
  s2 <- 0.1
  s3 <- 0.2
  s4 <- 0.3
  # Hardcode rawCFR for wild type data at 09/08/2020 - this allows start date to be changed.  Other options are commented out in covid_trimmed
  RawCFR = c(
    0.00006476028, 0.00005932501, 0.00004474461, 0.00007174780, 0.00011668950, 0.00020707650, 0.00045082861, 0.00083235867, 
    0.00135192176, 0.00274650970, 0.00466541696, 0.00847207527, 0.01470736106, 0.05199837337, 0.08980941759, 0.15752935748, 
    0.22651139233, 0.27821927091, 0.32550659352 )
  
#Add in ONSdata by hand.  this provides a check on the Case-based R and rescales the cases as testing winds slowly down
  engpop=56989570
  scotpop=5475660
  walespop=3174970
  nipop=1910623
  eng_prev<-c(
    0.05,0.05,0.05,0.07,0.11,0.19,0.21,0.41,0.62,
    0.79,1.04,1.13,1.20,1.22,1.16,0.96,0.88,
    1.04,1.18,1.47,2.06,2.08,1.88,1.87,1.55,
    1.28,0.88,0.69,0.45,0.37,0.29,0.30,0.27,
    0.30, 0.21,0.17, 0.10,0.08, 0.07,0.09, 0.09,
    0.16, 0.18,0.19, 0.22,0.39,0.61,1.06,1.36,
    1.57,1.32,1.33,1.28,1.39,1.41,1.38,1.28,
    1.14,1.21,1.44,1.63,1.79,2.02,2.02,1.70,1.51,
    1.58,1.65,1.64,1.72,2.21,2.83,3.71,6.00,
    6.85,5.47,4.82,4.83,5.18,4.49,3.84,3.55,
    3.80,4.87,6.39,7.56,7.60,6.92,5.90,4.42,2.91,2.21,1.90,1.60,1.44,1.46,2.07,2.50,
    3.35,3.95,5.27,5.77,4.83,3.86,2.63,2.22,1.64,1.41,1.29,1.41,1.57,2.03)*engpop/100

  scot_prev<-c(0.05,0.05,0.05,0.07,0.11,0.19, 0.21, 0.41,0.62,0.57,0.71,0.90,0.75,
               0.64,0.87,0.78,0.82,1.00,0.71,0.69,0.87,1.06,0.99,0.92,0.88,
               0.67,0.55,0.45,0.30,0.31,0.37,0.41,0.32,0.25,0.20,0.18,0.16,
               0.13,0.08,0.05,0.16,0.15,0.18,0.17,0.46,0.68,1.01,1.14,1.24,
               0.94,0.82,0.53,0.49,0.70,1.32,2.23,2.29,2.28,1.85,1.61,1.26,
               1.14,1.36,1.25,1.18,1.06,1.44,1.58,1.24,1.27,1.45,1.50,2.57,4.52,5.65,4.49,3.11,
               3.52,4.01, 4.17, 4.57, 5.33,5.70,7.15,9.00,8.57,7.54,5.98,5.35,4.14,3.55,3.01,2.32,2.57,
               2.01,2.36,3.36,4.76,5.47,5.94,6.34,6.48,5.17,4.95,3.12,2.56,1.82,1.98,2.16,1.88,2.22,2.15)*scotpop/100

  
#onsdat <-  read.xlsx(onsurl)
#  Would like to read these in directly but ONS datasheets aren't consistent.
# read_excel("https://www.ons.gov.uk/visualisations/dvc2050/region/datadownload.xlsx")
#download.file("https://www.ons.gov.uk/visualisations/dvc2050/region/datadownload.xlsx", destfile = "ons.xlsx")
#jnk=read_excel("ons.xlsx",sheet="England",col_names=FALSE,skip=4)
#eng_auto<-na.trim(jnk[[2]])*engpop/100
#jnk=read_excel("ons.xlsx",sheet="Scotland",col_names=FALSE,skip=4)
#scot_auto<-na.trim(jnk[[2]])*scotpop/100
#jnk=read_excel("ons.xlsx",sheet="Wales",col_names=FALSE,skip=4)
#wales_auto<-na.trim(jnk[[2]])*walespop/100
#jnk=read_excel("ons.xlsx",sheet="Northern Ireland",col_names=FALSE,skip=4)
#ni_auto<-na.trim(jnk[[2]])*nipop/100

