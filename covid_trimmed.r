#!/usr/bin/env Rscript
#
# Weight, Scale and Shift (WSS) Code
#
# Copyright 2021 Graeme Ackland, The University of Edinburgh,
#                James Ackland, The University of Cambridge
#
#### Header ####
if(interactive()){
  # Remove existing variables
  rm(list = ls())
}

# Read packages used by the script
library(readr, warn.conflicts = FALSE, quietly = TRUE)
library(dplyr, warn.conflicts = FALSE, quietly = TRUE)
library(tidyr, warn.conflicts = FALSE, quietly = TRUE)
library(ggplot2, warn.conflicts = FALSE, quietly = TRUE)
library(lubridate, warn.conflicts = FALSE, quietly = TRUE)
library(zoo, warn.conflicts = FALSE, quietly = TRUE)
library(RColorBrewer, warn.conflicts = FALSE, quietly = TRUE)

# Set the working directory from where the script is run.
setwd(".")

# Turn off scientific notation.
options(scipen = 999)

#Copy transition rates from covidsim.  There are three different functions for ICDFs, no idea why.  x-axis divided into 20 blocks of 5%.
#Will need to invert this

covidsimAge<-data.frame(
  "Prop_Mild_ByAge"=c(
    0.666244874,	0.666307235,	0.666002907,	0.665309462,	0.663636419,	0.660834577,	0.657465236,	0.65343285,	0.650261465,	0.64478501,	0.633943755,	0.625619329,	0.609080537,	0.600364976,	0.5838608,	0.566553872,	0.564646465,	0.564646465,	0.564646465
  ),
  "Prop_ILI_ByAge"=c(
    0.333122437,  0.333153617,	0.333001453, 0.332654731, 0.33181821, 0.330417289, 0.328732618, 0.326716425, 0.325130732, 0.322392505, 0.316971878, 0.312809664, 0.304540269, 0.300182488, 0.2919304, 0.283276936, 0.282323232, 0.282323232, 0.282323232
  ),
  "Prop_SARI_ByAge"=c(
    0.000557744, 0.000475283, 0.000877703, 0.001794658, 0.004006955, 0.007711884, 0.012167229, 0.017359248, 0.021140307, 0.027047193, 0.03708932, 0.039871236, 0.040788928, 0.027444452, 0.101605674, 0.142001415, 0.150469697, 0.150469697, 0.150469697
  ),
  "Prop_Critical_ByAge"=
    c(7.49444E-05, 6.38641E-05, 0.000117937, 0.000241149, 0.000538417, 0.00103625, 0.001634918, 0.002491477, 0.003467496, 0.005775292, 0.011995047, 0.021699771, 0.045590266, 0.072008084, 0.022603126, 0.008167778, 0.002560606, 0.002560606, 0.002560606
    ),
  "CFR_Critical_ByAge"=c(
    0.5234896, 0.5234896, 0.5234896, 0.5234896, 0.5234896, 0.5234896, 0.5234896, 0.5234896, 0.5234896, 0.5234896, 0.5234896, 0.5234896, 0.5234896, 0.5234896, 0.5234896, 0.5234896, 0.5234896, 0.5234896, 0.5234896
  ),
  "CFR_SARI_ByAge"=c(
    0.125893251, 0.12261338, 0.135672867, 0.152667869, 0.174303077, 0.194187895, 0.209361731, 0.224432564, 0.237013516, 0.257828065, 0.290874602, 0.320763971, 0.362563751, 0.390965457, 0.421151485, 0.447545892, 0.482, 0.482, 0.482
  ),
  "CFR_ILI_ByAge"=c(
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0)
)

covidsimICDF<-data.frame(
  "MildToRecovery_icdf"=c(
    0, 0.341579599, 0.436192391, 0.509774887, 0.574196702, 0.633830053, 0.690927761, 0.74691114, 0.802830695, 0.859578883, 0.918015187, 0.97906363, 1.043815683, 1.113669859, 1.190557274, 1.277356871, 1.378761429, 1.50338422, 1.670195767, 1.938414132, 2.511279379
  ),
  "ILIToRecovery_icdf"=c(
    0, 0.341579599, 0.436192391, 0.509774887, 0.574196702, 0.633830053, 0.690927761, 0.74691114, 0.802830695, 0.859578883, 0.918015187, 0.97906363, 1.043815683, 1.113669859, 1.190557274, 1.277356871, 1.378761429, 1.50338422, 1.670195767, 1.938414132, 2.511279379
  ),
  "ILIToSARI_icdf"=c(
    0, 0.341579599, 0.436192391, 0.509774887, 0.574196702, 0.633830053, 0.690927761, 0.74691114, 0.802830695, 0.859578883, 0.918015187, 0.97906363, 1.043815683, 1.113669859, 1.190557274, 1.277356871, 1.378761429, 1.50338422, 1.670195767, 1.938414132, 2.511279379
  ),
  "SARIToRecovery_icdf"=c(
    0, 0.634736097, 1.217461548, 1.805695261, 2.41206761, 3.044551205, 3.71010552, 4.415905623, 5.170067405, 5.982314035, 6.864787504, 7.833196704, 8.908589322, 10.12027655, 11.51100029, 13.14682956, 15.13821107, 17.69183155, 21.27093904, 27.35083955, 41.35442157
  ),
  "SARIToDeath_icdf"=c(
    0, 1.703470233, 2.39742257, 2.970367222, 3.491567676, 3.988046604, 4.474541783, 4.960985883, 5.455292802, 5.964726999, 6.496796075, 7.06004732, 7.665014091, 8.325595834, 9.061367792, 9.901900127, 10.8958347, 12.133068, 13.81280888, 16.56124574, 22.5803431
  ),
  "SARIToCritical_icdf"=c(
    0, 0.108407687, 0.220267228, 0.337653773, 0.46159365, 0.593106462, 0.733343356, 0.88367093, 1.045760001, 1.221701998, 1.414175806, 1.62669998, 1.864032461, 2.132837436, 2.442868902, 2.809242289, 3.257272257, 3.834402667, 4.647120033, 6.035113821, 9.253953212
  ),
  "CriticalToCritRecov_icdf"=c(
    0, 1.308310071, 1.87022015, 2.338694632, 2.76749788, 3.177830401, 3.581381361, 3.986127838, 4.398512135, 4.824525291, 5.270427517, 5.743406075, 6.252370864, 6.809125902, 7.430338867, 8.141231404, 8.983341913, 10.03350866, 11.46214198, 13.80540164, 18.95469153
  ),
  "CriticalToDeath_icdf"=c(
    0, 1.60649128, 2.291051747, 2.860938008, 3.382077741, 3.880425012, 4.37026577, 4.861330415, 5.361460943, 5.877935626, 6.4183471, 6.991401405, 7.607881726, 8.282065409, 9.034104744, 9.894486491, 10.91341144, 12.18372915, 13.9113346, 16.74394356, 22.96541429
  ),
  "CritRecovToRecov_icdf"=c(
    0, 0.133993315, 0.265922775, 0.402188416, 0.544657341, 0.694774487, 0.853984373, 1.023901078, 1.206436504, 1.403942719, 1.619402771, 1.856711876, 2.121118605, 2.419957988, 2.763950408, 3.169692564, 3.664959893, 4.301777536, 5.196849239, 6.7222126, 10.24997697
  ))
##covidsim has 17 agegroups.  We need 19,  assume same params for 85_89 & 90+ as for 80+

####, Read data ####
# Base URL to get the data
baseurl <- "https://api.coronavirus.data.gov.uk/v2/data?"

# Start and end date - the data to collect data from
startdate <- as.Date("2020/07/25")
#  To one week ago (-7)
enddate <-  Sys.Date()-7

#  Dates for the plots
plotdate=as.Date(c("2020-09-22",as.character(enddate)))
# Date of Christmas Eve
XMas= as.Date("2020/12/24")
XMstart=as.integer(XMas-startdate)
XMdays=12
XMend=XMstart+11
# Wanted to plot a Smooth spline discontinuous at
#UK lockdown Oct 31 (day 98) -Dec 2  (day 130) Jan 6 (day 165)  (day 1 = July 25)
lock1 = as.integer(as.Date("2020/10/31")-startdate)
unlock1 = as.integer(as.Date("2020/12/02")-startdate)
lock2 = as.integer(as.Date("2021/01/06")-startdate)
test_delay=7
lock1=lock1+test_delay
unlock1=unlock1+test_delay
lock2=lock2+test_delay
sagedelay=16 # Delay in producing R-number, for plots


# Total cases, deaths, tests
casesurl <- paste0(baseurl,
                   "areaType=nation&",
                   "areaCode=E92000001&",
                   "metric=newCasesBySpecimenDate&",
                   "metric=newDeaths28DaysByDeathDate&",
                   "metric=newPCRTestsByPublishDate&",
                   "format=csv")

# Explicitly define the types for the columns
coltypes <- cols(col_character(), col_character(),col_character(),
                 col_date(format="%Y-%m-%d"), col_integer(),
                 col_integer(), col_integer())

# Read the data
comdat <-  read_csv(file = casesurl, col_types = coltypes)

# Transform the data
comdat <- comdat %>%  select(date,
                             allCases = newCasesBySpecimenDate,
                             allDeaths = newDeaths28DaysByDeathDate,
                             tests = newPCRTestsByPublishDate,
                             inputCases = newCasesBySpecimenDate,
                             fpCases = newCasesBySpecimenDate) %>%
  filter(date >= startdate &
           date <= enddate ) %>%
  arrange(date)

# All UK cases (to estimate pre-Sept England Cases)
ukcaseurl <- paste0(baseurl,
                    "areaType=overview&",
                    "metric=newPCRTestsByPublishDate&",
                    "format=csv")

# Explicitly define the types for the columns
coltypes <- cols(col_character(), col_character(),col_character(),
                 col_date(format="%Y-%m-%d"), col_integer())
# Read the data
ukcasedat <-  read_csv(file = ukcaseurl, col_types = coltypes)

# Transform the data
ukcasedat <- ukcasedat %>%  select(date = date, tests = newPCRTestsByPublishDate) %>%
  filter(date >= startdate &
           date <= enddate ) %>%
  arrange(date)
# cases by age
ageurl <- paste0(baseurl,
                 "areaType=nation&",
                 "areaCode=E92000001&",
                 "metric=newCasesBySpecimenDateAgeDemographics&",
                 "format=csv")

# Explicitly define the types for the columns
# Age is a character as it giving a range, e.g. 00_04, 05_09, ...
coltypes <- cols(col_character(), col_character(),col_character(),
                 col_date(format="%Y-%m-%d"), col_character(),
                 col_integer(), col_integer(), col_double())

# read in the data
casedat <-  read_csv(file = ageurl, col_types = coltypes)

# Remap the ages column to be the header rows, remove the unassigned,
# 60+ and 00_59 columns, filter dates to be between the start and end
# dates and order the output by date
casedat <- casedat %>%
  select(date = date, age = age, values = cases) %>%
  pivot_wider(id_cols = date, names_from = age, values_from = values) %>%
  select(-unassigned, -"60+", -"00_59") %>%
  filter(date >= startdate & date <= enddate) %>%
  arrange(date)

#deaths by age
deathurl <- paste0(baseurl,
                   "areaType=nation&",
                   "areaCode=E92000001&",
                   "metric=newDeaths28DaysByDeathDateAgeDemographics&",
                   "format=csv")

# Explicitly define the types for the columns
coltypes <- cols(col_character(), col_character(),col_character(),
                 col_date(format="%Y-%m-%d"),col_character(),
                 col_double(), col_double(), col_double())
# Read the data
deathdat <-  read_csv(file = deathurl, col_types = coltypes)

# Map the ages column to become column headings for the different age groups
# for dates between the start and end date inclusive and then ensure that we
# end up with the same columns as for the case data above.
deathdat <- deathdat %>%
  select(date = date, age = age, values = deaths) %>%
  pivot_wider(id_cols = date, names_from = age, values_from = values) %>%
  select(-"60+", -"00_59") %>%
  filter(date >= startdate & date <= enddate) %>%
  arrange(date) %>%
  select(names(casedat))#deaths by age

#  Read in the Vaccination data
vacurl <- paste0(baseurl,
                 "areaType=nation&",
                 "areaCode=E92000001&",
                 "metric=cumVaccinationFirstDoseUptakeByPublishDatePercentage&",
                 "format=csv")

# Explicitly define the types for the columns
coltypes <- cols(col_character(), col_character(),col_character(),
                 col_date(format="%Y-%m-%d"), col_double())
# Read the data
vacdat <-  read_csv(file = vacurl, col_types = coltypes)

# Map the ages column to become column headings for the different age groups
# for dates between the start and end date inclusive and then ensure that we
# end up with the same columns as for the case data above.
vacdat <- vacdat %>%
  select(date = date,  values =cumVaccinationFirstDoseUptakeByPublishDatePercentage)
#  Scotland data https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=S92000003&metric=newCasesBySpecimenDate&metric=newDeaths28DaysByDeathDate&metric=newDeaths28DaysByPublishDate&format=csv
# https://www.opendata.nhs.scot/dataset/covid-19-in-scotland/resource/9393bd66-5012-4f01-9bc5-e7a10accacf4

scoturl <-  paste0(baseurl,
                   "areaType=nation&",
                   "areaCode=S92000003&",
                   "metric=newDeaths28DaysByDeathDate&",
                   "metric=newCasesBySpecimenDate&",
                   "metric=newDeaths28DaysByPublishDate&",
                   "format=csv")
coltypes <-  cols(
  date = col_date(format = "%Y-%m-%d"),
  newCasesBySpecimenDate = col_double(),
  newDeaths28DaysByPublishDate = col_double(),
  newDeaths28DaysByDeathDate = col_double()
)
#  trying and failing to get data from PHS
scotdeaths<- read.csv(file="https://www.opendata.nhs.scot/dataset/covid-19-in-scotland/resource/9393bd66-5012-4f01-9bc5-e7a10accacf4")
#scotdeaths<- read.csv(file="https://www.opendata.nhs.scot/api/3/action/datastore_search?resource_id=9393bd66-5012-4f01-9bc5-e7a10accacf4")
# Read in the data
scotdat <-  read_csv(file = scoturl, col_types = coltypes)

# Transform the data
scotdat <- scotdat %>%  select(date,
                               allCases = newCasesBySpecimenDate,
                               allDeaths = newDeaths28DaysByDeathDate,
                               inputCases = newCasesBySpecimenDate,
                               fpCases = newCasesBySpecimenDate) %>%
  filter(date >= startdate &
           date <= enddate ) %>%
  arrange(date)


# Regional data
regurl <- paste0(baseurl,
                 "areaType=region&",
                 "metric=newDeaths28DaysByDeathDate&",
                 "metric=newCasesBySpecimenDate&",
                 "format=csv")

# Specify the column types
coltypes <-  cols(
  areaCode = col_character(),
  areaName = col_character(),
  areaType = col_character(),
  date = col_date(format = "%Y-%m-%d"),
  newCasesBySpecimenDate = col_double(),
  newDeaths28DaysByDeathDate = col_double()
)

# Read in the data
regdat <-  read_csv(file = regurl, col_types = coltypes)

# Transform the data
regcases <- regdat %>%  select(date,areaName,areaCode,
                               Cases = newCasesBySpecimenDate
) %>%
  pivot_wider(id_cols = date, names_from = areaName, values_from = Cases) %>%
  filter(date >= startdate &
           date <= enddate )%>%
  arrange(date)

regdeaths <- regdat %>%  select(date,areaName,
                                Deaths = newDeaths28DaysByDeathDate,
) %>%
  pivot_wider(id_cols = date, names_from = areaName, values_from = Deaths) %>%
  filter(date >= startdate &
           date <= enddate )%>%
  arrange(date)

#  Get age data for regions because can't download simultaneously
regurl2 <- paste0(baseurl,
                  "areaType=region&",
                  "metric=newCasesBySpecimenDateAgeDemographics&",
                  "metric=newDeathsBySpecimenDateAgeDemographics&",
                  "format=csv")

# specify the column types
coltypes <- cols(
  areaCode = col_character(),
  areaName = col_character(),
  areaType = col_character(),
  date = col_date(format = "%Y-%m-%d"),
  age = col_character(),
  cases = col_double(),
  rollingSum = col_double(),
  rollingRate = col_double()
)

# Read in the data
regagedat <-  read_csv(file = regurl2, col_types = coltypes)

# Transform the data
regagedat <- regagedat %>%  select(date, areaName, age, cases) %>%
  filter(date >= startdate &
           date <= enddate ) %>%
  arrange(date)


eddata = read_csv(file = "https://api.coronavirus.data.gov.uk/v2/data?areaType=utla&areaCode=S12000019&metric=newCasesBySpecimenDateAgeDemographics&format=csv"
)


# Read in the UK government R estimate data from a csv file
coltypes <- cols(
  Date = col_date(format = "%Y-%m-%d"), UK_LowerBound = col_double(),
  UK_UpperBound = col_double(), England_LowerBound = col_double(),
  England_UpperBound = col_double(), EEng_LowerBound = col_double(),
  EEng_UpperBound = col_double(), Lon_LowerBound = col_double(),
  Lon_UpperBound = col_double(), Mid_LowerBound = col_double(),
  Mid_UpperBound = col_double(), NEY_LowerBound = col_double(),
  NEY_UpperBound = col_double(), NW_LowerBound = col_double(),
  NW_UpperBound = col_double(), SE_LowerBound = col_double(),
  SE_UpperBound = col_double(), SW_LowerBound = col_double(),
  SW_UpperBound = col_double()
)
Rest <- read_csv(file="data/R_estimate.csv", col_types = coltypes)

# Read in Scottish R value estimates
coltypes <- cols(
                Date = col_date(format = "%Y-%m-%d"),
                R_LowerBound = col_double(),
                R_UpperBound = col_double()
                )
R_ScotEst <- read_csv(file="data/R_scottish_estimate.csv", col_types = coltypes)

#### Get tests for England pre-Sept by taking the post-Sept fraction of all tests that were in England (0.867)
comdat$tests[1:58] = as.integer(ukcasedat$tests[1:58] * 0.867)
rm(ukcasedat)

# Plot all cases against date.
comdat %>% ggplot(aes(x=date,y=allCases)) + geom_line() +
  xlab("Date") + ylab("All cases")

# Remove weekend effect,  assuming each weekday has same number of cases over the
# epidemic, and national averages hold regionally.
days <-1:7
weeks<-as.integer(length(comdat$allCases)/7)-1

for(i in 1:weeks){
  for(j in 1:7){
    days[j]<-days[j]+comdat$allCases[7*i+j]
  }
}
casetot=sum(days)
days=7*days/casetot

# Rescale comdat and regcases
for(i in 1:length(comdat$allCases)){
  indexday=(i-1)%%7+1
  comdat$allCases[i]=comdat$allCases[i]/days[indexday]
  scotdat$allCases[i]=scotdat$allCases[i]/days[indexday]
  for (area in 2:10){
    regcases[i,area]=regcases[i,area]/days[indexday]
  }
}

# Fix Xmas anomaly over XMdays=12 days in comdat,regcases by linear fit
Xmasav = sum(comdat$allCases[XMstart:XMend])/XMdays
Xmasgrad=comdat$allCases[XMend]-comdat$allCases[XMstart]
for (i in XMstart:XMend){
  comdat$allCases[i]=Xmasav-Xmasgrad*(((XMend+XMstart)/2)-i)/XMdays
}
Xmasav = sum(scotdat$allCases[XMstart:XMend])/XMdays
Xmasgrad=scotdat$allCases[XMend]-scotdat$allCases[XMstart]
for (i in XMstart:XMend){
  scotdat$allCases[i]=Xmasav-Xmasgrad*(((XMend+XMstart)/2)-i)/XMdays
}

# Fix Xmas anomaly in regions
for (area in 2:10){
  Xmasav <- sum(regcases[XMstart:XMend,area])/XMdays
  Xmasgrad<-regcases[XMend,area]-regcases[XMstart,area]
  for (i in XMstart:XMend){
    regcases[i,area]<-Xmasav-Xmasgrad*(((XMend+XMstart)/2)-i)/XMdays
  }
}

for (i in 2:ncol(casedat)) {
  for (j in 1:nrow(casedat)) {
    indexday=(j-1)%%7+1
    casedat[j,i] <- as.integer(casedat[j,i]/days[indexday])
  }
}

# Fix Xmas and weekend anomaly in age data
for (iage in 2:ncol(casedat) ){
  Xmasav = sum(casedat[XMstart:XMend,iage])/XMdays
  Xmasgrad<-casedat[XMend,iage]-casedat[XMstart,iage]
  for (i in XMstart:XMend){
    casedat[i,iage]=as.integer(Xmasav-Xmasgrad*(((XMend+XMstart)/2)-i)/XMdays)
  }
}
rm(Xmasav,Xmasgrad,weeks,i,j,indexday)

# Set false positive adjustment at 0.004
for(i in 1:length(comdat$allCases)){
  comdat$fpCases[i]=comdat$allCases[i]-0.004*as.integer(comdat$tests[i])
}

plot(comdat$inputCases,x=comdat$date,xlab="Date",ylab="Cases")
lines(comdat$allCases,x=comdat$date, col="green",lwd=2)
lines(comdat$fpCases, x=comdat$date,col="red",lwd=2)

# Same graph using ggplot - alpha sets a level of transparency between 0 (opaque) to 1 (transparent)
ggplot(comdat,aes(x=date)) +
  geom_point(aes(y=inputCases),alpha=0.5) +
  geom_line(aes(y=allCases), colour="green", size=1.5, alpha=0.5) +
  geom_line(aes(y=fpCases),colour="red", size=1.5, alpha=0.5) +
  xlab("Dates") + ylab("Cases")

# Calculation of Rnumber, generation time = 4 days


#Make 28 day cdfs.  these are same for all age groups, but fractions Prop/CFR vary
#  Choose to use lognormal with logsd=logmean/4.0.  Data not available to do better
logmean = 2.534
MildToRecovery=dlnorm(1:28, logmean,  logmean/4.0) # These "Milds" are never recorded

logmean=2
ILIToRecovery=dlnorm(1:28, logmean,  logmean/4.0) 
ILIToSARI=dlnorm(1:28, logmean,  logmean/4.0)
logmean=1
SARIToRecovery=dlnorm(1:28, logmean,  logmean/4.0)
SARIToDeath=dlnorm(1:28, logmean,  logmean/4.0)
SARIToCritical=dlnorm(1:28, logmean,  logmean/4.0)
logmean=1.5
CriticalToCritRecov=dlnorm(1:28, logmean,  logmean/4.0)
CriticalToDeath=dlnorm(1:28, logmean,  logmean/4.0)
logmean=0.5
CritRecovToRecov=dlnorm(1:28, logmean,  logmean/4.0)

#  Normalise these distributions
MildToRecovery=MildToRecovery/sum(MildToRecovery)
ILIToRecovery=ILIToRecovery/sum(ILIToRecovery)
ILIToSARI=ILIToSARI/sum(ILIToSARI)
SARIToRecovery=SARIToRecovery/sum(SARIToRecovery)
SARIToDeath=SARIToDeath/sum(SARIToDeath)
SARIToCritical=SARIToCritical/sum(SARIToCritical)
CriticalToCritRecov=CriticalToCritRecov/sum(CriticalToCritRecov)
CriticalToDeath=CriticalToDeath/sum(CriticalToDeath)
CritRecovToRecov=CritRecovToRecov/sum(CriticalToCritRecov)
#  Follow infections through ILI (Case) - SARI (Hospital) - Crit (ICU) - CritRecov (Hospital)- Deaths
genTime=5

#  Zero dataframes. 
#  These are the numbers in each compartment at a given time
lengthofdata=  length(casedat$date)#-length(ILIToSARI)

#extend ILI longer than deathdat to allow for predictions (eventually)
ILI<-deathdat
for (i in length(casedat$date):(length(casedat$date)+length(ILIToSARI)) ){
  for (j in ncol(ILI)){
ILI[i,j] = 0.0
}  }
cols <- names(ILI)[2:ncol(ILI)]
ILI[cols] = 0.0
MILD <- ILI
SARI <- ILI
CRIT <- ILI 
CRITREC <- ILI 
RECOV <- ILI 
DEATH <- ILI 

#These are the new arrivals in each category.  NOT the increase.  Recov and death just increase
#Initialize with day 1 in place
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

#  Set day 1.  This assumes - wrongly - that there were zero cases before, but should autocorrect as those cases get resolved 

#  covidsimAge has no date row, so need to use iage-1

MILD[1,2:ncol(MILD)]=casedat[1,2:ncol(casedat)]*covidsimAge$Prop_Critical_ByAge

for (iage in (17:17)){  #(2:ncol(ILI))){  Reduced to one age group for debugging
        for (iday in (2:lengthofdata)){   
          xday=iday+length(SARIToCritical)
          # Add new cases to Mild (ignored), ILI, SARI and CRIT people in each  age group.
    # Bring forward cases from yesterday
    # Current values will typically be negative, as they are sums of people leaving the compartment
    # Nobody changes age band.
# Mild and ILI comes in from todays casedat
          newMILD[iday,iage]=as.double(casedat[iday,iage]*covidsimAge$Prop_Mild_ByAge[(iage-1)])
          newILI[iday,iage]=as.double(casedat[iday,iage]*covidsimAge$Prop_ILI_ByAge[(iage-1)])
#  SARI come in direct from todays casedat, and anticipate conversion from older ILI
    newSARI[iday,iage]=as.double(casedat[iday,iage])
    newSARI[(iday:xday),iage]= +newSARI[(iday:xday),iage] +as.double(newILI[iday,iage] *covidsimAge$Prop_SARI_ByAge[(iage-1)]) *ILIToSARI 

    #  CRIT comes in direct from todays casedat and converted from SARI   
    newCRIT[iday,iage]=as.double( casedat[iday,iage]*covidsimAge$Prop_Critical_ByAge[(iage-1)])
    newCRIT[iday:xday,iage] = as.double(newSARI[iday,iage] * covidsimAge$Prop_Critical_ByAge[(iage-1)]) *SARIToCritical+newCRIT[iday:xday,iage]
#  CRITREC comes only from CRIT  
    newCRITREC[(iday:xday),iage]= as.double(newCRIT[iday,iage]*(1.0-covidsimAge$CFR_Critical_ByAge[(iage-1)])) *CriticalToCritRecov +newCRITREC[(iday:xday),iage]
  
    #  todays new MILDs will all leave to REC
      oldMILD[(iday:xday),iage]=newMILD[iday,iage]*MildToRecovery
      RECOV[(iday:xday),iage]=RECOV[(iday:xday),iage]+oldMILD[(iday:xday),iage] 

          #  todays new ILIs will leave ILI to SARI    or REC
        recover=as.numeric(newILI[iday,iage] *(1.0-covidsimAge$Prop_SARI_ByAge[(iage-1)])) *ILIToRecovery
        toSARI= as.numeric(newILI[iday,iage] *covidsimAge$Prop_SARI_ByAge[(iage-1)]) *ILIToSARI
        oldILI[(iday:xday),iage]=recover + toSARI+ oldILI[(iday:xday),iage]
        RECOV[(iday:xday),iage]=RECOV[(iday:xday),iage]+recover
   
    #  todays new SARI  leave to  SARI to CRIT REC or DEAD 
      
 
      recover= as.numeric(newSARI[iday,iage] *(1.0-covidsimAge$CFR_SARI_ByAge[(iage-1)]-covidsimAge$Prop_Critical_ByAge[(iage-1)])) *SARIToRecovery
      toCRIT= as.numeric(newSARI[iday,iage] *covidsimAge$Prop_Critical_ByAge[(iage-1)]) *SARIToCritical
      dead=   as.numeric(newSARI[iday,iage] *covidsimAge$CFR_SARI_ByAge[(iage-1)])*SARIToDeath
      oldSARI[(iday:xday),iage]= recover+toCRIT+dead+oldSARI[(iday:xday),iage]
       RECOV[(iday:xday),iage]=RECOV[(iday:xday),iage]+recover
       DEATH[(iday:xday),iage]=DEATH[(iday:xday),iage]+dead
    
    
    #  todays new CRIT  leave to   CRITREC or DEAD    
    
      toCR =  CriticalToCritRecov *as.numeric(newCRIT[iday,iage]*(1.0-covidsimAge$CFR_Critical_ByAge[(iage-1)])) 
      dead = as.numeric(newCRIT[iday,iage] *covidsimAge$CFR_Critical_ByAge[(iage-1)])       * CriticalToDeath
      oldCRIT[(iday:xday),iage]=toCR+dead+oldCRIT[(iday:xday),iage]
      DEATH[(iday:xday),iage]=DEATH[(iday:xday),iage]+dead 
  
    #  todays new CRITREC will leave to  RECOVER    
    oldCRITREC[(iday:xday),iage]=CritRecovToRecov[iage-1]*newCRITREC[iday,iage]
    RECOV[(iday:xday),iage]=RECOV[(iday:xday),iage]+CritRecovToRecov*as.numeric(newCRITREC[iday,iage])
#  Finally, update todays totals: New cases + transfers from other compartments - transfers to other compartments + leftover from yesterday   
    MILD[iday,iage]=MILD[iday,iage]+newMILD[iday,iage]-oldMILD[iday,iage]+MILD[(iday-1),iage]
    ILI[iday,iage]=ILI[iday,iage]+newILI[iday,iage]-oldILI[iday,iage]+ILI[(iday-1),iage]
    SARI[iday,iage]=SARI[iday,iage]+newSARI[iday,iage]-oldSARI[iday,iage]+SARI[(iday-1),iage]
    CRIT[iday,iage]=CRIT[iday,iage]+newCRIT[iday,iage]-oldCRIT[iday,iage]+CRIT[(iday-1),iage]
    CRITREC[iday,iage]=CRITREC[iday,iage]+newCRITREC[iday,iage]-oldCRITREC[iday,iage]+ILI[(iday-1),iage]
    }
  }

# Create a vector to hold the results for various R-numbers
ninit <- as.double(1:nrow(comdat)) 
dfR <- data.frame(x=1.0:length(comdat$date),
date=comdat$date, gjaR=ninit, rawR=ninit,  fpR=ninit,  weeklyR=ninit,  bylogR=ninit,
  NE=ninit,  NW=ninit,  YH=ninit,  EM=ninit,  WM=ninit,  EE=ninit,  Lon=ninit,  SE=ninit,  SW=ninit,  Scot=ninit,
  p00=ninit,  p05=ninit,  p10=ninit,  p15=ninit,  p20=ninit,  p25=ninit,  p30=ninit,  p35=ninit,  p40=ninit,  p45=ninit,  p50=ninit,  p55=ninit,  p60=ninit,  p65=ninit,  p70=ninit,  p75=ninit,  p80=ninit,  p85=ninit,  p90=ninit  )
# df#Ito: gjaR[i]<-(1+(comdat$allCases[i]-comdat$allCases[i-1])*2*genTime/(comdat$allCases[i]+comdat$allCases[i-1]))
#  #Stratanovitch calculus
# rawR averages cases over previous genTime days - assumes genTime is the same as infectious period

# Check if there are any zero cases in the data
if(any(casedat==0)){
  for(name in names(casedat)){
    if(any(casedat[name]==0)){
      warning("Zero values found for ",name," for the date(s) ",
              paste(casedat[["date"]][which(casedat[name]==0)],collapse = ", "),".")
    }
  }
}

#  Generate R over all regions and ages,  gjaR is Ito, rawR is stratonovich, bylogR is harmonicIto fpR includes false positive correction
for(i in ((genTime+1):length(dfR$gjaR))    ){
  dfR$gjaR[i]=(1+(comdat$allCases[i]-comdat$allCases[i-1])*genTime/(comdat$allCases[i-1]))
  dfR$rawR[i]=1+ (comdat$allCases[i]-mean(comdat$allCases[(i-genTime):(i-1)]))*2/comdat$allCases[i-1]
  dfR$fpR[i]=(1+(comdat$fpCases[i]-comdat$fpCases[i-1])*genTime/(comdat$fpCases[i-1]))
  dfR$bylogR[i]=1+log(comdat$allCases[i]/comdat$allCases[i-1])*genTime
  dfR$NE[i]=1+log(regcases$`North East`[i]/regcases$`North East`[i-1])*genTime
  dfR$NW[i]=1+log(regcases$`North West`[i]/regcases$`North West`[i-1])*genTime
  dfR$YH[i]=1+log(regcases$`Yorkshire and The Humber`[i]/regcases$`Yorkshire and The Humber`[i-1])*genTime
  dfR$EM[i]=1+log(regcases$`East Midlands`[i]/regcases$`East Midlands`[i-1])*genTime
  dfR$WM[i]=1+log(regcases$`West Midlands`[i]/regcases$`West Midlands`[i-1])*genTime
  dfR$EE[i]=1+log(regcases$`East of England`[i]/regcases$`East of England`[i-1])*genTime
  dfR$Lon[i]=1+log(regcases$London[i]/regcases$London[i-1])*genTime
  dfR$SE[i]=1+log(regcases$`South East`[i]/regcases$`South East`[i-1])*genTime
  dfR$SW[i]=1+log(regcases$`South West`[i]/regcases$`South West`[i-1])*genTime
  dfR$Scot[i]=1+log(scotdat$allCases[i]/scotdat$allCases[i-1])*genTime
  dfR$p00[i]=1+log(casedat$'00_04'[i]/casedat$'00_04'[i-1])*genTime
  dfR$p05[i]=1+log(casedat$'05_09'[i]/casedat$'05_09'[i-1])*genTime
  dfR$p10[i]=1+log(casedat$'10_14'[i]/casedat$'10_14'[i-1])*genTime
  dfR$p15[i]=1+log(casedat$'15_19'[i]/casedat$'15_19'[i-1])*genTime
  dfR$p20[i]=1+log(casedat$'20_24'[i]/casedat$'20_24'[i-1])*genTime
  dfR$p25[i]=1+log(casedat$'25_29'[i]/casedat$'25_29'[i-1])*genTime
  dfR$p30[i]=1+log(casedat$'30_34'[i]/casedat$'30_34'[i-1])*genTime
  dfR$p35[i]=1+log(casedat$'35_39'[i]/casedat$'35_39'[i-1])*genTime
  dfR$p40[i]=1+log(casedat$'40_44'[i]/casedat$'40_44'[i-1])*genTime
  dfR$p45[i]=1+log(casedat$'45_49'[i]/casedat$'45_49'[i-1])*genTime
  dfR$p50[i]=1+log(casedat$'50_54'[i]/casedat$'50_54'[i-1])*genTime
  dfR$p55[i]=1+log(casedat$'55_59'[i]/casedat$'55_59'[i-1])*genTime
  dfR$p60[i]=1+log(casedat$'60_64'[i]/casedat$'60_64'[i-1])*genTime
  dfR$p65[i]=1+log(casedat$'65_69'[i]/casedat$'65_69'[i-1])*genTime
  dfR$p70[i]=1+log(casedat$'70_74'[i]/casedat$'70_74'[i-1])*genTime
  dfR$p75[i]=1+log(casedat$'75_79'[i]/casedat$'75_79'[i-1])*genTime
  if(casedat$'80_84'[i] != 0 & casedat$'80_84'[i-1] != 0){ # Deal with 0 cases
    dfR$p80[i]=1+log(casedat$'80_84'[i]/casedat$'80_84'[i-1])*genTime
  }else{
    dfR$p80[i] = NA
  }
  dfR$p85[i]=1+log(casedat$'85_89'[i]/casedat$'85_89'[i-1])*genTime
  if(casedat$'90+'[i] != 0 & casedat$'90+'[i-1] != 0){ # Deal with 0 cases
    dfR$p90[i]=1+log(casedat$'90+'[i]/casedat$'90+'[i-1])*genTime
  }else{
    dfR$p90[i] = NA
  }
}

for (i in 3:17){dfR[i,1]=dfR[i,2]}

for(i in 4:(length(dfR$weeklyR)-3)){
    day1=i-3
    day7=i+3
    dfR$weeklyR[i]=sum(dfR$gjaR[day1:day7])/7.0
}
# End effect
dfR$weeklyR[length(dfR$weeklyR)]=1.0
dfR$weeklyR[length(dfR$weeklyR)-1]=1.0
dfR$weeklyR[length(dfR$weeklyR)-2]=1.0

#Plot various types of smoothing on the R data

# Making the time windows agree
Govdat <- Rest[Rest$Date >= min(comdat$date) & Rest$Date <= max(comdat$date),]

#  Parameters for fitting splines and Loess
nospl=4
spdf=12
lospan=0.3

smoothweightR<-smooth.spline(dfR$bylogR,df=spdf,w=sqrt(comdat$allCases))
smoothweightRraw<-smooth.spline(dfR$rawR,df=spdf,w=sqrt(comdat$allCases))
smoothweightRgja<-smooth.spline(dfR$gjaR,df=spdf,w=sqrt(comdat$allCases))
smoothweightRfp<-smooth.spline(dfR$fpR,df=spdf,w=sqrt(comdat$fpCases))


smoothweightR$date<-comdat$date
smoothweightRfp$date<-dfR$date
smoothR<-smooth.spline(dfR$bylogR,df=14)
smoothR98<-smooth.spline(dfR$bylogR[1:lock1],df=nospl)
smoothR98$date<-dfR$date[1:lock1]
smoothR130<-smooth.spline(dfR$bylogR[lock1:unlock1],df=nospl)
smoothR130$date<-dfR$date[lock1:unlock1]
smoothR164<-smooth.spline(dfR$bylogR[unlock1:lock2],df=nospl)
smoothR164$x=smoothR164$x+unlock1
smoothR164$date<-dfR$date[unlock1:lock2]
smoothRend<-smooth.spline(dfR$bylogR[lock2:length(dfR$date)],df=nospl)
smoothRend$x=smoothRend$x+lock2
smoothRend$date<-dfR$date[lock2:length(dfR$gjaR)]
dfR$piecewise<-dfR$gjaR
for (i in 1:lock1){dfR$piecewise[i]=smoothR98$y[i]}
for (i in (lock1+1):unlock1){dfR$piecewise[i]=smoothR130$y[i-lock1]}
for (i in (unlock1+1):lock2){dfR$piecewise[i]=smoothR164$y[i-unlock1]}
for (i in (lock2+1):length(dfR$date)){dfR$piecewise[i]=smoothRend$y[i-lock2]}

#Plot R estimate vs data and fits discontinuous at lockdown
#  Have to move the Official R data back by 16 days !

#  All cases and Regions

plot(smoothweightR$y,ylab="Region R-number",xlab="Date",x=dfR$date)
for (i in 8:17){
  lines(smooth.spline(na.omit(dfR[i]),df=spdf)$y,col=i,x=dfR$date[!is.na(dfR[i])])
}

plot(smoothweightR$y,ylab="Agegroup R-number",xlab="Date",x=dfR$date)
for (i in 18:length(dfR)){
  lines(smooth.spline(na.omit(dfR[i]),df=spdf)$y,col=i,x=dfR$date[!is.na(dfR[i])])
}

plot(dfR$piecewise,x=smoothweightR$date,ylab="R-number",xlab="Date after Aug 25",title("England"),ylim=c(0.6,1.4),xlim=plotdate,cex.lab=2.0, cex.axis=2.0, cex.main=2.0, cex.sub=2.0)
lines(y=Rest$England_LowerBound,x=Rest$Date-sagedelay)
lines(y=Rest$England_UpperBound,x=Rest$Date-sagedelay)
lines(smoothweightR$y,col="blue",lwd=2,x=dfR$date)
#lines(predict(loess(gjaR ~ x, data=dfR,span=0.1)),col='red',x=dfR$date)
#lines(predict(loess(gjaR ~ x, data=dfR,span=0.2)),col='red',x=dfR$date)
lines(predict(loess(gjaR ~ x, data=dfR,span=0.3,weight=sqrt(comdat$allCases))),col='green',x=dfR$date,lwd=2)
lines(predict(loess(gjaR ~ x, data=dfR,span=0.3)),col='green',x=dfR$date)
lines(predict(loess(bylogR ~ x, data=dfR,span=0.3,weight=sqrt(comdat$allCases))),col='red',x=dfR$date,lwd=2)
lines(predict(loess(bylogR ~ x, data=dfR,span=0.3)),col='red',x=dfR$date)

plot(smoothweightR$y,ylab="R-number",xlab="Day")
#  Plot R continuous with many splines.  Not sure when fitting noise here!
for (ismooth in 4:28){
#  lines(smooth.spline(dfR$bylogR,df=ismooth,w=sqrt(comdat$allCases)))
  lines(predict(loess(bylogR ~ x, data=dfR,span=(4.0/ismooth))),col='red')
  }
points(dfR$bylogR, col = "red")
lines(smooth.spline(dfR$bylogR,df=14))
# Plot the UB and LB for the UK R estimates, have added a line commented out
# where you can plot your estimate for the R value - add your own data frame
# change date and R to what you called the columns - you probably have to have
# the same number of values corresponding to the same time frame - you may
# also want the range for England rather than the UK. Remove these lines they
# are for your benefit Graeme. You probably wnat to move this plot until after
# you have calculated your own Restimate.
Rest %>% ggplot(aes(x=Date)) + geom_ribbon(aes(Date,min=England_LowerBound,max=England_UpperBound),colour="red",alpha=0.25) +
  ylab("R Estimate") + xlab("Date")  # + geom_line(comdat,aes(date,R))


#Plot Regional R data vs Government  spdf is spline smoothing factor, lospan for loess

#  various options to silence pdf writing
pdfpo=FALSE

if(pdfpo){

if(interactive()){
  pdf(file = 'Scot.pdf')
  plot(smooth.spline(dfR$Scot,df=spdf,w=sqrt(scotdat$allCases))$y,ylab="R-number",xlab="Date",x=dfR$date,ylim=c(0.6,1.4),xlim=plotdate,cex.lab=2.0, cex.axis=2.0, cex.main=2.0, cex.sub=2.0,title("Scotland"))
  lines(y=Rest$UK_LowerBound,x=Rest$Date-sagedelay)
  lines(y=Rest$UK_UpperBound,x=Rest$Date-sagedelay)
  lines(predict(loess(Lon ~ x, data=dfR,span=lospan)),col='red',x=dfR$date)
  dev.off()
  pdf(file = 'Lon.pdf')
  plot(smooth.spline(dfR$Lon,df=spdf,w=sqrt(comdat$allCases))$y,ylab="R-number",xlab="Date",x=dfR$date,ylim=c(0.6,1.4),xlim=plotdate,cex.lab=2.0, cex.axis=2.0, cex.main=2.0, cex.sub=2.0)
  lines(y=Rest$Lon_LowerBound,x=Rest$Date-sagedelay)
  lines(y=Rest$Lon_UpperBound,x=Rest$Date-sagedelay)
  lines(predict(loess(Lon ~ x, data=dfR,span=lospan)),col='red',x=dfR$date,title("London"))
  invisible(dev.off())
  pdf(file = 'NW.pdf')
  plot(smooth.spline(dfR$NW,df=spdf,w=sqrt(comdat$allCases))$y,ylab="R-number",xlab="Date",x=dfR$date,ylim=c(0.6,1.4),xlim=plotdate,cex.lab=2.0, cex.axis=2.0, cex.main=2.0, cex.sub=2.0)
  lines(y=Rest$NW_LowerBound,x=Rest$Date-sagedelay)
  lines(y=Rest$NW_UpperBound,x=Rest$Date-sagedelay)
  lines(predict(loess(NW ~ x, data=dfR,span=lospan)),col='red',x=dfR$date,title("North West"))
  invisible(dev.off())
  pdf(file = 'NE.pdf')
  plot(smooth.spline(dfR$NE,df=spdf,w=sqrt(comdat$allCases))$y,ylab="R-number",xlab="Date",x=dfR$date,ylim=c(0.6,1.4),xlim=plotdate,cex.lab=2.0, cex.axis=2.0, cex.main=2.0, cex.sub=2.0)
  lines(y=Rest$NEY_LowerBound,x=Rest$Date-sagedelay)
  lines(y=Rest$NEY_UpperBound,x=Rest$Date-sagedelay)
  lines(predict(loess(NE ~ x, data=dfR,span=lospan)),col='red',x=dfR$date,title("North East"))
  invisible(dev.off())
  pdf(file = 'SW.pdf')
  plot(smooth.spline(dfR$SW,df=spdf,w=sqrt(comdat$allCases))$y,ylab="R-number",xlab="Date",x=dfR$date,ylim=c(0.6,1.4),xlim=plotdate,cex.lab=2.0, cex.axis=2.0, cex.main=2.0, cex.sub=2.0)
  lines(y=Rest$SW_LowerBound,x=Rest$Date-sagedelay)
  lines(y=Rest$SW_UpperBound,x=Rest$Date-sagedelay)
  lines(predict(loess(SW ~ x, data=dfR,span=lospan)),col='red',x=dfR$date,title("South West"))
  invisible(dev.off())
  pdf(file = 'SE.pdf')
  plot(smooth.spline(dfR$SE,df=spdf,w=sqrt(comdat$allCases))$y,ylab="R-number",xlab="Date",x=dfR$date,ylim=c(0.6,1.4),xlim=plotdate,cex.lab=2.0, cex.axis=2.0, cex.main=2.0, cex.sub=2.0)
  lines(y=Rest$SE_LowerBound,x=Rest$Date-sagedelay)
  lines(y=Rest$SE_UpperBound,x=Rest$Date-sagedelay)
  lines(predict(loess(SE ~ x, data=dfR,span=lospan)),col='red',x=dfR$date,title("South East"))
  invisible(dev.off())
  pdf(file = 'EE.pdf')
  plot(smooth.spline(dfR$EE,df=spdf,w=sqrt(comdat$allCases))$y,ylab="R-number",xlab="Date",x=dfR$date,ylim=c(0.6,1.4),xlim=plotdate,cex.lab=2.0, cex.axis=2.0, cex.main=2.0, cex.sub=2.0)
  lines(y=Rest$EEng_LowerBound,x=Rest$Date-sagedelay)
  lines(y=Rest$EEng_UpperBound,x=Rest$Date-sagedelay)
  lines(predict(loess(EE ~ x, data=dfR,span=lospan)),col='red',x=dfR$date,title("East England"))
  invisible(dev.off())
  pdf(file = 'EM.pdf')
  plot(smooth.spline(dfR$EM,df=spdf,w=sqrt(comdat$allCases))$y,ylab="R-number",xlab="Date",x=dfR$date,ylim=c(0.6,1.4),xlim=plotdate,cex.lab=2.0, cex.axis=2.0, cex.main=2.0, cex.sub=2.0)
  lines(y=Rest$Mid_LowerBound,x=Rest$Date-sagedelay)
  lines(y=Rest$Mid_UpperBound,x=Rest$Date-sagedelay)
  lines(predict(loess(EM ~ x, data=dfR,span=lospan)),col='red',x=dfR$date,title("East Midlands"))
  invisible(dev.off())
  pdf(file = 'WM.pdf')
  plot(smooth.spline(dfR$WM,df=spdf,w=sqrt(comdat$allCases))$y,ylab="R-number",xlab="Date",x=dfR$date,ylim=c(0.6,1.4),xlim=plotdate,cex.lab=2.0, cex.axis=2.0, cex.main=2.0, cex.sub=2.0)
  lines(y=Rest$Mid_LowerBound,x=Rest$Date-sagedelay)
  lines(y=Rest$Mid_UpperBound,x=Rest$Date-sagedelay)
  lines(predict(loess(WM ~ x, data=dfR,span=lospan)),col='red',x=dfR$date,title("West Midlands"))
  invisible(dev.off())
  pdf(file = 'YH.pdf')
  plot(smooth.spline(dfR$YH,df=spdf,w=sqrt(comdat$allCases))$y,ylab="R-number",xlab="Date",x=dfR$date,ylim=c(0.6,1.4),xlim=plotdate,cex.lab=2.0, cex.axis=2.0, cex.main=2.0, cex.sub=2.0)
  lines(y=Rest$NEY_LowerBound,x=Rest$Date-sagedelay)
  lines(y=Rest$NEY_UpperBound,x=Rest$Date-sagedelay)
  lines(predict(loess(YH ~ x, data=dfR,span=lospan)),col='red',x=dfR$date,title("Yorkshire"))
  invisible(dev.off())
  pdf(file = 'p05.pdf')
  plot(smooth.spline(dfR$p05,df=spdf,w=sqrt(comdat$allCases))$y,ylab="R-number",xlab="Date",x=dfR$date,ylim=c(0.6,1.4),xlim=plotdate,cex.lab=2.0, cex.axis=2.0, cex.main=2.0, cex.sub=2.0)
  lines(y=Rest$England_LowerBound,x=Rest$Date-sagedelay)
  lines(y=Rest$England_UpperBound,x=Rest$Date-sagedelay)
  lines(predict(loess(p05 ~ x, data=dfR,span=lospan)),col='red',x=dfR$date,title("05-09"))
  invisible(dev.off())
  pdf(file = 'p10.pdf')
  plot(smooth.spline(dfR$p10,df=spdf,w=sqrt(comdat$allCases))$y,ylab="R-number",xlab="Date",x=dfR$date,ylim=c(0.6,1.4),xlim=plotdate,cex.lab=2.0, cex.axis=2.0, cex.main=2.0, cex.sub=2.0)
  lines(y=Rest$England_LowerBound,x=Rest$Date-sagedelay)
  lines(y=Rest$England_UpperBound,x=Rest$Date-sagedelay)
  lines(predict(loess(p10 ~ x, data=dfR,span=lospan)),col='red',x=dfR$date,title("10-14"))
  invisible(dev.off())
  pdf(file = 'p15.pdf')
  plot(smooth.spline(dfR$p15,df=spdf,w=sqrt(comdat$allCases))$y,ylab="R-number",xlab="Date",x=dfR$date,ylim=c(0.6,1.4),xlim=plotdate,cex.lab=2.0, cex.axis=2.0, cex.main=2.0, cex.sub=2.0)
  lines(y=Rest$England_LowerBound,x=Rest$Date-sagedelay)
  lines(y=Rest$England_UpperBound,x=Rest$Date-sagedelay)
  lines(predict(loess(p15 ~ x, data=dfR,span=lospan)),col='red',x=dfR$date,title("15-19"))
  invisible(dev.off())
  pdf(file = 'p20.pdf')
  plot(smooth.spline(dfR$p20,df=spdf,w=sqrt(comdat$allCases))$y,ylab="R-number",xlab="Date",x=dfR$date,ylim=c(0.6,1.4),xlim=plotdate,cex.lab=2.0, cex.axis=2.0, cex.main=2.0, cex.sub=2.0)
  lines(y=Rest$England_LowerBound,x=Rest$Date-sagedelay)
  lines(y=Rest$England_UpperBound,x=Rest$Date-sagedelay)
  lines(predict(loess(p20 ~ x, data=dfR,span=lospan)),col='red',x=dfR$date,title("20-24"))
  invisible(dev.off())
  pdf(file = 'p25.pdf')
  plot(smooth.spline(dfR$p25,df=spdf,w=sqrt(comdat$allCases))$y,ylab="R-number",xlab="Date",x=dfR$date,ylim=c(0.6,1.4),xlim=plotdate,cex.lab=2.0, cex.axis=2.0, cex.main=2.0, cex.sub=2.0)
  lines(y=Rest$England_LowerBound,x=Rest$Date-sagedelay)
  lines(y=Rest$England_UpperBound,x=Rest$Date-sagedelay)
  lines(predict(loess(p25 ~ x, data=dfR,span=lospan)),col='red',x=dfR$date,title("25-29"))
  invisible(dev.off())
  pdf(file = 'p30.pdf')
  plot(smooth.spline(dfR$p30,df=spdf,w=sqrt(comdat$allCases))$y,ylab="R-number",xlab="Date",x=dfR$date,ylim=c(0.6,1.4),xlim=plotdate,cex.lab=2.0, cex.axis=2.0, cex.main=2.0, cex.sub=2.0)
  lines(y=Rest$England_LowerBound,x=Rest$Date-sagedelay)
  lines(y=Rest$England_UpperBound,x=Rest$Date-sagedelay)
  lines(predict(loess(p30 ~ x, data=dfR,span=lospan)),col='red',x=dfR$date,title("30-34"))
  invisible(dev.off())
  pdf(file = 'p35.pdf')
  plot(smooth.spline(dfR$p35,df=spdf,w=sqrt(comdat$allCases))$y,ylab="R-number",xlab="Date",x=dfR$date,ylim=c(0.6,1.4),xlim=plotdate,cex.lab=2.0, cex.axis=2.0, cex.main=2.0, cex.sub=2.0)
  lines(y=Rest$England_LowerBound,x=Rest$Date-sagedelay)
  lines(y=Rest$England_UpperBound,x=Rest$Date-sagedelay)
  lines(predict(loess(p35 ~ x, data=dfR,span=lospan)),col='red',x=dfR$date,title("35-39"))
  invisible(dev.off())
  pdf(file = 'p40.pdf')
  plot(smooth.spline(dfR$p40,df=spdf,w=sqrt(comdat$allCases))$y,ylab="R-number",xlab="Date",x=dfR$date,ylim=c(0.6,1.4),xlim=plotdate,cex.lab=2.0, cex.axis=2.0, cex.main=2.0, cex.sub=2.0)
  lines(y=Rest$England_LowerBound,x=Rest$Date-sagedelay)
  lines(y=Rest$England_UpperBound,x=Rest$Date-sagedelay)
  lines(predict(loess(p40 ~ x, data=dfR,span=lospan)),col='red',x=dfR$date,title("40-44"))
  invisible(dev.off())
  pdf(file = 'p45.pdf')
  plot(smooth.spline(dfR$p45,df=spdf,w=sqrt(comdat$allCases))$y,ylab="R-number",xlab="Date",x=dfR$date,ylim=c(0.6,1.4),xlim=plotdate,cex.lab=2.0, cex.axis=2.0, cex.main=2.0, cex.sub=2.0)
  lines(y=Rest$England_LowerBound,x=Rest$Date-sagedelay)
  lines(y=Rest$England_UpperBound,x=Rest$Date-sagedelay)
  lines(predict(loess(p45 ~ x, data=dfR,span=lospan)),col='red',x=dfR$date,title("45-49"))
  invisible(dev.off())
  pdf(file = 'p50.pdf')
  plot(smooth.spline(dfR$p50,df=spdf,w=sqrt(comdat$allCases))$y,ylab="R-number",xlab="Date",x=dfR$date,ylim=c(0.6,1.4),xlim=plotdate,cex.lab=2.0, cex.axis=2.0, cex.main=2.0, cex.sub=2.0)
  lines(y=Rest$England_LowerBound,x=Rest$Date-sagedelay)
  lines(y=Rest$England_UpperBound,x=Rest$Date-sagedelay)
  lines(predict(loess(p50 ~ x, data=dfR,span=lospan)),col='red',x=dfR$date,title("50-54"))
  invisible(dev.off())
  pdf(file = 'p55.pdf')
  plot(smooth.spline(dfR$p55,df=spdf,w=sqrt(comdat$allCases))$y,ylab="R-number",xlab="Date",x=dfR$date,ylim=c(0.6,1.4),xlim=plotdate,cex.lab=2.0, cex.axis=2.0, cex.main=2.0, cex.sub=2.0)
  lines(y=Rest$England_LowerBound,x=Rest$Date-sagedelay)
  lines(y=Rest$England_UpperBound,x=Rest$Date-sagedelay)
  lines(predict(loess(p55 ~ x, data=dfR,span=lospan)),col='red',x=dfR$date,title("55-59"))
  invisible(dev.off())
  pdf(file = 'p60.pdf')
  plot(smooth.spline(dfR$p60,df=spdf,w=sqrt(comdat$allCases))$y,ylab="R-number",xlab="Date",x=dfR$date,ylim=c(0.6,1.4),xlim=plotdate,cex.lab=2.0, cex.axis=2.0, cex.main=2.0, cex.sub=2.0)
  lines(y=Rest$England_LowerBound,x=Rest$Date-sagedelay)
  lines(y=Rest$England_UpperBound,x=Rest$Date-sagedelay)
  lines(predict(loess(p60 ~ x, data=dfR,span=lospan)),col='red',x=dfR$date,title("60-64"))
  invisible(dev.off())
  pdf(file = 'p65.pdf')
  plot(smooth.spline(dfR$p65,df=spdf,w=sqrt(comdat$allCases))$y,ylab="R-number",xlab="Date",x=dfR$date,ylim=c(0.6,1.4),xlim=plotdate,cex.lab=2.0, cex.axis=2.0, cex.main=2.0, cex.sub=2.0)
  lines(y=Rest$England_LowerBound,x=Rest$Date-sagedelay)
  lines(y=Rest$England_UpperBound,x=Rest$Date-sagedelay)
  lines(predict(loess(p65 ~ x, data=dfR,span=lospan)),col='red',x=dfR$date,title("65-69"))
  invisible(dev.off())
  pdf(file = 'p70.pdf')
  plot(smooth.spline(dfR$p70,df=spdf,w=sqrt(comdat$allCases))$y,ylab="R-number",xlab="Date",x=dfR$date,ylim=c(0.6,1.4),xlim=plotdate,cex.lab=2.0, cex.axis=2.0, cex.main=2.0, cex.sub=2.0)
  lines(y=Rest$England_LowerBound,x=Rest$Date-sagedelay)
  lines(y=Rest$England_UpperBound,x=Rest$Date-sagedelay)
  lines(predict(loess(p70 ~ x, data=dfR,span=lospan)),col='red',x=dfR$date,title("70-74"))
  invisible(dev.off())
  pdf(file = 'p75.pdf')
  plot(smooth.spline(dfR$p75,df=spdf,w=sqrt(comdat$allCases))$y,ylab="R-number",xlab="Date",x=dfR$date,ylim=c(0.6,1.4),xlim=plotdate,cex.lab=2.0, cex.axis=2.0, cex.main=2.0, cex.sub=2.0)
  lines(y=Rest$England_LowerBound,x=Rest$Date-sagedelay)
  lines(y=Rest$England_UpperBound,x=Rest$Date-sagedelay)
  lines(predict(loess(p75 ~ x, data=dfR,span=lospan)),col='red',x=dfR$date,title("75-79"))
  invisible(dev.off())
  pdf(file = 'p80.pdf')
  plot(smooth.spline(dfR$p80,df=spdf,w=sqrt(comdat$allCases))$y,ylab="R-number",xlab="Date",x=dfR$date,ylim=c(0.6,1.4),xlim=plotdate,cex.lab=2.0, cex.axis=2.0, cex.main=2.0, cex.sub=2.0)
  lines(y=Rest$England_LowerBound,x=Rest$Date-sagedelay)
  lines(y=Rest$England_UpperBound,x=Rest$Date-sagedelay)
  lines(predict(loess(p80 ~ x, data=dfR,span=lospan)),col='red',x=dfR$date,title("80-85"))
  invisible(dev.off())
  pdf(file = 'p85.pdf')
  plot(smooth.spline(dfR$p85,df=spdf,w=sqrt(comdat$allCases))$y,ylab="R-number",xlab="Date",x=dfR$date,ylim=c(0.6,1.4),xlim=plotdate,cex.lab=2.0, cex.axis=2.0, cex.main=2.0, cex.sub=2.0)
  lines(y=Rest$England_LowerBound,x=Rest$Date-sagedelay)
  lines(y=Rest$England_UpperBound,x=Rest$Date-sagedelay)
  lines(predict(loess(p85 ~ x, data=dfR,span=lospan)),col='red',x=dfR$date,title("85-89"))
  invisible(dev.off())
  pdf(file = 'p90.pdf')
  plot(smooth.spline(na.omit(dfR$p90),df=spdf,w=sqrt(comdat$allCases[!is.na(dfR$p90)]))$y,ylab="R-number",xlab="Date",
       x=dfR$date[!is.na(dfR$p90)],ylim=c(0.6,1.4),xlim=plotdate,cex.lab=2.0, cex.axis=2.0, cex.main=2.0, cex.sub=2.0)
  lines(y=Rest$England_LowerBound[!is.na(dfR$p90)],x=Rest$Date[!is.na(dfR$p90)]-sagedelay)
  lines(y=Rest$England_UpperBound[!is.na(dfR$p90)],x=Rest$Date[!is.na(dfR$p90)]-sagedelay)
  lines(predict(loess(p90 ~ x, data=dfR[!is.na(dfR$p90),],span=lospan)),col='red',x=dfR$date[!is.na(dfR$p90)],title("90+"))
  invisible(dev.off())
}
}

#Reverse Engineer cases from R-number - requires stratonovich calculus to get reversibility
# Initializations
#rm(PredictCases,PredictCasesSmoothR)

#  Use the same weekend-adjusted initial condition, regardless of smoothing effect
#  Start with the "correct" data

PredictCases<-comdat$allCases
PredictCasesLin<-comdat$allCases
PredictCasesRaw<-PredictCases
PredictCasesSmoothR<-PredictCases
PredictCasesdenoiseR<-PredictCases
PredictCasesMeanR<- PredictCases
PredictCasesLoessR<- PredictCases
dfR$smoothR<-smooth.spline(dfR$bylogR,df=spdf)$y
dfR$loessR<-predict(loess(bylogR ~ x, data=dfR,span=lospan))
meanR=mean(dfR$rawR)


for(i in (genTime+22):length(dfR$gjaR)){
  PredictCases[i]=PredictCases[i-1]*exp((dfR$bylogR[i]-1)/genTime)
  PredictCasesLin[i]=PredictCasesLin[i-1]*(1.0+(dfR$gjaR[i]-1)/genTime)
  PredictCasesRaw[i]=PredictCasesRaw[i-1]*(1.0+(dfR$rawR[i]-1)/genTime)
  PredictCasesMeanR[i]=PredictCasesMeanR[i-1]*(1.0+(meanR-1)/genTime)
  PredictCasesSmoothR[i]=PredictCasesSmoothR[i-1]*exp((dfR$smoothR[i]-1)/genTime)
  PredictCasesLoessR[i]=PredictCasesLoessR[i-1]*exp((dfR$loessR[i]-1)/genTime)
  }
denoise=sum(PredictCasesLoessR)/sum(comdat$allCases)
#  Averaging R is not the same as averaging e^R
#  Noise suppresses the growth rate in the model, Smoothed R grows too fast
#   Multiplier chosen to match final cases, because it is super-sensitive to noise in the initial day
#

PredictCasesLoessR=PredictCasesLoessR*sum(comdat$allCases)/sum(PredictCasesLoessR)
PredictCasesSmoothR=PredictCasesSmoothR*sum(comdat$allCases)/sum(PredictCasesSmoothR)
PredictCasesMeanR=PredictCasesMeanR*sum(comdat$allCases)/sum(PredictCasesMeanR)



plot(comdat$allCases,x=comdat$date,xlab="Date",ylab="Cases backdeduced from R")
lines(PredictCases,x=comdat$date, col="red")
lines(PredictCasesSmoothR,x=dfR$date, col="blue",lwd=2)
lines(PredictCasesMeanR,x=comdat$date, col="green",lwd=2)
lines(PredictCasesLoessR,x=comdat$date, col="violet",lwd=2)

# ggplot version of the same graph
tmpdat <- tibble(date=comdat$date,PredictCases=PredictCases,
                 PredictCasesLoessR=PredictCasesLoessR,
                 PredictCasesSmoothR=PredictCasesSmoothR,
                 PredictCasesMeanR=PredictCasesMeanR)
ggplot(comdat,aes(x=date)) + geom_point(aes(y=allCases),alpha=0.5) +
   geom_line(data=tmpdat, aes(x=date,y=PredictCases),colour="red",alpha=0.75) +
   geom_line(data=tmpdat, aes(x=date,y=PredictCasesSmoothR),colour="blue",alpha=0.75) +
   geom_line(data=tmpdat, aes(x=date,y=PredictCasesMeanR),colour="green",alpha=0.75) +
   geom_line(data=tmpdat, aes(x=date,y=PredictCasesLoessR),colour="violet",alpha=0.75) +
   xlab("Date") + ylab("Cases backdeduced from R")
rm(tmpdat)

# Load code to function to output to the web-ui interface
# From stackoverflow: 6456501
if(!exists("outputJSON", mode="function")) source("json_out.R")

# # Beginning of time series
t0 <-  min(dfR$date)

# Get the days for which data will be output
days <- as.integer(dfR$date - t0)

# Labels are optional
outputJSON(myt0 = t0,
           mydaysarray = days,
           myregion = "GB",
           mysubregion = "ENG", # see https://en.wikipedia.org/wiki/ISO_3166-2:GB
           mycalibrationCaseCount = NA,  # ADD VALUE, eg single number
           mycalibrationDate = NA,       # ADD VALUE, eg "2021-05-12"
           mycalibrationDeathCount=NA,   # ADD VALUE, eg single number
           myr0 = NA,
           myinterventionPeriods= NA,
           myCritRecov = NA,
           myCritical = NA,
           myILI = NA,
           myMild = NA,
           myR = dfR$piecewise,
           mySARI = NA,
           mycumCritRecov = NA,
           mycumCritical = NA,
           mycumILI = NA,
           mycumMild = NA,
           mycumSARI = NA,
           myincDeath = NA
)

#####  Figures and analysis for https://www.medrxiv.org/content/10.1101/2021.04.14.21255385v1


####  From here on we're reproducing figures from https://www.medrxiv.org/content/10.1101/2021.04.14.21255385v1
##### Fig 1. - Heatmaps ####
groups = colnames(casedat[2:20])
# casemelt = melt(as.matrix(casedat[2:20]))
# deathmelt = melt(as.matrix(deathdat[2:20]))
# colourscheme = rgb(1-rescale(casemelt$value), 1, 1-rescale(deathmelt$value))
# casemelt$value2 = colourscheme
# casemap = ggplot() +
#   geom_tile(data = casemelt, aes(Var1, Var2, fill = value)) +
#   scale_fill_gradient(low = "white", high = "blue") +
#   new_scale_fill() +
#   geom_tile(data = deathmelt, aes(Var1, Var2, fill = value)) +
#   scale_fill_gradient(low = "black", high = "red") +
#   new_scale_fill() +
#   geom_tile(data = casemelt, aes(Var1, Var2, fill = value2)) +
#   scale_fill_identity() +
#   labs(x = "Date", y = "Age Group") +
#   theme(legend.position = "none",
#         panel.grid.major = element_blank(),
#         panel.border = element_blank(),
#         panel.background = element_blank())
# print(casemap)
# rm(casemap, casemelt, deathmelt, colourscheme)
#

image(casedat$date, 1:19, as.matrix(casedat[2:20]),
      xlab = "Time", ylab = "Age group", col = hcl.colors(96, "Blues", rev = TRUE),
      axes = F, mgp = c(3.3, 1, 0))
axis.Date(1, at=seq(min(casedat$date), max(casedat$date), by="1 month"), format="%m-%Y")
axis(2, 1:19, labels = groups, las = 1, cex.axis = 0.8)
title(main = "Cases and Deaths")


deathmap = image(deathdat$date, 1:19, as.matrix(deathdat[2:20]),
                xlab = "", ylab = "", col = hcl.colors(96, "Reds", rev = TRUE),
                axes = F, mgp = c(3.3, 1, 0))
axis.Date(1, at=seq(min(deathdat$date), max(deathdat$date), by="1 month"), format="%m-%Y")
axis(2, 1:19, labels = groups, las = 1, cex.axis = 0.8)
title(main = "Deaths heatmap")
rm(deathmap, groups)

#### AGE GROUPS - Lognormal distribution ####
##We are fixing parameters at the clinical levels from Hawryluk et al.
logmean = 2.534
logsd = 0.613
lndist = dlnorm(1:28, logmean, logsd) #params from Hawryluk et al.
ggplot(data.frame(index = 1:28, prop = lndist)) +
  geom_point(aes(x = index, y = prop)) +
  labs(title = "Discretised Lognormal Distribution (Hawryluk)") +
  xlab("Time to Death") +
  ylab("Proportion of day zero cases")
rm(logmean, logsd)

#### AGE GROUPS - Gamma distribution ####
##We are fixing alpha at the clinical level of 4.447900991. Verity et al. find a global beta of 4.00188764
alpha = 4.447900991
beta = 4.00188764
gamdist = dgamma(1:28, shape = alpha, scale = beta) #params from Verity et al.
ggplot(data.frame(index = 1:28, prop = gamdist)) +
  geom_point(aes(x = index, y = prop)) +
  labs(title = "Discretised Gamma Distribution (Verity)") +
  xlab("Time to Death") +
  ylab("Proportion of day zero cases")
rm(alpha, beta)

#Spread each age group's cases by the distribution
logcases <- casedat
gamcases <- logcases
logcases[2:20] = NA_real_
gamcases[2:20] = NA_real_
for (agegroup in 2:20) {
  for (day in 28:nrow(logcases)) {
    logcases[day,agegroup] = sum(casedat[(day-27):day,agegroup] * rev(lndist))
    gamcases[day,agegroup] = sum(casedat[(day-27):day,agegroup] * rev(gamdist))
  }
}
rm(agegroup, day)

#Spread all cases by the distribution
comdat$logcaseload <- 0
comdat$gamcaseload <- 0
for (day in 28:nrow(comdat)) {
  comdat$logcaseload[day] = sum(comdat$allCases[(day-27):day] * rev(lndist))
  comdat$gamcaseload[day] = sum(comdat$allCases[(day-27):day] * rev(gamdist))
}
scotdat$logcaseload <- 0
scotdat$gamcaseload <- 0
for (day in 28:nrow(comdat)) {
  scotdat$logcaseload[day] = sum(scotdat$allCases[(day-27):day] * rev(lndist))
  scotdat$gamcaseload[day] = sum(scotdat$allCases[(day-27):day] * rev(gamdist))
}
#  Spread Regional cases by the lognormal & gammadistribution
reglnpredict<-regdeaths
reggampredict<-regdeaths
for (area in 2:10){
for (day in 28:nrow(comdat)){
  reglnpredict[day,area] = sum(regcases[(day-27):day,area] * rev(lndist))
  reggampredict[day,area] = sum(regcases[(day-27):day,area] * rev(gamdist))}}
rm(day,area)


#  Regional plots, with CFR input by hand
plot(regdeaths$London*55,x=regdeaths$date)
lines(reglnpredict$London,x=reglnpredict$date)
lines(reggampredict$London,x=reglnpredict$date)

plot(regdeaths$`North East`*55,x=regdeaths$date)
lines(reglnpredict$`North East`,x=reglnpredict$date)
plot(regdeaths$`North West`*55,x=regdeaths$date)
lines(y=reglnpredict$`North West`,x=reglnpredict$date)
lines(reglnpredict$`South West`,x=reglnpredict$date)
lines(reglnpredict$`South East`,x=reglnpredict$date)
lines(reglnpredict$`East Midlands` ,x=reglnpredict$date)
lines(reglnpredict$`East of England`,x=reglnpredict$date)
lines(reglnpredict$`West Midlands`,x=reglnpredict$date)
lines(reglnpredict$`Yorkshire and The Humber`,x=reglnpredict$date)

for (area in 2:10){
  lines(reglnpredict[2:279,area])}
#Plots
ggplot(logcases, aes(x = date)) +
  geom_line(aes(y = rowSums(logcases[,2:20])), na.rm = TRUE) +
  ggtitle("All age groups separately lognormal distributed")

#### Fig 2. Distributions ####
distdat = data.frame(days = 1:29, ln = c(lndist, 0), gam = c(gamdist, 0), exp = c(dexp(1:28, rate = 0.1), 0),
                     shift = c(rep(0, 14), 1, rep(0, 14)),
                     avgshift = c(rep(0, 11), rep((1/7),7), rep(0, 11)))
ggplot(data = distdat, aes(x = days)) +
  geom_line(aes(y = ln, color = "Lognormal"), size = 1, na.rm = TRUE) +
  geom_line(aes(y = gam, color = "Gamma"), size = 1, na.rm = TRUE) +
  geom_line(aes(y = exp, color = "Exponential"), size = 1, na.rm = TRUE) +
  geom_line(aes(y = shift, color = "Shift"), size = 1, na.rm = TRUE) +
  geom_line(aes(y = avgshift, color = "7-day Average and Shift"), size = 1, na.rm = TRUE) +
  labs(title = "Distributions", y = "Fraction of Deaths", x = "Days after case detected",
       colour = "Legend") +
  scale_color_manual(values = c("Lognormal" = "red", "Gamma" = "blue", "Exponential" = "green", "Shift" = "orange", "7-day Average and Shift" = "maroon")) +
  scale_x_continuous(breaks =  0:30) +
  coord_cartesian(ylim=c(0, 0.15)) +
  theme_bw()


#### AGE GROUPS - Gamma Model ####
#Calculate age-group CFRs to fit Oct-Nov from Gamma
gamageweights = data.frame(agegroup = names(casedat[2:20]), weight = 0, lowerbound = 0, upperbound = 0)
for (agegroup in 2:20) {
  daterange = seq.Date(as.Date("2020-10-01"), as.Date("2020-11-30"), by = "day")
  thesedeaths = unlist(filter(deathdat, date %in% daterange)[,agegroup])
  thesecases = unlist(filter(gamcases, date %in% daterange)[,agegroup])
  model = summary(lm(thesedeaths ~ thesecases))
  gamageweights[agegroup-1, "weight"] <- coef(model)[2,1]
  gamageweights[agegroup-1, "lowerbound"] <- coef(model)[2,1] - (2*coef(model)[2,2])
  gamageweights[agegroup-1, "upperbound"] <- coef(model)[2,1] + (2*coef(model)[2,2])
}
write.csv(gamageweights[10:19,], "forpub.csv")
rm(model)

gampred = gamcases
for (agegroup in 2:20) {
  gampred[,agegroup] = gamcases[, agegroup] * gamageweights$weight[agegroup-1]
}
gampred$allCasesPred = rowSums(gampred[,2:20])

#### AGE GROUPS - Lognormal Model ####
#Calculate age-group CFRs to fit Oct-Nov from Lognormal
logageweights = data.frame(agegroup = names(casedat[2:20]), weight = 0, lowerbound = 0, upperbound = 0)
for (agegroup in 2:20) {
  daterange = seq.Date(as.Date("2020-10-01"), as.Date("2020-11-30"), by = "day")
  thesedeaths = unlist(filter(deathdat, date %in% daterange)[,agegroup])
  thesecases = unlist(filter(logcases, date %in% daterange)[,agegroup])
  model = summary(lm(thesedeaths ~ thesecases))
  logageweights[agegroup-1, "weight"] <- coef(model)[2,1]
  logageweights[agegroup-1, "lowerbound"] <- coef(model)[2,1] - (2*coef(model)[2,2])
  logageweights[agegroup-1, "upperbound"] <- coef(model)[2,1] + (2*coef(model)[2,2])
}
rm(model)

logpred = logcases
for (agegroup in 2:20) {
  logpred[,agegroup] = logcases[, agegroup] * logageweights$weight[agegroup-1]
}
logpred$allCasesPred = rowSums(logpred[,2:20])

#### Original WSS (hardcoded) ####
WSS = data.frame(date = c("28/07/2020", "29/07/2020", "30/07/2020", "31/07/2020", "01/08/2020", "02/08/2020", "03/08/2020", "04/08/2020", "05/08/2020", "06/08/2020", "07/08/2020", "08/08/2020", "09/08/2020", "10/08/2020", "11/08/2020", "12/08/2020", "13/08/2020", "14/08/2020", "15/08/2020", "16/08/2020", "17/08/2020", "18/08/2020", "19/08/2020", "20/08/2020", "21/08/2020", "22/08/2020", "23/08/2020", "24/08/2020", "25/08/2020", "26/08/2020", "27/08/2020", "28/08/2020", "29/08/2020", "30/08/2020", "31/08/2020", "01/09/2020", "02/09/2020", "03/09/2020", "04/09/2020", "05/09/2020", "06/09/2020", "07/09/2020", "08/09/2020", "09/09/2020", "10/09/2020", "11/09/2020", "12/09/2020", "13/09/2020", "14/09/2020", "15/09/2020", "16/09/2020", "17/09/2020", "18/09/2020", "19/09/2020", "20/09/2020", "21/09/2020", "22/09/2020", "23/09/2020", "24/09/2020", "25/09/2020", "26/09/2020", "27/09/2020", "28/09/2020", "29/09/2020", "30/09/2020", "01/10/2020", "02/10/2020", "03/10/2020", "04/10/2020", "05/10/2020", "06/10/2020", "07/10/2020", "08/10/2020", "09/10/2020", "10/10/2020", "11/10/2020", "12/10/2020", "13/10/2020", "14/10/2020", "15/10/2020", "16/10/2020", "17/10/2020", "18/10/2020", "19/10/2020", "20/10/2020", "21/10/2020", "22/10/2020", "23/10/2020", "24/10/2020", "25/10/2020", "26/10/2020", "27/10/2020", "28/10/2020", "29/10/2020", "30/10/2020", "31/10/2020", "01/11/2020", "02/11/2020", "03/11/2020", "04/11/2020", "05/11/2020", "06/11/2020", "07/11/2020", "08/11/2020", "09/11/2020", "10/11/2020", "11/11/2020", "12/11/2020", "13/11/2020", "14/11/2020", "15/11/2020", "16/11/2020", "17/11/2020", "18/11/2020", "19/11/2020", "20/11/2020", "21/11/2020", "22/11/2020", "23/11/2020", "24/11/2020", "25/11/2020", "26/11/2020", "27/11/2020", "28/11/2020", "29/11/2020", "30/11/2020", "01/12/2020", "02/12/2020", "03/12/2020", "04/12/2020", "05/12/2020", "06/12/2020", "07/12/2020", "08/12/2020", "09/12/2020", "10/12/2020", "11/12/2020", "12/12/2020", "13/12/2020", "14/12/2020", "15/12/2020", "16/12/2020", "17/12/2020", "18/12/2020", "19/12/2020", "20/12/2020", "21/12/2020", "22/12/2020", "23/12/2020", "24/12/2020", "25/12/2020", "26/12/2020", "27/12/2020", "28/12/2020", "29/12/2020", "30/12/2020", "31/12/2020", "01/01/21", "02/01/21", "03/01/21", "04/01/21", "05/01/21", "06/01/21", "07/01/21", "08/01/21", "09/01/21", "10/01/21", "11/01/21", "12/01/21", "13/01/21", "14/01/21", "15/01/21", "16/01/21"),
                 values = c(15,15,14,15,16,16,17,17,17,18,19,18,17,17,17,16,16,14,13,13,13,12,12,12,12,12,12,12,13,14,14,13,15,20,24,27,29,31,38,41,39,39,39,38,38,36,37,39,40,41,43,46,48,50,53,58,61,63,64,70,76,81,86,94,101,107,117,125,136,144,151,152,156,165,172,177,185,194,204,210,222,239,251,264,274,280,287,294,294,297,304,312,313,314,337,347,356,356,359,365,374,373,383,391,397,402,402,398,391,383,369,355,340,332,322,305,293,284,277,273,267,266,268,268,267,268,269,271,275,274,277,285,297,309,320,333,356,374,392,409,425,437,447,466,487,502,497,459,483,516,510,589,646,690,730,778,801,890,884,889,918,980,959,939,921,904,885,870,849,821,856,804,751))
WSS$date = dmy(WSS$date)
WSS$date = WSS$date + 12

#### Model Fit Stats ####
#Get Autumn model fits
if(interactive()){
  model = lm(filter(comdat, date %in% daterange)$allDeaths ~ filter(gampred, date %in% daterange)$allCasesPred)
  summary(model)

  model = lm(filter(comdat, date %in% daterange)$allDeaths ~ filter(logpred, date %in% daterange)$allCasesPred)
  summary(model)

  model = lm(filter(comdat, date %in% daterange)$allDeaths ~ filter(WSS, date %in% daterange)$values)
  summary(model)

  #Get overall model fits
  model = lm(comdat$allDeaths ~ gampred$allCasesPred)
  summary(model)

  model = lm(comdat$allDeaths ~ logpred$allCasesPred)
  summary(model)

  model = lm(filter(comdat, date %in% WSS$date)$allDeaths ~ WSS$values)
  summary(model)
  rm(model)
}

#### Model plots ####
#Plot prediction against reality
ggplot(data = comdat, aes(x = date)) +
  geom_line(mapping = aes(y = allDeaths, color = "Deaths (Government Figures)"), size = 1, na.rm = TRUE) +
  geom_line(data = gampred, aes(y = allCasesPred, color = "Gamma Model Predicted Deaths"), size = 1, na.rm = TRUE) +
  geom_line(data = logpred, aes(y = allCasesPred, color = "Lognormal Model Predicted Deaths"), size = 1, na.rm = TRUE) +
  geom_line(data = WSS, aes(y = values, color = "WSS Original"), size = 1, na.rm = TRUE) +
  labs(title = "Predicted Deaths vs. Actual Deaths", color = "Legend") +
  ylab("Deaths") +
  xlab("Date") +
  scale_color_manual(values = c("Deaths (Government Figures)" = "Blue",
                                "Gamma Model Predicted Deaths" = "Red",
                                "Lognormal Model Predicted Deaths" = "Green",
                                "WSS Original" = "Orange")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_bw()


#### Plot all lognormal-derived CFRs ####
rollframe = as.data.frame(apply(logcases[,2:20], 2, rollmean, 7, na.pad = T))
rollframe$date = logcases$date
rollframe = pivot_longer(rollframe, cols = colnames(logcases[11:20]),
                         names_to = "agegroup", names_prefix = "X", values_to = "Cases")

deathroll = as.data.frame(apply(deathdat[,2:20], 2, rollmean, 7, na.pad = T))
deathroll$date = deathdat$date
deathframe = pivot_longer(deathroll, cols = colnames(deathdat[11:20]),
                         names_to = "agegroup", names_prefix = "X", values_to = "Deaths")

rollframe$Deaths = deathframe$Deaths
rollframe$CFR = rollframe$Deaths/rollframe$Cases
rm(deathframe)
rollframe = rollframe[301:(nrow(rollframe)-30),]


plotCFR = ggplot() 
  geom_line(data = rollframe, aes(x = date, y = CFR, color = agegroup), size = 1.1, na.rm = TRUE) +
  scale_colour_manual(values = rev(brewer.pal(10,"Set3"))) +
  labs(title = paste("Case Fatality Ratios by age group -  7-day rolling averages"),
       subtitle = "Lognormal model",
       x = "Date", y = "CFR") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_bw() +
  geom_rect(aes(xmin=as.Date("2020/12/01"), xmax=as.Date("2021/01/16"), ymin=0, ymax=Inf), fill = "red", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021/01/17"), xmax=Sys.Date(), ymin=0, ymax=Inf), fill = "green", alpha = 0.1)
print(plotCFR)

