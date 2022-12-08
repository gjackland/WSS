#!/usr/bin/env Rscript
#
# Weight, Scale and Shift (WSS) Code
#
# Copyright 2021 Graeme J Ackland, Mario Antonioletti, The University of Edinburgh,
#                James A Ackland, The University of Cambridge
#                David J Wallace.
#
# This code is made available under a GPL-3.0 License.
#
# Data used in making calculations is made available under an Open Government
# Licence. For more details see:
#
# http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/
#


# From Jan 6th  2022 Need to multiply cases in Scotland by fraction which are LFT and not reported, because, Scotland
#  Enter this array by hand copied from https://www.gov.scot/publications/coronavirus-covid-19-trends-in-daily-data/
# Another problem : Weekend behaviour of LFT is completely different to PCR


#  Italian data is here ... https://github.com/InPhyT/COVID19-Italy-Integrated-Surveillance-Data

source("CompartmentFunction.R")
source("medrxiv.R")
source("Predictions.R")
source("age_pdfplot.R")
source("Weekend.R")
source("CC_write.R")
source("covidSimData.R")
# Set the working directory to be the same as to where the script is run from.
setwd(".")

# Turn off scientific notation.
options(scipen = 999)

#### Read data ####
# distributions and populations hard coded in this routine
#covidsimAge<-covidSimData()
#population<-getPop()
# Base URL to get the UK government data
baseurl <- "https://api.coronavirus.data.gov.uk/v2/data?"


# Start and end date - in getParms

#  Dates for the plots
plotdate <- as.Date(c(as.character(startdate),as.character(enddate)))
# Wanted to plot a Smooth spline discontinuous at
# UK lockdown Oct 31 (day 98) -Dec 2  (day 130) Jan 6 (day 165)  (day 1 = July 25)
lock1 <- as.integer(as.Date("2020/10/31")-startdate)
unlock1 <- as.integer(as.Date("2020/12/02")-startdate)
lock2 <- as.integer(as.Date("2021/01/06")-startdate)
unlock2 <- as.integer(as.Date("2021/03/08")-startdate)
unlock3 <- as.integer(as.Date("2021/04/12")-startdate)
test_delay <- 12
lock1 <- lock1+test_delay
unlock1 <- unlock1+test_delay
lock2 <- lock2+test_delay
unlock2 <- unlock2+test_delay
unlock3 <- unlock3+test_delay

sagedelay <- 16 # Delay in producing R-number, for plots
#  Initiate list for Hospital data
Hospital<-list()

# Total cases, deaths, tests England
casesurl <- paste0(baseurl,
                   "areaType=nation&",
                   "areaCode=E92000001&",
                   "metric=newCasesBySpecimenDate&",
                   "metric=newDeaths28DaysByDeathDate&",
                   "metric=newPCRTestsByPublishDate&",
                   "metric=newPeopleVaccinatedFirstDoseByVaccinationDate&",
                   "metric=covidOccupiedMVBeds&",
                   "format=csv")

# Explicitly define the types for the columns
coltypes <- cols(col_character(), col_character(),col_character(),
                 col_date(format="%Y-%m-%d"), col_integer(),
                 col_integer(),  col_integer(), col_integer())

# Read the cases, deaths and tests data
comdat <-  read_csv(file = casesurl, col_types = coltypes)



# Transform the data
comdat <- comdat %>%  select(date,
                             allCases = newCasesBySpecimenDate,
                             allDeaths = newDeaths28DaysByDeathDate,
                             tests = newPCRTestsByPublishDate,
                             inputCases = newCasesBySpecimenDate,
                             fpCases = newCasesBySpecimenDate,
                             vaccines=newPeopleVaccinatedFirstDoseByVaccinationDate,
                             MVBeds=covidOccupiedMVBeds)  %>% arrange(date)

comdat[is.na(comdat)]<-0
#  Cumulative cases from beginning, regardless of startdate, although first wave is dubious
comdat$cumulative=comdat$allCases
for(i in 2:(nrow(comdat))){
  comdat$cumulative[i]=comdat$cumulative[i-1]+comdat$allCases[i]
}
comdat <- comdat %>%
  filter(date >= startdate &
           date <= enddate ) %>%
  arrange(date)

newurl="https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=cumAdmissions&metric=hospitalCases&metric=newAdmissions&format=csv"

# Explicitly define the types for the columns  Need to repeat call because??? data download restriction
coltypes <- cols(col_character(), col_character(),col_character(),
                 col_date(format="%Y-%m-%d"),
                 col_integer(),  col_integer(), col_integer())


tempdata<-read_csv(file = newurl, col_types = coltypes)

tempdata <-  tempdata %>%  select(date,
                            admissions = newAdmissions,
                             hospital = hospitalCases,
                             )%>%
  filter(date >= startdate & date <= enddate ) %>%   arrange(date)

comdat<-  cbind(comdat,tempdata[2:3])
rm(tempdata)
# All UK cases (to estimate pre-Sept England Cases)

ukcaseurl <- paste0(baseurl,
                    "areaType=overview&",
                    "metric=newPCRTestsByPublishDate&",
                    "format=csv")

# Explicitly define the types for the columns
coltypes <- cols(col_character(), col_character(),col_character(),
                 col_date(format="%Y-%m-%d"), col_double())

# Read the case data
ukcasedat <-  read_csv(file = ukcaseurl, col_types = coltypes)

# Transform the case data
ukcasedat <- ukcasedat %>%
             select(date = date, tests = newPCRTestsByPublishDate) %>%
             filter(date >= startdate & date <= enddate ) %>%
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
                 col_integer(), col_integer(), col_number())

# read in the cases by age data
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

vacdate="2020-12-08"
# vaccination by age
vacurl <- paste0(baseurl,
                 "areaType=nation&",
                 "areaCode=E92000001&",    # England
                 "metric=vaccinationsAgeDemographics&",
                 "format=csv")

# Explicitly define the types for the columns as same with cases
# Explicitly define the types for the columns
# Age is a character as it giving a range, e.g. 00_04, 05_09, ...
coltypes <- cols(areaCode=col_character(), areaName=col_character(),areaType=col_character(),
                 date=col_date(format="%Y-%m-%d"), age=col_character())

# Read in the vaccination data.
vacdat <-  read_csv(file = vacurl, col_types = coltypes)

# Transform the data to get vacdat compatible with casedat (must be a better way!).
vacdat <- vacdat %>%
  select(datetmp = date, age = age, values = cumVaccinationFirstDoseUptakeByVaccinationDatePercentage) %>%
  pivot_wider(id_cols = datetmp, names_from = age, values_from = values) %>%
  filter(datetmp >= vacdate  & datetmp <= enddate) %>%
  arrange(datetmp)

# Add vaccination data for the under 24s.

# casedat has age groups 00-04, 05-09, 10-14, 15-19, 20-24, rest are the same
# vacdat has age groups  16-17 18-24, rest are the same
vacdat<-cbind('20_24'=vacdat$'18_24',vacdat)
vacdat<-cbind('15_19'=0.4*vacdat$'18_24'+0.4*vacdat$'16_17',vacdat)
vacdat<-cbind('10_14'=0.0,vacdat)
vacdat<-cbind('05_09'=0.0,vacdat)
vacdat<-cbind('00_04'=0.0,vacdat)
vacdat<-cbind(date=vacdat$datetmp,vacdat)
vacdat$`18_24`<-NULL
vacdat$datetmp<-NULL
#  Extend vacdat to before programme started with zeroes
tmp<-NULL
tmp<-casedat %>% filter(date < vacdate)
tmp[2:20]=0.0

vacdat <- bind_rows(tmp,vacdat)
rm(tmp)

# convert to fraction
vacdat[2:length(vacdat)]<-vacdat[2:length(vacdat)]/100.0

# deaths by age
deathurl <- paste0(baseurl,
                   "areaType=nation&",
                   "areaCode=E92000001&",
                   "metric=newDeaths28DaysByDeathDateAgeDemographics&",
                   "format=csv")

# Explicitly define the types for the columns
coltypes <- cols(areaCode=col_character(), areaName=col_character(),areaType=col_character(),
                 date=col_date(format="%Y-%m-%d"),age=col_character(),
                 deaths=col_number(), rollingSum=col_number(), rollingRate=col_number())

# Read the deaths by age data
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

# discontinued Scotland data https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=S92000003&metric=newCasesBySpecimenDate&metric=newDeaths28DaysByDeathDate&metric=newDeaths28DaysByPublishDate&format=csv
# https://www.opendata.nhs.scot/dataset/covid-19-in-scotland/resource/9393bd66-5012-4f01-9bc5-e7a10accacf4

# Wales data https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=W92000004&metric=newCasesBySpecimenDate&metric=newDeaths28DaysByDeathDate&format=csv

walesurl <-  paste0(baseurl,
                    "areaType=nation&",
                    "areaCode=W92000004&",
                    "metric=newCasesBySpecimenDate&",
                    "metric=newDeaths28DaysByDeathDate&",
                    "format=csv")
coltypes <-  cols(
  date = col_date(format = "%Y-%m-%d"),
  newCasesBySpecimenDate = col_number(),
  newDeaths28DaysByDeathDate = col_number()
)

# Read in the Welsh deaths and case data
walesdat <-  read_csv(file = walesurl, col_types = coltypes)

# Transform the data
walesdat <- walesdat %>%  select(date,
                                 allCases = newCasesBySpecimenDate,
                                 allDeaths = newDeaths28DaysByDeathDate,
                                 inputCases = newCasesBySpecimenDate) %>%
                          filter(date >= startdate &
                                 date <= enddate ) %>%
                           arrange(date)

# Get the Northern Irish deaths and case data
NIurl <-  paste0(baseurl,
                 "areaType=nation&",
                 "areaCode=N92000002&",
                 "metric=newCasesBySpecimenDate&",
                 "metric=newDeaths28DaysByDeathDate&",
                 "format=csv")


# Read in the NI deaths and case data
# Not ethese are not reported sine 19/05/22
NIdat <-  read_csv(file = NIurl, col_types = coltypes)

# Transform the data
NIdat <- NIdat %>%  select(date,
                           allCases = newCasesBySpecimenDate,
                           allDeaths = newDeaths28DaysByDeathDate,
                           inputCases = newCasesBySpecimenDate) %>%
                    filter(date >= startdate &
                           date <= enddate ) %>%
                    arrange(date)

# Regional data for deaths and cases by specimen date
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
  newCasesBySpecimenDate = col_number(),
  newDeaths28DaysByDeathDate = col_number()
)

# Read in the regional case and death data
regdat <-  read_csv(file = regurl, col_types = coltypes)


# Transform the data
regcases <- regdat %>%  select(date, areaName, areaCode,
                               Cases = newCasesBySpecimenDate) %>%
                        pivot_wider(id_cols = date,
                                    names_from = areaName,
                                    values_from = Cases) %>%
                        filter(date >= startdate &
                               date <= enddate )%>%
                        arrange(date)

# Map the rows in the areaType column to become columns and map the death data
# to lie under the corresponding column.
regdeaths <- regdat %>%
             select(date, areaName, Deaths = newDeaths28DaysByDeathDate) %>%
             pivot_wider(id_cols = date,
                         names_from = areaName, values_from = Deaths) %>%
             filter(date >= startdate &
                    date <= enddate )%>%
             arrange(date)
regdeaths$NEY=regdeaths$`North East`+regdeaths$`Yorkshire and The Humber`
regdeaths$MD=regdeaths$`East Midlands`+regdeaths$`West Midlands`
regdeaths$England=regdeaths$NEY+regdeaths$MD+regdeaths$`North West`+regdeaths$London+regdeaths$`East of England`
+regdeaths$`South East`+regdeaths$`South West`

# Get the demographic data for regions because can't download simultaneously with
# the death data.
regurl2 <- paste0(baseurl,
                  "areaType=region&",
                  "metric=newCasesBySpecimenDateAgeDemographics&",
                  "format=csv")
regurl3 <- paste0(baseurl,
                  "areaType=region&",
                  "metric=newDeaths28DaysByDeathDateAgeDemographics&",
                  "format=csv")

# Specify the column types
coltypes <- cols(
  areaCode = col_character(),
  areaName = col_character(),
  areaType = col_character(),
  date = col_date(format = "%Y-%m-%d"),
  age = col_character(),
  cases = col_number(),
  rollingSum = col_number(),
  rollingRate = col_number()
)

# Read in the regional case and death data by age and filter
# Transform the data - reduce the number of columns and filter the data to
# lie between specific dates.
#regagedat <-  read_csv(file = regurl2, col_types = coltypes) %>%
#  select(date, areaName, age, cases) %>%
#  filter(date >= startdate &
#           date <= enddate ) %>%
#  filter(age!="unassigned") %>%
#  arrange(date)

# And do it again for deaths to avoid 500 error Specify the column types
#regurl3 <- paste0(baseurl,
#                  "areaType=region&",
#                  "metric=newDeaths28DaysByDeathDateAgeDemographics&",
#                  "format=csv")
#coltypes <- cols(
#  areaCode = col_character(),
#  areaName = col_character(),
#  areaType = col_character(),
#  date = col_date(format = "%Y-%m-%d"),
#  age = col_character(),
#  deaths = col_number(),
#  rollingSum = col_number(),
#  rollingRate = col_number()
#)

coltypes <- cols(
  Date = col_date(format = "%d/%m/%Y"), UK_LowerBound = col_number(),
  UK_UpperBound = col_number(), England_LowerBound = col_number(),
  England_UpperBound = col_number(), EEng_LowerBound = col_number(),
  EEng_UpperBound = col_number(), Lon_LowerBound = col_number(),
  Lon_UpperBound = col_number(), Mid_LowerBound = col_number(),
  Mid_UpperBound = col_number(), NEY_LowerBound = col_number(),
  NEY_UpperBound = col_number(), NW_LowerBound = col_number(),
  NW_UpperBound = col_number(), SE_LowerBound = col_number(),
  SE_UpperBound = col_number(), SW_LowerBound = col_number(),
  SW_UpperBound = col_number()
)

# Read in the data - this data is obtained by a different script.
Rest <- read_csv(file="data/R_estimate.csv", col_types = coltypes)


# Scottish URL from which to pull the data
dailycasesurl = "https://www.opendata.nhs.scot/dataset/b318bddf-a4dc-4262-971f-0ba329e09b87/resource/2dd8534b-0a6f-4744-9253-9565d62f96c2/download/trend_hb_20210610.csv"

# Column types  Amended 10/02/2022
coltypes <- cols(
  Date = col_date(format = "%Y%m%d"),
  HB = col_character(),
  HBName = col_character(),
  DailyPositive = col_double(),
  CumulativePositive = col_double(),
  DailyPositivePCROnly = col_double(),
  CumulativePositivePCROnly = col_double(),
  DailyPositiveLFDOnly = col_double(),
  CumulativePositiveLFDOnly = col_double(),
  DailyPositivePCRAndLFD = col_double(),
  CumulativePositivePCRAndLFD = col_double(),
  DailyDeaths = col_double(),
  CumulativeDeaths = col_double(),
  CrudeRateDeaths = col_double(),
  PositiveTests = col_double(),
  PositiveTestsLFDOnly = col_double(),
  HospitalAdmissions = col_double(),
  HospitalAdmissionsQF = col_character(),
  ICUAdmissions = col_double(),
  ICUAdmissionsQF = col_character(),
  PositivePillar1 = col_double(),
  PositivePillar2 = col_double()
)


# Get the data
scotdailycases = read_csv(dailycasesurl, col_types = coltypes)

# Make the NHS boards the columns - DailyPositives are the values of PCR+LFT from 10/02/2022
scotdailycases %>% select(date=Date,board=HBName, cases=DailyPositive)  %>%
  pivot_wider(names_from = board, values_from = cases) %>%
  filter(date >= startdate & date <= enddate )         %>%
  arrange(date) -> scotdailycasesbyboard

# Hospital data
scotdailycases %>% filter(HBName=="Scotland") %>%
  select(date = Date, newcritdat = ICUAdmissions, newsaridat = HospitalAdmissions) %>%
  filter(date >= startdate & date <= enddate) %>%
  arrange(date) -> jnk
Hospital$Scot$date<-as.Date(jnk$date)
Hospital$Scot$newcritdat<-jnk$newcritdat
Hospital$Scot$newsaridat<-jnk$newsaridat


# Make the NHS boards the columns - DailyPositives are the values
scotdailycasesbyboard <- scotdailycases   %>%
  select(date=Date,board = HBName,
         cases = DailyPositive)                 %>%
  pivot_wider(names_from = board,
              values_from = cases)              %>%
  filter(date >= startdate & date <= enddate )  %>%
  arrange(date)
# Make the NHS boards the columns - DailyPositives are the values
scotdailydeathsbyboard <- scotdailycases   %>%
  select(date=Date,board = HBName,
         deaths = DailyDeaths)                 %>%
  pivot_wider(names_from = board,
              values_from = deaths)              %>%
  filter(date >= startdate & date <= enddate )  %>%
  arrange(date)

# Join the scotdailycases with regcases by date
#  Assume Scottish cases are stable is unavailable (full_join) otherwise have to lose days for everywhere
regcases <- full_join(regcases, scotdailycasesbyboard, by = c("date" = "date"))
na.locf(regcases)

#### Get tests for England pre-Sept by taking the post-Sept fraction of all tests
#    that were in England (0.867), and set vaccines to zero
comdat$tests[1:58] <- as.integer(ukcasedat$tests[1:58] * 0.867)
comdat$vaccines[is.na(comdat$vaccines)] <- 0.0
#### Get the UK hospital data & Append data to tibble
####  MV beds = CRIT
####  hospitalCases = SARI+CRIT+CRITREC
HospitalUrl <- paste0(baseurl,
                      "areaType=overview&",
                      "metric=covidOccupiedMVBeds&",
                      "metric=hospitalCases&",
                      "metric=newAdmissions&",
                      "format=csv")

# Column types
coltypes <-  cols(
  areaCode = col_character(),
  areaName = col_character(),
  areaType = col_character(),
  date = col_date(format = "%Y-%m-%d"),
  covidOccupiedMVBeds = col_double(),
  hospitalCases = col_double(),
  newAdmissions = col_double()
)

# Get the hospital data

jnk <- read_csv(HospitalUrl, col_types = coltypes)

Hospital$UK <- tibble()
Hospital$UK  <-  jnk %>%
                  select(date = date, saridat = hospitalCases, newsaridat = newAdmissions, critdat=covidOccupiedMVBeds) %>%
                  filter(date >= startdate & date <= enddate ) %>%
                  arrange(date)
Hospital$UK$saridat <- na.locf(Hospital$UK$saridat)
Hospital$UK$newsaridat <- na.locf(Hospital$UK$newsaridat)
Hospital$UK$critdat <- na.locf(Hospital$UK$critdat)

# Add the Welsh and Northern Ireland cases data,  no age stratifications 
# Problems can occur here if these are updates before/after UK

regcases$Wales <- walesdat$allCases[1:nrow(regcases)]
regcases$NI <- NIdat$allCases[1:nrow(regcases)]
regcases$Wales<-na.locf(regcases$Wales)
regcases$NI<-na.locf(regcases$NI)
#  Welsh data typically a few day late - adjust after the crazy big drop
fixwales=((length(regcases$Wales)-5):length(regcases$Wales))
ffix<-FALSE
for (i in fixwales){
if(regcases$Wales[i]/regcases$Wales[i-1]<0.5){ffix=TRUE}
  if(ffix){regcases$Wales[i]=regcases$Wales[i-1]}
}

#remove random new Scottish "region"
within(regcases, rm("Golden Jubilee National Hospital"))->jnk
regcases<-jnk
# Remove the no longer needed input data  Keep Scottish data for ScottishData.R
rm(ukcasedat,jnk,coltypes,NIdat,walesdat)
rm(HospitalUrl,deathurl,casesurl,walesurl,NIurl,ageurl,baseurl,regurl,regurl2,regurl3,ukcaseurl,vacurl)

# Plot all cases against date: Used for the paper, uncomment to recreate
if(interactive()){
    comdat %>%
    filter(startdate+150 <= date & date <= enddate) %>%
    mutate(rollmean = zoo::rollmean(allCases, k = 7, fill = NA)) %>%  # 7-day average
    ggplot(aes(x=date)) + geom_line(aes(y=allCases)) + geom_point(aes(y=allCases)) +
    geom_line(aes(y=rollmean), colour="pink",na.rm = TRUE, size=2, alpha=0.5) +
    xlab("Date") + ylab("All cases")
}


# Add variant data to comdat  Kentfac tells us how much more lethal the variant is
# Numbers are fitted to death and hospitalisation data
comdat$Kent <- 0.0
comdat$India <- 0.0
comdat$Omicron <- 0.0

Kentdate <- as.integer(as.Date("2021/01/01")-startdate)

# Approximate Kent by logistic rise around 2021/01/01
# Same gen time, R+KEnttrans vs Wild  (transmission, NOT lethality factor)
for (i in 1:(nrow(comdat))){
  x= (i-Kentdate)*Kenttrans/genTime
  comdat$Kent[i]=1.0/(1.0+exp(-x))
}
Indiadate <- as.integer(as.Date("2021/05/15")-startdate)
# Approximate India by logistic rise around 2021/15/01: see covid19.sanger.
# Same genTime R+Indiatrans vs Kent  AY4.2 assumes same as India
for (i in 1:(nrow(comdat))){
  x= (i-Indiadate)*Indiatrans/genTime
  comdat$India[i]=1.0/(1.0+exp(-x))
}
Omicrondate <- as.integer(as.Date("2021/12/15")-startdate)
# Approximate Omicron by logistic rise around 2021/11/28: see covid19.sanger.
# Same genTime R+1 vs India
for (i in 1:(nrow(comdat))){
  x= (i-Omicrondate)*1.0/genTime
  comdat$Omicron[i]=1.0/(1.0+exp(-x))
}

rm(x)
# Kent is Kentfac worse, india is Indiafac worse

comdat$India<-comdat$India - comdat$Omicron
comdat$Kent<-comdat$Kent-comdat$India-comdat$Omicron
comdat$lethality<-1.0+ Kentfac*comdat$Kent + Indiafac*comdat$India + Omicronfac*comdat$Omicron

# Fix missing data to constant values
casedat <- na.locf(casedat)
comdat <- na.locf(comdat)
regcases <- na.locf(regcases)

# Remove weekend effect, assuming each weekday has same number of cases over the
# epidemic, and national averages hold regionally.  Also, smooth data through Xmas.
#  Pulled out comdat & scotdat to a function. Regions need to deal with tibble
comdat$allCases <- Weekend(comdat$allCases)
for (area in 2:length(regcases)){
  regcases[area]<-Weekend(regcases %>% pull(area))
}
for (iage in 2:length(casedat)){
  casedat[iage]<-Weekend(casedat %>% pull(iage))
}

# Build CrystalCast agegroups
xcastage <-casedat %>% select(`00_04`)
xcastage$'05_14' <-casedat$`05_09`+casedat$`10_14`
xcastage$'15_24' <-casedat$`15_19`+casedat$`20_24`
xcastage$'25_44' <-casedat$`25_29`+casedat$`30_34`+casedat$`35_39`+casedat$`40_44`
xcastage$'45_64' <-casedat$`45_49`+casedat$`50_54`+casedat$`55_59`+casedat$`60_64`
xcastage$'65_74' <-casedat$`65_69`+casedat$`70_74`
xcastage$'75+' <-casedat$`75_79`+casedat$`80_84`+casedat$`85_89`+casedat$`90+`

# Combination required to go from 9 to 7 English regions  regcases must be same length in all regions
regcases$NE_Yorks <- regcases$`North East` + regcases$`Yorkshire and The Humber`
regcases$Midlands <- regcases$`East Midlands` + regcases$`West Midlands`

regcases$England <- comdat$allCases[1:nrow(regcases)]

# Reorder regcases
regcases<-regcases[,c(1,2,3,4,5,6,7,9,10,8,23,26,27,11,12,13,14,15,16,17,18,19,20,21,22,24,25,28,29,30)]

# Set false positive adjustment at 0.004, extrapolate tests if the last few days are missing
comdat$fpCases <- comdat$allCases-0.004*as.integer(comdat$tests)

regcases$regions <- regcases$London + regcases$`South East` + regcases$`South West` +
                  regcases$NE_Yorks + regcases$Midlands + regcases$`North West` +
                  regcases$`East of England`
# Plot only if running interactively
if(interactive()){

  plot(comdat$inputCases,x=comdat$date,xlab="Date",ylab="Cases")
  lines(comdat$allCases,x=comdat$date, col="green",lwd=2)
  lines(comdat$fpCases, x=comdat$date,col="red",lwd=2)
  lines(regcases$regions, x=regcases$date,col="blue",lwd=2)

  # Same graph using ggplot - alpha sets a level of transparency
  # between 0 (opaque) to 1 (transparent)
  ggplot(comdat,aes(x=date)) +
    geom_point(aes(y=inputCases),alpha=0.5) +
    geom_line(aes(y=allCases), colour="green", size=1., alpha=0.5) +
    geom_line(aes(y=fpCases),colour="red", size=1., alpha=0.5)  +
    xlab("Dates") + ylab("Cases") +
    theme_bw()
}
##  Case Fatality ratio was determined from initial period 


# Get mean age-related CFR across the whole pandemic, with adjustment for vaccination
# giving people who would have died.  Alternative would be time dependent CFR
# RawCFR=colSums(deathdat[2:20]/(1-vacdat[2:20]*vacCFR))/colSums(casedat[2:ncol(casedat)])



#Add in ONSdata by hand in getParms.  for 12/4 use this to get R
#  Put the ONS prevalence data into the comdat array  
# ONS is delayed in reporting, and averaged over the previous week.
# By peak matching, this can be about 10 days

ons_delay=nrow(comdat)-7

approx(eng_prev,n=7*length(eng_prev))$y%>% tail(ons_delay)-> jnk
jnk[ons_delay:(ons_delay+7)]=jnk[ons_delay]
comdat$Eng_ons_prev<-jnk
approx(scot_prev,n=7*length(scot_prev))$y%>% tail(ons_delay)-> jnk
jnk[ons_delay:(ons_delay+7)]=jnk[ons_delay]
comdat$Scot_ons_prev<-jnk
comdat$Eng_ons_prev[(ons_delay+1):nrow(comdat)]=comdat$Eng_ons_prev[ons_delay]
comdat$Scot_ons_prev[(ons_delay+1):nrow(comdat)]=comdat$Scot_ons_prev[ons_delay]
comdat$Eng_ons_inc[1:(nrow(comdat))]<-0.0
comdat$Scot_ons_inc[1:(nrow(comdat))]<-0.0


#  Factor of 14 days infectious to convert prevalence to incidence.  Also delays it

comdat$Eng_ons_inc[1:(nrow(comdat)-14)]<-comdat$Eng_ons_prev[15:nrow(comdat)]/14
comdat$Scot_ons_inc[1:(nrow(comdat)-14)]<-comdat$Scot_ons_prev[15:nrow(comdat)]/14
#  Extrapolate final 14 days from end of ONS data
xtmp=1:14
Engfit<-lm(comdat$Eng_ons_inc[(nrow(comdat)-27):(nrow(comdat)-14)]~xtmp)
Engslope=Engfit$coefficients[2]
Engstart=comdat$Eng_ons_inc[(nrow(comdat)-14)]
Scotfit<-lm(comdat$Scot_ons_inc[(nrow(comdat)-27):(nrow(comdat)-14)]~xtmp)
Scotslope=Scotfit$coefficients[2]
Scotstart=comdat$Scot_ons_inc[(nrow(comdat)-14)]
for(iday in 1:14){
  comdat$Eng_ons_inc[(nrow(comdat)-14+iday)]=Engstart+iday*Engslope
  comdat$Scot_ons_inc[(nrow(comdat)-14+iday)]=Scotstart+iday*Scotslope
}

comdat$Missing_incidence_Raw=NA
comdat$Scot_Missing_incidence_Raw=NA
comdat$Missing_incidence_Raw[10:(nrow(comdat)-16)]=comdat$Eng_ons_inc[10:(nrow(comdat)-16)]/comdat$allCases[26:nrow(comdat)]
comdat$Missing_incidence_Raw[1:10]=comdat$Missing_incidence_Raw[10]
comdat$Missing_incidence_Raw<-na.locf(comdat$Missing_incidence_Raw, na.rm=FALSE)
comdat$Missing_incidence=smooth.spline((comdat$Missing_incidence_Raw),df=3)$y
comdat$Scot_Missing_incidence_Raw[10:(nrow(regcases)-16)]=comdat$Scot_ons_inc[10:(nrow(regcases)-16)]/regcases$Scotland[26:nrow(regcases)]
comdat$Scot_Missing_incidence_Raw[1:10]=comdat$Scot_Missing_incidence_Raw[10]
comdat$Scot_Missing_incidence_Raw<-na.locf(comdat$Scot_Missing_incidence_Raw, na.rm=FALSE)
comdat$Scot_Missing_incidence=smooth.spline((comdat$Scot_Missing_incidence_Raw),df=3)$y



# Scottish regions --------------------------------------------------------

# List from https://en.wikipedia.org/wiki/Local_government_in_Scotland
# NHS Western Isles = Bòrd SSN nan Eilean Siar
ScottishAuthorities <- c("Inverclyde","Renfrewshire","West Dunbartonshire",
                         "East Dunbartonshire","Glasgow","East Renfrewshire","North Lanarkshire",
                         "Falkirk","West Lothian","Edinburgh","Midlothian","East Lothian",
                         "Clackmannanshire","Fife","Dundee", "Angus","Aberdeenshire",
                         "Aberdeen","Moray","Highland","Na h-Eileanan Siar","Argyll and Bute",
                         "Perth and Kinross","Stirling","North Ayrshire",
                         "East Ayrshire","South Ayrshire","Dumfries and Galloway",
                         "South Lanarkshire","Scottish Borders","Orkney",
                         "Shetland")

# NHS trust do not exist in Scotland. There are 14 NHS boards in Scotland.
ScotNHSBoards <- c("NHS Ayrshire and Arran", "NHS Borders",
                   "NHS Dumfries and Galloway",
                   "NHS Fife", "NHS Forth Valley", "NHS Grampian",
                   "NHS Greater Glasgow and Clyde",
                   "NHS Highland", "NHS Lanarkshire", "NHS Lothian",
                   "NHS Orkney", "NHS Shetland",
                   "NHS Tayside", "NHS Western Isles")

# Lookup table to map Scottish councils to Scottish NHS boards
# Data derived from https://en.wikipedia.org/wiki/NHS_Scotland
BoardsToCouncils <- list(
  "NHS Ayrshire and Arran"=c("East Ayrshire", "North Ayrshire", "South Ayrshire"),
  "NHS Borders"=c("Scottish Borders"),
  "NHS Dumfries and Galloway"=c("	Dumfries and Galloway"),
  "NHS Fife"=c("Fife"),
  "NHS Forth Valley"=c("Clackmannanshire", "Falkirk", "Stirling"),
  "NHS Grampian"=c("Aberdeenshire", "City of Aberdeen", "Moray"),
  "NHS Greater Glasgow and Clyde"=c("City of Glasgow", "East Dunbartonshire",
                                    "East Renfrewshire", "Inverclyde",
                                    "Renfrewshire", "West Dunbartonshire"),
  "NHS Highland"=c("Highland", "Argyll and Bute"),
  "NHS Lanarkshire"=c("North Lanarkshire", "South Lanarkshire"),
  "NHS Lothian"=c("City of Edinburgh", "East Lothian", "Midlothian",
                  "West Lothian"),
  "NHS Orkney"=c("Orkney Islands"),
  "NHS Shetland"=c("Shetland Islands"),
  "NHS Tayside"=c("Angus", "City of Dundee", "Perth and Kinross"),
  "NHS Western Isles"=c("Outer Hebrides")
  
)


# Getting data ------------------------------------------------------------

# Start and end date - the data to collect data from
#  startdate and enddate inherited from a previous run of covid_trimmed
# WAS To one week ago (-7)  NOW read in all the data



# Base url for UK data
baseurl <- "https://api.coronavirus.data.gov.uk/v2/data?"

# Compose URL to download by upper local tier authority
# Getting:
#          newDeaths28DaysByDeathDate
#          newCasesBySpecimenDat
#
regurl <- paste0(baseurl,
                 "areaType=utla&",
                 "metric=newDeaths28DaysByDeathDate&",
                 "metric=newCasesBySpecimenDate&",
                 "format=csv")

# Column types
cols <-  cols(
  areaCode = col_character(),
  areaName = col_character(),
  areaType = col_character(),
  date = col_date(format = "%Y-%m-%d"),
  newCasesBySpecimenDate = col_double(),
  newDeaths28DaysByDeathDate = col_double()
)

# Get the data
alldat <- read_csv(regurl,col_types = cols)

# Keep only the Scottish data by council authority
ScotCasesDeaths <- alldat[alldat$areaName %in% ScottishAuthorities,]

# Get rid of all the data (quite sizeable)
rm(alldat)

# Get cases by age by tier authority
ageurl <- paste0(baseurl,
                 "areaType=utla&",
                 "metric=newCasesBySpecimenDateAgeDemographics&",
                 "format=csv")

# Column types
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

# Get all the data - ALL English data - no Scottish regions, just Authorities
allagedat <- read_csv(ageurl, col_types = coltypes)

# Restrict to Scottish regions and change from long to wide format
allagedat %>% filter(areaName %in% ScottishAuthorities) %>%  # Keep only Scottish regions
  select(date = date, age = age, values = cases, area = areaName) %>%
  pivot_wider(names_from = age, values_from = values ) %>%
  filter(date >= startdate & date <= enddate) %>%
  arrange(date) -> scotagedat

# Getting data by NHS region but this only supports English NHS regions
trust <- paste0(baseurl,
                "areaType=nhsRegion&",
                "metric=transmissionRateMin&",
                "metric=transmissionRateMaxe&",
                "format=csv")

# Define the columns
coltypes <- cols(
  areaCode = col_character(),
  areaName = col_character(),
  areaType = col_character(),
  date = col_date(format = "%Y-%m-%d"),
  transmissionRateMin = col_double()
)

# Read the data
nhsregion <- read_csv(trust, col_types = coltypes)


# Scottish Health and Social Care Open Data -------------------------------
# Covid related data:
#
#  https://www.opendata.nhs.scot/dataset/covid-19-in-scotland
#

# Total Cases By Health Board
# See https://www.opendata.nhs.scot/dataset/covid-19-in-scotland/resource/7fad90e5-6f19-455b-bc07-694a22f8d5dc

# URL from which to pull the data
scoturl <- "https://www.opendata.nhs.scot/dataset/b318bddf-a4dc-4262-971f-0ba329e09b87/resource/7fad90e5-6f19-455b-bc07-694a22f8d5dc/download/total_cases_by_hb_20210610.csv"

# Define the columns Date	HB	HBQF	HBName	NewPositive	TotalCases	CrudeRatePositive	NewDeaths	TotalDeaths
# CrudeRateDeaths	TotalPCROnly	NewPCROnly	TotalLFDOnly	NewLFDOnly	TotalLFDAndPCR	NewLFDAndPCR

coltypes <- cols(
  Date = col_date(format = "%Y%m%d"),
  HB = col_character(),
  HBQF = col_character(),
  HBName = col_character(),
  NewPositive = col_double(),
  TotalCases = col_double(),
  CrudeRatePositive = col_double(),
  TotalPCROnly = col_double(),
  TotalLFDOnly = col_double(),
  TotalLFDAndPCR = col_double()
)

# Get the data
scotdat <- read_csv(scoturl,col_types = coltypes)

# Scottish Daily Case Trends By Health Board
#
# See: https://www.opendata.nhs.scot/dataset/covid-19-in-scotland/resource/2dd8534b-0a6f-4744-9253-9565d62f96c2
#

# URL from which to pull the data
dailycasesurl = "https://www.opendata.nhs.scot/dataset/b318bddf-a4dc-4262-971f-0ba329e09b87/resource/2dd8534b-0a6f-4744-9253-9565d62f96c2/download/trend_hb_20210610.csv"

# Column types
coltypes <- cols(
  Date = col_date(format = "%Y%m%d"),
  HB = col_character(),
  HBName = col_character(),
  CumulativePositive = col_double(),
  CrudeRatePositive = col_double(),
  CrudeRate7DayPositive = col_double(),
  DailyDeaths = col_double(),
  CumulativeDeaths = col_double(),
  CrudeRateDeaths = col_double(),
  DailyNegative = col_double(),
  CumulativeNegative = col_double(),
  CrudeRateNegative = col_double(),
  TotalTests = col_double(),
  PositiveTests = col_double(),
  PositivePercentage = col_double(),
  PositivePercentage7Day = col_double(),
  TotalPillar1 = col_double(),
  TotalPillar2 = col_double(),
  HospitalAdmissions = col_double(),
  HospitalAdmissionsQF = col_character(),
  ICUAdmissions = col_double(),
  ICUAdmissionsQF = col_character(),
  PositivePillar1 = col_double(),
  PositivePillar2 = col_double()
)


# Make the NHS boards the columns - DailyPositives are the values
scotdailycases %>% select(date=Date,board=HBName, cases=DailyPositive)  %>%
  pivot_wider(names_from = board, values_from = cases) %>%
  filter(date >= startdate & date <= enddate )         %>%
  arrange(date) -> scotdailycasesbyboard

# Hospital data
scotdailycases %>% filter(HBName=="Scotland") %>%
  select(date = Date, newcritdat = ICUAdmissions, newsaridat = HospitalAdmissions) %>%
  filter(date >= startdate & date <= enddate) %>%
  arrange(date) -> jnk
Hospital$Scot$date<-as.Date(jnk$date)
Hospital$Scot$newcritdat<-jnk$newcritdat
Hospital$Scot$newsaridat<-jnk$newsaridat


# Daily Case Trends By Age and Sex
# See:
# https://www.opendata.nhs.scot/dataset/covid-19-in-scotland/resource/9393bd66-5012-4f01-9bc5-e7a10accacf4

# Data URL
ageurl <- "https://www.opendata.nhs.scot/dataset/b318bddf-a4dc-4262-971f-0ba329e09b87/resource/9393bd66-5012-4f01-9bc5-e7a10accacf4/download/trend_agesex_20210610.csv"

# Define the column typesDate	HB	HBQF	HBName	NewPositive	TotalCases	CrudeRatePositive	NewDeaths	TotalDeaths	CrudeRateDeaths	TotalPCROnly	NewPCROnly	TotalLFDOnly	NewLFDOnly	TotalLFDAndPCR	NewLFDAndPCR

coltypes <- cols(
  Date = col_date(format = "%Y%m%d"),
  Country = col_character(),
  Sex = col_character(),
  SexQF = col_character(),
  AgeGroup = col_character(),
  AgeGroupQF = col_character(),
  DailyPositive = col_double(),
  CumulativePositive = col_double(),
  DailyDeaths = col_double(),
  CumulativeDeaths = col_double(),
  CrudeRateDeaths = col_double()
)

# Get the data
scotagedat <- read_csv(ageurl, col_types = coltypes)


#Read in from  file since Scotland stopped publishing death data on June 2nd.  
#This may have to be updated by hand from NRS data starting at row 23 (June 6th)
# opendatascot API doesnt work, but they have a workaround.
#nrsdeath=read.csv("NRS_agedeath2022.csv")

#Scottish deaths from ods
opendatascot::ods_dataset("deaths-involving-coronavirus-covid-19",locationOfDeath="all",sex="all",causeOfDeath="covid-19-related",refArea="S92000003")->jnk 
jnk<- jnk %>% filter(refPeriod!="2022") %>% filter(refPeriod!="2021")%>% filter(refPeriod!="2020")
jnk$refPeriod<-as.Date(substr(jnk$refPeriod,5,nchar(jnk$refPeriod)))
jnk %>% filter(age=="all")%>% select(refPeriod,value)-> jnkall
jnk %>% filter(age=="1-14-years")%>% select(refPeriod,value)%>% filter(value<2000)-> jnk14
jnk %>% filter(age=="15-44-years")%>% select(refPeriod,value)%>% filter(value<2000)-> jnk44
jnk %>% filter(age=="45-64-years")%>% select(refPeriod,value)%>% filter(value<2000)-> jnk64
jnk %>% filter(age=="65-74-years")%>% select(refPeriod,value)%>% filter(value<2000)-> jnk74
jnk %>% filter(age=="75-84-years")%>% select(refPeriod,value)%>% filter(value<2000)-> jnk84
jnk %>% filter(age=="85-years-and-over")%>% select(refPeriod,value)%>% filter(value<2000)-> jnk100
#  odsdf should be readin to give odsdeath1 same as nrsdeath1  intention is that nrsdeath reading becomes obsolete
odsdf<-data.frame(jnkall$refPeriod,jnkall,jnkall,jnk14$value,jnk44$value,jnk64$value,jnk74$value,jnk84$value,jnk100$value)
# 
#nrsdeath1<-nrsdeath[22:nrow(nrsdeath),]
odsdeath1<-odsdf[117:nrow(odsdf),]
odsdeath1$jnkall.refPeriod<-"2022"
nrsdeath1<-odsdeath1
nrsdeathday <- nrsdeath1[rep(seq_len(nrow(nrsdeath1)), each=7),]
nrsdeathday[5:11] = nrsdeathday[5:11]/7.0


# Set up
ckanr_setup(url = "https://www.opendata.nhs.scot/")

# List data sets
#package_list(as="table")

tags <- tag_list(as="table")
#  Filter out the relevant columns  (Changed datastream from 10/02/2022)
corona <- tag_show("coronavirus", as = "table")
scotagedat[,c(1,3,5,7,8, 9,10)] %>% filter(Sex=="Total") %>% filter(Date>=casedat$date[1]) %>% filter(Date<=casedat$date[nrow(casedat)])->jnk 



#  Scottish data is in broader age groups.  To be compatible with the code,
#  subdivide it  into 5 year bands.  Use the UK casedat & deathdat to set 
#  up correct array sizes, and the demographics



scotage <-casedat
scotdeath <-deathdat

#  Fractions in each 5 year age group same as in England (casedat)
sum24=sum(casedat[2:4])
jnk %>% filter(AgeGroup == "0 to 14") -> jnk2
jnk2$DailyPositive<-Weekend(jnk2$DailyPositive)
jnkna=length(jnk2$DailyDeaths[(jnk2$Date>"2022-06-01")])
jnk2$DailyDeaths[(jnk2$Date>"2022-06-01")] <-nrsdeathday[1:jnkna,6]
scotage$'05_09' <-jnk2$DailyPositive*sum(casedat$`05_09`)/sum24
scotage$`00_04` <- jnk2$DailyPositive*sum(casedat$`00_04`)/sum24
scotage$`10_14` <- jnk2$DailyPositive*sum(casedat$`10_14`)/sum24
scotdeath$'05_09' <-jnk2$DailyDeaths *sum(casedat$`05_09`)/sum24
scotdeath$`00_04` <- jnk2$DailyDeaths*sum(casedat$`00_04`)/sum24
scotdeath$`10_14` <- jnk2$DailyDeaths*sum(casedat$`10_14`)/sum24

jnk %>% filter(AgeGroup == "15 to 19") -> jnk2
jnk2$DailyDeaths[(jnk2$Date>"2022-06-01")] <-nrsdeathday[1:jnkna,7]
scotage$`15_19` <- Weekend(jnk2$DailyPositive)
scotdeath$`15_19` <- jnk2$DailyDeaths*sum(casedat$`15_19`)/sum24

jnk %>% filter(AgeGroup == "20 to 24") -> jnk2
jnk2$DailyDeaths[(jnk2$Date>"2022-06-01")] <-nrsdeathday[1:jnkna,7]
scotage$`20_24` <- Weekend(jnk2$DailyPositive)
scotdeath$`20_24` <- jnk2$DailyDeaths*sum(casedat$`20_24`)/sum24

jnk %>% filter(AgeGroup == "25 to 44") -> jnk2
jnk2$DailyDeaths[(jnk2$Date>"2022-06-01")] <-nrsdeathday[1:jnkna,7]
temp <- Weekend(jnk2$DailyPositive)
sum24=sum(casedat[7:10])
scotage$`25_29` <- temp*sum(casedat$`25_29`)/sum24
scotage$`30_34` <- temp*sum(casedat$`30_34`)/sum24
scotage$`35_39` <- temp*sum(casedat$`35_39`)/sum24
scotage$`40_44` <- temp*sum(casedat$`40_44`)/sum24
scotdeath$`25_29` <- jnk2$DailyDeaths*sum(casedat$`25_29`)/sum24
scotdeath$`30_34` <- jnk2$DailyDeaths*sum(casedat$`30_34`)/sum24
scotdeath$`35_39` <- jnk2$DailyDeaths*sum(casedat$`35_39`)/sum24
scotdeath$`40_44` <- jnk2$DailyDeaths*sum(casedat$`40_44`)/sum24
jnk %>% filter(AgeGroup == "45 to 64") -> jnk2
jnk2$DailyDeaths[(jnk2$Date>"2022-06-01")] <-nrsdeathday[1:jnkna,8]

temp <- Weekend(jnk2$DailyPositive)
sum24=sum(casedat[11:14])

scotage$`45_49` <- temp*sum(casedat$`45_49`)/sum24
scotage$`50_54` <- temp*sum(casedat$`50_54`)/sum24
scotage$`55_59` <- temp*sum(casedat$`55_59`)/sum24
scotage$`60_64` <- temp*sum(casedat$`60_64`)/sum24

scotdeath$`45_49` <- jnk2$DailyDeaths*sum(casedat$`45_49`)/sum24
scotdeath$`50_54` <- jnk2$DailyDeaths*sum(casedat$`50_54`)/sum24
scotdeath$`55_59` <- jnk2$DailyDeaths*sum(casedat$`55_59`)/sum24
scotdeath$`60_64` <- jnk2$DailyDeaths*sum(casedat$`60_64`)/sum24

# so few deaths in younger groups, use case numbers as proxy. For over 65 use actual deaths
jnk %>% filter(AgeGroup == "65 to 74") -> jnk2
jnk2$DailyDeaths[(jnk2$Date>"2022-06-01")] <-nrsdeathday[1:jnkna,9]
sum24=sum(casedat[15:16])
sumRIP=sum(deathdat[15:16])
temp <- Weekend(jnk2$DailyPositive)
scotage$`65_69` <- temp*sum(casedat$`65_69`)/sum24
scotage$`70_74` <- temp*sum(casedat$`70_74`)/sum24
scotdeath$`65_69` <- jnk2$DailyDeaths*sum(deathdat$`65_69`)/sumRIP
scotdeath$`70_74` <- jnk2$DailyDeaths*sum(deathdat$`70_74`)/sumRIP
jnk %>% filter(AgeGroup == "75 to 84") -> jnk2
jnk2$DailyDeaths[(jnk2$Date>"2022-06-01")] <-nrsdeathday[1:jnkna,10]
temp <- Weekend(jnk2$DailyPositive)
sum24=sum(casedat[17:18])
sumRIP=sum(deathdat[17:18])
scotage$`75_79` <- temp*sum(casedat$`75_79`)/sum24
scotage$`80_84` <- temp*sum(casedat$`80_84`)/sum24
scotdeath$`75_79` <- jnk2$DailyDeaths*sum(deathdat$`75_79`)/sumRIP
scotdeath$`80_84` <- jnk2$DailyDeaths*sum(deathdat$`80_84`)/sumRIP
jnk %>% filter(AgeGroup == "85plus") -> jnk2
jnk2$DailyDeaths[(jnk2$Date>"2022-06-01")] <-nrsdeathday[1:jnkna,11]
sum24=sum(casedat[19:20])
sumRIP=sum(deathdat[19:20])
temp <- Weekend(jnk2$DailyPositive)
scotage$`85_89` <- temp*sum(casedat$`85_89`)/sum24
scotage$`90+` <- temp*sum(casedat$`90+`)/sum24
scotdeath$`85_89` <- jnk2$DailyDeaths*sum(deathdat$`85_89`)/sumRIP
scotdeath$`90+` <- jnk2$DailyDeaths*sum(deathdat$`90+`)/sumRIP
rm(sum24,sumRIP,jnk,jnk2,temp)


  #  https://www.imperial.ac.uk/mrc-global-infectious-disease-analysis/covid-19/report-41-rtm/
  # PropSARI taken from Knock et al to increase smoothly with age
  # Over 80 adjusted to fit national death reports
  # CFR_SARI cut c.f covidsim for intermediate ages because more serious cases go via CRIT
  covidsimAge<-data.frame(
    "Prop_ILI_ByAge"=c(
      0.333122437,  0.333153617,  0.333001453, 0.332654731, 0.33181821, 0.330417289, 0.328732618, 0.326716425, 0.325130732, 0.322392505, 0.316971878, 0.312809664, 0.304540269, 0.300182488, 0.2919304, 0.283276936, 0.282323232, 0.282323232, 0.282323232
    ),
    "Prop_SARI_ByAge"=c( 0.000557744, 0.000475283, 0.000877703, 0.001794658, 0.004006955, 0.007711884,
                         0.012167229, 0.017359248, 0.021140307, 0.027047193, 0.03708932, 0.039871236, 0.020788928,
                         0.017444452, 0.101605674, 0.142001415, 0.1747, 0.21, 0.25  ),
    # TEST"Prop_SARI_ByAge"=c( 0.0008, 0.000475283, 0.000477703, 0.001794658, 0.004006955, 0.007711884,
    #                      0.012167229, 0.017359248, 0.021140307, 0.027047193, 0.03, 0.035, 0.06,
    #                      0.08, 0.101605674, 0.142001415, 0.1747, 0.21, 0.25  ),
  #  "Prop_Critical_ByAge"=
  #    c(7.49444E-05, 6.38641E-05, 0.000117937, 0.000241149, 0.000538417, 0.00103625, 0.001634918, 0.002491477, 0.003467496, 0.005775292, 0.011995047, 0.021699771, 0.065590266, 0.082008084, 0.022603126, 0.008167778, 0.002560606, 0.002560606, 0.002560606
  #    ),
  #  "CFR_Critical_ByAge"=c(
  #    0.5234896, 0.5234896, 0.5234896, 0.5234896, 0.5234896, 0.5234896, 0.5234896, 0.5234896, 0.5234896, 0.5234896, 0.5234896, 0.5234896, 0.5234896, 0.5234896, 0.5234896, 0.5234896, 0.5234896, 0.5234896, 0.5234896
  #  ),
    # This was way out of line with the 20% https://journals.lww.com/ccmjournal/Fulltext/2021/02000/Improving_Survival_of_Critical_Care_Patients_With.5.aspx  https://ebn.bmj.com/content/early/2021/05/09/ebnurs-2020-103370
    #  52% is pre dexamethasone  from 14/11/22 eliminate CRIT route altogether
    "Prop_Critical_ByAge"=c(
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0),

    "CFR_Critical_ByAge"=c(
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0),
    
    
    "CFR_SARI_ByAge"=c(0.125893251, 0.12261338, 0.135672867, 0.152667869, 0.174303077, 0.194187895,
                       0.209361731, 0.224432564, 0.237013516, 0.125, 0.125, 0.125, 0.125, 0.1257277, 0.37110474,  0.421151485, 0.5782234,  0.6455841,  0.6930401
    ),
    "CFR_ILI_ByAge"=c(
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0),
    "Prop_Hosp_ByAge"=c(0.03, 0.0026 ,  0.00084 , 0.00042 ,0.00080, 0.0026, 0.0040 , 0.0063 , 0.012,  0.019,  0.023,  0.040,  0.096,  0.10,  0.24 ,  0.50, 0.6, 0.7,0.8),
    "Case_Hosp_ByAge"=c( 0.039,  0.001,  0.006,  0.009,  0.026 , 0.040,  0.042  ,0.045,  0.050,  0.074,  0.138,  0.198,  0.247,  0.414,  0.638,  
                         1.000,1.00 ,1.00 ,1.00) )
  # Admissions to April 30 0-5 839 6-17 831 18-65 42019 65-84 42640 85+ 20063
  # https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-hospital-activity/
  # TEST Adjust SARI to relate to actual admissions
  # covidsimAge$Prop_SARI_ByAge<-covidsimAge$Prop_Critical_ByAge*covidsimAge$Prop_Hosp_ByAge
  # Deatherror from colSums(deathdat[2:20])/colSums(casedat[2:20])/(colSums(DEATH[2:20]/colSums(newMILD[2:20]+newILI[2:20])))
  # IHR from Knock SM S9  CHR from Knock S8
  covidsimAge$Prop_Mild_ByAge= 1.0 - (covidsimAge$Prop_Critical_ByAge+covidsimAge$Prop_ILI_ByAge+covidsimAge$Prop_SARI_ByAge)

  
# UK population by age https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland
  popdat<-c(67081234,3782330,4147413,4045114,3683680,4133158,4476630,4521975,4404100,4091543,4303967,4616017,4510851,3855818,3355381,3363906,2403759,1726223,1049866,609503)
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
    "Lon"=c(9095459,582490,605204,561365,
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
                 400834,360684,305248,289590,204947,143858,86413,45018),
    "Wales"=c(3174970, 160688, 180503, 187819, 173842, 200210,
              202513, 201034, 186338, 177662,183427, 215927,223724,
              202578, 180391, 183716, 135819, 91798,55843, 31138 ),
    "NI"=c(1910623,116146,127557,129856,114652,111442,118998
           ,126555,125362,120465,120391,130049,129139
           ,112714,92622,82889,67237,44075,26191,14283)
  )
  population$MD<-population$EM+population$WM
  population$NEY<-population$Yorks+population$NE
  population$UK<-popdat
 


