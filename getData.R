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
# distributions and populations
covidsimAge<-covidSimData()
population<-getPop()
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
                             MVBeds=covidOccupiedMVBeds)%>%
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
regcases <- inner_join(regcases, scotdailycasesbyboard, by = c("date" = "date"))

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

comdat$Missing_incidence=smooth.spline((comdat$Eng_ons_inc/comdat$allCases),df=6)$y
comdat$Scot_Missing_incidence=smooth.spline((comdat$Scot_ons_inc[1:nrow(regcases)]/regcases$Scotland),df=6)$y


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
nrsdeath=read.csv("NRS_agedeath2022.csv")
nrsdeath1<-nrsdeath[22:nrow(nrsdeath),]
nrsdeathday <- nrsdeath1[rep(seq_len(nrow(nrsdeath1)), each=7),]
nrsdeathday[5:11] = nrsdeathday[5:11]/7.0


# install and load the package
# install.packages("ckanr")
library(ckanr)

# Set up
ckanr_setup(url = "https://www.opendata.nhs.scot/")

# List data sets
package_list(as="table")

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



