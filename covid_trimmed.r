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

# Remove existing variables if in an interactive session.
if(interactive()){
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


source("CompartmentFunction.R")
source("medrxiv.R")

# Set the working directory to be the same as to where the script is run from.
setwd(".")

# Turn off scientific notation.
options(scipen = 999)

# Copy transition rates from covidsim.  There are three different functions for
# ICDFs (Inverse Cumulative Distribution Function).  x-axis divided into 20 blocks of 5%.
# Will need to invert this.  Recent versions include a "Stepdown" state which seems to entail dying in CRITREC
#  https://www.imperial.ac.uk/mrc-global-infectious-disease-analysis/covid-19/report-41-rtm/
# PropSARI taken from Knock et al to increase smoothly with age
# Over 80 adjusted to fit national death reports
# CFR_SARI cut c.f covidsim for intermediate ages because more serious cases go via CRIT
# UK population by age https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland
popdat<-c(3782330,4147413,4045114,3683680,4133158,4476630,4521975,4404100,4091543,4303967,4616017,4510851,3855818,3355381,3363906,2403759,1726223,1049866,609503)
covidsimAge<-data.frame(
  "Prop_ILI_ByAge"=c(
    0.333122437,  0.333153617,	0.333001453, 0.332654731, 0.33181821, 0.330417289, 0.328732618, 0.326716425, 0.325130732, 0.322392505, 0.316971878, 0.312809664, 0.304540269, 0.300182488, 0.2919304, 0.283276936, 0.282323232, 0.282323232, 0.282323232
  ),
  "Prop_SARI_ByAge"=c(
    0.000557744, 0.000475283, 0.000877703, 0.001794658, 0.004006955, 0.007711884, 0.012167229, 0.017359248, 0.021140307, 0.027047193, 0.03708932, 0.039871236, 0.020788928, 0.017444452, 0.101605674, 0.142001415, 0.1747, 0.21, 0.25
  ),
  "Prop_Critical_ByAge"=
    c(7.49444E-05, 6.38641E-05, 0.000117937, 0.000241149, 0.000538417, 0.00103625, 0.001634918, 0.002491477, 0.003467496, 0.005775292, 0.011995047, 0.021699771, 0.065590266, 0.082008084, 0.022603126, 0.008167778, 0.002560606, 0.002560606, 0.002560606
    ),
  "CFR_Critical_ByAge"=c(
    0.5234896, 0.5234896, 0.5234896, 0.5234896, 0.5234896, 0.5234896, 0.5234896, 0.5234896, 0.5234896, 0.5234896, 0.5234896, 0.5234896, 0.5234896, 0.5234896, 0.5234896, 0.5234896, 0.5234896, 0.5234896, 0.5234896
  ),
  "CFR_SARI_ByAge"=c(
    0.125893251, 0.12261338, 0.135672867, 0.152667869, 0.174303077, 0.194187895, 0.209361731, 0.224432564, 0.237013516, 0.125, 0.125, 0.125, 0.125, 0.1257277, 0.37110474,  0.421151485, 0.5782234,  0.6455841,  0.6930401
  ),
  "CFR_ILI_ByAge"=c(
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0),
  "Prop_Hosp_ByAge"=c(0.03, 0.0026 ,  0.00084 , 0.00042 ,0.00080, 0.0026, 0.0040 , 0.0063 , 0.012,  0.019,  0.023,  0.040,  0.096,  0.10,  0.24 ,  0.50, 0.6, 0.7,0.8),
  "Case_Hosp_ByAge"=c( 0.039,  0.001,  0.006,  0.009,  0.026 , 0.040,  0.042  ,0.045,  0.050,  0.074,  0.138,  0.198,  0.247,  0.414,  0.638,  1.000,1.00 ,1.00 ,1.00),
"Deatherror"=c(0.32060231, 0.17841065, 0.05670156, 0.02800124, 0.01342003, 0.01179716, 0.01460613, 0.01983603, 0.02779927, 0.08124622, 0.09198597,
   0.15295026, 0.22286942, 1.13541013, 1.12529118, 1.91515160, 1.97455542, 2.15335157, 2.23153492 )
  )
# Admissions to April 30 0-5 839 6-17 831 18-65 42019 65-84 42640 85+ 20063
# https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-hospital-activity/


# Deatherror from colSums(deathdat[2:20])/colSums(casedat[2:20])/(colSums(DEATH[2:20]/colSums(newMILD[2:20]+newILI[2:20])))
# IHR from Knock SM S9  CHR from Knock S8
covidsimAge$Prop_Mild_ByAge= 1.0 - (covidsimAge$Prop_Critical_ByAge+covidsimAge$Prop_ILI_ByAge+covidsimAge$Prop_SARI_ByAge)
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
  )
  )
## covidsim has 17 agegroups.  We need 19, assume same params for 85_89 & 90+ as for 80+
# Data from Knock et al  case-> Hosp Triage -> ICU DEath|Hosp Death |ICU Death in Stepdown
Knock<-t(data.frame(
"00_04" = c( 0.039, 0.243, 0.039, 0.282, 0.091, 0),
"05_09" = c( 0.001, 0.289, 0.037, 0.286, 0.083, 0),
"10_14" = c( 0.006, 0.338, 0.035, 0.291, 0.077, 0),
"15_19"= c( 0.009, 0.389, 0.035, 0.299, 0.074, 0),
"20_24"= c( 0.026, 0.443, 0.036, 0.310, 0.074, 0),
"25_29" = c(0.040, 0.503, 0.039, 0.328, 0.076, 0),
"30_34"= c( 0.042, 0.570, 0.045, 0.353, 0.080, 0),
"35_39" = c(0.045, 0.653, 0.055, 0.390, 0.086, 0),
"40_44"= c( 0.050, 0.756, 0.074, 0.446, 0.093, 0),
"45_49" = c(0.074, 0.866, 0.107, 0.520, 0.102, 0),
"50_54" = c(0.138, 0.954, 0.157, 0.604, 0.117, 0),
"55_59" = c(0.198, 1.000, 0.238, 0.705, 0.148, 0),
"60_64" = c(0.247, 0.972, 0.353, 0.806, 0.211, 0),
"65_69" = c(0.414, 0.854, 0.502, 0.899, 0.332, 0),
"70_74" = c(0.638, 0.645, 0.675, 0.969, 0.526, 0),
"75_79" = c(1.000, 0.402, 0.832, 1.000, 0.753, 0),
"80_84" =c( 0.873, 0.107, 1.000, 0.918, 1.000, 0),
"85_89" =c( 0.873, 0.107, 1.000, 0.918, 1.000, 0),
"90+" =c( 0.873, 0.107, 1.000, 0.918, 1.000, 0)
)
)

#### Read data ####
# Base URL to get the UK government data
baseurl <- "https://api.coronavirus.data.gov.uk/v2/data?"

# Start and end date - the date to collect data from
startdate <- as.Date("2020/08/09") #as.Date("2020/08/09")

# Lose only the last day of data - use tail correction for reporting delay
# Weekend data can be sketchy Extend the enddate if run on Monday morning
enddate <-  Sys.Date()-5
# Set the generation time
genTime <- 5
#  Dates for the plots
plotdate <- as.Date(c("2020-09-22",as.character(enddate)))
# Date of Christmas Eve
XMas <- as.Date("2020/12/24")
XMstart <- as.integer(XMas-startdate)
XMdays <- 12
XMend <- XMstart+11
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

# Total cases, deaths, tests
casesurl <- paste0(baseurl,
                   "areaType=nation&",
                   "areaCode=E92000001&",
                   "metric=newCasesBySpecimenDate&",
                   "metric=newDeaths28DaysByDeathDate&",
                   "metric=newPCRTestsByPublishDate&",
                   "metric=newPeopleVaccinatedFirstDoseByVaccinationDate&",
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
                             vaccines=newPeopleVaccinatedFirstDoseByVaccinationDate)%>%
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

# Scotland data https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=S92000003&metric=newCasesBySpecimenDate&metric=newDeaths28DaysByDeathDate&metric=newDeaths28DaysByPublishDate&format=csv
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
  newCasesBySpecimenDate = col_number(),
  newDeaths28DaysByPublishDate = col_number(),
  newDeaths28DaysByDeathDate = col_number()
)
#  Trying and failing to get data from PHS
#scotdeaths<- read.csv(file="https://www.opendata.nhs.scot/api/3/action/datastore_search?resource_id=9393bd66-5012-4f01-9bc5-e7a10accacf4")
#
# Data is not being returned as a CSV but JSON you have to use:
#
# library(jsonlite)
# d <- jsonlite::fromJSON("https://www.opendata.nhs.scot/api/3/action/datastore_search?resource_id=9393bd66-5012-4f01-9bc5-e7a10accacf4",
#                         flatten = TRUE)
#
# This still returns contents as a list so you will have to rummage around to extract the actual contents that you require in
# the data structure returned.


# Read in the Scottish deaths and case data
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


# Read in the Welsh deaths and case data
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

# Get the demographic data for regions because can't download simultaneously with
# the death data.
regurl2 <- paste0(baseurl,
                  "areaType=region&",
                  "metric=newCasesBySpecimenDateAgeDemographics&",
                  "metric=newDeathsBySpecimenDateAgeDemographics&",
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

# Read in the regional case and death data by age
regagedat <-  read_csv(file = regurl2, col_types = coltypes)

# Transform the data - reduce the number of columns and filter the data to
# lie between specific dates.
regagedat <- regagedat %>%
             select(date, areaName, age, cases) %>%
             filter(date >= startdate &
                    date <= enddate ) %>%
             arrange(date)

# Define the columns for the UK government R estimate data from a csv file
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
  DailyPositive = col_double(),
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

# Get the Scottish daily cases by health board data
scotdailycases = read_csv(dailycasesurl, col_types = coltypes)

# Make the NHS boards the columns - DailyPositives are the values
scotdailycasesbyboard <- scotdailycases                                %>%
                         select(date=Date,board = HBName,
                                cases = DailyPositive)                 %>%
                         pivot_wider(names_from = board,
                                     values_from = cases)              %>%
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
d <- read_csv(HospitalUrl, col_types = coltypes)

HospitalData <- tibble()
HospitalData <- rev( bind_rows(HospitalData,d) )

HospitalData  <-  HospitalData %>%
                  filter(date >= startdate & date <= enddate ) %>%
                  arrange(date)

# Remove the no longer needed input data
rm(ukcasedat,scotdailycases,scotdailycasesbyboard,d,coltypes)
rm(HospitalUrl,deathurl,casesurl,scoturl,walesurl,NIurl,ageurl,baseurl,regurl,regurl2,ukcaseurl,vacurl)

# Plot all cases against date: Used for the paper, uncomment to recreate
if(interactive()){
    comdat %>%
    filter(startdate+150 <= date & date <= enddate) %>%
    mutate(rollmean = zoo::rollmean(allCases, k = 7, fill = NA)) %>%  # 7-day average
    ggplot(aes(x=date)) + geom_line(aes(y=allCases)) + geom_point(aes(y=allCases)) +
    geom_line(aes(y=rollmean), colour="pink",na.rm = TRUE, size=2, alpha=0.5) +
    xlab("Date") + ylab("All cases")
}

# Add the Welsh and Northern Ireland cases data
regcases$Wales <- walesdat$allCases
regcases$NI <- NIdat$allCases

# Tail correction.  Assumes we read in all but the last row
if(enddate == (Sys.Date()-1)){
  scotdat$allCases[nrow(scotdat)]=scotdat$allCases[nrow(scotdat)]*1.05
  scotdat$allCases[nrow(scotdat)-1]=scotdat$allCases[nrow(scotdat)-1]*1.005
  regcases[nrow(regcases),2:ncol(regcases)]=regcases[nrow(regcases),2:ncol(regcases)]*1.05
  regcases[nrow(regcases-1),2:ncol(regcases)]=regcases[nrow(regcases-1),2:ncol(regcases)]*1.005
  regcases[nrow(regcases),2:ncol(regcases)]=regcases[nrow(regcases),2:ncol(regcases)]*1.05
  regcases[nrow(regcases-1),2:ncol(regcases)]=regcases[nrow(regcases-1),2:ncol(regcases)]*1.005
}

# Add variant data to comdat  Kentfac tells us how much more virulent the variant is
# Numbers are fitted to death and hospitalistaion data
comdat$Kent <- 0.0
comdat$India <- 0.0
Kentfac <- 0.6
Indiafac <- 0.9
Kentdate <- as.integer(as.Date("2021/01/01")-startdate)

# Approximate Kent by logistic rise around 2021/01/01
# Same gen time, R+0.3 vs Wild  (0.3 is NOT lethality factor)
for (i in 1:nrow(comdat)){
  x= (i-Kentdate)*0.3/genTime
  comdat$Kent[i]=1.0/(1.0+exp(-x))
}
Indiadate <- as.integer(as.Date("2021/05/15")-startdate)
# Approximate India by logistic rise around 2021/15/01: see covid19.sanger.
# Same genTime R+0.4 vs Kent
for (i in 1:nrow(comdat)){
  x= (i-Indiadate)*0.4/genTime
  comdat$India[i]=1.0/(1.0+exp(-x))
}
rm(x)
# Kent is Kentfac worse, india is Indiafac worse
comdat$Kent<-comdat$Kent-comdat$India
comdat$lethality<-1.0+ Kentfac*comdat$Kent + Indiafac*comdat$India

# Fix missing data to constant values
HospitalData <- na.locf(HospitalData)
casedat <- na.locf(casedat)
comdat <- na.locf(comdat)
regcases <- na.locf(regcases)
scotdat <- na.locf(scotdat)

# Get mean age-related CFR across the whole pandemic
RawCFR=colSums(deathdat[2:ncol(deathdat)])/colSums(casedat[2:ncol(casedat)])

# Remove weekend effect, assuming each weekday has same number of cases over the
# epidemic, and national averages hold regionally.
days <-1:7
weeks<-as.integer(length(comdat$allCases)/7)-1

for(i in 1:weeks){
  for(j in 1:7){
    days[j]<-days[j]+comdat$allCases[7*i+j]
  }
}
casetot <- sum(days)
days <- 7*days/casetot

# Rescale comdat and regcases
for(i in 1:nrow(comdat)){
  indexday <- (i-1)%%7+1
  comdat$allCases[i] <- comdat$allCases[i]/days[indexday]
  scotdat$allCases[i] <- scotdat$allCases[i]/days[indexday]
  for (area in 2:length(regcases)){
    regcases[i,area] <- regcases[i,area]/days[indexday]
  }
}

# Fix Xmas anomaly over XMdays=12 days in comdat, regcases by linear fit
Xmasav <- sum(comdat$allCases[XMstart:XMend])/XMdays
Xmasgrad <- comdat$allCases[XMend]-comdat$allCases[XMstart]
for (i in XMstart:XMend){
  comdat$allCases[i]=Xmasav-Xmasgrad*(((XMend+XMstart)/2)-i)/XMdays
}
Xmasav <- sum(scotdat$allCases[XMstart:XMend])/XMdays
Xmasgrad <- scotdat$allCases[XMend]-scotdat$allCases[XMstart]
for (i in XMstart:XMend){
  scotdat$allCases[i] <- Xmasav-Xmasgrad*(((XMend+XMstart)/2)-i)/XMdays
}

# Fix Xmas anomaly in regions
for (area in 2:length(regcases)){
  Xmasav <- sum(regcases[XMstart:XMend,area])/XMdays
  Xmasgrad <- regcases[XMend,area]-regcases[XMstart,area]
  for (i in XMstart:XMend){
    regcases[i,area]<-Xmasav-Xmasgrad*(((XMend+XMstart)/2)-i)/XMdays
  }
}


for (i in 2:ncol(casedat)) {
  for (j in 1:nrow(casedat)) {
    indexday <- (j-1)%%7+1
    casedat[j,i] <- as.integer(casedat[j,i]/days[indexday])
  }
}

# Fix Xmas and weekend anomaly in age data
for (iage in 2:ncol(casedat) ){
  Xmasav <- sum(casedat[XMstart:XMend,iage])/XMdays
  Xmasgrad <- casedat[XMend,iage]-casedat[XMstart,iage]
  for (i in XMstart:XMend){
    casedat[i,iage] <- as.integer(Xmasav-Xmasgrad*(((XMend+XMstart)/2)-i)/XMdays)
  }
}
rm(Xmasav,Xmasgrad,weeks,i,j,indexday)
# Build CrystalCast agegroups
xcastage <-casedat %>% select(`00_04`)
xcastage$'05_14' <-casedat$`05_09`+casedat$`10_14`
xcastage$'15_24' <-casedat$`15_19`+casedat$`20_24`
xcastage$'25_44' <-casedat$`25_29`+casedat$`30_34`+casedat$`35_39`+casedat$`40_44`
xcastage$'45_64' <-casedat$`45_49`+casedat$`50_54`+casedat$`55_59`+casedat$`60_64`
xcastage$'65_74' <-casedat$`65_69`+casedat$`70_74`
xcastage$'75+' <-casedat$`75_79`+casedat$`80_84`+casedat$`85_89`+casedat$`90+`

# Combination required to go from 9 to 7 English regions
regcases$NE_Yorks <- regcases$`North East` + regcases$`Yorkshire and The Humber`
regcases$Midlands <- regcases$`East Midlands` + regcases$`West Midlands`
regcases$England <- comdat$allCases

# Reorder regcases
regcases<-regcases[,c(1,2,3,4,5,6,7,9,10,8,23,26,27,11,12,13,14,15,16,17,18,19,20,21,22,24,25,28,29,30)]

# Set false positive adjustment at 0.004, extrapolate tests if the last few days are missing
comdat$fpCases <- comdat$allCases-0.004*as.integer(comdat$tests)

comdat$regions <- regcases$London + regcases$`South East` + regcases$`South West` +
                  regcases$NE_Yorks + regcases$Midlands + regcases$`North West` +
                  regcases$`East of England`

# Plot only if running interactively
if(interactive()){

  plot(comdat$inputCases,x=comdat$date,xlab="Date",ylab="Cases")
  lines(comdat$allCases,x=comdat$date, col="green",lwd=2)
  lines(comdat$fpCases, x=comdat$date,col="red",lwd=2)
  lines(comdat$regions, x=comdat$date,col="blue",lwd=2)

  # Same graph using ggplot - alpha sets a level of transparency
  # between 0 (opaque) to 1 (transparent)
  ggplot(comdat,aes(x=date)) +
    geom_point(aes(y=inputCases),alpha=0.5) +
    geom_line(aes(y=allCases), color="green", size=1.5, alpha=0.5) +
    geom_line(aes(y=fpCases),color="red", size=1.5, alpha=0.5) +
    xlab("Dates") + ylab("Cases") +
    theme_bw()
}

CFR_All_ByAge=colSums(deathdat[2:ncol(deathdat)])/colSums(casedat[2:ncol(casedat)])


#  Compartment model now done with a function
out <- Compartment(casedat,  covidsimAge, RawCFR, comdat)
# Unpack the data

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
# Remove the list construct
rm(out)


# End of compartment section

# Monitoring plots
if(interactive()){
  plot(rowSums(deathdat[2:20]))
  lines(rowSums(DEATH[2:20]),col="blue")
  plot(HospitalData$covidOccupiedMVBeds)
  lines(rowSums(CRIT[2:20]),col="blue")
  plot(HospitalData$newAdmissions)
  lines(rowSums(newSARI[2:20]),col="blue")
  plot(HospitalData$hospitalCases)
  lines(rowSums(SARI[2:20]+CRIT[2:20]+CRITREC[2:20]))
}

# Smoothcasedat
smoothcases=smooth.spline(comdat$allCases, df=20)

# Create a vector to hold the results for various R-numbers
ninit <- as.numeric(1:nrow(comdat))/as.numeric(1:nrow(comdat))
dfR <- data.frame(x=1.0:length(comdat$date),
                  date=comdat$date, itoR=ninit, stratR=ninit, rawR=ninit,  fpR=ninit,  weeklyR=ninit,  bylogR=ninit,
                  p00=ninit,  p05=ninit,  p10=ninit,  p15=ninit,  p20=ninit,  p25=ninit,  p30=ninit,
                  p35=ninit,  p40=ninit,  p45=ninit,  p50=ninit,  p55=ninit,  p60=ninit,  p65=ninit,
                  p70=ninit,  p75=ninit,  p80=ninit,  p85=ninit,  p90=ninit, x05=ninit, x15=ninit,
                  x25=ninit, x45=ninit, x65=ninit, x75=ninit, regions=ninit, smoothcasesR=ninit)
# Ito, Stratanovitch and exponential calculus
# rawR averages cases over previous genTime days - assumes genTime is the same as infectious period

# Check if there are any zero cases in the data
if(any(CASE==0)){
  for(name in names(CASE)){
    if(any(CASE[name]==0)){
      warning("Zero values found for ",name," for the date(s) ",
              paste(CASE[["date"]][which(CASE[name]==0)],collapse = ", "),".")
    }
  }
}
rat <- regcases
for(i in (2:nrow(regcases))    ){
  rat[i,2:ncol(regcases)] <- 1+log(regcases[i,2:ncol(regcases)]/regcases[(i-1),2:ncol(regcases)])*genTime
}
# Rest first row to 1, because there's no data
# Fix R=1 not NaN or Inf when previous cases are zero
# Its not really defined.  This generates a warning which we can ignore
rat[1,2:ncol(regcases)]<-1.0
rat[is.na(rat)] <- 1.0
rat[rat==Inf] <- 1.0
rat[rat==-Inf] <- 1.0

startplot <- rat$date[1]
endplot <- enddate


if(interactive()){
  plot(smooth.spline(rat$Scotland[startplot <= rat$date & rat$date <= endplot],df=14)$y,
       x=rat$date[startplot <= rat$date & rat$date <= endplot],
       ylim=c(0.7,1.40),xlab="Date",ylab="R, Scotland")

  rat %>% filter(startplot < date & date < endplot) %>%
    pivot_longer(!date,names_to = "Region", values_to="R") %>%
    ggplot(aes(x=date, y=R, colour=Region)) + coord_cartesian(ylim=c(0.8,1.5)) +
    geom_smooth(formula= y ~ x, method = "loess", span=0.3) +  guides(color = "none") +
    facet_wrap(vars(Region)) +
    theme(axis.text.x=element_text(angle=90,hjust=1)) +xlab("Date")

  #  Plot UK nations and English regions
  rat[,c(1,2,3,4,5,6,7,8,9,10,11,12,13)]%>% filter(startplot < date & date < endplot) %>%
    pivot_longer(!date,names_to = "Region", values_to="R") %>%
    ggplot(aes(x=date, y=R, colour=Region)) +
    coord_cartesian(ylim=c(0.5,1.9))+ geom_smooth(formula= y ~ x, method = "loess", span=0.3) +
    guides(color = "none") + facet_wrap(vars(Region)) +
    theme(axis.text.x=element_text(angle=90,hjust=1)) +xlab("Date")

}


#  Generate R over all ages, with some options for the calculus  itoR is Ito, stratR is stratonovich, bylogR is harmonic Ito fpR includes false positive correction
#  Avoid zero cases in R-calculation
casedat[casedat==0]=1

for(i in ((genTime+1):length(dfR$itoR))    ){
  dfR$itoR[i]=(1+(comdat$allCases[i]-comdat$allCases[i-1])*genTime/(comdat$allCases[i-1]))
  dfR$stratR[i]=1+ (comdat$allCases[i]-comdat$allCases[i-1])*genTime/mean(comdat$allCases[(i-1):i])
  dfR$fpR[i]=(1+(comdat$fpCases[i]-comdat$fpCases[i-1])*genTime/(comdat$fpCases[i-1]))
  dfR$bylogR[i]=1+log(comdat$allCases[i]/comdat$allCases[i-1])*genTime
  dfR$regions[i]=1+log(comdat$regions[i]/comdat$regions[i-1])*genTime
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
    dfR$p80[i]=1+log(casedat$'80_84'[i]/casedat$'80_84'[i-1])*genTime
   dfR$p85[i]=1+log(casedat$'85_89'[i]/casedat$'85_89'[i-1])*genTime
   dfR$p90[i]=1+log(casedat$'90+'[i]/casedat$'90+'[i-1])*genTime
#   Same from CrystalCast age groupings
   dfR$x05[i]=1+log(xcastage$`05_14`[i]/xcastage$`05_14`[i-1])*genTime
   dfR$x15[i]=1+log(xcastage$`15_24`[i]/xcastage$`15_24`[i-1])*genTime
   dfR$x25[i]=1+log(xcastage$`25_44`[i]/xcastage$`25_44`[i-1])*genTime
   dfR$x45[i]=1+log(xcastage$`45_64`[i]/xcastage$`45_64`[i-1])*genTime
   dfR$x65[i]=1+log(xcastage$`65_74`[i]/xcastage$`65_74`[i-1])*genTime
   dfR$x75[i]=1+log(xcastage$'75+'[i]/xcastage$'75+'[i-1])*genTime
   dfR$smoothcasesR[i]=1+log(smoothcases$y[i]/smoothcases$y[i-1])*genTime
}

dfR$smoothRlog<-smooth.spline(dfR$bylogR,df=20,w=sqrt(comdat$allCases))$y
dfR$smoothRito<-smooth.spline(dfR$itoR,df=20,w=sqrt(comdat$allCases))$y
dfR$smoothRstrat<-smooth.spline(dfR$stratR,df=20,w=sqrt(comdat$allCases))$y
dfR$smoothRegions<-smooth.spline(dfR$regions,df=20,w=sqrt(comdat$regions))$y
dfR$loessR<-predict(loess(bylogR~x,data=dfR,span=0.25))
dfR[is.na(dfR)]=1.0
dfR[dfR==Inf]=1.0
dfR[dfR==-Inf]=1.0


# Set day 1, for plotting purposes
for (i in 3:nrow(dfR)){dfR[i,1]=dfR[i,2]}

for(i in 4:(length(dfR$weeklyR)-3)){
    day1=i-3
    day7=i+3
    dfR$weeklyR[i]=sum(dfR$itoR[day1:day7])/7.0
}
# End effect
dfR$weeklyR[length(dfR$weeklyR)]=1.0
dfR$weeklyR[length(dfR$weeklyR)-1]=1.0
dfR$weeklyR[length(dfR$weeklyR)-2]=1.0

# Plot various types of smoothing on the R data

# Making the time windows agree
Govdat <- Rest[Rest$Date >= min(comdat$date) & Rest$Date <= max(comdat$date),]

# Parameters for fitting splines and Loess
nospl=8
spdf=18
lospan=0.3

smoothweightR<-smooth.spline(dfR$bylogR,df=spdf,w=sqrt(comdat$allCases))
smoothweightRstrat<-smooth.spline(dfR$stratR,df=spdf,w=sqrt(comdat$allCases))
smoothweightRito<-smooth.spline(dfR$itoR,df=spdf,w=sqrt(comdat$allCases))
smoothweightRfp<-smooth.spline(dfR$fpR,df=spdf,w=sqrt(comdat$fpCases))
rat$smoothScotland <-smooth.spline(rat$Scotland,df=spdf,w=sqrt(regcases$Scotland))$y
rat$smoothNW <-smooth.spline(rat$`North West`,df=spdf,w=sqrt(regcases$`North West`))$y
rat$smoothNEY <-smooth.spline(rat$NE_Yorks,df=spdf,w=sqrt(regcases$NE_Yorks))$y
rat$smoothLondon <-smooth.spline(rat$London,df=spdf,w=sqrt(regcases$London))$y
rat$smoothEE <-smooth.spline(rat$`East of England`,df=spdf,w=sqrt(regcases$`East of England`))$y
rat$smoothMid <-smooth.spline(rat$Midlands,df=spdf,w=sqrt(regcases$Midlands))$y
rat$smoothSE <-smooth.spline(rat$`South East`,df=spdf,w=sqrt(regcases$`South East`))$y
rat$smoothSW <-smooth.spline(rat$`South West`,df=spdf,w=sqrt(regcases$`South West`))$y
rat$smoothWales <-smooth.spline(rat$Wales,df=spdf,w=sqrt(regcases$Wales))$y
rat$smoothNI<-smooth.spline(rat$NI,df=spdf,w=sqrt(regcases$NI))$y
rat$smoothEngland<-smooth.spline(rat$England,df=spdf,w=sqrt(regcases$England))$y
smoothweightR$date<-comdat$date
smoothweightRfp$date<-dfR$date

smoothR1<-smooth.spline(dfR$bylogR[1:(lock1-1)],df=lock1/14)
smoothR1$date<-dfR$date[1:lock1-1]
smoothR2<-smooth.spline(dfR$bylogR[lock1:(unlock1-1)],df=(unlock1-lock1)/14)
smoothR2$x=smoothR2$x+lock1
smoothR2$date<-dfR$date[lock1:unlock1-1]
smoothR3<-smooth.spline(dfR$bylogR[unlock1:(lock2-1)],df=(lock2-unlock1)/14)
smoothR3$x=smoothR3$x+unlock1
smoothR3$date<-dfR$date[unlock1:(lock2-1)]
smoothR4<-smooth.spline(dfR$bylogR[lock2:(unlock2-1)],df=(unlock2-lock2)/14)
smoothR4$x=smoothR4$x+unlock2
smoothR4$date<-dfR$date[lock2:(unlock2-1)]
smoothRend<-smooth.spline(dfR$bylogR[unlock2:length(dfR$date)],df=(length(dfR$date)-unlock2)/14)
smoothRend$x=smoothRend$x+unlock2
smoothRend$date<-dfR$date[unlock2:length(dfR$itoR)]
dfR$piecewise<-dfR$itoR
dfR$piecewise[1:(lock1-1)]=smoothR1$y
dfR$piecewise[lock1:(unlock1-1)]=smoothR2$y
dfR$piecewise[unlock1:(lock2-1)]=smoothR3$y
dfR$piecewise[lock2:(unlock2-1)]=smoothR4$y
dfR$piecewise[unlock2:length(dfR$itoR)]=smoothRend$y
rm(smoothR1,smoothR2,smoothR3,smoothR4,smoothRend)
# Plot R estimate vs data and fits discontinuous at lockdown
#  Have to move the Official R data back by 16 days !

#  All cases and Regions

plot(smoothweightR$y,ylab="Regional R-number",xlab="Date",x=dfR$date)

for (i in 8:17){
  lines(smooth.spline(na.omit(dfR[i]),df=12)$y,col=i,x=dfR$date[!is.na(dfR[i])])
}

plot(dfR$bylogR,x=smoothweightR$date,ylab="R-number",xlab="",
     title("R, England"),ylim=c(0.6,1.6),xlim=plotdate,cex.lab=1.6, cex.axis=1.6, cex.main=1.6, cex.sub=1.6)
lines(Rest$England_LowerBound,x=Rest$Date-sagedelay,lwd=2)
lines(y=Rest$England_UpperBound,x=Rest$Date-sagedelay,lwd=2)
lines(dfR$piecewise,col="violet",lwd=3,x=dfR$date)
lines(smoothweightR$y,col="blue",lwd=3,x=dfR$date)
lines(predict(loess(itoR ~ x, data=dfR,span=0.3,weight=sqrt(comdat$allCases))),col='green',x=dfR$date,lwd=3)
#lines(predict(loess(itoR ~ x, data=dfR,span=0.3)),col='green',x=dfR$date)
lines(predict(loess(bylogR ~ x, data=dfR,span=0.3,weight=sqrt(comdat$allCases))),col='red',x=dfR$date,lwd=3)
#lines(predict(loess(bylogR ~ x, data=dfR,span=0.3)),col='red',x=dfR$date)

plot(dfR$piecewise,x=smoothweightR$date,ylab="R-number",xlab="",
     title("R, England"),ylim=c(0.6,1.4),xlim=plotdate,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.6)
lines(Rest$England_LowerBound,x=(Rest$Date-sagedelay))
lines(y=Rest$England_UpperBound,x=Rest$Date-sagedelay)
lines(predict(loess(bylogR ~ x, data=dfR,span=0.05,weight=sqrt(comdat$allCases))),col='red',x=dfR$date,lwd=2)
lines(predict(loess(bylogR ~ x, data=dfR,span=0.1,weight=sqrt(comdat$allCases))),col='green',x=dfR$date,lwd=2)
lines(predict(loess(bylogR ~ x, data=dfR,span=0.2,weight=sqrt(comdat$allCases))),col='blue',x=dfR$date,lwd=2)
lines(predict(loess(bylogR ~ x, data=dfR,span=0.3,weight=sqrt(comdat$allCases))),col='violet',x=dfR$date,lwd=3)


### Smoothing Filters
s1=0.05
s2=0.1
s3=0.2
s4=0.3
filteredR <-append(
  append(tail(predict(loess(bylogR ~ x, data=dfR,weight=comdat$allCases, span=s1))),
         tail(predict(loess(bylogR ~ x, data=dfR,weight=comdat$allCases,span=s2))) ) ,
  append(tail(predict(loess(bylogR ~ x, data=dfR,weight=comdat$allCases,span=s3))),
         tail(predict(loess(bylogR ~ x, data=dfR,weight=comdat$allCases,span=s4))))
)
R_England_BestGuess<- mean(filteredR)
R_England_Quant <-unname(quantile(filteredR, probs=c(0.05,0.25,0.5,0.75,0.95)))

filteredR <-append(
  append(tail(predict(loess(Scotland ~ as.numeric(date), data=rat,span=s1))),
         tail(predict(loess(Scotland ~ as.numeric(date), data=rat,span=s2))) ) ,
  append(tail(predict(loess(Scotland ~ as.numeric(date), data=rat,span=s3))),
         tail(predict(loess(Scotland ~ as.numeric(date), data=rat,span=s4))))
)
R_Scotland_BestGuess <-mean(filteredR)
R_Scotland_Quant <-unname(quantile(filteredR, probs=c(0.05,0.25,0.5,0.75,0.95)))

rat$tmp=rat$London

filteredR <-append(
  append(tail(predict(loess(tmp ~ as.numeric(date), data=rat,weight=regcases$London,span=s1))),
         tail(predict(loess(tmp ~ as.numeric(date), data=rat,weight=regcases$London,span=s2))) ) ,
  append(tail(predict(loess(tmp ~ as.numeric(date), data=rat,weight=regcases$London,span=s3))),
         tail(predict(loess(tmp ~ as.numeric(date), data=rat,weight=regcases$London,span=s4))))
)
R_London_BestGuess <-mean(filteredR)
R_London_Quant <-unname(quantile(filteredR, probs=c(0.05,0.25,0.5,0.75,0.95)))

rat$tmp=rat$Midlands

filteredR <-append(
  append(tail(predict(loess(tmp ~ as.numeric(date), data=rat,weight=regcases$Midlands,span=s1))),
         tail(predict(loess(tmp ~ as.numeric(date), data=rat,weight=regcases$Midlands,span=s2))) ) ,
  append(tail(predict(loess(tmp ~ as.numeric(date), data=rat,weight=regcases$Midlands,span=s3))),
         tail(predict(loess(tmp ~ as.numeric(date), data=rat,weight=regcases$Midlands,span=s4))))
)
R_Midlands_BestGuess <-mean(filteredR)
R_Midlands_Quant <-unname(quantile(filteredR, probs=c(0.05,0.25,0.5,0.75,0.95)))

rat$tmp = rat$`North West`

filteredR <-append(
  append(tail(predict(loess(tmp ~ as.numeric(date), data=rat,weight=regcases$`North West` ,span=s1))),
         tail(predict(loess(tmp ~ as.numeric(date), data=rat,weight=regcases$`North West`,span=s2))) ) ,
  append(tail(predict(loess(tmp ~ as.numeric(date), data=rat,weight=regcases$`North West`,span=s3))),
         tail(predict(loess(tmp ~ as.numeric(date), data=rat,weight=regcases$`North West`,span=s4))))
)

R_NW_BestGuess <-mean(filteredR)
R_NW_Quant <-unname(quantile(filteredR, probs=c(0.05,0.25,0.5,0.75,0.95)))

rat$tmp = rat$NE_Yorks

filteredR <-append(
  append(tail(predict(loess(tmp ~ as.numeric(date), data=rat,weight=regcases$NE_Yorks ,span=s1))),
         tail(predict(loess(tmp ~ as.numeric(date), data=rat,weight=regcases$NE_Yorks,span=s2))) ) ,
  append(tail(predict(loess(tmp ~ as.numeric(date), data=rat,weight=regcases$NE_Yorks,span=s3))),
         tail(predict(loess(tmp ~ as.numeric(date), data=rat,weight=regcases$NE_Yorks,span=s4))))
)

R_NEY_BestGuess <-mean(filteredR)
R_NEY_Quant <-unname(quantile(filteredR, probs=c(0.05,0.25,0.5,0.75,0.95)))


rat$tmp = rat$`East of England`

filteredR <-append(
  append(tail(predict(loess(tmp ~ as.numeric(date), data=rat,weight=regcases$`East of England`,span=s1))),
         tail(predict(loess(tmp ~ as.numeric(date), data=rat,weight=regcases$`East of England`,span=s2))) ) ,
  append(tail(predict(loess(tmp ~ as.numeric(date), data=rat,weight=regcases$`East of England`,span=s3))),
         tail(predict(loess(tmp ~ as.numeric(date), data=rat,weight=regcases$`East of England`,span=s4))))
)

R_EE_BestGuess <-mean(filteredR)
R_EE_Quant <-unname(quantile(filteredR, probs=c(0.05,0.25,0.5,0.75,0.95)))



rat$tmp <- rat$`South East`

filteredR <-append(
  append(tail(predict(loess(tmp ~ as.numeric(date), data=rat,weight=regcases$`South East`,span=s1))),
         tail(predict(loess(tmp ~ as.numeric(date), data=rat,weight=regcases$`South East`,span=s2))) ) ,
  append(tail(predict(loess(tmp ~ as.numeric(date), data=rat,weight=regcases$`South East`,span=s3))),
         tail(predict(loess(tmp ~ as.numeric(date), data=rat,weight=regcases$`South East`,span=s4))))
)

R_SE_BestGuess <-mean(filteredR)
R_SE_Quant <-unname(quantile(filteredR, probs=c(0.05,0.25,0.5,0.75,0.95)))



rat$tmp <- rat$`South West`

filteredR <-append(
  append(tail(predict(loess(tmp ~ as.numeric(date), data=rat,weight=regcases$`South West`,span=s1))),
         tail(predict(loess(tmp ~ as.numeric(date), data=rat,weight=regcases$`South West`,span=s2))) ) ,
  append(tail(predict(loess(tmp ~ as.numeric(date), data=rat,weight=regcases$`South West`,span=s3))),
         tail(predict(loess(tmp ~ as.numeric(date), data=rat,weight=regcases$`South West`,span=s4))))
)

R_SW_BestGuess <-mean(filteredR)
R_SW_Quant <-unname(quantile(filteredR, probs=c(0.05,0.25,0.5,0.75,0.95)))


rat$tmp <- rat$Wales
filteredR <-append(
  append(tail(predict(loess(tmp ~ as.numeric(date), data=rat,weight=regcases$Wales,span=s1))),
         tail(predict(loess(tmp ~ as.numeric(date), data=rat,weight=regcases$Wales,span=s2))) ) ,
  append(tail(predict(loess(tmp ~ as.numeric(date), data=rat,weight=regcases$Wales,span=s3))),
         tail(predict(loess(tmp ~ as.numeric(date), data=rat,weight=regcases$Wales,span=s4))))
)
R_Wales_BestGuess <-mean(filteredR)
R_Wales_Quant <-unname(quantile(filteredR, probs=c(0.05,0.25,0.5,0.75,0.95)))

rat$tmp <-dfR$regions
filteredR <-append(
  append(tail(predict(loess(tmp ~ as.numeric(date),weight=regcases$England, data=rat,span=s1))),
         tail(predict(loess(tmp ~ as.numeric(date),weight=regcases$England,  data=rat,span=s2))) ) ,
  append(tail(predict(loess(tmp ~ as.numeric(date),weight=regcases$England,  data=rat,span=s3))),
         tail(predict(loess(tmp ~ as.numeric(date),weight=regcases$England,  data=rat,span=s4))))
)
R_Regions_BestGuess <-mean(filteredR)
R_Regions_Quant <-unname(quantile(filteredR, probs=c(0.05,0.25,0.5,0.75,0.95)))

filteredR <-append(
  append(tail(predict(loess(NI ~ as.numeric(date),weight=regcases$NI, data=rat,span=s1))),
         tail(predict(loess(NI ~ as.numeric(date),weight=regcases$NI,  data=rat,span=s2))) ) ,
  append(tail(predict(loess(NI ~ as.numeric(date),weight=regcases$NI,  data=rat,span=s3))),
         tail(predict(loess(NI ~ as.numeric(date),weight=regcases$NI,  data=rat,span=s4))))
)

R_NI_BestGuess <-mean(filteredR)
R_NI_Quant <-unname(quantile(filteredR, probs=c(0.05,0.25,0.5,0.75,0.95)))

#  Delete the tmp column
rat <- rat[,-which(names(rat)=="tmp")]


##########   Age groups  ########

filteredR <-append(
  append(tail(predict(loess(p00 ~ as.numeric(date), data=dfR,span=s1))),
         tail(predict(loess(p00 ~ as.numeric(date), data=dfR,span=s2))) ) ,
  append(tail(predict(loess(p00 ~ as.numeric(date), data=dfR,span=s3))),
         tail(predict(loess(p00 ~ as.numeric(date), data=dfR,span=s4))))
)

Growth_00_BestGuess <- mean(filteredR)
Growth_00_Quant <-unname(quantile(filteredR, probs=c(0.05,0.25,0.5,0.75,0.95)))


filteredR <-append(
  append(tail(predict(loess(p05 ~ as.numeric(date), data=dfR,span=s1))),
         tail(predict(loess(p05 ~ as.numeric(date), data=dfR,span=s2))) ) ,
  append(tail(predict(loess(p05 ~ as.numeric(date), data=dfR,span=s3))),
         tail(predict(loess(p05 ~ as.numeric(date), data=dfR,span=s4))))
)
Growth_05_BestGuess <- mean(filteredR)
Growth_05_Quant <-unname(quantile(filteredR, probs=c(0.05,0.25,0.5,0.75,0.95)))


filteredR <-append(
  append(tail(predict(loess(p15 ~ as.numeric(date), data=dfR,span=s1))),
         tail(predict(loess(p15 ~ as.numeric(date), data=dfR,span=s2))) ) ,
  append(tail(predict(loess(p15 ~ as.numeric(date), data=dfR,span=s3))),
         tail(predict(loess(p15 ~ as.numeric(date), data=dfR,span=s4))))
)
Growth_15_BestGuess <- mean(filteredR)
Growth_15_Quant <-unname(quantile(filteredR, probs=c(0.05,0.25,0.5,0.75,0.95)))


filteredR <-append(
  append(tail(predict(loess(x25 ~ as.numeric(date), data=dfR,span=s1))),
         tail(predict(loess(x25 ~ as.numeric(date), data=dfR,span=s2))) ) ,
  append(tail(predict(loess(x25 ~ as.numeric(date), data=dfR,span=s3))),
         tail(predict(loess(x25 ~ as.numeric(date), data=dfR,span=s4))))
)
Growth_25_BestGuess <- mean(filteredR)
Growth_25_Quant <-unname(quantile(filteredR, probs=c(0.05,0.25,0.5,0.75,0.95)))

filteredR <-append(
  append(tail(predict(loess(x45 ~ as.numeric(date), data=dfR,span=s1))),
         tail(predict(loess(x45 ~ as.numeric(date), data=dfR,span=s2))) ) ,
  append(tail(predict(loess(x45 ~ as.numeric(date), data=dfR,span=s3))),
         tail(predict(loess(x45 ~ as.numeric(date), data=dfR,span=s4))))
)
Growth_45_BestGuess <- mean(filteredR)
Growth_45_Quant <-unname(quantile(filteredR, probs=c(0.05,0.25,0.5,0.75,0.95)))


filteredR <-append(
  append(tail(predict(loess(x65 ~ as.numeric(date), data=dfR,span=s1))),
         tail(predict(loess(x65 ~ as.numeric(date), data=dfR,span=s2))) ) ,
  append(tail(predict(loess(x65 ~ as.numeric(date), data=dfR,span=s3))),
         tail(predict(loess(x65 ~ as.numeric(date), data=dfR,span=s4))))
)
Growth_65_BestGuess <- mean(filteredR)
Growth_65_Quant <-unname(quantile(filteredR, probs=c(0.05,0.25,0.5,0.75,0.95)))



filteredR <-append(
  append(tail(predict(loess(x75 ~ as.numeric(date), data=dfR,span=s1))),
         tail(predict(loess(x75 ~ as.numeric(date), data=dfR,span=s2))) ) ,
  append(tail(predict(loess(x75 ~ as.numeric(date), data=dfR,span=s3))),
         tail(predict(loess(x75 ~ as.numeric(date), data=dfR,span=s4))))
)
Growth_75_BestGuess <- mean(filteredR)
Growth_75_Quant <-unname(quantile(filteredR, probs=c(0.05,0.25,0.5,0.75,0.95)))

rm(filteredR)

plot(smoothweightR$y,ylab="R-number",xlab="Day",ylim=c(0.5,1.6))
#  Plot R continuous with many splines.
for (ismooth in 4:30){
#  lines(smooth.spline(dfR$bylogR,df=ismooth,w=sqrt(comdat$allCases)))
  lines(predict(loess(bylogR ~ x, data=dfR,span=(4.0/ismooth))),col='red')
  }


# Plot Regional R data vs Government  spdf is spline smoothing factor, lospan for loess

#  various options to silence pdf writing
pdfpo=FALSE

#  Plot age data
dfR[,c(2,9:27)]%>% filter(startplot < date & date < endplot) %>%
  pivot_longer(!date,names_to = "Age", values_to="R") %>%
  ggplot(aes(x=date, y=R, colour=Age)) +
  coord_cartesian(ylim=c(0.5,1.9))+ geom_smooth(formula = y ~ x, method = "loess", span=0.3) +
  guides(color = "none") + facet_wrap(vars(Age)) +
  theme(axis.text.x=element_text(angle=90,hjust=1)) + xlab("Date")



if(pdfpo){

if(interactive()){
  pdf(file = 'p05.pdf')
  plot(smooth.spline(dfR$p05,df=spdf,w=sqrt(comdat$allCases))$y,ylab="R-number, 05-09",xlab="Date",x=dfR$date,ylim=c(0.6,1.4),xlim=plotdate,cex.lab=1.6, cex.axis=1.6, cex.main=1.6, cex.sub=1.6)
  lines(y=Rest$England_LowerBound,x=Rest$Date-sagedelay)
  lines(y=Rest$England_UpperBound,x=Rest$Date-sagedelay)
  lines(predict(loess(p05 ~ x, data=dfR,span=lospan)),col='red',x=dfR$date)
  invisible(dev.off())
  pdf(file = 'p10.pdf')
  plot(smooth.spline(dfR$p10,df=spdf,w=sqrt(comdat$allCases))$y,ylab="R-number, 10-14",xlab="Date",x=dfR$date,ylim=c(0.6,1.4),xlim=plotdate,cex.lab=1.6, cex.axis=1.6, cex.main=1.6, cex.sub=1.6)
  lines(y=Rest$England_LowerBound,x=Rest$Date-sagedelay)
  lines(y=Rest$England_UpperBound,x=Rest$Date-sagedelay)
  lines(predict(loess(p10 ~ x, data=dfR,span=lospan)),col='red',x=dfR$date)
  invisible(dev.off())
  pdf(file = 'p15.pdf')
  plot(smooth.spline(dfR$p15,df=spdf,w=sqrt(comdat$allCases))$y,ylab="R-number, 15-19",xlab="Date",x=dfR$date,ylim=c(0.6,1.4),xlim=plotdate,cex.lab=1.6, cex.axis=1.6, cex.main=1.6, cex.sub=1.6)
  lines(y=Rest$England_LowerBound,x=Rest$Date-sagedelay)
  lines(y=Rest$England_UpperBound,x=Rest$Date-sagedelay)
  lines(predict(loess(p15 ~ x, data=dfR,span=lospan)),col='red',x=dfR$date)
  invisible(dev.off())
  pdf(file = 'p20.pdf')
  plot(smooth.spline(dfR$p20,df=spdf,w=sqrt(comdat$allCases))$y,ylab="R-number, 20-24",xlab="Date",x=dfR$date,ylim=c(0.6,1.4),xlim=plotdate,cex.lab=1.6, cex.axis=1.6, cex.main=1.6, cex.sub=1.6)
  lines(y=Rest$England_LowerBound,x=Rest$Date-sagedelay)
  lines(y=Rest$England_UpperBound,x=Rest$Date-sagedelay)
  lines(predict(loess(p20 ~ x, data=dfR,span=lospan)),col='red',x=dfR$date)
  invisible(dev.off())
  pdf(file = 'p25.pdf')
  plot(smooth.spline(dfR$p25,df=spdf,w=sqrt(comdat$allCases))$y,ylab="R-number, 25-29",xlab="Date",x=dfR$date,ylim=c(0.6,1.4),xlim=plotdate,cex.lab=1.6, cex.axis=1.6, cex.main=1.6, cex.sub=1.6)
  lines(y=Rest$England_LowerBound,x=Rest$Date-sagedelay)
  lines(y=Rest$England_UpperBound,x=Rest$Date-sagedelay)
  lines(predict(loess(p25 ~ x, data=dfR,span=lospan)),col='red',x=dfR$date)
  invisible(dev.off())
  pdf(file = 'p30.pdf')
  plot(smooth.spline(dfR$p30,df=spdf,w=sqrt(comdat$allCases))$y,ylab="R-number, 30-34",xlab="Date",x=dfR$date,ylim=c(0.6,1.4),xlim=plotdate,cex.lab=1.6, cex.axis=1.6, cex.main=1.6, cex.sub=1.6)
  lines(y=Rest$England_LowerBound,x=Rest$Date-sagedelay)
  lines(y=Rest$England_UpperBound,x=Rest$Date-sagedelay)
  lines(predict(loess(p30 ~ x, data=dfR,span=lospan)),col='red',x=dfR$date)
  invisible(dev.off())
  pdf(file = 'p35.pdf')
  plot(smooth.spline(dfR$p35,df=spdf,w=sqrt(comdat$allCases))$y,ylab="R-number, 35-39",xlab="Date",x=dfR$date,ylim=c(0.6,1.4),xlim=plotdate,cex.lab=1.6, cex.axis=1.6, cex.main=1.6, cex.sub=1.6)
  lines(y=Rest$England_LowerBound,x=Rest$Date-sagedelay)
  lines(y=Rest$England_UpperBound,x=Rest$Date-sagedelay)
  lines(predict(loess(p35 ~ x, data=dfR,span=lospan)),col='red',x=dfR$date)
  invisible(dev.off())
  pdf(file = 'p40.pdf')
  plot(smooth.spline(dfR$p40,df=spdf,w=sqrt(comdat$allCases))$y,ylab="R-number, 40-44",xlab="Date",x=dfR$date,ylim=c(0.6,1.4),xlim=plotdate,cex.lab=1.6, cex.axis=1.6, cex.main=1.6, cex.sub=1.6)
  lines(y=Rest$England_LowerBound,x=Rest$Date-sagedelay)
  lines(y=Rest$England_UpperBound,x=Rest$Date-sagedelay)
  lines(predict(loess(p40 ~ x, data=dfR,span=lospan)),col='red',x=dfR$date)
  invisible(dev.off())
  pdf(file = 'p45.pdf')
  plot(smooth.spline(dfR$p45,df=spdf,w=sqrt(comdat$allCases))$y,ylab="R-number, 45-49",xlab="Date",x=dfR$date,ylim=c(0.6,1.4),xlim=plotdate,cex.lab=1.6, cex.axis=1.6, cex.main=1.6, cex.sub=1.6)
  lines(y=Rest$England_LowerBound,x=Rest$Date-sagedelay)
  lines(y=Rest$England_UpperBound,x=Rest$Date-sagedelay)
  lines(predict(loess(p45 ~ x, data=dfR,span=lospan)),col='red',x=dfR$date)
  invisible(dev.off())
  pdf(file = 'p50.pdf')
  plot(smooth.spline(dfR$p50,df=spdf,w=sqrt(comdat$allCases))$y,ylab="R-number, 50-54",xlab="Date",x=dfR$date,ylim=c(0.6,1.4),xlim=plotdate,cex.lab=1.6, cex.axis=1.6, cex.main=1.6, cex.sub=1.6)
  lines(y=Rest$England_LowerBound,x=Rest$Date-sagedelay)
  lines(y=Rest$England_UpperBound,x=Rest$Date-sagedelay)
  lines(predict(loess(p50 ~ x, data=dfR,span=lospan)),col='red',x=dfR$date)
  invisible(dev.off())
  pdf(file = 'p55.pdf')
  plot(smooth.spline(dfR$p55,df=spdf,w=sqrt(comdat$allCases))$y,ylab="R-number, 55-59",xlab="Date",x=dfR$date,ylim=c(0.6,1.4),xlim=plotdate,cex.lab=1.6, cex.axis=1.6, cex.main=1.6, cex.sub=1.6)
  lines(y=Rest$England_LowerBound,x=Rest$Date-sagedelay)
  lines(y=Rest$England_UpperBound,x=Rest$Date-sagedelay)
  lines(predict(loess(p55 ~ x, data=dfR,span=lospan)),col='red',x=dfR$date)
  invisible(dev.off())
  pdf(file = 'p60.pdf')
  plot(smooth.spline(dfR$p60,df=spdf,w=sqrt(comdat$allCases))$y,ylab="R-number, 60-64",xlab="Date",x=dfR$date,ylim=c(0.6,1.4),xlim=plotdate,cex.lab=1.6, cex.axis=1.6, cex.main=1.6, cex.sub=1.6)
  lines(y=Rest$England_LowerBound,x=Rest$Date-sagedelay)
  lines(y=Rest$England_UpperBound,x=Rest$Date-sagedelay)
  lines(predict(loess(p60 ~ x, data=dfR,span=lospan)),col='red',x=dfR$date)
  invisible(dev.off())
  pdf(file = 'p65.pdf')
  plot(smooth.spline(dfR$p65,df=spdf,w=sqrt(comdat$allCases))$y,ylab="R-number, 65-69",xlab="Date",x=dfR$date,ylim=c(0.6,1.4),xlim=plotdate,cex.lab=1.6, cex.axis=1.6, cex.main=1.6, cex.sub=1.6)
  lines(y=Rest$England_LowerBound,x=Rest$Date-sagedelay)
  lines(y=Rest$England_UpperBound,x=Rest$Date-sagedelay)
  lines(predict(loess(p65 ~ x, data=dfR,span=lospan)),col='red',x=dfR$date)
  invisible(dev.off())
  pdf(file = 'p70.pdf')
  plot(smooth.spline(dfR$p70,df=spdf,w=sqrt(comdat$allCases))$y,ylab="R-number, 70-74",xlab="Date",x=dfR$date,ylim=c(0.6,1.4),xlim=plotdate,cex.lab=1.6, cex.axis=1.6, cex.main=1.6, cex.sub=1.6)
  lines(y=Rest$England_LowerBound,x=Rest$Date-sagedelay)
  lines(y=Rest$England_UpperBound,x=Rest$Date-sagedelay)
  lines(predict(loess(p70 ~ x, data=dfR,span=lospan)),col='red',x=dfR$date)
  invisible(dev.off())
  pdf(file = 'p75.pdf')
  plot(smooth.spline(dfR$p75,df=spdf,w=sqrt(comdat$allCases))$y,ylab="R-number, 75-79",xlab="Date",x=dfR$date,ylim=c(0.6,1.4),xlim=plotdate,cex.lab=1.6, cex.axis=1.6, cex.main=1.6, cex.sub=1.6)
  lines(y=Rest$England_LowerBound,x=Rest$Date-sagedelay)
  lines(Rest$England_UpperBound,col="red",x=Rest$Date-sagedelay)
  lines(predict(loess(p75 ~ x, data=dfR,span=lospan)),col='red',x=dfR$date)
  invisible(dev.off())
  pdf(file = 'p80.pdf')
  plot(smooth.spline(dfR$p80,df=spdf,w=sqrt(comdat$allCases))$y,ylab="R-number, 80-85",xlab="Date",x=dfR$date,ylim=c(0.6,1.4),xlim=plotdate,cex.lab=1.6, cex.axis=1.6, cex.main=1.6, cex.sub=1.6)
  lines(y=Rest$England_LowerBound,x=Rest$Date-sagedelay)
  lines(y=Rest$England_UpperBound,x=Rest$Date-sagedelay)
  lines(predict(loess(p80 ~ x, data=dfR,span=lospan)),col='red',x=dfR$date)
  invisible(dev.off())
  pdf(file = 'p85.pdf')
  plot(smooth.spline(dfR$p85,df=spdf,w=sqrt(comdat$allCases))$y,ylab="R-number, 85-89",xlab="Date",x=dfR$date,ylim=c(0.6,1.4),xlim=plotdate,cex.lab=1.6, cex.axis=1.6, cex.main=1.6, cex.sub=1.6)
  lines(y=Rest$England_LowerBound,x=Rest$Date-sagedelay)
  lines(y=Rest$England_UpperBound,x=Rest$Date-sagedelay)
  lines(predict(loess(p85 ~ x, data=dfR,span=lospan)),col='red',x=dfR$date)
  invisible(dev.off())
  pdf(file = 'p90.pdf')
  plot(smooth.spline(na.omit(dfR$p90),df=spdf,w=sqrt(comdat$allCases[!is.na(dfR$p90)]))$y,ylab="R-number, 90+",xlab="Date",
       x=dfR$date[!is.na(dfR$p90)],ylim=c(0.6,1.4),xlim=plotdate,cex.lab=1.6, cex.axis=1.6, cex.main=1.6, cex.sub=1.6)
  lines(y=Rest$England_LowerBound[!is.na(dfR$p90)],x=Rest$Date[!is.na(dfR$p90)]-sagedelay)
  lines(y=Rest$England_UpperBound[!is.na(dfR$p90)],x=Rest$Date[!is.na(dfR$p90)]-sagedelay)
  lines(predict(loess(p90 ~ x, data=dfR[!is.na(dfR$p90),],span=lospan)),col='red',x=dfR$date[!is.na(dfR$p90)])
  invisible(dev.off())
}
}

# Reverse Engineer cases from R-number - requires stratonovich calculus to get reversibility
# Initializations

#  Use the same weekend-adjusted initial condition, regardless of smoothing effect
#  Start with the "correct" data

Predict <- data.frame(x=1.0:length(comdat$date),
                      date=comdat$date,
                      c=comdat$allCases,
Lin=comdat$allCases,
Raw=comdat$allCases,
SmoothRlog=comdat$allCases,
SmoothRito=comdat$allCases,
SmoothRstrat=comdat$allCases,
MeanR=comdat$allCases,
smoothcasesR=comdat$allCases
)
meanR=mean(dfR$stratR)
startpred=genTime+22
for(i in startpred:length(dfR$date)){
  Predict$c[i]=Predict$c[i-1]*exp((dfR$bylogR[i]-1)/genTime)
  Predict$Lin[i]=Predict$Lin[i-1]*(1.0+(dfR$itoR[i]-1)/genTime)
  Predict$SmoothRstrat[i]=Predict$SmoothRstrat[i-1]*(1.0+(dfR$stratR[i]-1)/genTime)
  Predict$MeanR[i]=Predict$MeanR[i-1]*(1.0+(meanR-1)/genTime)
  Predict$SmoothRlog[i]=Predict$SmoothRlog[i-1]*exp((dfR$smoothRlog[i]-1)/genTime)
  Predict$SmoothRito[i]=Predict$SmoothRito[i-1]*exp((dfR$smoothRito[i]-1)/genTime)
  Predict$smoothcasesR[i]=Predict$smoothcasesR[i-1]*exp((dfR$smoothcasesR[i]-1)/genTime)
    }
#  Averaging R is not the same as averaging e^R
#  Noise suppresses the growth rate in the model, Smoothed R grows too fast
#   Multiplier chosen to match final cases, because it is super-sensitive to noise in the initial day
#

Predict$smoothcasesR =Predict$smoothcasesR*sum(comdat$allCases)/sum(Predict$smoothcasesR)
Predict$SmoothRito =Predict$SmoothRito*sum(comdat$allCases)/sum(Predict$SmoothRito)
Predict$SmoothRlog=Predict$SmoothRlog*sum(comdat$allCases)/sum(Predict$SmoothRlog)
Predict$MeanR=Predict$MeanR*sum(comdat$allCases)/sum(Predict$MeanR)

if(interactive()){

  sum(Predict$MeanR)
  sum(Predict$SmoothRlog)
  sum(Predict$SmoothRito)
  sum(Predict$smoothcasesR)

  plot(comdat$allCases,x=Predict$date,xlab="Date",ylab="Cases backdeduced from R"
       ,xlim=c(Predict$date[(startpred+10)],Predict$date[350]))
  lines(Predict$c,x=Predict$date, col="black",lwd=2)
  lines(Predict$SmoothRlog,x=Predict$date, col="blue",lwd=2)
  lines(Predict$SmoothRito,x=Predict$date, col="violet",lwd=2)
  lines(Predict$MeanR,x=Predict$date, col="green",lwd=2)
  lines(Predict$smoothcasesR,x=Predict$date, col="red",lwd=2)

  dfR$meanR=meanR
  plot(dfR$bylogR,x=dfR$date,xlab="",ylab="R"
       ,xlim=c(dfR$date[(startpred)],dfR$date[350]),ylim=c(-1,3))
  lines(dfR$smoothRlog,x=dfR$date, col="blue",lwd=2)
  lines(dfR$smoothRito,x=dfR$date, col="violet",lwd=2)
  lines(dfR$meanR,x=dfR$date, col="green",lwd=2)
  lines(dfR$smoothcasesR,x=dfR$date, col="red",lwd=2)

  # ggplot version of the same graph
  tibble(date=comdat$date,c=Predict$c,
         smoothcasesR=Predict$smoothcasesR,
         SmoothRlog=Predict$SmoothRlog,
         SmoothRito=Predict$SmoothRito,
         MeanR=Predict$MeanR) ->tmpdat
  ggplot(comdat,aes(x=date)) + geom_point(aes(y=allCases),alpha=0.5) +
    geom_line(data=tmpdat, aes(x=date,y=c),colour="black",alpha=0.75) +
    geom_line(data=tmpdat, aes(x=date,y=SmoothRlog),colour="blue",alpha=0.75) +
    geom_line(data=tmpdat, aes(x=date,y=SmoothRito),colour="violet",alpha=0.75) +
    geom_line(data=tmpdat, aes(x=date,y=MeanR),colour="green",alpha=0.75) +
    geom_line(data=tmpdat, aes(x=date,y=smoothcasesR),colour="red",alpha=0.75) +
    xlab("Date") + ylab("Cases backdeduced from R") + theme_bw()

  tibble(date=comdat$date,c=Predict$c,
         smoothcasesR=dfR$smoothcasesR,
         SmoothRlog=dfR$smoothRlog,
         SmoothRito=dfR$smoothRito,
         bylogR=dfR$bylogR,
         MeanR=mean(dfR$bylogR) )->tmpdat
  ggplot(comdat,aes(x=date)) +
    geom_point(data=tmpdat, aes(x=date,y=bylogR),colour="black",alpha=0.75) +
    geom_line(data=tmpdat, aes(x=date,y=SmoothRlog),colour="blue",alpha=0.75) +
    geom_line(data=tmpdat, aes(x=date,y=SmoothRito),colour="violet",alpha=0.75) +
    geom_line(data=tmpdat, aes(x=date,y=MeanR),colour="green",alpha=0.75) +
    geom_line(data=tmpdat, aes(x=date,y=smoothcasesR),colour="red",alpha=0.75) +
    xlab("Date") + ylab("R") + theme_bw()
  rm(tmpdat)

}

# Load code to function to output to the web-ui interface
# From stackoverflow: 6456501
if(!exists("outputJSON", mode="function")) source("json_wss.R")

# Get requested outputs from the web-ui. For the web-ui the
# file must be "/data/input/inputFile.json".
if(dir.exists("/data/input")){
  infile <- "/data/input/inputFile.json"
}else{
  infile <- "data/sample-inputFile.json"
}

# Get input data from the web interface or a test file
dataIn <- getInput(infile)

# NOTE: These are the regions and subregions being asked for - data should be produced
# that corresponds to these.  Add UI to avoid name clash with CrystalCast
UI_region <- dataIn$region
UI_subregion <- dataIn$subregion

# Read these parameters to output again
calibrationDate <- dataIn$parameters$calibrationDate
calibrationCaseCount <- dataIn$parameters$calibrationCaseCount
calibrationDeathCount <- dataIn$parameters$calibrationDeathCount
interventionPeriods <- dataIn$parameters$interventionPeriods

# Beginning of time series
t0 <-  min(dfR$date)

# Get the days for which data will be output
days <- as.integer(dfR$date - t0)

# Labels are optional
myCritRecov <- as.integer(rowSums(CRITREC[2:20]))
myCritical <- as.integer(rowSums(CRIT[2:20]))
myILI <- as.integer(rowSums(ILI[2:20]))
myMild <- as.integer(rowSums(MILD[2:20]))
mySARI <-  as.integer(rowSums(ILI[2:20]))
mynewCritRecov <- as.integer(rowSums(newCRITREC[2:20]))
mynewCritical <- as.integer(rowSums(newCRIT[2:20]))
mynewILI <- as.integer(rowSums(newILI[2:20]))
mynewMild <- as.integer(rowSums(newMILD[2:20]))
mynewSARI <-  as.integer(rowSums(newILI[2:20]))
outputJSON(myt0 = t0,
           mydaysarray = days,
           myregion = UI_region,
           mysubregion = UI_subregion, # see https://en.wikipedia.org/wiki/ISO_3166-2:GB
           mycalibrationCaseCount = calibrationCaseCount,
           mycalibrationDate = calibrationDate,
           mycalibrationDeathCount = calibrationDeathCount,
           myr0 = NA,
           myinterventionPeriods= interventionPeriods,
           myCritRecov = myCritRecov,
           myCritical = myCritical,
           myILI = myILI,
           myMild = myMild,
           myR = dfR$piecewise,
           mySARI = as.integer(rowSums(SARI[2:20])),
           mycumCritRecov = cumsum(mynewCritRecov),
           mycumCritical = cumsum(mynewCritical),
           mycumILI = cumsum(mynewILI),
           mycumMild = cumsum(mynewMild),
           mycumSARI = cumsum(mynewSARI),
           myincDeath = as.integer(rowSums(DEATH[2:20]))
)
CrystalCast=TRUE
if(CrystalCast){
  "Edinburgh,WSS,Nowcast,Cases,v1"
  date()
  date()
  "All,Scotland,R"
  R_Scotland_BestGuess
}

#####  Figures and analysis for https://www.medrxiv.org/content/10.1101/2021.04.14.21255385v1
# Date not encapuslated and broken because of hardcoded dates
# Nothing should be returned or changed by this analysis
if(FALSE) medout<-MedrxivPaper()


################################################################
###  Finally, Use all this to make predictions for England (Scotland & Regions in separate compartment.R code)
###Assume that R and lethality are constants

{
  predtime = 28
  region="England"
  R_BestGuess=R_England_BestGuess
  #  For loop over time, predCASE using R numbers
  lengthofdata <- nrow(casedat)
  agerange <- (2:ncol(ILI))
  predCASE<-ILI[lengthofdata,(1:20)]
  predCASE[1,(2:20)]<-CASE[lengthofdata,(2:20)] #  Growth rate by age group
  predCASE[1,1]=enddate

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

    # R decays back to 1 with growth rate down 5% a day
    # R is the same in all age groups

    if(R_BestGuess > 1.0) {R_BestGuess=(R_BestGuess-1)*0.95+1.0}

    predCASE[(ipred+1),(2:20)]<-predCASE[ipred,(2:20)]*exp((R_BestGuess-1)/genTime)
    predCASE[ipred+1,1]<-startdate+iday
    ipred=ipred+1
    # End of compartment section
  }
}

# CrystalCast output - use CC.R

#Monitoring plots
startplot=startdate+3
endplot=startdate+lengthofdata+predtime-3
rbind(CASE,predCASE)->plotCASE
plot(rowSums(plotCASE[2:20]),x=plotCASE$date,xlim=c(startplot,endplot))

plot(HospitalData$newAdmissions,x=HospitalData$date, ylab="Hospital Admission",xlab="Date",xlim=c(startplot,endplot-11
                                                                                                ))
lines(rowSums(newSARI[2:20]),x=newSARI$date,col="blue")

plot(HospitalData$hospitalCases,x=HospitalData$date,ylab="Hospital Cases",xlab="Date",xlim=c((startplot),endplot))
lines(rowSums(SARI[2:20]+CRIT[2:20]+CRITREC[2:20]),x=SARI$date,col='red')

plot(rowSums(newMILD[2:20]+newILI[2:20]),xlim=c((startplot),endplot),col="blue",x=newMILD$date,type="l",xlab="Date",ylab="Cases")
points(rowSums(CASE[2:20]),x=deathdat$date)
lines(rowSums(newMILD[2:10]+newILI[2:10]),col="green",x=newMILD$date,type="l",xlab="Date",ylab="Cases")
lines(rowSums(newMILD[11:20]+newILI[11:20]),col="red",x=newMILD$date,type="l",xlab="Date",ylab="Cases")


plot(HospitalData$covidOccupiedMVBeds,x=HospitalData$date,ylab="ICU Occupation",xlab="Date",xlim=c(startplot,endplot))
lines(rowSums(CRIT[2:20]*0.5),col="blue",x=CRIT$date)

plot(rowSums(DEATH[2:20]),col="blue",x=DEATH$date, type="l",ylab="Deaths"
     ,xlab="Date",xlim=c(startplot,endplot-11))
points(rowSums(deathdat[2:20]),x=deathdat$date)

# This needs to be the last routine called for the UI, by default it returns
# success (0), if there is no success setStatus() should be called. By default
# it will return -1 but you can set a value setStatus(1). Any non-zero value
# will indicate a problem.  For interactive work "quit" can end Rstudio session altogether
if(! interactive()){quit(status=returnStatus())}

