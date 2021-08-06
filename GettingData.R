#!/usr/bin/env Rscript
#
# Data used in making calculations is made available under an Open Government
# Licence. For more details see:
#
# http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/
#


# Load packages -----------------------------------------------------------

# Read packages used by the script
library(readr, warn.conflicts = FALSE, quietly = TRUE)
library(dplyr, warn.conflicts = FALSE, quietly = TRUE)
library(tidyr, warn.conflicts = FALSE, quietly = TRUE)


# Region lookup -----------------------------------------------------------

#
# UK country codes available from:
#
# https://geoportal.statistics.gov.uk/datasets/countries-december-2018-names-and-codes-in-the-united-kingdom/explore
#
# Look-up table mapping ONS codes to 7 English regions.
#
# https://geoportal.statistics.gov.uk/datasets/nhs-england-region-april-2020-names-and-codes-in-england/explore?showTable=true
#
# These codes are also made available under an Open Government Licence:
#
# http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/
#
code2region <- data.frame(
  W92000004 = "GB-WLS",
  S92000003 = "GB-SCT",
  E92000001 = "GB-ENG",
  N92000002 = "GB-NIR",
  E12000007 = "London",
  E12000008 = "South East",
  E12000009 = "South West",
  E12000006 = "East of England",
  E12000002 = "East Midlands",
  E12000005 = "West Midlands",
  E12000003 = "Yorkshire and The Humber",
  E12000001 = "North East",
  E12000002 = "North West"
)

region2code  <- data.frame(
  "GB-WLS" = "W92000004",
  "GB-SCT" = "S92000003",
  "GB-ENG" = "E92000001",
  "GB-NIR" = "N92000002",
  "London" = "E12000007",
  "South East" = "E12000008",
  "South West" = "E12000002",
  "East of England" = "E12000006",
  "East Midlands" = "E12000002",
  "West Midlands" = "E12000005",
  "North East" = "E12000001",
  "North West" = "E12000002",
  "Yorkshire and The Humber" = "E12000003",
  check.names = FALSE
)

# Download data -----------------------------------------------------------

# Region to get data for
subregion <- "East of England"

# Get the code for the subregion
code <- region2code[subregion]

# Check if an English subregion - start with E4
isEngSubregion <-  grepl("^E12",code)

# Base URL to get the UK government data
baseurl <- "https://api.coronavirus.data.gov.uk/v2/data?"

# Start and end date for the data
# for the enddate lose only the last day of data -
# use a tail correction for reporting delay
startdate <- as.Date("2020/07/25")
enddate <-  Sys.Date()-5

if (!isEngSubregion) { # Dealing with a GB country state
   header <- paste0("areaType=nation&","areaCode=",code,"&")
} else { # Dealing with an English region
   header <- paste0("areaType=region&","areaCode=",code,"&") # no data
   # header <- paste0("areaCode=",code,"&") - error
   #header <- paste0("areaType=region&")
}

# Create URL for total cases, deaths, tests and vaccinations
casesurl <- paste0(baseurl,
                   header,
                   "metric=newCasesBySpecimenDate&",
                   "metric=newDeaths28DaysByDeathDate&",
                   "metric=newPCRTestsByPublishDate&",
                   "metric=newPeopleVaccinatedFirstDoseByVaccinationDate&",
                   "format=csv")

# Explicitly define the types for the columns
coltypes <- cols(col_character(), col_character(),col_character(),
                 col_date(format="%Y-%m-%d"), #col_integer(),
                 col_integer(),  col_integer(), col_integer())

# Read the cases, deaths and tests data
comdat <-  read_csv(file = casesurl, col_types = coltypes)
