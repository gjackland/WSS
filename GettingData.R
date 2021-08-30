#!/usr/bin/env Rscript
#
# Data used in making calculations is made available under an Open Government
# Licence. For more details see:
#
# http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/
#

#  ISSUES #####################################################################
#
# * newPCRTestsByPublishDate are all NULL for English regions for the total
#   cases, deaths and tests.
# * Data for all UK cases is not regional.
# * For the vaccination data what does "Transform the data to get vacdat
#   compatible with casedat (must be a better way!)." mean?
# * vacdat$`18_24`<-NULL, there is non-zero data there. Do you really just
#   want to remove it?
# * Have not included the scotdat data as this should (?) be avilable via
#   the UK data?
# * Not read in the scotdailycases - is this different from using the GB-SCT
#   case?
# * There appears to be no hospital data for the English regions.
# * No cases by age for GB_SCT, GB-WLS, GB-NIR
# * No vaccination data for GB-WLS, GB-NIR.
# * No death by age for GB-NIR.
# * No regional cases by age data for GB-WLS.


# Load packages -----------------------------------------------------------

# Read packages used by the script
library(readr, warn.conflicts = FALSE, quietly = TRUE)
library(dplyr, warn.conflicts = FALSE, quietly = TRUE)
library(tidyr, warn.conflicts = FALSE, quietly = TRUE)

# Region look-up tables ---------------------------------------------------

#
# UK country codes available from:
#
# https://geoportal.statistics.gov.uk/datasets/countries-december-2018-names-and-codes-in-the-united-kingdom/explore
#
# Nine English regions ONS codes as used to specify the data at the UK Gov
# Corona web site:
#
# https://coronavirus.data.gov.uk/
#
# 9 Englis region ONS codes:
#
# https://geoportal.statistics.gov.uk/datasets/ons::regions-december-2017-names-and-codes-in-england/explore
#
# There is also 7 English regions ONS codes.
#
# https://geoportal.statistics.gov.uk/datasets/nhs-england-region-april-2020-names-and-codes-in-england/explore?showTable=true
#
# All these codes and the data are made available under an Open Government Licence:
#
# http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/
#

# 9 English regions +  UK home nations
codeTo9region <- data.frame(
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

region9Tocode  <- data.frame(
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

# 7 English regions + 4 UK nations
codeTo7region <- data.frame(
  W92000004 = "GB-WLS",
  S92000003 = "GB-SCT",
  E92000001 = "GB-ENG",
  N92000002 = "GB-NIR",
  E40000003 = "London",
  E40000005 = "South East",
  E40000006 = "South West",
  E40000007 = "East of England",
  E40000008 = "Midlands",
  E40000009 = "North East and Yorkshire",
  E40000010 = "North West"
)

region7Tocode  <- data.frame(
  "GB-WLS" = "W92000004",
  "GB-SCT" = "S92000003",
  "GB-ENG" = "E92000001",
  "GB-NIR" = "N92000002",
  "London" = "E40000003",
  "South East" = "E40000005",
  "South West" = "E40000006",
  "East of England" = "E40000007",
  "Midlands" = "E40000008",
  "North East and Yorkshire" = "E40000009",
  "North West" = "E40000010"
)

# 9 to 7 region data mapping assumption:
#
# Midlands  = East Midlands + West Midlands
# E40000008 =  E12000002 + E12000005
#
# North East = North East and Yorkshire = North East + Yorkshire and The Humber
#              E40000009                = E12000001 + E12000003
#
# This means that when the Midlands come up then we have to pull data twice and
# add them up.

# Set region  -----------------------------------------------------------

# Region to get data for - eventually get this from the web-ui as a code.
# Translate the 7 region code to the 9 region one if necessary.
subregion <- "GB-SCT"

# Get the 9 region codes to deal with the special cases of two regions that
# need to be amalgamated.
if (subregion == "Midlands"){
  code <-  c("E12000002", "E12000005")
} else if (subregion == "North East"){
  code <- c("E12000001","E12000003")
} else {
  code <- region9Tocode[subregion]
}

# getURL function ---------------------------------------------------------

# Function to return one or more URLs to query for the requested data based
# on the region codes.
getURL <- function(code, query){

  # Set the url
  url <-  ""

  # Base URL to get the UK government data
  baseurl <- "https://api.coronavirus.data.gov.uk/v2/data?"

  # Are we dealing with an English subregion - start with E12
  isEngSubregion <-  grepl("^E12",code[1])

  if (!isEngSubregion) {
    header <- paste0("areaType=nation&","areaCode=",code,"&")
    url <- paste0(baseurl, header, query)
  } else {
    for(c in code){
      header <- paste0("areaType=region&","areaCode=",c,"&")
      if(url == ""){
        url <- paste0(baseurl, header, query)
      } else {
        url <- c(url, paste0(baseurl, header, query))
      }
    }
  }

  return(url)
}


# getData function --------------------------------------------------------

# Get data from the URLs - if more than one URL is specified it will do
# up to two fetches and add the two data frames.
getData <-  function(urls) {
  
  # Can only deal with up to two URLs.
  if(length(urls) > 2){
    stop("Can only deal with up to 2 URLs.")
  }

  # Data to be returned
  dat <- NULL

  # Flag to indicate if this is the first time over the loop
  first_time <-  TRUE

  # Loop over the URLs
  for(url in urls){

    # Get the data
    d <- read_csv(url,
                  progress = FALSE,
                  show_col_types = FALSE)

    if(nrow(d) == 0){
      warning("No data returned for ", url,".\n\n")
      next
    }

    # Only do once unless we need to get 2 data sets and merge the data
    if (first_time) {

      dat <- d
      first_time <- FALSE

    } else {

      # Intersection of data frame by start and end dates
      start <- max(min(d$date), min(dat$date))
      end   <- min(max(d$date), max(dat$date))

      # Filter data by the start and end date
      d1 <- d[start <= d$date & d$date <= end,]
      d2 <- dat[start <= dat$date & dat$date <= end,]

      # Check we have the same number of rows (no in between dates)
      if(nrow(d1) != nrow(d2)){
        stop("Data frames are not the of the same size.")
      }

      # Area codes
      d1areaCode <- unique(d1$areaCode)
      d2areaCode <- unique(d2$areaCode)

      # Reset the output data frame
      dat <- NULL

      # Set the areaCode and areaName for data frames that need to be merged
      # North East = North East and Yorkshire = North East + Yorkshire and The Humber
      #              E40000009                = E12000001 + E12000003
      if ((d1areaCode == "E12000003" & d2areaCode == "E12000001") |
          (d1areaCode == "E12000001" & d2areaCode == "E12000003")) {
         areaCode <- "E40000009"
         areaName <- "North East"
      }

      # Midlands  = East Midlands + West Midlands
      # E40000008 =  E12000002 + E12000005
      if ((d1areaCode == "E12000002" & d2areaCode == "E12000005") |
          (d1areaCode == "E12000005" & d2areaCode == "E12000002")) {
        areaCode <- "E40000008"
        areaName <- "Midlands"
      }

      # Loop round the columns to add the two data frames
      # NOTE will not work for character based columns!
      for (name in names(d1)) {

        if(name == "areaCode" | name == "areaName"){

          dat$areaCode <- rep(areaCode, nrow(d1))
          dat$areaName <- rep(areaName, nrow(d1))

        } else if (name == "date") {

          # Check dates match up
          if(all(d1$date == d2$date)){

            dat$date <- d1$date

          } else {

            stop("Not all dates match for ",d1$areaName," and ",
                 d2$areaName,".")
          }

        } else if (name == "areaType") {

          # Set the areaType to the same type it was before
          dat$areaType <- d1$areaType

        } else if (is.numeric(d1[[name]]) & is.numeric(d2[[name]])) {

          # Add columns together if both are numeric
          dat[name] <- d1[name] + d2[name]

        } else if (all(is.na(d1[[name]])) & all(is.na(d2[[name]]))){

          # If column is all NA set column to the same
          dat[name] <- d1[name]

          # Issue a warning message
          message("All values for ", name, " are NAs.")

        } else if (is.character(d1[[name]]) & is.character(d2[[name]])) {

          # if columns all characters check the values are the same and set
          # accordingly
          if(all(d1[[name]] == d2[[name]])){

            dat[name] <- d1[name]

          } else {

            # Otherwise issue a warning
            warning("The column ", name, " is a chacter vector but not all ",
                    "values are equal.")
          }

        } else {

          # Otherwise do not know how to handle this column and some additional
          # bit of code will be required.
          warning("Do not know how to handle ", name, ".")

        }
      } # End for loop round columns

    }

    # make sure we return a tibble
    dat <- as_tibble(dat)
  }

  return(dat)
}

# Set parameters ------------------------------------------------------------

# Base URL to get the UK government data
baseurl <- "https://api.coronavirus.data.gov.uk/v2/data?"


# Set parameters ------------------------------------------------------------

# Base URL to get the UK government data
baseurl <- "https://api.coronavirus.data.gov.uk/v2/data?"

# Start and end date for the data
# for the end date lose only the last day of data -
# use a tail correction for reporting delay
startdate <- as.Date("2020/07/25")
enddate <-  Sys.Date()-5

# Cases, deaths and tests -----------------------------------------

# Status
message("Getting cases, deaths and test data for ", subregion)

# Construct the query part of the URL NOT including the baseurl, the areaType
# and the areaRegion as that is generated from the codes in the getURL function.
query <- paste0("metric=newCasesBySpecimenDate&",
                "metric=newDeaths28DaysByDeathDate&",
                "metric=newPCRTestsByPublishDate&",
                "metric=newPeopleVaccinatedFirstDoseByVaccinationDate&",
                "format=csv")

# Get the URL(s)
urls <- getURL(code, query)

# Get the data.
comdat <- getData(urls)

# Check that we have data
if (is.null(comdat)){

  warning("No cases, deaths and test data for ",subregion,".")

} else {

  # Transform the data
  #
  # newPCRTestsByPublishDate are all NULL for English regions.
  #

  comdat <- comdat %>%  select(date,
                               allCases = newCasesBySpecimenDate,
                               allDeaths = newDeaths28DaysByDeathDate,
                               tests = newPCRTestsByPublishDate,
                               inputCases = newCasesBySpecimenDate,
                               fpCases = newCasesBySpecimenDate,
                               vaccines=newPeopleVaccinatedFirstDoseByVaccinationDate) %>%
    filter(date >= startdate & date <= enddate ) %>%
    arrange(date)


}


# All UK cases -------------------------------------------------------

# This is not regionally based. Not sure if it has to be generalised for the
# other regions/nations. Leave as is for now but needs reviewing. If needed
# will need to refactor the URL function.

# All UK cases (to estimate pre-Sept England Cases)
ukcaseurl <- paste0(baseurl,
                    "areaType=overview&",
                    "metric=newPCRTestsByPublishDate&",
                    "format=csv")

# Explicitly define the types for the columns
coltypes <- cols(col_character(), col_character(),col_character(),
                 col_date(format="%Y-%m-%d"), col_integer())

# Read the case data
ukcasedat <-  read_csv(file = ukcaseurl, col_types = coltypes)

# Transform the case data
ukcasedat <- ukcasedat %>%  select(date = date, tests = newPCRTestsByPublishDate) %>%
                            filter(date >= startdate &  date <= enddate ) %>%
                            arrange(date)

# Cases by age ------------------------------------------------------------

# Cases by age
message("Getting cases by age data for ", subregion)

# Construct the query
agequery <- paste0("metric=newCasesBySpecimenDateAgeDemographics&",
                   "format=csv")

# Get the URL(s)
urls <- getURL(code, agequery)

# Get the data.
casedat <- getData(urls)

# Check that we have data
if(is.null(casedat)){

  warning("No cases by age data for ",subregion,".")

} else {

  # Remap the ages column to be the header rows, remove the unassigned,
  # 60+ and 00_59 columns, filter dates to be between the start and end
  # dates and order the output by date
  casedat <- casedat %>%
    select(date = date, age = age, values = cases) %>%
    pivot_wider(id_cols = date, names_from = age, values_from = values) %>%
    select(-unassigned, -"60+", -"00_59") %>%
    filter(date >= startdate & date <= enddate) %>%
    arrange(date)
}

# Vaccination by age ------------------------------------------------------

# Status
message("Getting vaccination by age data for ", subregion)

# Vaccination start date
vacdate="2020-12-08"

# Query
vacquery <- paste0("metric=vaccinationsAgeDemographics&",
                   "format=csv")

# Get the URL(s)
urls <- getURL(code, vacquery)

# Get the data.
vacdat <- getData(urls)

# Check that we have data
if (is.null(vacdat)){

  warning("No vaccination data for ",subregion,".")

} else {

  # Transform the data to get vacdat compatible with casedat (must be a better way!).
  vacdat <- vacdat %>%
    select(datetmp = date, age = age,
           values = cumVaccinationFirstDoseUptakeByVaccinationDatePercentage) %>%
    pivot_wider(id_cols = datetmp, names_from = age, values_from = values) %>%
    filter(datetmp >=vacdate  & datetmp <= enddate) %>%
    arrange(datetmp)

  # Add vaccination data for the under 24s.
  vacdat$`18_24`<-NULL
  vacdat<-cbind('20_24'=0.0,vacdat)
  vacdat<-cbind('15_19'=0.0,vacdat)
  vacdat<-cbind('10_14'=0.0,vacdat)
  vacdat<-cbind('05_09'=0.0,vacdat)
  vacdat<-cbind('00_04'=0.0,vacdat)
  vacdat<-cbind(date=vacdat$datetmp,vacdat)
  vacdat$datetmp<-NULL

  if (!is.null(casedat)) {

    tmp <-casedat %>% filter(date < vacdate)
    tmp[2:20]=0.0
    vacdat <- bind_rows(tmp,vacdat)
    rm(tmp)

  }


  # convert to fraction
  vacdat[2:length(vacdat)]<-vacdat[2:length(vacdat)]/100

}


# Deaths by age -----------------------------------------------------------

# Status
message("Getting deaths by age data for ", subregion)

# Query
deathquery <- paste0("metric=newDeaths28DaysByDeathDateAgeDemographics&",
                     "format=csv")

# Get the URL(s)
urls <- getURL(code, deathquery)

# Get the data.
deathdat <- getData(urls)

# Check that we have data
if (is.null(deathdat)) {

  warning("No death by age data for ",subregion,".")

} else {

  # Map the ages column to become column headings for the different age groups
  # for dates between the start and end date inclusive and then ensure that we
  # end up with the same columns as for the case data above.
  deathdat <- deathdat %>%
    select(date = date, age = age, values = deaths) %>%
    pivot_wider(id_cols = date, names_from = age, values_from = values) %>%
    select(-"60+", -"00_59") %>%
    filter(date >= startdate & date <= enddate) %>%
    arrange(date) %>%
    select(names(casedat)) #deaths by age

}


# Regional data for deaths and cases by specimen date ---------------------

# Status
message("Getting regional data for ", subregion)

regquery <- paste0("metric=newDeaths28DaysByDeathDate&",
                   "metric=newCasesBySpecimenDate&",
                   "format=csv")

# Get the URL(s)
urls <- getURL(code, regquery)

# Get the data.
regdat <- getData(urls)

# Check that we have data
if (is.null(regdat)){

  warning("No regional data for ",subregion,".")

} else {

  # Transform the data
  regcases <- regdat %>%  select(date,areaName,areaCode,
                                 Cases = newCasesBySpecimenDate) %>%
    pivot_wider(id_cols = date, names_from = areaName, values_from = Cases) %>%
    filter(date >= startdate & date <= enddate ) %>%
    arrange(date)

  regdeaths <- regdat %>%  select(date,areaName,Deaths = newDeaths28DaysByDeathDate) %>%
    pivot_wider(id_cols = date, names_from = areaName, values_from = Deaths) %>%
    filter(date >= startdate & date <= enddate )%>%
    arrange(date)

}


# Get age data for regions because can't download simultaneously
regquery2 <- paste0("metric=newCasesBySpecimenDateAgeDemographics&",
                    "metric=newDeathsBySpecimenDateAgeDemographics&",
                    "format=csv")
# Get the URL(s)
urls <- getURL(code, regquery2)

# Get the data.
regagedat <- getData(urls)

# Check that we have data
if (is.null(regagedat)) {

  warning("No regional cases by age data for ",subregion,".")

} else {

  # Transform the data
  regagedat <- regagedat %>%  select(date, areaName, age, cases) %>%
    filter(date >= startdate & date <= enddate ) %>%
    arrange(date)
}


# R estimate --------------------------------------------------------------

# Read in the UK government R estimate data from a csv file
coltypes <- cols(
  Date = col_date(format = "%Y-%m-%d"), UK_LowerBound = col_number(),
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

# Read the data from a csv file.
Rest <- read_csv(file="data/R_estimate.csv", col_types = coltypes)

# Hospital data -----------------------------------------------------------

# Status
message("Getting hospital data for ", subregion)

#### Get the UK hospital data & Append data to tibble
####  MV beds = CRIT
####  hospitalCases = SARI+CRIT+CRITREC
hospquery <- paste0("metric=covidOccupiedMVBeds&",
                    "metric=hospitalCases&",
                    "metric=newAdmissions&",
                    "format=csv")

# Get the URL(s)
urls <- getURL(code, hospquery)

# Get the data.
d <- getData(urls)

# Check that we have data
if(is.null(d) > 0){

  warning("No hospital data for ",subregion,".")

} else {

  HospitalData <- tibble()
  HospitalData <- rev( bind_rows(HospitalData,d) )

  HospitalData  <-  HospitalData %>%
    filter(date >= startdate & date <= enddate ) %>%
    arrange(date)

}

