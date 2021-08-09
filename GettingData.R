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


# Region look-up -----------------------------------------------------------

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


# Download data -----------------------------------------------------------

# Region to get data for - eventually get this from the web-ui as a code.
# Translate the 7 region code to the 9 region one if necessary.
subregion <- "North East"

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

# Get data from the URLs.
getData <-  function(urls) {

  # Data to be returned
  dat <- ""

  # Flag to indicate if this is the first time over the loop
  first_time <-  TRUE

  # Loop over the URLs
  for(url in urls){

    # Get the data
    d <- read_csv(url,
                  progress = FALSE,
                  show_col_types = FALSE)

    if (first_time) {
      dat <- d
      first_time <- FALSE
    } else {

      # Intersection of dates
      start <- max(min(d$date), min(dat$date))
      end   <- min(max(d$date), max(dat$date))

      # Filter data by date
      d1 <- d[start <= d$date & d$date <= end,]
      d2 <- dat[start <= dat$date & dat$date <= end,]

      # Area codes
      d1areaCode <- unique(d1$areaCode)
      d2areaCode <- unique(d2$areaCode)

      # Check we have the smae number of rows
      if(nrow(d1) != nrow(d2)){
        stop("Data frames are not the of the same size.")
      }

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
      # Loop round the columns
      # NOTE will not work for character based columns!
      for (name in names(d1)) {
        if(name == "areaCode" | name == "areaName"){
          next # set these up later
        } else if (name == "date") {
          # Check dates match up
          if(all(d1$date == d2$date)){
            dat$date <- d1$date
          } else {
            stop("Not all dates match for ",d1$areaName," and ",d2$areaName,".")
          }
        } else if (name == "areaType") {
          dat$areaType <- d1$areaType
        } else if (is.numeric(d1[[name]]) & is.numeric(d2[[name]])) {
          # Add columns together
          dat[name] <- d1[name] + d2[name]
        } else if (all(is.na(d1[[name]])) & all(is.na(d2[[name]]))){
          dat[name] <- d1[name]
          message("All values for ", name, " are NAs.")
        } else if (is.character(d1[[name]]) & is.character(d2[[name]])) {
          if(all(d1[[name]] == d2[[name]])){
            dat[name] <- d1[name]
          } else {
            warning("The column ", name, " is a chacter vector but not all values are equal.")
          }
        } else {
          warning("Do not know how to handle ", name, ".")
        }
      } # End for loop

      # set values
      dat$areaCode <-  areaCode
      dat$areaName <- areaName
    }
  }

  return(dat)
}


# Get the data ------------------------------------------------------------

# Start and end date for the data
# for the end date lose only the last day of data -
# use a tail correction for reporting delay
startdate <- as.Date("2020/07/25")
enddate <-  Sys.Date()-5

# Construct the query part of the URL not including the baseurl, the areaType
# and the areaRegion as that is generated from the codes in the getURL function.
query <- paste0("metric=newCasesBySpecimenDate&",
                "metric=newDeaths28DaysByDeathDate&",
                "metric=newPCRTestsByPublishDate&",
                "metric=newPeopleVaccinatedFirstDoseByVaccinationDate&",
                "format=csv")

# Get the URL(s)
urls <- getURL(code, query)

# Get the data.
dat <- getData(urls)


