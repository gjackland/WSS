#!/usr/bin/env Rscript

# Read packages used by the script
library(readr, warn.conflicts = FALSE, quietly = TRUE)
library(dplyr, warn.conflicts = FALSE, quietly = TRUE)

# Base URL to get the data
baseurl <- "https://api.coronavirus.data.gov.uk/v2/data?"

# Getting data should observe a fair usage policy.
#
# Due to the large volume of data that may be downloaded through this page,
# a fair usage policy applies:
#
# Throttling: Each user is limited to 10 download requests per any 100–second period,
#             with a maximum rate limit of 100 per hour.
# Metric limit: Each download request may contain up to a maximum number of 5 metrics.
#               This excludes the default metrics.
# Freshness: Identical requests are only refreshed once every 150 seconds.
#

# Construct a look up table for Regions (a named vector):
#
# East Midlands	            E12000004
# East of England	            E12000006
# London	                    E12000007
# North East	                E12000001
# North West	                E12000002
# South East	                E12000008
# South West	                E12000009
# West Midlands	            E12000005
# Yorkshire and The Humber	    E12000003
#
regions <- c("E12000004","E12000006","E12000007","E12000001","E12000002",
             "E12000008","E12000009","E12000005","E12000003")
names(regions) <- c("East Midlands","East of England","London","North East","North West",
                    "South East","South West","West Midlands","Yorkshire and The Humber")

# Age demographics cannot are not compatible with the other quantities so we
# need to be able to construct two different types of URLs
getAgeDeathDemographicsURL <- function(reg){
    baseurl <- "https://api.coronavirus.data.gov.uk/v2/data?"
    area <- paste0("areaCode=",reg,"&")
    u <- paste0(baseurl,
                "areaType=region&",
                 area,
                "metric=newDeaths28DaysByDeathDateAgeDemographics&",
                "format=csv")
    return(u)
}

getDeathURL <- function(reg){
    baseurl <- "https://api.coronavirus.data.gov.uk/v2/data?"
    area <- paste0("areaCode=",reg,"&")
    u <- paste0(baseurl,
                "areaType=region&",
                 area,
                "metric=newOnsDeathsByRegistrationDate&",
                "metric=newDeaths28DaysByDeathDate&",
                "metric=newCasesBySpecimenDate&",
                "format=csv")
    return(u)
}

# Column types for the newDeaths28DaysByDeathDateAgeDemographics
democols <- cols(
    areaCode = col_character(),
    areaName = col_character(),
    areaType = col_character(),
    date = col_date(format = "%Y-%m-%d"),
    age = col_character(),
    deaths = col_double(),
    rollingSum = col_double(),
    rollingRate = col_double()
)

# tibble for death demographics
AgeDeathDemograhics <- tibble()

# Loop round the regions
for(region in names(regions)){

    # Diagnostic message
    message(paste("Getting data for",region))

    # Get the Age Death demographic URL for the region
    url1 <- getAgeDeathDemographicsURL(regions[region])

    # Get the data
    d <- read_csv(url1,col_types = democols)

    # Append data to tibble
    AgeDeathDemograhics <- bind_rows(AgeDeathDemograhics,d)
}

# Write data to a file
write_csv(AgeDeathDemograhics,"data/AgeDeathDemographics.csv")

# Get hospital data
HospitalURL <- function(reg){
  u <- paste0(baseurl,
             "areaType=overview&",
             "metric=covidOccupiedMVBeds&",
             "metric=hospitalCases&",
             "metric=newAdmissions&",
             "format=csv")
 return(u)
}

# Define the URL
urlh <- HospitalURL()

# Column types
coltypes <- cols(
  areaCode = col_character(),
  areaName = col_character(),
  areaType = col_character(),
  date = col_date(format = "%Y-%m-%d"),
  covidOccupiedMVBeds = col_double(),
  hospitalCases = col_double(),
  newAdmissions = col_double()
)

# Get the data
d <- read_csv(urlh, col_types = coltypes)

# Append data to tibble
HospitalData <- tibble()
HospitalData <- rev(bind_rows(HospitalData,d))

