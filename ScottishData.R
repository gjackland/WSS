# Routines and arrays used to derive Scottish data


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
startdate <- as.Date("2020/07/25")

# To one week ago (-7)
enddate <-  Sys.Date()-7

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

# Get all the data - ALL English data - no Scottish regions
allagedat <- read_csv(ageurl, col_types = coltypes)

# Restrict to Scottish regions and change from long to wide format
# allagedat %>% filter(areaName %in% ScottishAuthorities) %>%  # Keep only Scottish regions
#               select(date = date, age = age, values = cases, area = areaName) %>%
#               pivot_wider(names_from = age, values_from = values ) %>%
#               filter(date >= startdate & date <= enddate) %>%
#               arrange(date) -> scotagedat

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

# Define the columns
coltypes <- cols(
                  Date = col_date(format = "%Y%m%d"),
                  HB = col_character(),
                  HBQF = col_character(),
                  HBName = col_character(),
                  NewPositive = col_double(),
                  TotalCases = col_double(),
                  CrudeRatePositive = col_double(),
                  NewDeaths = col_double(),
                  TotalDeaths = col_double(),
                  CrudeRateDeaths = col_double(),
                  TotalNegative = col_double(),
                  CrudeRateNegative = col_double()
                )

# Get the data
scotdat <- read_csv(scoturl,col_types = coltypes)

# Daily Case Trends By Health Board
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

# Get the data
dailycases = read_csv(dailycasesurl, col_types = coltypes)

# Daily Case Trends By Age and Sex
# See:
# https://www.opendata.nhs.scot/dataset/covid-19-in-scotland/resource/9393bd66-5012-4f01-9bc5-e7a10accacf4

# Data URL
ageurl <- "https://www.opendata.nhs.scot/dataset/b318bddf-a4dc-4262-971f-0ba329e09b87/resource/9393bd66-5012-4f01-9bc5-e7a10accacf4/download/trend_agesex_20210610.csv"

# Define the column types
coltypes <- cols(
                Date = col_date(format = "%Y%m%d"),
                Country = col_character(),
                Sex = col_character(),
                SexQF = col_character(),
                AgeGroup = col_character(),
                AgeGroupQF = col_character(),
                DailyPositive = col_double(),
                CumulativePositive = col_double(),
                CrudeRatePositive = col_double(),
                DailyDeaths = col_double(),
                CumulativeDeaths = col_double(),
                CrudeRateDeaths = col_double(),
                CumulativeNegative = col_double(),
                CrudeRateNegative = col_double()
                )

# Get the data
scotagedat <- read_csv(ageurl, col_types = coltypes)

# Querying CKAN -----------------------------------------------------------


# install and load the package
# install.packages("ckanr")
library(ckanr)

# Set up
ckanr_setup(url = "https://www.opendata.nhs.scot/")

# List data sets
package_list(as="table")

tags <- tag_list(as="table")

corona <- tag_show("coronavirus", as = "table")

pckg <- package_show("covid-19-wider-impacts-deaths", as ="table")
