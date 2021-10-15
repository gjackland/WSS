
source("CompartmentFunction.R")
source("Predictions.R")
source("CC_write.R")
#  Do the regions, after a full run of covid_trimmed
region="London"
londonurl="https://api.coronavirus.data.gov.uk/v2/data?areaType=region&areaCode=E12000007&metric=newCasesBySpecimenDateAgeDemographics&format=csv"

coltypes <- cols(col_character(), col_character(),col_character(),
                 col_date(format="%Y-%m-%d"), col_character(),
                 col_integer(), col_integer(), col_number())
# Remap the ages column to be the header rows, remove the unassigned,
# 60+ and 00_59 columns, filter dates to be between the start and end
# dates and order the output by date
london <-  read.csv(file=londonurl)
london <- london%>%
  select(date = date, age = age, values = cases) %>%
  pivot_wider(id_cols = date, names_from = age, values_from = values) %>%
  select(-unassigned, -"60+", -"00_59") %>%
  filter(date >= startdate & date <= enddate) %>%
  arrange(date)
for (iage in 2:length(london)){
  london[iage]<-Weekend(london %>% pull(iage)) 
}
london$date<-as.Date(london$date)
complondon<- Compartment(london,  covidsimAge, RawCFR, comdat,3,nrow(london))
jnk<-Predictions(complondon,R_London_BestGuess)
CC_write(jnk,region)
source("CompartmentFunction.R")
source("Predictions.R")
source("CC_write.R")