#!/usr/bin/env Rscript
#
# Code to write out excel using the CC Schema.
#

# Load packages
library(xlsx)
library(lubridate)


# Initiate variables
group <- "Edinburgh"
model <-  "WSS"
scenario <- "NowCast"
modeltype <- "Cases"
version <- 0.1
today <- today()
ageband <-  "All"
region <- "Scotland"
valuetype <- "R"

CC <- data.frame(
  Group = group,
  Model = model,
  Scenario = scenario,
  ModelType = modeltype,
  Version = version,
  "Creation Day" = day(today),
  "Creation Month" = month(today),
  "Creation Year" = year(today),
  "Day of Value" = NA,
  "Month of Value" = NA,
  "Year of Value" = NA,
  AgeBand = ageband,
  Geography = region,
  ValueType = valuetype,
  Value = NA,
  "Quantile 0.05" = NA,
  "Quantile 0.1" = NA,
  "Quantile 0.15" = NA,
  "Quantile 0.2" = NA,
  "Quantile 0.25" = NA,
  "Quantile 0.3" = NA,
  "Quantile 0.35" = NA,
  "Quantile 0.4" = NA,
  "Quantile 0.45" = NA,
  "Quantile 0.5" = NA,
  "Quantile 0.55" = NA,
  "Quantile 0.6" = NA,
  "Quantile 0.65" = NA,
  "Quantile 0.7" = NA,
  "Quantile 0.75" = NA,
  "Quantile 0.8" = NA,
  "Quantile 0.85" = NA,
  "Quantile 0.9" = NA,
  "Quantile 0.95" = NA
)

# Need to create a list to add more rows to the CrystalCast df
newrow <- list(group, model, scenario, modeltype, version, day(today),
               month(today), year(today), NA, NA, NA, ageband, region,
               valuetype, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
               NA, NA, NA, NA, NA, NA, NA, NA
               )

# Add the new row
CC <- rbind(CC, newrow)

# Write to excel
write.xlsx(CC, file = "Data/WSS_CC.xlsx", sheetName = "WSS", row.names = FALSE)
