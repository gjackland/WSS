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

if(!interactive()){
  options(error = function() traceback(2))
}


# Set the working directory to be the same as to where the script is run from.
setwd(".")

source("json_wss.R")

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
# that corresponds to these.  Add connector to avoid name clash with CrystalCast
connector_region <- dataIn$region
connector_subregion <- dataIn$subregion

# Read these parameters to output again
calibrationDate <- dataIn$parameters$calibrationDate
calibrationCaseCount <- dataIn$parameters$calibrationCaseCount
calibrationDeathCount <- dataIn$parameters$calibrationDeathCount
interventionPeriods <- dataIn$parameters$interventionPeriods

if(connector_region != "GB" || connector_subregion != "GB-ENG") {
  # Any other regions are currently unsupported
  if(! interactive()){quit(status=10)}
}

source("covid_trimmed.r")

# Beginning of time series
t0 <-  min(dfR$date)

# Get the days for which data will be output
days <- as.integer(dfR$date - t0)

# Labels are optional
myCritRecov <- as.integer(rowSums(compEng$CRITREC[2:20]))
myCritical <- as.integer(rowSums(compEng$CRIT[2:20]))
myILI <- as.integer(rowSums(compEng$ILI[2:20]))
myMild <- as.integer(rowSums(compEng$MILD[2:20]))
mySARI <-  as.integer(rowSums(compEng$SARI[2:20]))
mynewCritRecov <- as.integer(rowSums(compEng$newCRITREC[2:20]))
mynewCritical <- as.integer(rowSums(compEng$newCRIT[2:20]))
mynewILI <- as.integer(rowSums(compEng$newILI[2:20]))
mynewMild <- as.integer(rowSums(compEng$newMILD[2:20]))
mynewSARI <-  as.integer(rowSums(compEng$newSARI[2:20]))
outputJSON(myt0 = t0,
           mydaysarray = days,
           myregion = connector_region,
           mysubregion = connector_subregion, # see https://en.wikipedia.org/wiki/ISO_3166-2:GB
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
           mySARI = as.integer(rowSums(compEng$SARI[2:20])),
           mycumCritRecov = cumsum(mynewCritRecov),
           mycumCritical = cumsum(mynewCritical),
           mycumILI = cumsum(mynewILI),
           mycumMild = cumsum(mynewMild),
           mycumSARI = cumsum(mynewSARI),
           myincDeath = as.integer(rowSums(compEng$DEATH[2:20]))
)

# This needs to be the last routine called for the connector, by default it returns
# success (0), if there is no success setStatus() should be called. By default
# it will return -1 but you can set a value setStatus(1). Any non-zero value
# will indicate a problem.  For interactive work "quit" can end Rstudio session altogether
if(! interactive()){quit(status=returnStatus())}
