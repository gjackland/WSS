# Function to output JSON for the web-ui interface.
library(jsonlite)

## Get input from the web-ui.
#
# For usage in the web-ui the filename has to be: /data/input/inputFile.json
# To use outwith the web-ui a sample file, data/sample-inputFile.json, has been
# provided. If the file cannot be found a warning message will be issued and
# execution will continue. Usage:
#
# dataIn <- getInput("data/sample-inputFile.json")
#
# The input schema can be found at:
#
# https://raw.githubusercontent.com/covid-policy-modelling/model-runner/epcc/packages/api/schema/input.json
#
# The following data is made available and the mechanism for accessing.
#
# dataIn$region - the region to be modeled
# dataIn$subregion - the subregion to be modeled
# dataIn$parameters$calibrationDate - An ISO-8601 string, e.g. "2021-06-17",
#                   encoding the date of the most recent case data in the region.
# dataIn$parameters$calibrationCaseCount -The total number of confirmed cases in
#                  the region before the calibration date.
# dataIn$parameters$calibrationDeathCount - The total number of deaths in the
#                  region before the calibration date.
# dataIn$parameters$interventionPeriods - A list of time periods, each with a
#                  different set of interventions. This may contain (only the
#                  startDate and reductionPopulationContact are required):
#
#      startDate - An ISO-8601 string encoding the date that these interventions
#                  begin.
#      reductionPopulationContact - The estimated reduction in population contact
#                  resulting from all of the above interventions. Some models
#                  require this generalized parameter instead of the individual
#                  interventions.
#      socialDistancing - The level of social distancing in the region.
#      schoolClosure - The level of school closure in the region.
#      voluntaryHomeQuarantine - The level to which entire households self-isolate
#                                when one member of the household has symptoms.
#      caseIsolation - The level to which individuals with symptoms self-isolate.
#
# dataIn$parameters$r0 - The assumed reproduction number for the virus.
#                  If this is null, then each model will use its own default value.
#

getInput <- function(filename)
{
    if(file.exists(filename)){
        jsonin <- read_json(filename)
        return(jsonin)
    }else{
        warning("File: ",filename," not found.")
    }
}

## Output Usage example
#
# # Build a list of interventions:
# int1 <- list(caseIsolation="aggressive",
#              reductionPopulationContact=38,
#              schoolClosure="aggressive",
#              startDate="2020-03-13",
#              voluntaryHomeQuarantine="aggressive")
# int2 <- list(caseIsolation="aggressive",
#              reductionPopulationContact=57,
#              schoolClosure="aggressive",
#              socialDistancing="moderate",
#              startDate="2020-03-19",
#              voluntaryHomeQuarantine="aggressive")
#
# interventions <- list(int1,int2)
#
# # Labels are optional unless arguments are given in a different
# # order. All arguments must be present
#
# outputJSON(myt0 = "2021-05-27",
#            mydaysarray = c(1,2,3,4,5,6),
#            myregion = "GB",
#            mysubregion = "ENG",
#            mycalibrationCaseCount = 43464,
#            mycalibrationDate = "2021-05-27",
#            mycalibrationDeathCount=1755,
#            myr0 = NA,
#            myinterventionPeriods= interventions,
#            myCritRecov = NA,
#            myCritical = NA,
#            myILI = NA,
#            myMild = NA,
#            myR = NA,
#            mySARI = NA,
#            mycumCritRecov = NA,
#            mycumCritical = NA,
#            mycumILI = NA,
#            mycumMild = NA,
#            mycumSARI = NA,
#            myincDeath = NA
# )
#
#---------------------------------------------------
# The JSON output schema:
#
# https://raw.githubusercontent.com/covid-policy-modelling/model-runner/epcc/packages/api/schema/output.json
#
# Output examples:
#
# https://github.com/covid-policy-modelling/model-runner/blob/main/packages/api/src/model-output.ts
# https://github.com/covid-policy-modelling/web-ui/tree/main/data
#
#---------------------------------------------------
#
# Arguments to the function:
#
# time0: An ISO-8601, e.g. "2021-05-27, string encoding the date that each timeseries begins.
# days: Each value is a number of days after `time0` that correspond to every series of metrics.
#       output for time0 counts as 0.
# myregion: ISO 3166 country code for the region, e.g. "GB",
#           see https://www.iso.org/iso-3166-country-codes.html
# mysubregion: ISO 3166-2:GB subregion code, e.g. ENG, NIR, SCT, WLS,
#              see https://en.wikipedia.org/wiki/ISO_3166-2:GB for more options
# mycalibrationCaseCount: The total number of confirmed cases in the region before the calibration date.
# mycalibrationDate: An ISO-8601 string encoding the date of the most recent case data in the region.
# mycalibrationDeathCount: The total number of deaths in the region before the calibration date.
# myr0: The assumed reproduction number for the virus. If this is NA, then each
#       model will use its own default value.
# myinterventionPeriods: a list array containing the following properties:
#     caseIsolation: The level to which individuals with symptoms self-isolate.
#                    One of "mild"/"moderate"/"aggressive"
#     reductionPopulationContact (required): The estimated reduction in population contact resulting from
#                                 all of the above interventions. Some models require this generalized
#                                 parameter instead of the individual interventions.
#                     Number.
#     schoolClosure: The level of school closure in the region.
#                    One of "mild"/"moderate"/"aggressive"
#     socialDistancing: The level of social distancing in the region.
#                    One of "mild"/"moderate"/"aggressive"
#     startDate (required): An ISO-8601 string encoding the date that these interventions begin.
#     voluntaryHomeQuarantine: The level to which entire households self-isolate when one member
#                    of the household has symptoms.
#                    One of "mild"/"moderate"/"aggressive"
#
# If an NA is supplied  to any of the following quantities an array of zeros will be created
# of the correct length as output.
#
# myCritRecov: The total number of confirmed cases in the region before the calibration date.
# myCritical: Current number of critical cases on this day (assume represents ICU demand).
# myILI: Current number of influenza-like illness cases on this day (assume represents GP demand).
# myMild: Current number of mild cases on this day.
# myR: R-number on this day.
# mySARI: Current number of Severe Acute Respiratory Illness cases on this day (assume represents hospital demand).
# mycumCritRecov: Total number of patients recovered from critical cases since the beginning of the epidemic.
# mycumCritical: Total number of critical cases since the beginning of the epidemic.
# mycumILI: Total number of influence-like illnesses since the beginning of the epidemic.
# mycumMild: Total number of mild cases since the beginning of the epidemic.
# mycumSARI: Total number of severe acute respiratory illnesses since the beginning of the epidemic.
# myincDeath: Number of deaths occurring on this day.
#---------------------------------------------------
outputJSON <- function(myt0,
                       mydaysarray,
                       myregion,
                       mysubregion,
                       mycalibrationCaseCount,
                       mycalibrationDate,
                       mycalibrationDeathCount,
                       myr0,
                       myinterventionPeriods,
                       myCritRecov,
                       myCritical,
                       myILI,
                       myMild,
                       myR,
                       mySARI,
                       mycumCritRecov,
                       mycumCritical,
                       mycumILI,
                       mycumMild,
                       mycumSARI,
                       myincDeath){
    ## Time section
    time0 <- myt0
    myextent <- c(min(mydaysarray),max(mydaysarray))

    mytime <- list(t0=time0,
                   timestamps=mydaysarray,
                   extent=myextent)

    ## Metadata section
    myparameters <- list(calibrationCaseCount=mycalibrationCaseCount,
                         calibrationDate=mycalibrationDate,
                         r0=myr0,
                         calibrationDeathCount=mycalibrationDeathCount,
                         interventionPeriods=myinterventionPeriods
    )

    mymetadata <- list(region=myregion,
                       subregion=mysubregion,
                       parameters=myparameters)

    ## Aggregates section

    # Create a default vector with 0s for data not available
    default <- numeric(length(mydaysarray))

    if(length(myCritRecov) == 1){if(is.na(myCritRecov)){myCritRecov <- default}}
    if(length(myCritical) == 1){if(is.na(myCritical)){myCritical <- default}}
    if(length(myILI) == 1){if(is.na(myILI)){myILI <- default}}
    if(length(myMild) == 1){if(is.na(myMild)){myMild <- default}}
    if(length(myR) == 1){if(is.na(myR)){myR <- default}}
    if(length(mySARI) == 1){if(is.na(mySARI)){mySARI <- default}}
    if(length(mycumCritRecov) == 1){if(is.na(mycumCritRecov)){mycumCritRecov <- default}}
    if(length(mycumCritical) == 1){if(is.na(mycumCritical)){mycumCritical <- default}}
    if(length(mycumILI) == 1){if(is.na(mycumILI)){mycumILI <- default}}
    if(length(mycumMild) == 1){if(is.na(mycumMild)){mycumMild <- default}}
    if(length(mycumSARI) == 1){if(is.na(mycumSARI)){mycumSARI <- default}}
    if(length(myincDeath) == 1){if(is.na(myincDeath)){myincDeath <- default}}

    myaggregates <- list(CritRecov = myCritRecov,
                         Critical = myCritical,
                         ILI = myILI,
                         Mild = myMild,
                         R = myR,
                         SARI = mySARI,
                         cumCritRecov = mycumCritRecov,
                         cumCritical = mycumCritical,
                         cumILI = mycumILI,
                         cumMild = mycumMild,
                         cumSARI = mycumSARI,
                         incDeath = myincDeath
    )

    modelVersion = Sys.getenv("WSS_VERSION")
    mymodel = list(name="WSS", modelVersion=modelVersion, connectorVersion=modelVersion)
    ## Build up the object to be output to JSON
    myobject <- list(time = mytime,
                     metadata = mymetadata,
                     model = mymodel,
                     aggregate = list(metrics=myaggregates))

    ## Output to JSON
    if(dir.exists("/data/output")){
        write_json(myobject, "/data/output/data.json", pretty = TRUE, auto_unbox = TRUE, na ="null")
    }else if(dir.exists("./data")){
         message("JSON output written to ./data/data.json")
         write_json(myobject, "./data/data.json", pretty = TRUE, auto_unbox = TRUE, na ="null")
    }else{
         toJSON(myobject,pretty = TRUE,auto_unbox = TRUE, na ="null")
    }

}

# Need to let the web interface know that the simulation has completed successfully.
# 0 - for success, any other number for a fail.

# Default is success
Status <- 0

# Function to set the status - assume it sets it to fail
setStatus <- function(stat=1){
    Status <<- stat
}

# Return the status
# A the end of the R code where this is used add the line:
#
# quit(status=returnStatus())
#
# If you call just returnStatus() the program will exit with:
#
# [1] 0
#
# which is not what you want.

returnStatus <- function(){
    return(Status)
}
