# Function to output JSON for the web-ui interface.
library(jsonlite)

# Temporary output JSON document.
json <- '{
    "time": {
        "t0": "2021-05-21",
        "timestamps": [1,2,3],
        "extent": [1,100]
    },
    "metadata": {
        "region": "GB",
        "subregion": "England",
        "parameters": {
            "calibrationCaseCount": 0,
            "calibrationDate": "2021-05-21",
            "r0": null,
            "calibrationDeathCount": 0,
            "interventionPeriods": []
        }
    },
    "aggregate": {
        "metrics": {
            "Critical": [],
            "CritRecov": [],
            "cumCritical": [],
            "cumCritRecov": [],
            "cumILI": [],
            "cumMild": [],
            "SARI": [],
            "cumSARI": [],
            "ILI": [],
            "incDeath": [],
            "Mild": [],
            "R": []
        }
    }

}
'

# The JSON output schema:
#
# https://raw.githubusercontent.com/covid-policy-modelling/model-runner/epcc/packages/api/schema/output.json
#
# Output examples:
#
# https://github.com/covid-policy-modelling/model-runner/blob/main/packages/api/src/model-output.ts
# https://github.com/covid-policy-modelling/web-ui/tree/main/data
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

outputJSON <- function(myt0,
                       mydaysarray,
                       myregion,
                       mysubregion,
                       mycalibrationCaseCount,
                       mycalibrationDate,
                       mycalibrationDeathCount,
                       myr0,
                       myinterventionPeriods,
                       myCritRecov

){
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

    if(is.na(myCritRecov)){myCritRecov <- default}

    myaggregates <- list(CritRecov = myCritRecov)

    ## Build up the object to be output to JSON
    myobject <- list(time = mytime,
                     metadata = mymetadata,
                     aggregates = myaggregates)

    ## Output to JSON
    toJSON(myobject,pretty = TRUE,auto_unbox = TRUE, na ="null")

}

# Build a list of interventions:
int1 <- list(caseIsolation="aggressive",
             reductionPopulationContact=38,
             schoolClosure="aggressive",
             startDate="2020-03-13",
             voluntaryHomeQuarantine="aggressive")
int2 <- list(caseIsolation="aggressive",
             reductionPopulationContact=57,
             schoolClosure="aggressive",
             socialDistancing="moderate",
             startDate="2020-03-19",
             voluntaryHomeQuarantine="aggressive")

interventions <- list(int1,int2)

# Example input - labels are optional unless arguments are given in a different
# order.
outputJSON(myt0 = "2021-05-27",
           mydaysarray = c(1,2,3,4,5,6),
           myregion = "GB",
           mysubregion = "ENG",
           mycalibrationCaseCount = 43464,
           mycalibrationDate = "2021-05-27",
           mycalibrationDeathCount=1755,
           myr0 = NA,
           myinterventionPeriods= interventions,
           myCritRecov = NA
           )



## Aggregates section
default <- vector(mode="numeric", length = length(mytimestamps))
MildCases <- default
InfluenzaLikeIllness <- default
SevereAcuteRespiratoryIllness <- default
CriticalCases <- default
CriticalRecovery <- default
DeathsonDay <- default
CummulativeMild <- default
CummulativeILI <- default
CummulativeSARI <- default
CummulativeCritical <- default
CummulativeCriticalRecovery <- default
Rnumber <- default

myaggregates <- list(Mild=MildCases,
                     ILI=InfluenzaLikeIllness,
                     SARI=SevereAcuteRespiratoryIllness,
                     Critical=CriticalCases,
                     CritRecov=CriticalRecovery,
                     incDeath=DeathsonDay,
                     cumMild=CummulativeMild,
                     cumILI=CummulativeILI,
                     cumSARI=CummulativeSARI,
                     cumCritical=CummulativeCritical,
                     cumCritRecov=CummulativeCriticalRecovery,
                     R=Rnumber
                     )

## Build up the object

myobject <- list(time=mytime, metadata=mymetadata,aggregates=myaggregates)

toJSON(myobject,pretty = TRUE,auto_unbox = TRUE, na ="null")
