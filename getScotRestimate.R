#!/usr/bin/env Rscript
#
# Code to get the UK reproductive number (R) from UK.
#
library(tibble, warn.conflicts = FALSE, quietly = TRUE)
library(readr, warn.conflicts = FALSE, quietly = TRUE)
library(dplyr, warn.conflicts = FALSE, quietly = TRUE)
library(tidyr, warn.conflicts = FALSE, quietly = TRUE)
library(readxl, warn.conflicts = FALSE, quietly = TRUE)
library(xml2, warn.conflicts = FALSE, quietly = TRUE)
library(rvest, warn.conflicts = FALSE, quietly = TRUE)

# From stackoverflow 14469522
stop_quietly <- function() {
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}

# Get the Scottish Government R estimates from the following website
baseurl <- "https://data.gov.scot/coronavirus-covid-19"

# URL data of where the information is held
Rurl <- paste0(baseurl,"/data.html")

# Get the URL that holds the time series
read_html(Rurl) %>% html_nodes(xpath='//a[contains(text(),"Download the data")]') %>%
  html_attr("href") -> Rpath

# Get the file name from the URL
file <- basename(Rpath)

# Construct the full data URL
dataurl <-paste0(baseurl,"/",Rpath)

# Print available data
message(paste0("\nCurrent data file available: \"",gsub("%20"," ",file),"\"."))

# Substitute embedded spaces by underscores.
file <- gsub("%20","_",file)
# Remove brackets
file <- gsub("[()]","",file)

# Create a data subdirectory if it does not exist
if(!dir.exists("data")){
  dir.create("data")
}

# Download the file with the data if it does not already exist
if(!file.exists(paste0("data/",file))){
  download.file(dataurl,destfile = paste0("data/",file),quiet = TRUE)
}else{
  message("Data file already exists locally, not downloading again. Terminating ...\n\n")
  stop_quietly()
}

# Read the contents of the file
# skip the first 3 rows, suppress warning messages about Notes not being dates.
options(warn=-1)
Rest <- suppressMessages(read_excel(paste0("data/",file), sheet = "1.1_R", skip=3,
                   col_types = c("date","text","numeric")))
options(warn=1)

# Remove null values
Rest %>% filter(!is.na(Date)) -> Rest

# Get rid of the time in the dates
Rest$Date <- as.Date(Rest$Date,format="%Y-%m-%d")

# Change into a wide format
Rest %>%  pivot_wider(names_from=Variable, values_from = Value) %>%
          select(Date,R_LowerBound=`R lower bound`,R_UpperBound=`R upper bound`) -> Rest


# Write the data to a CSV file
write_csv(Rest,file="data/R_scottish_estimate.csv")

