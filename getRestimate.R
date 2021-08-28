#!/usr/bin/env Rscript
#  Broken August 2021?
# Code to get the UK reproductive number (R) estimate.
# Data is made available under an Open Government Licence.
# For more details see:
#
# http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/
#

library(tibble, warn.conflicts = FALSE, quietly = TRUE)
library(readr, warn.conflicts = FALSE, quietly = TRUE)
library(dplyr, warn.conflicts = FALSE, quietly = TRUE)
library(readODS, warn.conflicts = FALSE, quietly = TRUE)
library(xml2, warn.conflicts = FALSE, quietly = TRUE)
library(rvest, warn.conflicts = FALSE, quietly = TRUE)

# From stackoverflow 14469522
stop_quietly <- function() {
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}

# Get the Government R estimates
# URL data of where the information is held
r_url <- "https://www.gov.uk/guidance/the-r-value-and-growth-rate"

# Get the URL that holds the time series
read_html(r_url) %>%
  html_nodes(xpath = '//a[contains(text(),"time series of published")]') %>%
  html_attr("href") -> r_url

# Get the file name from the URL
file <- basename(r_url)

# Print available data
message(paste0("\nCurrent data file available: ", file, "."))

# Create a data subdirectory if it does not exist
if (!dir.exists("data")) {
  dir.create("data")
}

# Download the file with the data if it does not already exist

if (!file.exists(paste0("data/uk-data", file))) {
  download.file(r_url, destfile = paste0("data/uk-data/", file), quiet = TRUE)
}else{
  message("Data file already exists locally, not downloading again.
          Terminating ...\n\n")
  stop_quietly()
}

# Read the contents of the file
# skip the first 8 rows, table header and merged cells (read can't handle)
# read "."s as NAs as the "." is used to mean not applicable

r_est <- read_ods(paste0("data/uk-data/", file), sheet = "Table1_-_R",
                 skip = 8, na = ".")

# Rename the columns
names(r_est) <- c("", "Date", "UK_LowerBound", "UK_UpperBound",
                 "England_LowerBound", "England_UpperBound",
                 "EEng_LowerBound", "EEng_UpperBound",
                 "Lon_LowerBound", "Lon_UpperBound", "Mid_LowerBound",
                 "Mid_UpperBound", "NEY_LowerBound", "NEY_UpperBound",
                 "NW_LowerBound", "NW_UpperBound", "SE_LowerBound",
                 "SE_UpperBound", "SW_LowerBound", "SW_UpperBound")

# Remove the first column that contains nothing
r_est <- r_est[, -1]

# Convert to a tibble
r_est <- as_tibble(r_est)

# Convert character dates to dates
r_est$Date <- as.Date(r_est$Date, format = "%d-%b-%y")

# Remove NA values
r_est %>% filter(!is.na(Date)) -> r_est

# Write the data to a CSV file
write_csv(r_est, file = "data/R_estimate.csv")

