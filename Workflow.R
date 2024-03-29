# Remove existing variables if in an interactive session.
if(interactive()){
  rm(list = ls())
}
# install and load the package previously
# install.packages("devtools")
#devtools::install_github("datasciencescotland/opendatascot")
# Read packages used by all scripts
library(readr, warn.conflicts = FALSE, quietly = TRUE)
library(dplyr, warn.conflicts = FALSE, quietly = TRUE)
library(tidyr, warn.conflicts = FALSE, quietly = TRUE)
library(ggplot2, warn.conflicts = FALSE, quietly = TRUE)
library(lubridate, warn.conflicts = FALSE, quietly = TRUE)
library(zoo, warn.conflicts = FALSE, quietly = TRUE)
library(RColorBrewer, warn.conflicts = FALSE, quietly = TRUE)
library("readxl")
library("openxlsx")
library("ckanr")
# install and load the packages once
##install.packages("Rtools")
#install.packages("devtools")
#devtools::install_github("datasciencescotland/opendatascot")

#  getData and spim_trust are alternate (online or sftp) sources of case data
source("./getParms.R")
#source("./spim_trust.R")  To be sorted out
source("./getData.r")
source("./Rnumber.R")
source("./Regional.R")
