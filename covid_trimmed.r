#!/usr/bin/env Rscript
#
# Weigh, Scale and Shift (WSS) Code
#
# Copyright 2021 Graeme Ackland, The University of Edinburgh
#
#### Header ####

if(interactive()){
  # Remove existing variables
  rm(list = ls())
}

library(haven, warn.conflicts = FALSE, quietly = TRUE)
library(reshape2, warn.conflicts = FALSE, quietly = TRUE)
library(stats, warn.conflicts = FALSE, quietly = TRUE)
library(dplyr, warn.conflicts = FALSE, quietly = TRUE)
library(tidyr, warn.conflicts = FALSE, quietly = TRUE)
library(ggplot2, warn.conflicts = FALSE, quietly = TRUE)
library(magrittr, warn.conflicts = FALSE, quietly = TRUE)
library(ggseas, warn.conflicts = FALSE, quietly = TRUE)
library(astsa, warn.conflicts = FALSE, quietly = TRUE)
suppressMessages(library(forecast, warn.conflicts = FALSE, quietly = TRUE))
library(effsize, warn.conflicts = FALSE, quietly = TRUE)
library(lubridate, warn.conflicts = FALSE, quietly = TRUE)
library(ggthemes, warn.conflicts = FALSE, quietly = TRUE)
library(zoo, warn.conflicts = FALSE, quietly = TRUE)
library(RColorBrewer, warn.conflicts = FALSE, quietly = TRUE)
suppressMessages(library(corrplot, warn.conflicts = FALSE, quietly = TRUE))
library(rjson, warn.conflicts = FALSE, quietly = TRUE)
library(tibble, warn.conflicts = FALSE, quietly = TRUE)
library(ggnewscale, warn.conflicts = FALSE, quietly = TRUE)
library(scales, warn.conflicts = FALSE, quietly = TRUE)

setwd(".")
options(scipen = 999)

#### Read data ####
#total cases, deaths, tests
comdat = read.csv(file = "https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=newCasesBySpecimenDate&metric=newDeaths28DaysByDeathDate&metric=newVirusTests&format=csv")
comdat = select(comdat, date = date, allCases = newCasesBySpecimenDate, allDeaths = newDeaths28DaysByDeathDate, tests = newVirusTests)
comdat$date = as.Date(comdat$date)
comdat = dplyr::filter(comdat, date >= as.Date("2020/07/25") & date <= Sys.Date()-7)
comdat = comdat[order(comdat$date),]
row.names(comdat) = 1:nrow(comdat)

#All UK cases (to estimate pre-Sept England Cases)
ukcasedat = read.csv(file = "https://api.coronavirus.data.gov.uk/v2/data?areaType=overview&metric=newVirusTests&format=csv")
ukcasedat = select(ukcasedat, date = date, tests = newVirusTests)
ukcasedat$date = as.Date(ukcasedat$date)
ukcasedat = dplyr::filter(ukcasedat, date >= as.Date("2020/07/25") & date <= Sys.Date()-7)
ukcasedat = ukcasedat[order(ukcasedat$date),]
row.names(ukcasedat) = 1:nrow(ukcasedat)


#cases by age
casedat = read.csv(file = "https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=newCasesBySpecimenDateAgeDemographics&format=csv")
casedat = select(casedat, date = date, age = age, values = cases)
casedat = pivot_wider(casedat, id_cols = date, names_from = age, values_from = values)
casedat = select(casedat, -unassigned, -"60+", -"00_59")
casedat$date = as.Date(casedat$date)
casedat = dplyr::filter(casedat, date >= as.Date("2020/07/25") & date <= Sys.Date()-7)
casedat = casedat[order(casedat$date),]
row.names(casedat) = 1:nrow(casedat)

#deaths by age
deathdat = read.csv(file = "https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=newDeaths28DaysByDeathDateAgeDemographics&format=csv")
deathdat = select(deathdat, date = date, age = age, values = deaths)
deathdat = pivot_wider(deathdat, id_cols = date, names_from = age, values_from = values)
deathdat = select(deathdat, -"60+", -"00_59")
deathdat$date = as.Date(deathdat$date)
deathdat = dplyr::filter(deathdat, date >= as.Date("2020/07/25") & date <= Sys.Date()-7)
deathdat = deathdat[order(deathdat$date),]
deathdat = deathdat[, names(casedat)]
row.names(deathdat) = 1:nrow(deathdat)

#### Get tests for England pre-Sept by taking the post-Sept fraction of all tests that were in england (0.867)
comdat$tests[1:58] = ukcasedat[1:58,"tests"] * 0.867
rm(ukcasedat)

#### Fig 1. - Heatmaps ####
groups = colnames(casedat[2:20])
# casemelt = melt(as.matrix(casedat[2:20]))
# deathmelt = melt(as.matrix(deathdat[2:20]))
# colourscheme = rgb(1-rescale(casemelt$value), 1, 1-rescale(deathmelt$value))
# casemelt$value2 = colourscheme
# casemap = ggplot() +
#   geom_tile(data = casemelt, aes(Var1, Var2, fill = value)) +
#   scale_fill_gradient(low = "white", high = "blue") +
#   new_scale_fill() +
#   geom_tile(data = deathmelt, aes(Var1, Var2, fill = value)) +
#   scale_fill_gradient(low = "black", high = "red") +
#   new_scale_fill() +
#   geom_tile(data = casemelt, aes(Var1, Var2, fill = value2)) +
#   scale_fill_identity() +
#   labs(x = "Date", y = "Age Group") +
#   theme(legend.position = "none",
#         panel.grid.major = element_blank(),
#         panel.border = element_blank(),
#         panel.background = element_blank())
# print(casemap)
# rm(casemap, casemelt, deathmelt, colourscheme)
#

image(casedat$date, 1:19, as.matrix(casedat[2:20]),
      xlab = "Time", ylab = "Age group", col = hcl.colors(96, "Blues", rev = TRUE),
      axes = F, mgp = c(3.3, 1, 0))
axis.Date(1, at=seq(min(casedat$date), max(casedat$date), by="1 month"), format="%m-%Y")
axis(2, 1:19, labels = groups, las = 1, cex.axis = 0.8)
title(main = "Cases and Deaths")


deathmap = image(deathdat$date, 1:19, as.matrix(deathdat[2:20]),
                xlab = "", ylab = "", col = hcl.colors(96, "Reds", rev = TRUE),
                axes = F, mgp = c(3.3, 1, 0))
axis.Date(1, at=seq(min(deathdat$date), max(deathdat$date), by="1 month"), format="%m-%Y")
axis(2, 1:19, labels = groups, las = 1, cex.axis = 0.8)
title(main = "Deaths heatmap")
rm(casemap, deathmap, groups)

#### AGE GROUPS - Lognormal distribution ####
##We are fixing parameters at the clinical levels from Hawryluk et al.
logmean = 2.534
logsd = 0.613
lndist = dlnorm(1:28, logmean, logsd) #params from Hawryluk et al.
ggplot(data.frame(index = 1:28, prop = lndist)) +
  geom_point(aes(x = index, y = prop)) +
  labs(title = "Discretised Lognormal Distribution (Hawryluk)") +
  xlab("Time to Death") +
  ylab("Proportion of day zero cases")
rm(logmean, logsd)

#Spread each age group's cases by the distribution
logcases = casedat
logcases[2:20] = NA_real_
for (agegroup in 2:20) {
  for (day in 28:nrow(logcases)) {
    logcases[day,agegroup] = sum(casedat[(day-27):day,agegroup] * rev(lndist))
  }
}
rm(agegroup, day)

#Spread all cases by the distribution
comdat$logcaseload = 0
for (day in 28:nrow(comdat)) {
  comdat$logcaseload[day] = sum(comdat$allCases[(day-27):day] * rev(lndist))
}
rm(day)

#Plots
logcasesageplot = ggplot(logcases, aes(x = date)) +
  geom_line(aes(y = rowSums(logcases[,2:20]))) +
  ggtitle("All age groups separately lognormal distributed")
logcasesageplot
rm(logcasesageplot)


#### AGE GROUPS - Gamma distribution ####
##We are fixing alpha at the clinical level of 4.447900991. Verity et al. find a global beta of 4.00188764
alpha = 4.447900991
beta = 4.00188764
gamdist = dgamma(1:28, shape = alpha, scale = beta) #params from Verity et al.
ggplot(data.frame(index = 1:28, prop = gamdist)) +
  geom_point(aes(x = index, y = prop)) +
  labs(title = "Discretised Gamma Distribution (Verity)") +
  xlab("Time to Death") +
  ylab("Proportion of day zero cases")
rm(alpha, beta)

#Spread each age group's cases by the distribution
gamcases = casedat
gamcases[2:20] = NA_real_
for (agegroup in 2:20) {
  for (day in 28:nrow(gamcases)) {
    gamcases[day,agegroup] = sum(casedat[(day-27):day,agegroup] * rev(gamdist))
  }
}
rm(agegroup, day)

#Spread all cases by the distribution
comdat$gamcaseload = 0
for (day in 28:nrow(comdat)) {
  comdat$gamcaseload[day] = sum(comdat$allCases[(day-27):day] * rev(gamdist))
}


#### Fig 2. Distributions ####
distdat = data.frame(days = 1:29, ln = c(lndist, 0), gam = c(gamdist, 0), exp = c(dexp(1:28, rate = 0.1), 0),
                     shift = c(rep(0, 14), 1, rep(0, 14)),
                     avgshift = c(rep(0, 11), rep((1/7),7), rep(0, 11)))
ggplot(data = distdat, aes(x = days)) +
  geom_line(aes(y = ln, color = "Lognormal"), size = 1) +
  geom_line(aes(y = gam, color = "Gamma"), size = 1) +
  geom_line(aes(y = exp, color = "Exponential"), size = 1) +
  geom_line(aes(y = shift, color = "Shift"), size = 1) +
  geom_line(aes(y = avgshift, color = "7-day Average and Shift"), size = 1) +
  labs(title = "Distributions", y = "Fraction of Deaths", x = "Days after case detected",
       colour = "Legend") +
  scale_color_manual(values = c("Lognormal" = "red", "Gamma" = "blue", "Exponential" = "green", "Shift" = "orange", "7-day Average and Shift" = "maroon")) +
  scale_x_continuous(breaks =  0:30) +
  coord_cartesian(ylim=c(0, 0.15)) +
  theme_bw()


#### AGE GROUPS - Gamma Model ####
#Calculate age-group CFRs to fit Oct-Nov from Gamma
gamageweights = data.frame(agegroup = names(casedat[2:20]), weight = 0, lowerbound = 0, upperbound = 0)
for (agegroup in 2:20) {
  daterange = seq.Date(as.Date("2020-10-01"), as.Date("2020-11-30"), by = "day")
  thesedeaths = unlist(filter(deathdat, date %in% daterange)[,agegroup])
  thesecases = unlist(filter(gamcases, date %in% daterange)[,agegroup])
  model = summary(lm(thesedeaths ~ thesecases))
  gamageweights[agegroup-1, "weight"] <- coef(model)[2,1]
  gamageweights[agegroup-1, "lowerbound"] <- coef(model)[2,1] - (2*coef(model)[2,2])
  gamageweights[agegroup-1, "upperbound"] <- coef(model)[2,1] + (2*coef(model)[2,2])
}
write.csv(gamageweights[10:19,], "forpub.csv")
rm(model)

gampred = gamcases
for (agegroup in 2:20) {
  gampred[,agegroup] = gamcases[, agegroup] * gamageweights$weight[agegroup-1]
}
gampred$allCasesPred = rowSums(gampred[,2:20])

#### AGE GROUPS - Lognormal Model ####
#Calculate age-group CFRs to fit Oct-Nov from Lognormal
logageweights = data.frame(agegroup = names(casedat[2:20]), weight = 0, lowerbound = 0, upperbound = 0)
for (agegroup in 2:20) {
  daterange = seq.Date(as.Date("2020-10-01"), as.Date("2020-11-30"), by = "day")
  thesedeaths = unlist(filter(deathdat, date %in% daterange)[,agegroup])
  thesecases = unlist(filter(logcases, date %in% daterange)[,agegroup])
  model = summary(lm(thesedeaths ~ thesecases))
  logageweights[agegroup-1, "weight"] <- coef(model)[2,1]
  logageweights[agegroup-1, "lowerbound"] <- coef(model)[2,1] - (2*coef(model)[2,2])
  logageweights[agegroup-1, "upperbound"] <- coef(model)[2,1] + (2*coef(model)[2,2])
}
rm(model)

logpred = logcases
for (agegroup in 2:20) {
  logpred[,agegroup] = logcases[, agegroup] * logageweights$weight[agegroup-1]
}
logpred$allCasesPred = rowSums(logpred[,2:20])

#### Original WSS (hardcoded) ####
WSS = data.frame(date = c("28/07/2020", "29/07/2020", "30/07/2020", "31/07/2020", "01/08/2020", "02/08/2020", "03/08/2020", "04/08/2020", "05/08/2020", "06/08/2020", "07/08/2020", "08/08/2020", "09/08/2020", "10/08/2020", "11/08/2020", "12/08/2020", "13/08/2020", "14/08/2020", "15/08/2020", "16/08/2020", "17/08/2020", "18/08/2020", "19/08/2020", "20/08/2020", "21/08/2020", "22/08/2020", "23/08/2020", "24/08/2020", "25/08/2020", "26/08/2020", "27/08/2020", "28/08/2020", "29/08/2020", "30/08/2020", "31/08/2020", "01/09/2020", "02/09/2020", "03/09/2020", "04/09/2020", "05/09/2020", "06/09/2020", "07/09/2020", "08/09/2020", "09/09/2020", "10/09/2020", "11/09/2020", "12/09/2020", "13/09/2020", "14/09/2020", "15/09/2020", "16/09/2020", "17/09/2020", "18/09/2020", "19/09/2020", "20/09/2020", "21/09/2020", "22/09/2020", "23/09/2020", "24/09/2020", "25/09/2020", "26/09/2020", "27/09/2020", "28/09/2020", "29/09/2020", "30/09/2020", "01/10/2020", "02/10/2020", "03/10/2020", "04/10/2020", "05/10/2020", "06/10/2020", "07/10/2020", "08/10/2020", "09/10/2020", "10/10/2020", "11/10/2020", "12/10/2020", "13/10/2020", "14/10/2020", "15/10/2020", "16/10/2020", "17/10/2020", "18/10/2020", "19/10/2020", "20/10/2020", "21/10/2020", "22/10/2020", "23/10/2020", "24/10/2020", "25/10/2020", "26/10/2020", "27/10/2020", "28/10/2020", "29/10/2020", "30/10/2020", "31/10/2020", "01/11/2020", "02/11/2020", "03/11/2020", "04/11/2020", "05/11/2020", "06/11/2020", "07/11/2020", "08/11/2020", "09/11/2020", "10/11/2020", "11/11/2020", "12/11/2020", "13/11/2020", "14/11/2020", "15/11/2020", "16/11/2020", "17/11/2020", "18/11/2020", "19/11/2020", "20/11/2020", "21/11/2020", "22/11/2020", "23/11/2020", "24/11/2020", "25/11/2020", "26/11/2020", "27/11/2020", "28/11/2020", "29/11/2020", "30/11/2020", "01/12/2020", "02/12/2020", "03/12/2020", "04/12/2020", "05/12/2020", "06/12/2020", "07/12/2020", "08/12/2020", "09/12/2020", "10/12/2020", "11/12/2020", "12/12/2020", "13/12/2020", "14/12/2020", "15/12/2020", "16/12/2020", "17/12/2020", "18/12/2020", "19/12/2020", "20/12/2020", "21/12/2020", "22/12/2020", "23/12/2020", "24/12/2020", "25/12/2020", "26/12/2020", "27/12/2020", "28/12/2020", "29/12/2020", "30/12/2020", "31/12/2020", "01/01/21", "02/01/21", "03/01/21", "04/01/21", "05/01/21", "06/01/21", "07/01/21", "08/01/21", "09/01/21", "10/01/21", "11/01/21", "12/01/21", "13/01/21", "14/01/21", "15/01/21", "16/01/21"),
                 values = c(15,15,14,15,16,16,17,17,17,18,19,18,17,17,17,16,16,14,13,13,13,12,12,12,12,12,12,12,13,14,14,13,15,20,24,27,29,31,38,41,39,39,39,38,38,36,37,39,40,41,43,46,48,50,53,58,61,63,64,70,76,81,86,94,101,107,117,125,136,144,151,152,156,165,172,177,185,194,204,210,222,239,251,264,274,280,287,294,294,297,304,312,313,314,337,347,356,356,359,365,374,373,383,391,397,402,402,398,391,383,369,355,340,332,322,305,293,284,277,273,267,266,268,268,267,268,269,271,275,274,277,285,297,309,320,333,356,374,392,409,425,437,447,466,487,502,497,459,483,516,510,589,646,690,730,778,801,890,884,889,918,980,959,939,921,904,885,870,849,821,856,804,751))
WSS$date = dmy(WSS$date)
WSS$date = WSS$date + 12

#### Model Fit Stats ####
#Get Autumn model fits
model = lm(filter(comdat, date %in% daterange)$allDeaths ~ filter(gampred, date %in% daterange)$allCasesPred)
summary(model)

model = lm(filter(comdat, date %in% daterange)$allDeaths ~ filter(logpred, date %in% daterange)$allCasesPred)
summary(model)

model = lm(filter(comdat, date %in% daterange)$allDeaths ~ filter(WSS, date %in% daterange)$values)
summary(model)

#Get overall model fits
model = lm(comdat$allDeaths ~ gampred$allCasesPred)
summary(model)

model = lm(comdat$allDeaths ~ logpred$allCasesPred)
summary(model)

model = lm(filter(comdat, date %in% WSS$date)$allDeaths ~ WSS$values)
summary(model)
rm(model)

#### Model plots ####
#Plot prediction against reality
ggplot(data = comdat, aes(x = date)) +
  geom_line(mapping = aes(y = allDeaths, color = "Deaths (Government Figures)"), size = 1) +
  geom_line(data = gampred, aes(y = allCasesPred, color = "Gamma Model Predicted Deaths"), size = 1) +
  geom_line(data = logpred, aes(y = allCasesPred, color = "Lognormal Model Predicted Deaths"), size = 1) +
  geom_line(data = WSS, aes(y = values, color = "WSS Original"), size = 1)
  labs(title = "Predicted Deaths vs. Actual Deaths", color = "Legend") +
  ylab("Deaths") +
  xlab("Date") +
  scale_color_manual(values = c("Deaths (Government Figures)" = "Blue",
                                "Gamma Model Predicted Deaths" = "Red",
                                "Lognormal Model Predicted Deaths" = "Green",
                                "WSS Original" = "Orange")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_bw()


#### Plot all lognormal-derived CFRs ####
rollframe = as.data.frame(apply(logcases[,2:20], 2, rollmean, 7, na.pad = T))
rollframe$date = logcases$date
rollframe = pivot_longer(rollframe, cols = colnames(logcases[11:20]),
                         names_to = "agegroup", names_prefix = "X", values_to = "Cases")

deathroll = as.data.frame(apply(deathdat[,2:20], 2, rollmean, 7, na.pad = T))
deathroll$date = deathdat$date
deathframe = pivot_longer(deathroll, cols = colnames(deathdat[11:20]),
                         names_to = "agegroup", names_prefix = "X", values_to = "Deaths")

rollframe$Deaths = deathframe$Deaths
rollframe$CFR = rollframe$Deaths/rollframe$Cases
rm(deathframe)
rollframe = rollframe[301:(nrow(rollframe)-30),]

plot = ggplot() +
  geom_line(data = rollframe, aes(x = date, y = CFR, color = agegroup), size = 1.1) +
  scale_colour_manual(values = rev(brewer.pal(10,"Set3"))) +
  labs(title = paste("Case Fatality Ratios by age group -  7-day rolling averages"),
       subtitle = "Lognormal model",
       x = "Date", y = "CFR") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_bw() +
  geom_rect(aes(xmin=as.Date("2020/12/01"), xmax=as.Date("2021/01/16"), ymin=0, ymax=Inf), fill = "red", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021/01/17"), xmax=Sys.Date(), ymin=0, ymax=Inf), fill = "green", alpha = 0.1)
print(plot)

