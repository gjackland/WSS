##### Cut and pasted from main code, so CFR work can run independently of R and container
#####  Figures and analysis for https://www.medrxiv.org/content/10.1101/2021.04.14.21255385v1
  ####  From here on we're reproducing figures from https://www.medrxiv.org/content/10.1101/2021.04.14.21255385v1
  ##### Fig 1. - Heatmaps ####
  groups = colnames(casedat[2:20])

  image(casedat$date, 1:19, as.matrix(casedat[2:20]),
        xlab = "Time", ylab = "Age group", col = hcl.colors(325, "Blues", rev = TRUE),
        axes = F, mgp = c(3.3, 1, 0))
  axis.Date(1, at=seq(min(casedat$date), max(casedat$date), by="1 month"), format="%m-%Y")
  axis(2, 1:19, labels = groups, las = 1, cex.axis = 0.8) +
  title(main = "Cases and Deaths")

  # ggplot alternative to the above
  library(dplyr)
  library(ggplot2)
  library(tidyr)

  mycols <- names(casedat)[2:ncol(casedat)]
  mindate <- format(min(casedat$date),"%d/%m/%y")
  maxdate <- format(max(casedat$date),"%d/%m/%y")

  casedat %>% pivot_longer(cols = mycols, names_to = "Ages", values_to = "cases") %>%
    ggplot(aes(x = date, y = Ages, z = cases)) +
    stat_summary_2d(fun = "sum", binwidth = c(1,1)) +
    ggtitle(paste0("Cases ",mindate," - ",maxdate)) +
    xlab("Time") + ylab("Age group") + theme_minimal() +
    scale_fill_continuous(high = "#273871", low = "#F4FAFE") +
    scale_x_date(date_labels = "%b-%y") +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(fill = "Cases")


  deathmap = image(deathdat$date, 1:19, as.matrix(deathdat[2:20]),
                   xlab = "", ylab = "", col = hcl.colors(96, "Reds", rev = TRUE),
                   axes = F, mgp = c(3.3, 1, 0))
  axis.Date(1, at=seq(min(deathdat$date), max(deathdat$date), by="1 month"), format="%m-%Y")
  axis(2, 1:19, labels = groups, las = 1, cex.axis = 0.8)
  title(main = "Deaths heatmap")
  rm(deathmap, groups)

  # Alternative ggplot version of the dath plot
  mycols <- names(deathdat)[2:ncol(deathdat)]
  mindate <- format(min(deathdat$date),"%d/%m/%y")
  maxdate <- format(max(deathdat$date),"%d/%m/%y")

  deathdat %>% pivot_longer(cols = mycols, names_to = "Ages", values_to = "deaths") %>%
    ggplot(aes(x = date, y = Ages, z = deaths)) +
    stat_summary_2d(fun = "sum", binwidth = c(1,1)) +
    ggtitle(paste0("Deaths ",mindate," - ",maxdate)) +
    xlab("Time") + ylab("Age group") + theme_minimal() +
    scale_fill_continuous(high = "#6D0026", low = "#FCF5F2") +
    scale_x_date(date_labels = "%b-%y") +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(fill = "Deaths")

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
  logcases <- deathdat
  gamcases <- logcases

  for (agegroup in 2:20) {
    for (day in 28:nrow(logcases)) {
      logcases[day,agegroup] = sum(casedat[(day-27):day,agegroup] * rev(lndist))
      gamcases[day,agegroup] = sum(casedat[(day-27):day,agegroup] * rev(gamdist))
    }
}
rm(agegroup, day)

  #Spread all cases by the distribution
  comdat$logcaseload <- 0
  comdat$gamcaseload <- 0
  for (day in 28:nrow(comdat)) {
    comdat$logcaseload[day] = sum(comdat$allCases[(day-27):day] * rev(lndist))
    comdat$gamcaseload[day] = sum(comdat$allCases[(day-27):day] * rev(gamdist))
  }
  scotdat$logcaseload <- 0
  scotdat$gamcaseload <- 0
  for (day in 28:nrow(comdat)) {
    scotdat$logcaseload[day] = sum(scotdat$allCases[(day-27):day] * rev(lndist))
    scotdat$gamcaseload[day] = sum(scotdat$allCases[(day-27):day] * rev(gamdist))
  }
  #  Spread Regional cases by the lognormal & gammadistribution
  reglnpredict<-regdeaths
  reggampredict<-regdeaths
  for (area in 2:length(regcases)){
    for (day in 28:nrow(comdat)){
      reglnpredict[day,area] = sum(regcases[(day-27):day,area] * rev(lndist))
      reggampredict[day,area] = sum(regcases[(day-27):day,area] * rev(gamdist))}}
  rm(day,area)


  #  Regional plots, with CFR input by hand  - obsolete.  Serves only to show how bad it is to exclude age and vaccine data
  #plot(regdeaths$London*55,x=regdeaths$date)
  #lines(reglnpredict$London,x=reglnpredict$date)
  #lines(reggampredict$London,x=reglnpredict$date)

  #plot(regdeaths$`North East`*55,x=regdeaths$date)
  #lines(reglnpredict$`North East`,x=reglnpredict$date)
  #plot(regdeaths$`North West`*55,x=regdeaths$date)
  #lines(y=reglnpredict$`North West`,x=reglnpredict$date)
  #plot(regdeaths$`South West`*55,x=regdeaths$date)
  #lines(reglnpredict$`South West`,x=reglnpredict$date)
  #plot(regdeaths$`South East`*55,x=regdeaths$date)
  #lines(reglnpredict$`South East`,x=reglnpredict$date)
  #plot(regdeaths$`East Midlands`*55,x=regdeaths$date)
  #lines(reglnpredict$`East Midlands` ,x=reglnpredict$date)
  #plot(regdeaths$`East of England`*55,x=regdeaths$date)
  #lines(reglnpredict$`East of England`,x=reglnpredict$date)
  #plot(regdeaths$`West Midlands`*55,x=regdeaths$date)
  #lines(reglnpredict$`West Midlands`,x=reglnpredict$date)
  #plot(regdeaths$`Yorkshire and The Humber`*55,x=regdeaths$date)
  #lines(reglnpredict$`Yorkshire and The Humber`,x=reglnpredict$date)

  for (area in 2:length(regcases)){
    lines(reglnpredict[2:279,area])}
  #Plots
  ggplot(logcases, aes(x = date)) +
    geom_line(aes(y = rowSums(logcases[,2:20])), na.rm = TRUE) +
    ggtitle("All age groups separately lognormal distributed")

  #### Fig 2. Distributions ####
  distdat = data.frame(days = 1:29, ln = c(lndist, 0), gam = c(gamdist, 0), exp = c(dexp(1:28, rate = 0.1), 0),
                       shift = c(rep(0, 14), 1, rep(0, 14)),
                       avgshift = c(rep(0, 11), rep((1/7),7), rep(0, 11)))
  ggplot(data = distdat, aes(x = days)) +
    geom_line(aes(y = ln, color = "Lognormal"), size = 1, na.rm = TRUE) +
    geom_line(aes(y = gam, color = "Gamma"), size = 1, na.rm = TRUE) +
    geom_line(aes(y = exp, color = "Exponential"), size = 1, na.rm = TRUE) +
    geom_line(aes(y = shift, color = "Shift"), size = 1, na.rm = TRUE) +
    geom_line(aes(y = avgshift, color = "7-day Average and Shift"), size = 1, na.rm = TRUE) +
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
  #  This line fit can be a problem if we don't run the main code starting in August
    model = lm(filter(comdat, date %in% daterange)$allDeaths ~ filter(WSS, date %in% daterange)$values)
    summary(model)

    #Get overall model fits
    model = lm(comdat$allDeaths[startdate:enddate] ~ gampred$allCasesPred[startdate:enddate])
    summary(model)

    model = lm(comdat$allDeaths[startdate:enddate] ~ logpred$allCasesPred[startdate:enddate])
    summary(model)

    model = lm(filter(comdat, date %in% WSS$date)$allDeaths ~ WSS$values)
    summary(model)
    rm(model)


  #### Model plots ####
  #Plot prediction against reality
  ggplot(data = comdat, aes(x = date)) +
    geom_line(mapping = aes(y = allDeaths, color = "Deaths (Government Figures)"), size = 1, na.rm = TRUE) +
    geom_line(data = gampred, aes(y = allCasesPred, color = "Gamma Model Predicted Deaths"), size = 1, na.rm = TRUE) +
    geom_line(data = logpred, aes(y = allCasesPred, color = "Lognormal Model Predicted Deaths"), size = 1, na.rm = TRUE) +
    geom_line(data = WSS, aes(y = values, color = "WSS Original"), size = 1, na.rm = TRUE) +
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
  rollframe$CFR =  rollframe$Deaths/rollframe$Cases
  rm(deathframe)
  rollframe = rollframe[301:(nrow(rollframe)-30),]


  ggplot() +
    geom_line(data = rollframe, aes(x = date, y = CFR, color = agegroup), size = 1.1, na.rm = TRUE) +
    scale_colour_manual(values = rev(brewer.pal(10,"Set3"))) +
    labs(title = paste("Case Fatality Ratios by age group -  7-day rolling averages"),
         subtitle = "Lognormal model",
         x = "Date", y = "CFR") +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    theme_bw() +
    geom_rect(aes(xmin=as.Date("2020/12/01"), xmax=as.Date("2021/01/16"), ymin=0, ymax=Inf), fill = "red", alpha = 0.1) +
    geom_rect(aes(xmin=as.Date("2021/01/17"), xmax=Sys.Date(), ymin=0, ymax=Inf), fill = "green", alpha = 0.1)


  #### Same thing with smoothing lognormal-derived CFRs ####
  rollframe = as.data.frame(apply(logcases[,2:20], 2, rollmean, 14, na.pad = T))
  rollframe$date = logcases$date
  rollframe = pivot_longer(rollframe, cols = colnames(logcases[11:20]),
                           names_to = "agegroup", names_prefix = "X", values_to = "Cases")

  deathroll = as.data.frame(apply(deathdat[,2:20], 2, rollmean, 14, na.pad = T))
  deathroll$date = deathdat$date
  deathframe = pivot_longer(deathroll, cols = colnames(deathdat[11:20]),
                            names_to = "agegroup", names_prefix = "X", values_to = "Deaths")

  rollframe$Deaths = deathframe$Deaths
  rollframe$CFR =  rollframe$Deaths/rollframe$Cases

  rollframe[is.na(rollframe)]=1.0
  rollframe[rollframe==Inf]=1.0
  rollframe[rollframe==-Inf]=1.0
  rm(deathframe)
  rollframe = rollframe[301:(nrow(rollframe)-30),]

  ggplot() +
    geom_smooth(method = NULL, span=0.3, data = rollframe, aes(x = date, y = CFR, color = agegroup), size = 1.1, na.rm = TRUE) +
    scale_colour_manual(values = rev(brewer.pal(10,"Set3"))) + ylim(0.0,0.5)
  labs(title = paste("Case Fatality Ratios by age group -  7-day rolling averages"),
       subtitle = "Lognormal model",
       x = "Date", y = "CFR") +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    theme_bw() +
    geom_rect(aes(xmin=as.Date("2020/12/01"), xmax=as.Date("2021/01/16"), ymin=0, ymax=1.0), fill = "red", alpha = 0.1) +
    geom_rect(aes(xmin=as.Date("2021/01/17"), xmax=Sys.Date(), ymin=0, ymax=1.0), fill = "green", alpha = 0.1)


  vacdat %>% filter( "2020/10/1"< date & date < endplot) %>%
    pivot_longer(!date,names_to = "agegroup", values_to="Vaccinations") %>%
    ggplot(aes(x=date, y=Vaccinations, colour=agegroup)) +
    coord_cartesian(ylim=c(0.0,1.0))+ geom_smooth(formula= y ~ x, method = "loess", span=0.2) +
    guides(color = "none") + facet_wrap(vars(agegroup))
