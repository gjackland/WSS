#  set up two dataframes, rat is the same size as regcases dfR size of comdat, to store r number estimates

rat <- regcases

# Create a vector to hold the results for various R-numbers
ninit <- as.numeric(1:nrow(comdat))/as.numeric(1:nrow(comdat))
dfR <- data.frame(x=1.0:length(comdat$date),
                  date=comdat$date, itoR=ninit, stratR=ninit, rawR=ninit,  fpR=ninit,  weeklyR=ninit,  bylogR=ninit,
                  ONSEng=ninit, ONSScot=ninit, p00=ninit,  p05=ninit,  p10=ninit,  p15=ninit,  p20=ninit,  p25=ninit,  p30=ninit,
                  p35=ninit,  p40=ninit,  p45=ninit,  p50=ninit,  p55=ninit,  p60=ninit,  p65=ninit,
                  p70=ninit,  p75=ninit,  p80=ninit,  p85=ninit,  p90=ninit, x05=ninit, x15=ninit,
                  x25=ninit, x45=ninit, x65=ninit, x75=ninit, regions=ninit, smoothcasesR=ninit)
# Ito, Stratanovitch and exponential calculus
# rawR averages cases over previous genTime days - assumes genTime is the same as infectious period

for(i in (2:nrow(regcases))    ){
  rat[i, 2:ncol(regcases)] <- 1 + log(regcases[i, 2:ncol(regcases)]/regcases[(i-1), 2:ncol(regcases)])*genTime
}

# Reset first row to 1, because there's no data
# Fix R=1 not NaN or Inf when previous cases are zero
# Its not really defined.  This generates a warning which we can ignore
rat[1, 2:ncol(regcases)] <- 1.0
rat[is.na(rat)] <- 1.0
rat[rat==Inf] <- 1.0
rat[rat==-Inf] <- 1.0

startplot <- rat$date[1]
endplot <- enddate


if(interactive()){
  plot(smooth.spline(rat$Scotland[startplot <= rat$date & rat$date <= endplot],df=14)$y,
       x=rat$date[startplot <= rat$date & rat$date <= endplot],
       ylim=c(0.7,1.40),xlab="Date",ylab="R, Scotland")

  rat %>% filter(startplot < date & date < endplot) %>%
    pivot_longer(!date,names_to = "Region", values_to="R") %>%
    ggplot(aes(x=date, y=R, colour=Region)) + coord_cartesian(ylim=c(0.8,1.5)) +
    geom_smooth(formula= y ~ x, method = "loess", span=0.3) +  guides(color = "none") +
    facet_wrap(vars(Region)) +
    theme(axis.text.x=element_text(angle=90,hjust=1)) +xlab("Date")

  #  Plot UK nations and English regions
  rat[,c(1,2,3,4,5,6,7,8,9,10,11,12,13)]%>% filter(startplot < date & date < endplot) %>%
    pivot_longer(!date,names_to = "Region", values_to="R") %>%
    ggplot(aes(x=date, y=R, colour=Region)) +
    coord_cartesian(ylim=c(0.5,1.9))+ geom_smooth(formula= y ~ x, method = "loess", span=0.3) +
    guides(color = "none") + facet_wrap(vars(Region)) +
    theme(axis.text.x=element_text(angle=90,hjust=1)) +xlab("Date")

  #  Plot Scottish regions
  rat[,c(1,11,14,15,16,17,18,19,20,21,22,23,24,25,26,27)]%>% filter(startplot < date & date < endplot) %>%
    pivot_longer(!date,names_to = "Region", values_to="R") %>%
    ggplot(aes(x=date, y=R, colour=Region)) +
    coord_cartesian(ylim=c(0.5,1.9))+ geom_smooth(formula= y ~ x, method = "loess", span=0.3) +
    guides(color = "none") + facet_wrap(vars(Region)) +
    theme(axis.text.x=element_text(angle=90,hjust=1)) +xlab("Date")

}


#  Generate R over all ages, with some options for the calculus  itoR is Ito, stratR is stratonovich, bylogR is harmonic Ito fpR includes false positive correction
#  Avoid zero cases in R-calculation
casedat[casedat == 0] <- 1

for(i in ((genTime+1):length(dfR$itoR))){
  dfR$itoR[i]=(1+(comdat$allCases[i]-comdat$allCases[i-1])*genTime/(comdat$allCases[i-1]))
  dfR$stratR[i]=1+ (comdat$allCases[i]-comdat$allCases[i-1])*genTime/mean(comdat$allCases[(i-1):i])
  dfR$fpR[i]=(1+(comdat$fpCases[i]-comdat$fpCases[i-1])*genTime/(comdat$fpCases[i-1]))
  dfR$bylogR[i]=1+log(comdat$allCases[i]/comdat$allCases[i-1])*genTime
  dfR$ONSEng[i]=1+log(comdat$Eng_ons_inc[i]/comdat$Eng_ons_inc[i-1])*genTime
  dfR$ONSScot[i]=1+log(comdat$Scot_ons_inc[i]/comdat$Scot_ons_inc[i-1])*genTime
  dfR$regions[i]=1+log(regcases$regions[i]/regcases$regions[i-1])*genTime
  dfR$p00[i]=1+log(casedat$'00_04'[i]/casedat$'00_04'[i-1])*genTime
  dfR$p05[i]=1+log(casedat$'05_09'[i]/casedat$'05_09'[i-1])*genTime
  dfR$p10[i]=1+log(casedat$'10_14'[i]/casedat$'10_14'[i-1])*genTime
  dfR$p15[i]=1+log(casedat$'15_19'[i]/casedat$'15_19'[i-1])*genTime
  dfR$p20[i]=1+log(casedat$'20_24'[i]/casedat$'20_24'[i-1])*genTime
  dfR$p25[i]=1+log(casedat$'25_29'[i]/casedat$'25_29'[i-1])*genTime
  dfR$p30[i]=1+log(casedat$'30_34'[i]/casedat$'30_34'[i-1])*genTime
  dfR$p35[i]=1+log(casedat$'35_39'[i]/casedat$'35_39'[i-1])*genTime
  dfR$p40[i]=1+log(casedat$'40_44'[i]/casedat$'40_44'[i-1])*genTime
  dfR$p45[i]=1+log(casedat$'45_49'[i]/casedat$'45_49'[i-1])*genTime
  dfR$p50[i]=1+log(casedat$'50_54'[i]/casedat$'50_54'[i-1])*genTime
  dfR$p55[i]=1+log(casedat$'55_59'[i]/casedat$'55_59'[i-1])*genTime
  dfR$p60[i]=1+log(casedat$'60_64'[i]/casedat$'60_64'[i-1])*genTime
  dfR$p65[i]=1+log(casedat$'65_69'[i]/casedat$'65_69'[i-1])*genTime
  dfR$p70[i]=1+log(casedat$'70_74'[i]/casedat$'70_74'[i-1])*genTime
  dfR$p75[i]=1+log(casedat$'75_79'[i]/casedat$'75_79'[i-1])*genTime
  dfR$p80[i]=1+log(casedat$'80_84'[i]/casedat$'80_84'[i-1])*genTime
  dfR$p85[i]=1+log(casedat$'85_89'[i]/casedat$'85_89'[i-1])*genTime
  dfR$p90[i]=1+log(casedat$'90+'[i]/casedat$'90+'[i-1])*genTime
#   Same from CrystalCast age groupings
   dfR$x05[i]=1+log(xcastage$`05_14`[i]/xcastage$`05_14`[i-1])*genTime
   dfR$x15[i]=1+log(xcastage$`15_24`[i]/xcastage$`15_24`[i-1])*genTime
   dfR$x25[i]=1+log(xcastage$`25_44`[i]/xcastage$`25_44`[i-1])*genTime
   dfR$x45[i]=1+log(xcastage$`45_64`[i]/xcastage$`45_64`[i-1])*genTime
   dfR$x65[i]=1+log(xcastage$`65_74`[i]/xcastage$`65_74`[i-1])*genTime
   dfR$x75[i]=1+log(xcastage$'75+'[i]/xcastage$'75+'[i-1])*genTime
#  dfR$smoothcasesR[i]=1+log(smoothcases$y[i]/smoothcases$y[i-1])*genTime
}

dfR$smoothRlog <- smooth.spline(dfR$bylogR,df=20,w=sqrt(comdat$allCases))$y
dfR$smoothRito <- smooth.spline(dfR$itoR,df=20,w=sqrt(comdat$allCases))$y
dfR$smoothRstrat <- smooth.spline(dfR$stratR,df=20,w=sqrt(comdat$allCases))$y
dfR$loessR <- predict(loess(bylogR~x,data=dfR,span=0.25))
dfR[is.na(dfR)] <- 1.0
dfR[dfR == Inf] <- 1.0
dfR[dfR == -Inf] <- 1.0

# Set day 1, for plotting purposes
for (i in 3:nrow(dfR)){dfR[i,1] <- dfR[i,2]}

for(i in 4:(nrow(dfR)-3)){
    day1 <- i-3
    day7 <- i+3
    dfR$weeklyR[i] <- sum(dfR$itoR[day1:day7])/7.0
}

# End effect
dfR$weeklyR[length(dfR$weeklyR)] <- 1.0
dfR$weeklyR[length(dfR$weeklyR)-1] <- 1.0
dfR$weeklyR[length(dfR$weeklyR)-2] <- 1.0

# Plot various types of smoothing on the R data

# Making the time windows agree
Govdat <- Rest[Rest$Date >= min(comdat$date) & Rest$Date <= max(comdat$date),]

# Parameters for fitting splines and Loess
nospl <- 8
spdf <- length(dfR$rawR)/14
lospan <- 0.3

smoothweightR <- smooth.spline(dfR$bylogR,df=spdf,w=sqrt(comdat$allCases))$y
smoothweightRstrat <- smooth.spline(dfR$stratR,df=spdf,w=sqrt(comdat$allCases))$y
smoothweightRito <- smooth.spline(dfR$itoR,df=spdf,w=sqrt(comdat$allCases))$y
dfR$smoothweightRfp <- smooth.spline(dfR$fpR,df=spdf,w=sqrt(comdat$fpCases))$y
dfR$smoothONSEng <- smooth.spline(dfR$ONSEng,df=spdf,w=sqrt(comdat$Eng_ons_prev))$y
dfR$smoothONSScot <- smooth.spline(dfR$ONSScot,df=spdf,w=sqrt(comdat$Scot_ons_prev))$y
rat$smoothScotland <- smooth.spline(rat$Scotland,df=spdf,w=sqrt(regcases$Scotland))$y
rat$smoothNW <-smooth.spline(rat$`North West`,df=spdf,w=sqrt(regcases$`North West`))$y
rat$smoothNEY <-smooth.spline(rat$NE_Yorks,df=spdf,w=sqrt(regcases$NE_Yorks))$y
rat$smoothLon <-smooth.spline(rat$London,df=spdf,w=sqrt(regcases$London))$y
rat$smoothEE <-smooth.spline(rat$`East of England`,df=spdf,w=sqrt(regcases$`East of England`))$y
rat$smoothMD <-smooth.spline(rat$Midlands,df=spdf,w=sqrt(regcases$Midlands))$y
rat$smoothSE <-smooth.spline(rat$`South East`,df=spdf,w=sqrt(regcases$`South East`))$y
rat$smoothSW <-smooth.spline(rat$`South West`,df=spdf,w=sqrt(regcases$`South West`))$y
rat$smoothWales <-smooth.spline(rat$Wales,df=spdf,w=sqrt(regcases$Wales))$y
rat$smoothNI<-smooth.spline(rat$NI,df=spdf,w=sqrt(regcases$NI))$y
rat$smoothEngland<-smooth.spline(rat$England,df=spdf,w=sqrt(regcases$England))$y
jnkR=rat$`NHS Lothian`
jnkC=regcases$'NHS Lothian'
rat$smoothLothian <-smooth.spline(jnkR,df=spdf,w=sqrt(jnkC))$y
jnkR=rat$`NHS Greater Glasgow and Clyde`
jnkC=regcases$`NHS Greater Glasgow and Clyde`
rat$smoothGlasgow <-smooth.spline(jnkR,df=spdf,w=sqrt(jnkC))$y
rat$smoothLon <-smooth.spline(rat$London,df=spdf,w=sqrt(regcases$London))$y
#  Piecewise R between lockdowns only if its within the startdate-enddate period
if(lock1>1){
smoothR1<-smooth.spline(dfR$bylogR[1:(lock1-1)],df=lock1/14)
smoothR1$date<-dfR$date[1:lock1-1]
smoothR2<-smooth.spline(dfR$bylogR[lock1:(unlock1-1)],df=(unlock1-lock1)/14)
smoothR2$x=smoothR2$x+lock1
smoothR2$date<-dfR$date[lock1:unlock1-1]
smoothR3<-smooth.spline(dfR$bylogR[unlock1:(lock2-1)],df=(lock2-unlock1)/14)
smoothR3$x=smoothR3$x+unlock1
smoothR3$date<-dfR$date[unlock1:(lock2-1)]
smoothR4<-smooth.spline(dfR$bylogR[lock2:(unlock2-1)],df=(unlock2-lock2)/14)
smoothR4$x=smoothR4$x+unlock2
smoothR4$date<-dfR$date[lock2:(unlock2-1)]
smoothRend<-smooth.spline(dfR$bylogR[unlock2:length(dfR$date)],df=(length(dfR$date)-unlock2)/14)
smoothRend$x=smoothRend$x+unlock2
smoothRend$date<-dfR$date[unlock2:length(dfR$itoR)]
dfR$piecewise<-dfR$itoR
dfR$piecewise[1:(lock1-1)]=smoothR1$y
dfR$piecewise[lock1:(unlock1-1)]=smoothR2$y
dfR$piecewise[unlock1:(lock2-1)]=smoothR3$y
dfR$piecewise[lock2:(unlock2-1)]=smoothR4$y
dfR$piecewise[unlock2:length(dfR$itoR)]=smoothRend$y

rm(smoothR1,smoothR2,smoothR3,smoothR4,smoothRend,jnkR,jnkC)
# Plot R estimate vs data and fits discontinuous at lockdown
#  Have to move the Official R data back by 16 days !

#  All cases and Regions
if(interactive()){

  plot(dfR$smoothweightR,ylab="Regional R-number",xlab="Date",x=dfR$date)

  for (i in 8:17){
    lines(smooth.spline(na.omit(dfR[i]),df=20)$y,col=i,x=dfR$date[!is.na(dfR[i])])
  }

  # ggplot graph - create a temporary tibble
  d <- tibble(x = dfR$date,
              y = smooth.spline(na.omit(dfR[8]),df=12)$y,
              type = rep(names(dfR)[8],nrow(dfR)))

  # Populate the tibble
  for(i in names(dfR)[9:17]){
    d <- add_row(d,
                 x = dfR$date,
                 y = smooth.spline(na.omit(dfR[i]),df=12)$y,
                 type = rep(i, nrow(dfR)))
  }

  # Generate the graph
  data.frame(x=dfR$date, y=dfR$smoothweightR) %>%
    ggplot(aes(x, y)) + geom_point(alpha = 0.5) +
    theme_bw()  + xlab("Date") + ylab("Regional R-number") +
    geom_line(data=d,aes(x = x, y = y, colour = type, linetype = type) )

  # remove temporary tibble
  rm(d)

  plot(dfR$bylogR,x=dfR$date,ylab="R-number",xlab="",
       title("R, England"),ylim=c(0.6,1.6),xlim=plotdate,cex.lab=1.6, cex.axis=1.6, cex.main=1.6, cex.sub=1.6)
  lines(Rest$England_LowerBound,x=Rest$Date-sagedelay,lwd=2)
  lines(y=Rest$England_UpperBound,x=Rest$Date-sagedelay,lwd=2)
  if(exists("dfR$piecewise")){lines(dfR$piecewise,col="violet",lwd=3,x=dfR$date)}
  lines(dfR$smoothweightR,col="blue",lwd=3,x=dfR$date)
  lines(predict(loess(itoR ~ x, data=dfR,span=0.3,weight=sqrt(comdat$allCases))),col='green',x=dfR$date,lwd=3)
  #lines(predict(loess(itoR ~ x, data=dfR,span=0.3)),col='green',x=dfR$date)
  lines(predict(loess(bylogR ~ x, data=dfR,span=0.3,weight=sqrt(comdat$allCases))),col='red',x=dfR$date,lwd=3)
  #lines(predict(loess(bylogR ~ x, data=dfR,span=0.3)),col='red',x=dfR$date)

  # ggplot version of the same plot
  dfR %>% ggplot(aes(x = date, y = bylogR)) + geom_point(alpha=0.5, na.rm = TRUE) +
    theme_bw() + ylab("R-number") + xlab("Date") + ylim(0.6, 1.6) +
    geom_ribbon(data = Rest,
                aes(x = Date - sagedelay,
                    ymin = England_LowerBound,
                    ymax = England_UpperBound,
                    fill = "red",
                    colour = "black"), alpha = 0.1, inherit.aes = FALSE, show.legend = FALSE) +
    geom_line(data = dfR, aes(x = date, y = piecewise), colour = "violet", size = 1.25) +
    geom_line(data = data.frame(x = dfR$date, y = dfR$smoothweightR), aes(x = x, y = y), colour = "blue", size = 1.25) +
    geom_line(data = data.frame(x = dfR$date,
                                y = predict(loess(itoR ~ x, data = dfR, span = 0.3, weight = sqrt(comdat$allCases)))),
              aes(x = x, y = y), colour = "green", size = 1.25) +
    geom_line(data = data.frame(x = dfR$date,
                                y = predict(loess(bylogR ~ x, data = dfR, span = 0.3, weight = sqrt(comdat$allCases)))),
              aes(x = x, y = y), colour = "red", size = 1.25) + ggtitle("R, England") +
    theme(plot.title = element_text(hjust = 0.5))


  plot(dfR$piecewise,x=dfR$smoothweightR,ylab="R-number",xlab="",
       title("R, England"),ylim=c(0.6,1.4),xlim=plotdate,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.6)
  lines(Rest$England_LowerBound,x=(Rest$Date-sagedelay))
  lines(y=Rest$England_UpperBound,x=Rest$Date-sagedelay)
  lines(predict(loess(bylogR ~ x, data=dfR,span=0.05,weight=sqrt(comdat$allCases))),col='red',x=dfR$date,lwd=2)
  lines(predict(loess(bylogR ~ x, data=dfR,span=0.1,weight=sqrt(comdat$allCases))),col='green',x=dfR$date,lwd=2)
  lines(predict(loess(bylogR ~ x, data=dfR,span=0.2,weight=sqrt(comdat$allCases))),col='blue',x=dfR$date,lwd=2)
  lines(predict(loess(bylogR ~ x, data=dfR,span=0.3,weight=sqrt(comdat$allCases))),col='violet',x=dfR$date,lwd=3)

  # ggplot version of the same plot
  dfR %>% ggplot(aes(x = date, y = piecewise)) + geom_point(alpha=0.5, na.rm = TRUE) +
          theme_bw() + ylab("R-number") + xlab("Date") + ylim(0.6, 1.4) + ggtitle("R, England") +
          theme(plot.title = element_text(hjust = 0.5)) +
          geom_ribbon(data = Rest,
                     aes(x = Date - sagedelay,
                        ymin = England_LowerBound,
                        ymax = England_UpperBound,
                        fill = "red",
                        colour = "black"), alpha = 0.05, inherit.aes = FALSE, show.legend = FALSE) +
          geom_line(data = data.frame(x = dfR$date,
                                      y = predict(loess(bylogR ~ x, data = dfR, span = 0.05, weight = sqrt(comdat$allCases)))),
                    aes(x = x, y = y), inherit.aes = FALSE, colour = "red", size = 1) +
          geom_line(data = data.frame(x = dfR$date,
                                      y = predict(loess(bylogR ~ x, data = dfR, span = 0.1, weight = sqrt(comdat$allCases)))),
                    aes(x = x, y = y), colour = "green", size = 1) +
          geom_line(data = data.frame(x = dfR$date,
                                     y = predict(loess(bylogR ~ x, data = dfR, span = 0.2, weight = sqrt(comdat$allCases)))),
                    aes(x = x, y = y), colour = "blue", size = 1) +
          geom_line(data = data.frame(x = dfR$date,
                                y = predict(loess(bylogR ~ x, data = dfR, span = 0.3, weight = sqrt(comdat$allCases)))),
              aes(x = x, y = y), colour = "violet", size = 1.25)

}# End piecewise R for 2020 lockdowns calculate and plot section
}# End interactive session


R_BestGuess <- list()
R_Quant <- list()


#  Daily stats are quite messed up since end of PCR testing.  Use smoothed R estimates for Quartiles
tmp <-estimate_R(rat$smoothEngland,dfR$date,comdat$allCases)
R_BestGuess$England <-tmp[1]
R_Quant$England <-tmp[2:6]


tmp <-estimate_R(rat$smoothScotland,rat$date,regcases$Scotland)
#  Scottish data reporting is so delayed in summer 2022 that its better to use ONS

tmp <-estimate_R(dfR$smoothONSScot,rat$date,regcases$Scotland)
R_BestGuess$Scotland <-tmp[1]
R_Quant$Scotland <-tmp[2:6]

tmp <-estimate_R(rat$smoothLon,rat$date,regcases$London)
R_BestGuess$Lon <-tmp[1]
R_Quant$Lon <-tmp[2:6]

tmp <-estimate_R(rat$smoothMD,rat$date,regcases$Midlands)
R_BestGuess$Midlands <-tmp[1]
R_Quant$Midlands <-tmp[2:6]

tmp <-estimate_R(rat$smoothNW,rat$date,regcases$'North West')
R_BestGuess$NW <-tmp[1]
R_Quant$NW <-tmp[2:6]

tmp <-estimate_R(rat$smoothNEY,rat$date,regcases$NE_Yorks)
R_BestGuess$NEY <-tmp[1]
R_Quant$NEY <-tmp[2:6]

tmp <-estimate_R(rat$smoothEE,rat$date,regcases$`East of England`)
R_BestGuess$EE <-tmp[1]
R_Quant$EE <-tmp[2:6]

tmp <-estimate_R(rat$smoothSE,rat$date,regcases$`South East`)
R_BestGuess$SE <-tmp[1]
R_Quant$SE <-tmp[2:6]
tmp <-estimate_R(rat$smoothSW,rat$date,regcases$`South West`)
R_BestGuess$SW <-tmp[1]
R_Quant$SW <-tmp[2:6]

tmp <-estimate_R(rat$smoothWales,rat$date,regcases$Wales)
R_BestGuess$Wales <-tmp[1]
R_Quant$Wales <-tmp[2:6]

tmp <-estimate_R(dfR$regions,dfR$date,regcases$England)
R_BestGuess$Regions <-tmp[1]
R_Quant$Regions <-tmp[2:6]

tmp <-estimate_R(rat$smoothNI,rat$date,regcases$NI)
R_BestGuess$NI <-tmp[1]
R_Quant$NI <-tmp[2:6]

tmp <-estimate_R(rat$smoothLothian,rat$date,regcases$NI)
R_BestGuess$Lothian <-tmp[1]
R_Quant$Lothian <-tmp[2:6]

tmp <-estimate_R(rat$smoothGlasgow,rat$date,regcases$NI)
R_BestGuess$Glasgow <-tmp[1]
R_Quant$Glasgow <-tmp[2:6]

tmp <-estimate_R(rat$`NHS Ayrshire and Arran`,rat$date,regcases$NI)
R_BestGuess$Ayr <-tmp[1]
R_Quant$Ayr <-tmp[2:6]

tmp <-estimate_R(rat$`NHS Borders`,rat$date,regcases$NI)
R_BestGuess$Borders<-tmp[1]
R_Quant$Borders <-tmp[2:6]

tmp <-estimate_R(rat$`NHS Dumfries and Galloway`,rat$date,regcases$NI)
R_BestGuess$Dumfries <-tmp[1]
R_Quant$Dumfries <-tmp[2:6]

tmp <-estimate_R(rat$`NHS Forth Valley`,rat$date,regcases$NI)
R_BestGuess$Forth <-tmp[1]
R_Quant$Forth <-tmp[2:6]

tmp <-estimate_R(rat$`NHS Grampian`,rat$date,regcases$NI)
R_BestGuess$Gramp <-tmp[1]
R_Quant$Gramp <-tmp[2:6]

tmp <-estimate_R(rat$`NHS Highland`,rat$date,regcases$NI)
R_BestGuess$Highland <-tmp[1]
R_Quant$Highland <-tmp[2:6]

tmp <-estimate_R(rat$`NHS Fife`,rat$date,regcases$NI)
R_BestGuess$Fife <-tmp[1]
R_Quant$Fife <-tmp[2:6]

tmp <-estimate_R(rat$`NHS Tayside`,rat$date,regcases$NI)
R_BestGuess$Tayside <-tmp[1]
R_Quant$Tayside <-tmp[2:6]

tmp <-estimate_R(rat$`NHS Lanarkshire`,rat$date,regcases$NI)
R_BestGuess$Lanark<-tmp[1]
R_Quant$Lanark <-tmp[2:6]

tmp <-estimate_R(rat$`NHS Orkney`,rat$date,regcases$NI)
R_BestGuess$Orkney <-tmp[1]
R_Quant$Orkney <-tmp[2:6]

tmp <-estimate_R(rat$`NHS Western Isles`,rat$date,regcases$NI)
R_BestGuess$WI <-tmp[1]
R_Quant$WI <-tmp[2:6]

tmp <-estimate_R(rat$`NHS Shetland`,rat$date,regcases$NI)
R_BestGuess$Shetland <-tmp[1]
R_Quant$Shetland <-tmp[2:6]


##########   Age groups  ########

tmp <-estimate_R(dfR$p00,dfR$date,comdat$allCases)
R_BestGuess$x00 <-tmp[1]
R_Quant$x00 <-tmp[2:6]

tmp <-estimate_R(dfR$p05,dfR$date,comdat$allCases)
R_BestGuess$x05 <-tmp[1]
R_Quant$x05 <-tmp[2:6]

tmp <-estimate_R(dfR$p15,dfR$date,comdat$allCases)
R_BestGuess$x15 <-tmp[1]
R_Quant$x15 <-tmp[2:6]

tmp <-estimate_R(dfR$p25,dfR$date,comdat$allCases)
R_BestGuess$x25 <-tmp[1]
R_Quant$x25 <-tmp[2:6]

tmp <-estimate_R(dfR$p45,dfR$date,comdat$allCases)
R_BestGuess$x45 <-tmp[1]
R_Quant$x45 <-tmp[2:6]

tmp <-estimate_R(dfR$p65,dfR$date,comdat$allCases)
R_BestGuess$x65 <-tmp[1]
R_Quant$x65 <-tmp[2:6]

tmp <-estimate_R(dfR$p75,dfR$date,comdat$allCases)
R_BestGuess$x75 <-tmp[1]
R_Quant$x75 <-tmp[2:6]


if(interactive()){

  plot(dfR$smoothweightR,ylab="R-number",xlab="Day",ylim=c(0.5,1.6))
  #  Plot R continuous with many splines.
  for (ismooth in 4:30){
    #  lines(smooth.spline(dfR$bylogR,df=ismooth,w=sqrt(comdat$allCases)))
    lines(predict(loess(bylogR ~ x, data=dfR,span=(4.0/ismooth))),col='red')
  }

  # Temp tibble
  ismooth <-  4
  d <- tibble(x = seq_len(nrow(dfR)),
             y = predict(loess(bylogR ~ x, data=dfR,span=(4.0/ismooth))),
             type = rep(ismooth, nrow(dfR))
            )
  for (ismooth in 5:30){
    d <- add_row(d,x = seq_len(nrow(dfR)),
                 y = predict(loess(bylogR ~ x, data=dfR,span=(4.0/ismooth))),
                 type = rep(ismooth, nrow(dfR)))
  }

  # plot the data
  data.frame(x = dfR$date,y = dfR$smoothweightR) %>%
  ggplot(aes(x = x, y = y)) + geom_point(alpha = 0.25) +
  theme_bw() + xlab("Day") + ylab("R-number") +
  geom_line(data = d, aes(x = x, y = y, colour = factor(type)), alpha = 0.25, show.legend = FALSE)

  # Remove the temporary tibble
  rm(d)
}



# Plot Regional R data vs Government  spdf is spline smoothing factor, lospan for loess

#  various options to silence pdf writing
pdfpo <- FALSE

#  Plot age data
dfR[,c(2,9:27)]%>% filter(startplot < date & date < endplot) %>%
  pivot_longer(!date,names_to = "Age", values_to="R") %>%
  ggplot(aes(x=date, y=R, colour=Age)) +
  coord_cartesian(ylim=c(0.5,1.9))+ geom_smooth(formula = y ~ x, method = "loess", span=0.3) +
  guides(color = "none") + facet_wrap(vars(Age)) +
  theme(axis.text.x=element_text(angle=90,hjust=1)) + xlab("Date")

#  Function to generate plots of R by age

if(interactive()&pdfpo){age_pdfplot(dfR,Rest,sagedelay,comdat,spdf)}



# Reverse Engineer cases from R-number - requires stratonovich calculus to get reversibility
# Initializations

#  Use the same weekend-adjusted initial condition, regardless of smoothing effect
#  Start with the "correct" data

Predict <- data.frame(x=1.0:length(comdat$date),
                      date=comdat$date,
                      c=comdat$allCases,
Lin=comdat$allCases,
Raw=comdat$allCases,
SmoothRlog=comdat$allCases,
SmoothRito=comdat$allCases,
SmoothRstrat=comdat$allCases,
MeanR=comdat$allCases,
smoothcasesR=comdat$allCases
)
meanR=mean(dfR$stratR)
startpred=genTime+22
for(i in startpred:length(dfR$date)){
  Predict$c[i]=Predict$c[i-1]*exp((dfR$bylogR[i]-1)/genTime)
  Predict$Lin[i]=Predict$Lin[i-1]*(1.0+(dfR$itoR[i]-1)/genTime)
  Predict$SmoothRstrat[i]=Predict$SmoothRstrat[i-1]*(1.0+(dfR$stratR[i]-1)/genTime)
  Predict$MeanR[i]=Predict$MeanR[i-1]*(1.0+(meanR-1)/genTime)
  Predict$SmoothRlog[i]=Predict$SmoothRlog[i-1]*exp((dfR$smoothRlog[i]-1)/genTime)
  Predict$SmoothRito[i]=Predict$SmoothRito[i-1]*exp((dfR$smoothRito[i]-1)/genTime)
  Predict$smoothcasesR[i]=Predict$smoothcasesR[i-1]*exp((dfR$smoothcasesR[i]-1)/genTime)
    }
#  Averaging R is not the same as averaging e^R
#  Noise suppresses the growth rate in the model, Smoothed R grows too fast
#   Multiplier chosen to match final cases, because it is super-sensitive to noise in the initial day
#

Predict$smoothcasesR =Predict$smoothcasesR*sum(comdat$allCases)/sum(Predict$smoothcasesR)
Predict$SmoothRito =Predict$SmoothRito*sum(comdat$allCases)/sum(Predict$SmoothRito)
Predict$SmoothRlog=Predict$SmoothRlog*sum(comdat$allCases)/sum(Predict$SmoothRlog)
Predict$MeanR=Predict$MeanR*sum(comdat$allCases)/sum(Predict$MeanR)

if(interactive()&FALSE){

  sum(Predict$MeanR)
  sum(Predict$SmoothRlog)
  sum(Predict$SmoothRito)
  sum(Predict$smoothcasesR)

  plot(comdat$allCases,x=Predict$date,xlab="Date",ylab="Cases backdeduced from R")
  lines(Predict$c,x=Predict$date, col="black",lwd=2)
  lines(Predict$SmoothRlog,x=Predict$date, col="blue",lwd=2)
  lines(Predict$SmoothRito,x=Predict$date, col="violet",lwd=2)
  lines(Predict$MeanR,x=Predict$date, col="green",lwd=2)
  lines(Predict$smoothcasesR,x=Predict$date, col="red",lwd=2)

  dfR$meanR=meanR
  plot(dfR$bylogR,x=dfR$date,xlab="",ylab="R"
       ,xlim=c(dfR$date[(startpred)],dfR$date[350]),ylim=c(-1,3))
  lines(dfR$smoothRlog,x=dfR$date, col="blue",lwd=2)
  lines(dfR$smoothRito,x=dfR$date, col="violet",lwd=2)
  lines(dfR$meanR,x=dfR$date, col="green",lwd=2)
  lines(dfR$smoothcasesR,x=dfR$date, col="red",lwd=2)

  # Create a temporary tibble to plot the data
  tmpdat <- tibble(date = comdat$date,
                   c = Predict$c,
                   smoothcasesR = Predict$smoothcasesR,
                   SmoothRlog = Predict$SmoothRlog,
                   SmoothRito = Predict$SmoothRito,
                   MeanR = Predict$MeanR)

  # Plot the data
  ggplot(comdat, aes(x = date)) + geom_point(aes(y = allCases), alpha = 0.2) +
    geom_line(data = tmpdat, aes(x = date, y = c), colour = "black", alpha = 0.5, size = 1.1) +
    geom_line(data = tmpdat, aes(x = date, y = SmoothRlog), colour = "blue", alpha = 0.5, size = 1.1) +
    geom_line(data = tmpdat, aes(x = date, y = SmoothRito), colour = "violet", alpha = 0.5, size = 1.1) +
    geom_line(data = tmpdat, aes(x = date, y = MeanR), colour = "green", alpha = 0.5, size = 1.1) +
    geom_line(data = tmpdat, aes(x = date, y = smoothcasesR), colour = "red", alpha = 0.5, size = 1.1) +
    xlab("Date") + ylab("Cases backdeduced from R") + theme_bw()

  # Create a temporary tibble for plotting
  tmpdat <-  tibble(date = comdat$date,
                    c = Predict$c,
                    smoothcasesR = dfR$smoothcasesR,
                    SmoothRlog = dfR$smoothRlog,
                    SmoothRito = dfR$smoothRito,
                    bylogR = dfR$bylogR,
                    MeanR = meanR )
  # Plot the data
  ggplot(comdat, aes(x = date)) +
    geom_point(data = tmpdat, aes(x = date, y = bylogR), colour = "black",alpha = 0.25, na.rm = TRUE) +
    geom_line(data = tmpdat, aes(x = date, y = SmoothRlog), colour = "blue",alpha = 0.75, size = 1.25) +
    geom_line(data = tmpdat, aes(x = date, y = SmoothRito), colour = "violet",alpha = 0.75, size = 1.25) +
    geom_line(data = tmpdat, aes(x = date, y = MeanR), colour = "green",alpha = 0.75, size = 1.25) +
    geom_line(data = tmpdat, aes(x = date, y = smoothcasesR), colour = "red", alpha = 0.75, size = 1.25) +
    xlab("Date") + ylab("R") + theme_bw() + ylim(0.7, 1.4)

  # Remove the temporary array
  rm(tmpdat)

}

#####  Figures and analysis for https://www.medrxiv.org/content/10.1101/2021.04.14.21255385v1
# Date not encapsulated and may become broken because of hard coded dates
# Nothing should be returned or changed by this analysis

medrxiv<-FALSE
if(interactive()&medrxiv){medout<-MedrxivPaper()}


