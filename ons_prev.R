# This source is for testing only -  main workflow uses getParms
#from August 7th 2020 
library(dplyr, warn.conflicts = FALSE, quietly = TRUE)
engpop=56989570
scotpop=5475660
eng_prev<-c(
  0.05,0.05,0.05,0.07,0.11,0.19,0.21,0.41,0.62,
  0.79,1.04,1.13,1.20,1.22,1.16,0.96,0.88,
  1.04,1.18,1.47,2.06,2.08,1.88,1.87,1.55,
  1.28,0.88,0.69,0.45,0.37,0.29,0.30,0.27,
  0.30, 0.21,0.17, 0.10,0.08, 0.07,0.09, 0.09,
  0.16, 0.18,0.19, 0.22,0.39,0.61,1.06,1.36,
  1.57,1.32,1.33,1.28,1.39,1.41,1.38,1.28,
  1.14,1.21,1.44,1.63,1.79,2.02,2.02,1.70,1.51,
  1.58,1.65,1.64,1.72,2.21,2.83,3.71,6.00,
  6.85,5.47,4.82,4.83,5.18,4.49,3.84,3.55,
  3.80,4.87,6.39,7.56,7.60,6.92,5.90,4.42,2.91,2.21,1.90,1.60,1.44,1.46,2.07)*engpop/100
scot_prev<-c(0.05,0.05,0.05,0.07,0.11,0.19, 0.21, 0.41,0.62,0.57,0.71,0.90,0.75,
             0.64,0.87,0.78,0.82,1.00,0.71,0.69,0.87,1.06,0.99,0.92,0.88,
             0.67,0.55,0.45,0.30,0.31,0.37,0.41,0.32,0.25,0.20,0.18,0.16,
             0.13,0.08,0.05,0.16,0.15,0.18,0.17,0.46,0.68,1.01,1.14,1.24,
             0.94,0.82,0.53,0.49,0.70,1.32,2.23,2.29,2.28,1.85,1.61,1.26,
             1.14,1.36,1.25,1.18,1.06,1.44,1.58,1.24,1.27,1.45,1.50,2.57,4.52,5.65,4.49,3.11,
             3.52,4.01, 4.17, 4.57, 5.33,5.70,7.15,9.00,8.57,7.54,5.98,5.35,4.14,3.55,
             3.01,2.32,2.57,2.01,2.36,3.36)*scotpop/100

#  Put the ONS prevalence data into the comdat array
approx(eng_prev,n=7*length(eng_prev))$y%>% tail(nrow(comdat))-> comdat$Eng_ons_prev
approx(scot_prev,n=7*length(scot_prev))$y%>% tail(nrow(comdat))-> comdat$Scot_ons_prev

comdat$Eng_ons_inc[1:(nrow(comdat))]<-0.0
comdat$Scot_ons_inc[1:(nrow(comdat))]<-0.0

#  Factor of 12 days infectious to convert prevalence to incidence.  Also delays it

comdat$Eng_ons_inc[1:(nrow(comdat)-12)]<-comdat$Eng_ons_prev[13:nrow(comdat)]/12
comdat$Scot_ons_inc[1:(nrow(comdat)-12)]<-comdat$Scot_ons_prev[13:nrow(comdat)]/12

xtmp=1:14
Engfit<-lm(comdat$Eng_ons_inc[(nrow(comdat)-25):(nrow(comdat)-12)]~xtmp)
Engslope=Engfit$coefficients[2]
Engstart=comdat$Eng_ons_inc[(nrow(comdat)-12)]
Scotfit<-lm(comdat$Scot_ons_inc[(nrow(comdat)-25):(nrow(comdat)-12)]~xtmp)
Scotslope=Scotfit$coefficients[2]
Scotstart=comdat$Scot_ons_inc[(nrow(comdat)-12)]
for(iday in 1:12){
comdat$Eng_ons_inc[(nrow(comdat)-12+iday)]=Engstart+iday*Engslope
comdat$Scot_ons_inc[(nrow(comdat)-12+iday)]=Scotstart+iday*Scotslope
}
plot(smooth.spline(comdat$Eng_ons_inc,df=10)$y)
lines(smooth.spline(regcases$England,df=10)$y)

comdat$Missing_incidence=smooth.spline((comdat$Eng_ons_inc/regcases$England),df=20)$y

comdat$Scot_Missing_incidence=smooth.spline((comdat$Scot_ons_inc/regcases$Scotland),df=20)$y
plot(comdat$Scot_Missing_incidence)
plot(comdat$Missing_incidence)
lines(regcases$Scotland
     )
