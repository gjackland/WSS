Weekend <- function(cdat){
# Remove weekend effect, assuming each weekday has same number of cases over the
# epidemic, and national averages hold regionally.
#  Input vector of cases, like comdat$allcases
  XMas <- as.Date("2020/12/24")
  XMstart <- as.integer(XMas-startdate)
  XMdays <- 12
  XMend <- XMstart+11

days <-1:7
weeks<-as.integer(length(cdat)/7)-1

for(i in 1:weeks){
  for(j in 1:7){
    days[j]<-days[j]+cdat[7*i+j]
  }
}
casetot <- sum(days)
days <- 7*days/casetot

# Rescale comdat and regcases
for(i in 1:length(cdat)){
  indexday <- (i-1)%%7+1
  cdat[i] <- cdat[i]/days[indexday]
}
#  Xmas correction 2020
if(startdate<XMas){
Xmasav <- sum(cdat[XMstart:XMend])/XMdays
Xmasgrad <- cdat[XMend]-cdat[XMstart]
for (i in XMstart:XMend){
  cdat[i]=Xmasav-Xmasgrad*(((XMend+XMstart)/2)-i)/XMdays
}
}
return(cdat)
}

#  Function to do nowcast for R based on smoothing over 200 days
estimate_R <- function(rat_in,date_in,reg_in){
  rat_range<-rat_in[(length(rat_in)-200):length(rat_in)]
  date_range<-c(1:201)
  reg_range<-reg_in[(length(rat_in)-200):length(rat_in)]
  filteredR<-  suppressWarnings(append(
  append(tail(predict(loess(rat_range ~ as.numeric(date_range),weight=reg_range ,span=s1))),
         tail(predict(loess(rat_range ~ as.numeric(date_range),weight=reg_range, span=s2))) ) ,
  append(tail(predict(loess(rat_range ~ as.numeric(date_range),weight=reg_range,span=s3))),
         tail(predict(loess(rat_range ~ as.numeric(date_range),weight=reg_range,span=s4))))
   ))
   R_Quant <-unname(quantile(filteredR, probs=c(0.05,0.25,0.5,0.75,0.95)))
   R <-mean(filteredR)
#  align mean with Quantiles   
   R_Quant <- R-R_Quant[3]+R_Quant
   return( c(R,R_Quant) )
}
