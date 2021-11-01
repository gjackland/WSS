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
Xmasav <- sum(cdat[XMstart:XMend])/XMdays
Xmasgrad <- cdat[XMend]-cdat[XMstart]
for (i in XMstart:XMend){
  cdat[i]=Xmasav-Xmasgrad*(((XMend+XMstart)/2)-i)/XMdays
}


return(cdat)
}
