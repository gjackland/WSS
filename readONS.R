# 
read.xlsx("covid19infectionsurveydatasets20211119scotland.xlsx",3)-> jnkC

positivity<-data_frame(x=6:85)
positivity$England = as.numeric(jnkC$X2[6:85])
positivity$Wales = as.numeric(jnkC$X7[6:85])
positivity$NI = as.numeric(jnkC$X12[6:85])
positivity$Scotland = as.numeric(jnkC$X17[6:85])
plot(positivity)

read.xlsx("covid19infectionsurveydatasets20211119scotland.xlsx",4)-> jnkC
incidence<-data_frame(x=6:85)
incidence$England = as.numeric(jnkC$X2[6:85])
incidence$Wales = as.numeric(jnkC$X7[6:85])
incidence$NI = as.numeric(jnkC$X12[6:85])
incidence$Scotland = as.numeric(jnkC$X17[6:85])
plot(incidence)

read.xlsx("covid19infectionsurveydatasets20211119scotland.xlsx",9)-> jnkC
incidence<-data_frame(x=6:85)
agepositive$Scotland = as.numeric(jnkC[2:3][7:47])
plot(incidence)


