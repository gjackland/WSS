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



read.csv("20211124 COVID19 Deaths.csv")-> jnkDeath
jnk <- jnkDeath %>%  select(phec_name,
                             age, specimen_date,dateadmission,dod) 

jnk$specimen<- as.Date(jnk$specimen_date, "%d/%m/%Y")
jnk$admission<- as.Date(jnk$dateadmission, "%d/%m/%Y")
jnk$dod <- as.Date(jnk$dod, "%d/%m/%Y")

jnk %>% arrange(jnk$dod)

jnk$CASEtoSARI <- jnk$admission-jnk$specimen
jnk$SARItoDEATH <- jnk$dod-jnk$admission
listy=(as.integer((jnk$CASEtoSARI[60000:length(jnk$CASEtoSARI)])))
listy<-listy[listy<50]
listy<-listy[listy>-10]
hist(listy,breaks=51)
listy=(as.integer(na.locf(jnk$SARItoDEATH)))
listy<-listy[listy>0]
listy<-listy[listy<51]
hist(listy,breaks=50)


