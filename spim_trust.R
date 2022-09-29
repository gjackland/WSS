#  Read only Regional and National data.  Build 11 lists of regions and nations 

spimregdat <- openxlsx::read.xlsx("spim_trust.xlsx",3)
spimregdat <- spimregdat %>% filter(ReportLevel=="Region"|ReportLevel=="Nation")
spimregdat$DateVal<-as.Date(spimregdat$DateVal)
spimregdat <- spimregdat %>% filter(DateVal >= startdate & DateVal <= enddate)
spim <- list()
spim$London <-data.frame()
spim$London <-spimregdat %>% filter(ReportLevel=="Region") %>% filter(Geography=="London") %>% 
  select(date="DateVal",hospital_inc,
         death28="PHE_type28_death_inc_line",
         hospital_inc_new,PHE_type60cod_death_inc_dash,PHE_type28_death_inc_dash,
         deathPHE="PHE_confirmed_death_inc_line",CHESS_icu_prevalence,
         CHESS_P1_cases,CHESS_P2_cases,CPNS_death_inc_dash,CPNS_death_inc_line,
         hospital_prev,hospital_inc_RepeatDiagnosis,icu_prev,discharges)
spim$MD <- spimregdat %>% filter(ReportLevel=="Region") %>% filter(Geography=="Midlands") %>% 
  select(date="DateVal",hospital_inc,
         death28="PHE_type28_death_inc_line",
         hospital_inc_new,PHE_type60cod_death_inc_dash,PHE_type28_death_inc_dash,
         deathPHE="PHE_confirmed_death_inc_line",CHESS_icu_prevalence,
         CHESS_P1_cases,CHESS_P2_cases,CPNS_death_inc_dash,CPNS_death_inc_line,
         hospital_prev,hospital_inc_RepeatDiagnosis,icu_prev,discharges)
spim$NEY <- spimregdat %>% filter(ReportLevel=="Region") %>% filter(Geography=="North East and Yorkshire") %>% 
  select(date="DateVal",hospital_inc,
         death28="PHE_type28_death_inc_line",
         hospital_inc_new,PHE_type60cod_death_inc_dash,PHE_type28_death_inc_dash,
         deathPHE="PHE_confirmed_death_inc_line",CHESS_icu_prevalence,
         CHESS_P1_cases,CHESS_P2_cases,CPNS_death_inc_dash,CPNS_death_inc_line,
         hospital_prev,hospital_inc_RepeatDiagnosis,icu_prev,discharges)
spim$NW <- spimregdat %>% filter(ReportLevel=="Region") %>% filter(Geography=="North West") %>% 
  select(date="DateVal",hospital_inc,
         death28="PHE_type28_death_inc_line",
         hospital_inc_new,PHE_type60cod_death_inc_dash,PHE_type28_death_inc_dash,
         deathPHE="PHE_confirmed_death_inc_line",CHESS_icu_prevalence,
         CHESS_P1_cases,CHESS_P2_cases,CPNS_death_inc_dash,CPNS_death_inc_line,
         hospital_prev,hospital_inc_RepeatDiagnosis,icu_prev,discharges)
spim$EE <- spimregdat %>% filter(ReportLevel=="Region") %>% filter(Geography=="East of England") %>% 
  select(date="DateVal",hospital_inc,
         death28="PHE_type28_death_inc_line",
         hospital_inc_new,PHE_type60cod_death_inc_dash,PHE_type28_death_inc_dash,
         deathPHE="PHE_confirmed_death_inc_line",CHESS_icu_prevalence,
         CHESS_P1_cases,CHESS_P2_cases,CPNS_death_inc_dash,CPNS_death_inc_line,
         hospital_prev,hospital_inc_RepeatDiagnosis,icu_prev,discharges)
spim$SE <- spimregdat %>% filter(ReportLevel=="Region") %>% filter(Geography=="South East") %>% 
  select(date="DateVal",hospital_inc,
         death28="PHE_type28_death_inc_line",
         hospital_inc_new,PHE_type60cod_death_inc_dash,PHE_type28_death_inc_dash,
         deathPHE="PHE_confirmed_death_inc_line",CHESS_icu_prevalence,
         CHESS_P1_cases,CHESS_P2_cases,CPNS_death_inc_dash,CPNS_death_inc_line,
         hospital_prev,hospital_inc_RepeatDiagnosis,icu_prev,discharges)
spim$SW <- spimregdat %>% filter(ReportLevel=="Region") %>% filter(Geography=="South West") %>% 
  select(date="DateVal",hospital_inc,
         death28="PHE_type28_death_inc_line",
         hospital_inc_new,PHE_type60cod_death_inc_dash,PHE_type28_death_inc_dash,
         deathPHE="PHE_confirmed_death_inc_line",CHESS_icu_prevalence,
         CHESS_P1_cases,CHESS_P2_cases,CPNS_death_inc_dash,CPNS_death_inc_line,
         hospital_prev,hospital_inc_RepeatDiagnosis,icu_prev,discharges)
spim$Wales <- spimregdat %>% filter(ReportLevel=="National") %>% filter(Geography=="Wales") %>% 
  select(date="DateVal",hospital_inc,
         death28="PHE_type28_death_inc_line",
         hospital_inc_new,PHE_type60cod_death_inc_dash,PHE_type28_death_inc_dash,
         deathPHE="PHE_confirmed_death_inc_line",CHESS_icu_prevalence,
         CHESS_P1_cases,CHESS_P2_cases,CPNS_death_inc_dash,CPNS_death_inc_line,
         hospital_prev,hospital_inc_RepeatDiagnosis,icu_prev,discharges)
spim$Scot <- spimregdat %>% filter(ReportLevel=="National") %>% filter(Geography=="Scotland") %>% 
  select(date="DateVal",hospital_inc,
         death28="PHE_type28_death_inc_line",
         hospital_inc_new,PHE_type60cod_death_inc_dash,PHE_type28_death_inc_dash,
         deathPHE="PHE_confirmed_death_inc_line",CHESS_icu_prevalence,
         CHESS_P1_cases,CHESS_P2_cases,CPNS_death_inc_dash,CPNS_death_inc_line,
         hospital_prev,hospital_inc_RepeatDiagnosis,icu_prev,discharges)
spim$Eng <- spimregdat %>% filter(ReportLevel=="National") %>% filter(Geography=="England") %>% 
  select(date="DateVal",hospital_inc,
         death28="PHE_type28_death_inc_line",
         hospital_inc_new,PHE_type60cod_death_inc_dash,PHE_type28_death_inc_dash,
         deathPHE="PHE_confirmed_death_inc_line",CHESS_icu_prevalence,
         CHESS_P1_cases,CHESS_P2_cases,CPNS_death_inc_dash,CPNS_death_inc_line,
         hospital_prev,hospital_inc_RepeatDiagnosis,icu_prev,discharges)
spim$NI <- spimregdat %>% filter(ReportLevel=="National") %>% filter(Geography=="Northern Ireland") %>% 
  select(date="DateVal",hospital_inc,
         death28="PHE_type28_death_inc_line",
         hospital_inc_new,PHE_type60cod_death_inc_dash,PHE_type28_death_inc_dash,
         deathPHE="PHE_confirmed_death_inc_line",CHESS_icu_prevalence,
         CHESS_P1_cases,CHESS_P2_cases,CPNS_death_inc_dash,CPNS_death_inc_line,
         hospital_prev,hospital_inc_RepeatDiagnosis,icu_prev,discharges)

