source("organize_variables.R")
#Fix misclassification----
##sex----
TBCSstimu$Sex<-as.factor(TBCSstimu$Sex)
##probiotic intake----
TBCSstimu$probioticintake_6m<-as.factor(TBCSstimu$probioticintake_6m)
TBCSstimu$probioticintake_18m<-as.factor(TBCSstimu$probioticintake_18m)
TBCSstimu$probioticintake_3y<-as.factor(TBCSstimu$probioticintake_3y)
TBCSstimu$probioticintake_5y<-as.factor(TBCSstimu$probioticintake_5y)

##mother education----
TBCSstimu$medu_6m<-as.factor(TBCSstimu$medu_6m)
TBCSstimu$medu_18m<-as.factor(TBCSstimu$medu_18m)
TBCSstimu$medu_3y<-as.factor(TBCSstimu$medu_3y)
TBCSstimu$medu_5y<-as.factor(TBCSstimu$medu_5y)

##socioeconomic Status----
TBCSstimu$Socioeco_6m<-as.factor(TBCSstimu$Socioeco_6m)
TBCSstimu$Socioeco_18m<-as.factor(TBCSstimu$Socioeco_18m)
TBCSstimu$Socioeco_3y<-as.factor(TBCSstimu$Socioeco_3y)
TBCSstimu$Socioeco_5y<-as.factor(TBCSstimu$Socioeco_5y)

##breastfeeding----
TBCSstimu$breastfeedingdays_6m<-as.numeric(TBCSstimu$breastfeedingdays_6m)
TBCSstimu$breastfeedingmonths_6m<-as.numeric(TBCSstimu$breastfeedingmonths_6m)

##dairy intake----
TBCSstimu$dairyintake_18m<-as.factor(TBCSstimu$dairyintake_18m)
TBCSstimu$dairyintake_5y<-as.factor(TBCSstimu$dairyintake_5y)

##gastroentertitis----
TBCSstimu$Gastro_6m<-as.factor(TBCSstimu$Gastro_6m)
TBCSstimu$Gastro_18m<-as.factor(TBCSstimu$Gastro_18m)
TBCSstimu$Gastro_3y<-as.factor(TBCSstimu$Gastro_3y)
TBCSstimu$Gastro_5y<-as.factor(TBCSstimu$Gastro_5y)

##atopic dermatitis
TBCSstimu$AD_6m<-as.factor(TBCSstimu$AD_6m)
TBCSstimu$AD_18m<-as.factor(TBCSstimu$AD_18m)
TBCSstimu$AD_3y<-as.factor(TBCSstimu$AD_3y)
TBCSstimu$AD_5y<-as.factor(TBCSstimu$AD_5y)
#Probioticintake (exposure)----
##exposure combine----
exposureb45y<-TBCSstimu%>%
  select(Sampleid,
         probioticintake_6m,
         probioticintake_18m,
         probioticintake_3y,
         probioticintake_5y,)%>%
  mutate(across(-Sampleid,
                ~ if_else(as.numeric(as.character(.)) == 1, 1, 0, missing = 0))) %>% #replace 8,9 and other number with 0

##add up exposure score----
  mutate(probiotic_score=rowSums(across(c(
        probioticintake_6m,
        probioticintake_18m,
        probioticintake_3y,
        probioticintake_5y,)),na.rm=TRUE))%>%

##everuser/neveruser----
  mutate(probioticintake=case_when(probiotic_score==0~0,
                                   probiotic_score>0~1))%>%
##probiotic grouping----
mutate(probiotic_group = case_when(
  probioticintake_6m == 0 & probioticintake_18m == 0 &
    probioticintake_3y == 0 & probioticintake_5y == 0            ~ "Never",
  probioticintake_6m == 1 & probioticintake_18m == 1 &
    probioticintake_3y == 1 & probioticintake_5y == 1            ~ "Always",
  (probioticintake_6m == 1 | probioticintake_18m == 1) &
    probioticintake_3y == 0 & probioticintake_5y == 0           ~ "Early",
  probioticintake_6m == 0 & probioticintake_18m == 0 &
    (probioticintake_3y == 1 | probioticintake_5y == 1)         ~ "Late",
  TRUE                                                           ~ "Intermittent"
),
probiotic_group = factor(probiotic_group,
                         levels = c("Never", "Early", "Late", "Intermittent", "Always")))
#Early Puberty (outcome)----
NHIRD_OPD<-NHIRD_OPD%>%
  mutate(EarlyPuberty_OPD=if_else(
    if_any(c(ICD9CM_1,ICD9CM_2,ICD9CM_3),
           ~ .x %in% c("E301","2591")),1,0))
NHIRD_IPD<-NHIRD_IPD%>%
  mutate(EarlyPuberty_IPD=if_else(
    if_any(c(ICD9CM_1,ICD9CM_2,ICD9CM_3,ICD9CM_4,ICD9CM_5),
           ~ .x %in% c("E301","2591")),1,0))
#Appendicitis (sensitivity analysis)----
NHIRD_OPD<-NHIRD_OPD%>%
  mutate(Appendicitis_OPD=if_else(
    if_any(c(ICD9CM_1,ICD9CM_2,ICD9CM_3),
           ~ .x %in% c("K352","K353","K3580","K3589","K37","K36",
                       "5400","5401","5409","541","542")),1,0))
NHIRD_IPD<-NHIRD_IPD%>%
  mutate(Appendicitis_IPD=if_else(
    if_any(c(ICD9CM_1,ICD9CM_2,ICD9CM_3,ICD9CM_4,ICD9CM_5),
           ~ .x %in% c("K352","K353","K3580","K3589","K37","K36",
                       "5400","5401","5409","541","542")),1,0))
#Gastroenteritis(sensitivity analysis)----
gastro_all<-TBCSstimu%>%
  select(Sampleid,
         gastroenteritis_6m,
         gastroenteritis_18m,
         gastroenteritis_3y,
         gastroenteritis_5y,)
##change variables to numeric----
gastro_all$gastroenteritis_6m<-as.numeric(as.character(gastro_all$gastroenteritis_6m))
gastro_all$gastroenteritis_18m<-as.numeric(as.character(gastro_all$gastroenteritis_18m))
gastro_all$gastroenteritis_3y<-as.numeric(as.character(gastro_all$gastroenteritis_3y))
gastro_all$gastroenteritis_5y<-as.numeric(as.character(gastro_all$gastroenteritis_5y))
##add up gastro score----
gastro_all<-gastro_all%>%
  mutate(total=rowSums(across(c(
    gastroenteritis_6m,
    gastroenteritis_18m,
    gastroenteritis_3y,
    gastroenteritis_5y,)),na.rm=TRUE))
##gastro/no gastro----
gastro_all<-gastro_all%>%
  mutate(gastroenteritis=case_when(total==0~0,
                                   total>0~1))