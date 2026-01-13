source("organize_variables.R")
library("arsenal")
#Fix misclassification----
##sex----
TBCSstimu$B_SEX<-as.factor(TBCSstimu$B_SEX)
##probiotic intake----
TBCSstimu$pribioticintake_6m<-as.factor(TBCSstimu$probioticintake_6m)
TBCSstimu$probioticintake_18m<-as.factor(TBCSstimu$probioticintake_18m)
TBCSstimu$probioticintake_3y<-as.factor(TBCSstimu$probioticintake_3y)
TBCSstimu$probioticinkake_5y<-as.factor(TBCSstimu$probioticintake_5y)
TBCSstimu$probioticintake_8y<-as.factor(TBCSstimu$probioticintake_8y)

##father/mother education----
TBCSstimu$fedu_6m<-as.factor(TBCSstimu$fedu_6m)
TBCSstimu$medu_6m<-as.factor(TBCSstimu$medu_6m)
TBCSstimu$fedu_18m<-as.factor(TBCSstimu$fedu_18m)
TBCSstimu$medu_18m<-as.factor(TBCSstimu$medu_18m)
TBCSstimu$fedu_3y<-as.factor(TBCSstimu$fedu_3y)
TBCSstimu$medu_3y<-as.factor(TBCSstimu$medu_3y)
TBCSstimu$fedu_5y<-as.factor(TBCSstimu$fedu_5y)
TBCSstimu$medu_5y<-as.factor(TBCSstimu$medu_5y)
TBCSstimu$fedu_8y<-as.factor(TBCSstimu$fedu_8y)
TBCSstimu$medu_8y<-as.factor(TBCSstimu$medu_8y)

##socioeconomic Status----
TBCSstimu$Socioeco_6m<-as.factor(TBCSstimu$Socioeco_6m)
TBCSstimu$Socioeco_18m<-as.factor(TBCSstimu$Socioeco_18m)
TBCSstimu$Socioeco_3y<-as.factor(TBCSstimu$Socioeco_3y)
TBCSstimu$Socioeco_5y<-as.factor(TBCSstimu$Socioeco_5y)
TBCSstimu$Socioeco_8y<-as.factor(TBCSstimu$Socioeco_8y)

##breastfeeding----
TBCSstimu$breastfeeding_6m<-as.factor(TBCSstimu$breastfeeding_6m)
TBCSstimu$breastfeeding_18m<-as.factor(TBCSstimu$breastfeeding_18m)

##dairy intake----
TBCSstimu$dairyintake_18m<-as.factor(TBCSstimu$dairyintake_18m)
TBCSstimu$dairyintake_5y<-as.factor(TBCSstimu$dairyintake_5y)
TBCSstimu$dairyintake_8y<-as.factor(TBCSstimu$dairyintake_8y)

#Probioticintake (exposure)----
##exposure combine----
exposureb45y<-TBCSstimu%>%
  select(Sampleid,
         probioticintake_6m,
         probioticintake_18m,
         probioticintake_3y,
         probioticintake_5y,
         probioticintake_8y)

##change N/A into zero----
exposureb45y_clean<-exposureb45y
  exposureb45y_clean$probioticintake_6m[is.na(exposureb45y_clean$probioticintake_6m)]<-0
  exposureb45y_clean$probioticintake_18m[is.na(exposureb45y_clean$probioticintake_18m)]<-0
  exposureb45y_clean$probioticintake_3y[is.na(exposureb45y_clean$probioticintake_3y)]<-0
  exposureb45y_clean$probioticintake_5y[is.na(exposureb45y_clean$probioticintake_5y)]<-0
  exposureb45y_clean$probioticintake_8y[is.na(exposureb45y_clean$probioticintake_8y)]<-0

##change variables to numeric
exposureb45y_clean$probioticintake_18m<-as.numeric(as.character(exposureb45y_clean$probioticintake_18m))
exposureb45y_clean$probioticintake_3y<-as.numeric(as.character(exposureb45y_clean$probioticintake_3y))
exposureb45y_clean$probioticintake_8y<-as.numeric(as.character(exposureb45y_clean$probioticintake_8y))

##add up exposure score----
exposureb45y_clean<-exposureb45y_clean%>%
  mutate(total=rowSums(across(c(
        probioticintake_6m,
        probioticintake_18m,
        probioticintake_3y,
        probioticintake_5y,
        probioticintake_8y)),na.rm=TRUE))
##everuser/neveruser----
exposureb45y_clean<-exposureb45y_clean%>%
  mutate(probioticintake=case_when(total==0|total>=40~0,
                                   total>0|total<40~1))
#Early Puberty (outcome)----
NHIRD_OPD<-NHIRD_OPD%>%
  mutate(EarlyPuberty_OPD=if_else(
    if_any(c(ICD9CM_1,ICD9CM_2,ICD9CM_3),
           ~ .x %in% c("E301","2591")),1,0))
NHIRD_IPD<-NHIRD_IPD%>%
  mutate(EarlyPuberty_IPD=if_else(
    if_any(c(ICD9CM_1,ICD9CM_2,ICD9CM_3,ICD9CM_4,ICD9CM_5),
           ~ .x %in% c("E301","2591")),1,0))