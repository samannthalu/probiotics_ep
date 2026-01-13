#Select column from different dataset----
##exposure selection----
exposure<-exposureb45y_clean%>%
  select(Sampleid,probioticintake)
##covariate selection----
TBCSstimu5y<-TBCSstimu%>%
  select(Sampleid,B_SEX,height_5y,weight_5y,dairyintake_5y,fedu_5y,medu_5y)
###BMI counting----
TBCSstimu5y<-TBCSstimu5y%>%
  mutate(BMI_5y=weight_5y/(height_5y/100)^2)
##outcome selection----
IPD_outcome<-NHIRD_IPD%>%
  select(Sampleid,IN_DATE,EarlyPuberty_IPD)
OPD_outcome<-NHIRD_OPD%>%
  select(Sampleid,FUNC_DATE,EarlyPuberty_OPD)
###combine NHIRD OPD/IPD outcome(is this neccesary?)
outcome
#Combine final dataset
TBCSstimu_fixed<-TBCSstimu5y%>%
  full_join(exposure,by="Sampleid")%>%
  full_join(NHIRD_IPD,by="Sampleid")%>%
  full_join(NHIRD_OPD,by="Sampleid")