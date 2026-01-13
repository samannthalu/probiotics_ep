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
