#Select column from different dataset----
##exposure selection----
exposure<-exposureb45y_clean%>%
  select(Sampleid,probioticintake)
##covariate selection----
TBCSstimu5y<-TBCSstimu%>%
  select(Sampleid,
         B_SEX,
         height_5y,
         weight_5y,
         breastfeedingdays_6m,
         breastfeedingmonths_6m,
         dairyintake_5y,
         fedu_5y,
         medu_5y,
         Socioeco_5y)
###breastfeeding duration calculate---
TBCSstimu5y<-TBCSstimu5y%>%    
  mutate(breastfeeding=if_else((breastfeedingmonths_6m>1&breastfeedingmonths_6m<99)|
       (breastfeedingdays_6m>30&breastfeedingdays_6m<999),1,0))
###BMI calculate----
TBCSstimu5y<-TBCSstimu5y%>%
  mutate(BMI_5y=weight_5y/(height_5y/100)^2)
##outcome selection----
IPD_outcome<-NHIRD_IPD%>%
  select(Sampleid,IN_DATE,EarlyPuberty_IPD)
OPD_outcome<-NHIRD_OPD%>%
  select(Sampleid,FUNC_DATE,EarlyPuberty_OPD)
###combine NHIRD OPD/IPD outcome----
IPD_outcome_rename<-IPD_outcome%>%
  rename(FUNC_DATE=IN_DATE,
         EarlyPuberty=EarlyPuberty_IPD)
OPD_outcome_rename<-OPD_outcome%>%
  rename(EarlyPuberty=EarlyPuberty_OPD)
EarlyPuberty_combine<-rbind(IPD_outcome_rename,OPD_outcome_rename)
####find case IDs----
caseID<-EarlyPuberty_combine%>%
  filter(EarlyPuberty==1)%>%
  pull(Sampleid)%>%
  unique()
####deal with case (EarlyPuberty=1)----
cases<-EarlyPuberty_combine%>%
  filter(Sampleid %in% caseID)%>%
  filter(EarlyPuberty==1)%>%
  group_by(Sampleid)%>%
  slice_min(`FUNC_DATE`,n=1)%>%
  ungroup()
####deal with controls(EarlyPuberty=0)
controls<-EarlyPuberty_combine%>%
  filter(!Sampleid %in% caseID)%>%
  group_by(Sampleid)%>%
  slice_max(`FUNC_DATE`,n=1)%>%
  ungroup()
####combine----
outcome_combine<-rbind(cases,controls)
#Combine final dataset----
Finaldataset<-TBCSstimu5y%>%
  select(Sampleid,
         B_SEX,
         BMI_5y,
         dairyintake_5y,
         breastfeeding,
         fedu_5y,
         medu_5y,
         Socioeco_5y)%>%
  full_join(exposure,by="Sampleid")%>%
  full_join(outcome_combine,by="Sampleid")

##change factor----
Finaldataset$probioticintake<-factor(Finaldataset$probioticintake)
Finaldataset$EarlyPuberty<-factor(Finaldataset$EarlyPuberty)