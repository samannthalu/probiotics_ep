#Select column from different dataset----
##exposure selection----
exposure<-exposureb45y_clean%>%
  select(Sampleid,probioticintake)
##gastroenteritis selection----
gastro<-gastro_all%>%
  select(Sampleid,gastroenteritis)
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
         Socioeco_5y,)
###breastfeeding duration calculate---
TBCSstimu5y<-TBCSstimu5y%>%    
  mutate(breastfeeding=if_else((breastfeedingmonths_6m>1&breastfeedingmonths_6m<99)|
       (breastfeedingdays_6m>30&breastfeedingdays_6m<999),1,0))
###BMI calculate----
TBCSstimu5y<-TBCSstimu5y%>%
  mutate(BMI_5y=weight_5y/(height_5y/100)^2)
#Outcome selection----
IPD_outcome<-NHIRD_IPD%>%
  select(Sampleid,IN_DATE,EarlyPuberty_IPD)
OPD_outcome<-NHIRD_OPD%>%
  select(Sampleid,FUNC_DATE,EarlyPuberty_OPD)
##combine NHIRD OPD/IPD outcome----
IPD_outcome_rename<-IPD_outcome%>%
  rename(EarlyPuberty_FUNC_DATE=IN_DATE,
         EarlyPuberty=EarlyPuberty_IPD)
OPD_outcome_rename<-OPD_outcome%>%
  rename(EarlyPuberty=EarlyPuberty_OPD,
         EarlyPuberty_FUNC_DATE=FUNC_DATE)
EarlyPuberty_combine<-rbind(IPD_outcome_rename,OPD_outcome_rename)
###find case IDs----
caseID<-EarlyPuberty_combine%>%
  filter(EarlyPuberty==1)%>%
  pull(Sampleid)%>%
  unique()
###deal with case(EarlyPuberty=1)----
cases<-EarlyPuberty_combine%>% 
  filter(Sampleid %in% caseID)%>%
  filter(EarlyPuberty==1)%>% #find out people who had early puberty
  group_by(Sampleid)%>%
  slice_min(`EarlyPuberty_FUNC_DATE`,n=1)%>% #use their earliest record
  ungroup()
###deal with controls(EarlyPuberty=0)
controls<-EarlyPuberty_combine%>%
  filter(!Sampleid %in% caseID)%>% 
  group_by(Sampleid)%>%
  slice_max(`EarlyPuberty_FUNC_DATE`,n=1)%>% #people who doesn't have early puberty use their last record
  ungroup()
###combine----
outcome_combine_ep<-rbind(cases,controls)

#Outcome selection(appendicitis)----
IPD_outcome_ap<-NHIRD_IPD%>%
  select(Sampleid,IN_DATE,Appendicitis_IPD)
OPD_outcome_ap<-NHIRD_OPD%>%
  select(Sampleid,FUNC_DATE,Appendicitis_OPD)
##combine NHIRD OPD/IPD outcome(appendicitis)----
IPD_outcome_rename_ap<-IPD_outcome_ap%>%
  rename(Appendicitis_FUNC_DATE=IN_DATE,
         Appendicitis=Appendicitis_IPD)
OPD_outcome_rename_ap<-OPD_outcome_ap%>%
  rename(Appendicitis_FUNC_DATE=FUNC_DATE,
         Appendicitis=Appendicitis_OPD)
Appendicitis_combine<-rbind(IPD_outcome_rename_ap,OPD_outcome_rename_ap)
###find case IDs(appendicitis)----
caseID_ap<-Appendicitis_combine%>%
  filter(Appendicitis==1)%>%
  pull(Sampleid)%>%
  unique()
###deal with case(Appendicitis=1)----
cases_ap<-Appendicitis_combine%>% 
  filter(Sampleid %in% caseID_ap)%>%
  filter(Appendicitis==1)%>% #find out people who had early puberty
  group_by(Sampleid)%>%
  slice_min(`Appendicitis_FUNC_DATE`,n=1)%>% #use their earliest record
  ungroup()
###deal with controls(Appendicitis=0)
controls_ap<-Appendicitis_combine%>%
  filter(!Sampleid %in% caseID_ap)%>% 
  group_by(Sampleid)%>%
  slice_max(`Appendicitis_FUNC_DATE`,n=1)%>% #people who doesn't have early puberty use their last record
  ungroup()
###combine(appencitis)----
outcome_combine_ap<-rbind(cases_ap,controls_ap)
#Combine Final dataset----
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
  full_join(gastro,by="Sampleid")%>%
  full_join(outcome_combine,by="Sampleid")%>%
  full_join(outcome_combine_ap,by="Sampleid")

##change factor----
Finaldataset$probioticintake<-factor(Finaldataset$probioticintake)
Finaldataset$EarlyPuberty<-factor(Finaldataset$EarlyPuberty)
Finaldataset$Appendicitis<-factor(Finaldataset$Appendicitis)