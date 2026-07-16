#Select column from different dataset----
##exposure selection----
exposure<-exposureb45y%>%
  select(Sampleid,
         probioticintake,
         probiotic_score,
         probiotic_group)
##gastroenteritis selection----
gastro<-gastro_all%>%
  select(Sampleid,gastroenteritis)
##AD selection----
AD<-AD_all%>%
  select(Sampleid,AD)
##5y proboiitc available (exclusion)----
prob5y<-prob5y_available

##covariate selection----
TBCSstimu5y<-TBCSstimu%>%
  select(Sampleid,
         B_SEX,
         height_5y,
         weight_5y,
         breastfeedingdays_6m,
         breastfeedingmonths_6m,
         dairyintake_5y,
         medu_5y,
         Socioeco_5y,
         y5_year,
         y5_month,
         y5_day,
         y8_year,
         y8_month,
         y8_day)
###breastfeeding duration calculate---
TBCSstimu5y<-TBCSstimu5y%>%    
  mutate(breastfeeding=if_else((breastfeedingmonths_6m>1&breastfeedingmonths_6m<99)|
       (breastfeedingdays_6m>30&breastfeedingdays_6m<999),1,0))
###BMI calculate----
TBCSstimu5y<-TBCSstimu5y%>%
  mutate(BMI_5y=weight_5y/(height_5y/100)^2)
###Cox analysis date calculate----
###date calculate----
TBCSstimu5y<-TBCSstimu5y%>%
  mutate(start_date     = make_date(y5_year , y5_month, y5_day),   
         survey_date_8y = make_date(y8_year , y8_month, y8_day),   
         cutoff_date    = case_when(B_SEX=="1" ~ start_date + years(4),  
                                    B_SEX=="2" ~ start_date + years(3)))

#Combine Final dataset----
Finaldataset<-TBCSstimu5y%>%
  select(Sampleid,
         B_SEX,
         BMI_5y,
         height_5y,
         weight_5y,
         dairyintake_5y,
         breastfeeding,
         medu_5y,
         Socioeco_5y,
         start_date,
         survey_date_8y,
         cutoff_date)%>%
  left_join(exposure,by="Sampleid")%>%
  left_join(gastro,by="Sampleid")%>%
  left_join(AD,by="Sampleid")%>%
  left_join(prob5y,by="Sampleid")%>%
  left_join(ep_outcome,by="Sampleid")%>% 
  left_join(ap_outcome,by="Sampleid")%>%
  left_join(om_outcome,by="Sampleid")%>%
  
##filter prople didn't follow at wave 5y and had early puberty before age 5----
Finaldataset<-Finaldataset%>%
  filter(!is.na(start_date))%>%
  filter(!(EarlyPuberty == 1 & EarlyPuberty_FUNC_DATE < start_date))

##change factor----
Finaldataset$probioticintake<-factor(Finaldataset$probioticintake)
Finaldataset$EarlyPuberty<-factor(Finaldataset$EarlyPuberty)
Finaldataset$Appendicitis<-factor(Finaldataset$Appendicitis)

##missing pattern----
missing_result<-Finaldataset%>%
  missing_pattern(dependent="EarlyPuberty",
                  explanatory=c("B_SEX",
                                "BMI_5y",
                                "height_5y",
                                "weight_5y",
                                "dairyintake_5y",
                                "breastdeeding",
                                "medu_5y",
                                "Socioeco_5y",
                                "probioticintake",
                                "gastroeneteritis",
                                "Appendicitis"))

missingresult<-as.data.frame(missing_result)
missingresult$n <- rownames(missingresult)
missingresult<- missingresult%>%
  relocate(n)

write.csv(
  missingresult,
  "missing_pattern_result.csv",
  row.names = FALSE)
library(dplyr)

#BMI missing----
missing_summary <- Finaldataset %>%
  summarise(
    Total_N          = n(),
    Height_missing   = sum(is.na(height_5y)),
    Weight_missing   = sum(is.na(weight_5y)),
    Both_missing     = sum(is.na(height_5y) & is.na(weight_5y)),
    Only_height_miss = sum(is.na(height_5y) & !is.na(weight_5y)),
    Only_weight_miss = sum(!is.na(height_5y) & is.na(weight_5y)),
    BMI_missing      = sum(is.na(BMI_5y)))

missing_table <- data.frame(
  Variable = names(missing_summary),
  N        = as.numeric(missing_summary[1, ]),
  Percent  = round(as.numeric(missing_summary[1, ]) / missing_summary$Total_N * 100, 1))

print(missing_table)

write.csv(missing_table, "missing_height_weight_summary.csv", row.names = FALSE)