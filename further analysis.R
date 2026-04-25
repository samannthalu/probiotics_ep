#Sort data for cox----
##select survey date from each wave----
surv_select<-TBCSstimu%>%
  select(Sampleid,
         y5_year,y5_month,y5_day,
         y8_year,y8_month,y8_day)
##select data from each dataset----
surv_select2<-Finaldataset%>%
  select(Sampleid,
         B_SEX,
         probioticintake,
         EarlyPuberty_FUNC_DATE,
         EarlyPuberty,
         Appendicitis_FUNC_DATE,
         Appendicitis,
         dairyintake_5y,
         breastfeeding,
         medu_5y,
         BMI_5y,
         Socioeco_5y)
#Combine selected data----
surv_data<-surv_select%>%
  full_join(surv_select2,by="Sampleid")
##organize date----
surv_data<-surv_data%>%
  mutate(start_date=make_date(y5_year, y5_month, y5_day),
         survey_date_8y=make_date(y8_year, y8_month, y8_day))
##classify event and set up end date----
###set cutoff date----
surv_data<-surv_data%>%
  mutate(cutoff_date = case_when(
    B_SEX == "1" ~ start_date + years(4),  #boys end at 9y 
    B_SEX == "2" ~ start_date + years(3))) #girls end at 8y
###eliminate child had early puberty before 5y----
surv_data_clean<-surv_data%>%
  select(Sampleid,
         B_SEX,
         start_date,
         probioticintake,
         dairyintake_5y,
         breastfeeding,
         BMI_5y,
         Socioeco_5y,
         medu_5y,
         EarlyPuberty,
         EarlyPuberty_FUNC_DATE,
         Appendicitis,
         Appendicitis_FUNC_DATE,
         cutoff_date,
         survey_date_8y)

surv_data_clean<-surv_data_clean%>%
  filter(!(EarlyPuberty == 1 & EarlyPuberty_FUNC_DATE < start_date))

surv_data_clean<-surv_data_clean%>% #appendicitis(negative control)
  filter(!(Appendicitis == 1 & Appendicitis_FUNC_DATE < start_date))
###define event----
surv_data_clean<-surv_data_clean %>%
  mutate(ep_event = case_when(
    EarlyPuberty == 1 & EarlyPuberty_FUNC_DATE >= start_date &
      EarlyPuberty_FUNC_DATE <= cutoff_date ~ 1,
      TRUE ~ 0))

surv_data_clean<-surv_data_clean %>% #appendicitis(negative control)
  mutate(ap_event = case_when(
    Appendicitis == 1 & Appendicitis_FUNC_DATE >= start_date &
      Appendicitis_FUNC_DATE <= cutoff_date ~ 1,
    TRUE ~ 0))
###calculate end date----
surv_data_clean<-surv_data_clean%>%
  mutate(ep_end_date = case_when(
      ep_event == 1 ~ EarlyPuberty_FUNC_DATE,
      TRUE ~ cutoff_date),
    ep_followup_time = as.numeric(
      ep_end_date - start_date) / 30)

surv_data_clean<-surv_data_clean%>% #appendicitis(negative control)
  mutate(ap_end_date = case_when(
    ap_event == 1 ~ Appendicitis_FUNC_DATE,
    TRUE ~ cutoff_date),
    ap_followup_time = as.numeric(
      ap_end_date - start_date) / 30)
#Cox model(EarlyPuberty)----
##Crude----
cox_model_ep<-coxph(Surv(ep_followup_time, ep_event) ~ probioticintake,
                 data = surv_data_clean)
##adjusted Cox model----
surv_data_labeled_ep<-surv_data_clean%>%
  mutate(probioticintake.factor = factor(probioticintake,levels = c(0, 1),
                                   labels = c("No","Yes")),
         sex.factor = factor(B_SEX,levels = c("1", "2"),
                             labels = c("Boy", "Girl")),
         breastfeeding.factor = factor(breastfeeding,levels = c(0, 1),
                                       labels = c("No breastfeeding","Breastfeeding")),
         dairyintake.factor = factor(dairyintake_5y,levels = c(0,1,2,3,4,5,8,9),
                                     labels = c("Never",
                                                "less than 1 time a week",
                                                "1-2 times a week",
                                                "3 to 5 times a week",
                                                "everyday/almost everyday",
                                                "Unsure","Not Applicable",
                                                "Unknown")),
         medu.factor = factor(medu_5y,levels = c(1,2,3),
                              labels = c ("Junior High&below",
                                         "Senior High/Vocational",
                                         "University&above")),
         socioeco.factor = factor(Socioeco_5y,levels = c(1,2,3,4,5,6,7,8,9,88,98,99),
                                  labels = c("<10,000",
                                             ">=10,000,<20,000",
                                             ">=20,000,<30,000",
                                             ">=30,000,<50,000",
                                             ">=50,000,<70,000",
                                             ">=70,000,<100,000",
                                             ">=100,000,<150,000",
                                             ">=150,000,<200,000",
                                             ">=200,000",
                                             "Not Appliable",
                                             "Refused/Don't Know",
                                             "Unknown")),
         BMI.numeric = as.numeric(BMI_5y))

explanatory=c("probioticintake.factor",
              "sex.factor",
              "dairyintake.factor",
              "breastfeeding.factor",
              "BMI.numeric",
              "socioeco.factor",
              "medu.factor")
dependent = "Surv(ep_followup_time, ep_event)"
surv_data_labeled_ep %>%
  finalfit(dependent, explanatory) -> t4
knitr::kable(t4, row.names=FALSE, align=c("l", "l", "r", "r", "r", "r"))

write.csv(t4, "Table4_HR_results_ep.csv", row.names = FALSE)
#Schoenfeld residual tests----
##cox for Schoenfeld residual tests----
cox_schoenfeld_ep<-coxph(Surv(ep_followup_time, ep_event) ~ 
                        probioticintake.factor +
                        sex.factor +
                        dairyintake.factor +
                        breastfeeding.factor +
                        BMI.numeric +
                        socioeco.factor +
                        medu.factor,
                      data = surv_data_labeled_ep)
##check PH assumption----
cox.zph(cox_schoenfeld_ep)
##plot----
png("Cox_PH_Check_ep.png", width=8, height=6, units="in", res=400)
plot(cox.zph(cox_schoenfeld_ep))
dev.off()

png("Cox_PH_Check_ep_V2.png", width = 8, height = 6, units = "in", res = 400)
plot(cox.zph(cox_schoenfeld_ep), 
     cex = 0.8,     #調整殘差點的大小
     resid = TRUE)  #確保顯示殘差點
abline(h = 0, col = "red", lty = 3) #y=0 reference
dev.off()
##HR plot----
png("HR_plot_final_ep.png", width = 12, height = 10, units = "in", res = 400)

print(
  surv_data_labeled_ep %>%
    hr_plot(dependent = "Surv(ep_followup_time, ep_event)",
            explanatory = c(
              "probioticintake.factor",
              "sex.factor",
              "dairyintake.factor",
              "breastfeeding.factor",
              "BMI.numeric",
              "socioeco.factor",
              "medu.factor"),
            dependent_label = "Early puberty"))
dev.off()

#Negative control----
##OR----
dat5<-Finaldataset
dat5$probioticintake<-factor(dat5$probioticintake,
                             levels = c(0, 1),
                             labels = c("Neverusers", "Everusers"))
dat5$B_SEX<-factor(dat5$B_SEX,
                   levels = c(1, 2),
                   labels = c("Male", "Female"))
dat5$Appendicitis<-factor(dat5$Appendicitis,
                          levels = c(0,1),
                          labels = c("No Appendicitis","Appendicitis"))
dat5$breastfeeding<-factor(dat5$breastfeeding,
                           levels = c(0,1),
                           labels = c("No breastfeeding","breastfeeding"))
dat5$dairyintake_5y<-factor(dat5$dairyintake_5y,
                            levels = c(0,1,2,3,4,5,8,9),
                            labels = c("Never",
                                       "less than 1 time a week",
                                       "1-2 times a week",
                                       "3 to 5 times a week",
                                       "everyday/almost everyday",
                                       "Unsure","Not Applicable",
                                       "Unknown"))
dat5$medu_5y<-factor(dat5$medu_5y,
                     levels=c(1,2,3),
                     labels=c("Junior High&below",
                              "Senior High/Vocational",
                              "University&above"))
dat5$Socioeco_5y<-factor(dat5$Socioeco_5y,
                         levels=c(1,2,3,4,5,6,7,8,9,88,98,99),
                         labels=c("<10,000",
                                  ">=10,000,<20,000",
                                  ">=20,000,<30,000",
                                  ">=30,000,<50,000",
                                  ">=50,000,<70,000",
                                  ">=70,000,<100,000",
                                  ">=100,000,<150,000",
                                  ">=150,000,<200,000",
                                  ">=200,000",
                                  "Not Appliable",
                                  "Refused/Don't Know",
                                  "Unknown"))
dat5$BMI_5y<-as.numeric(dat5$BMI_5y)

label(dat5$probioticintake) <- "Probiotic intake"
label(dat5$B_SEX)           <- "Sex"
label(dat5$BMI_5y)          <- "BMI at age 5 (kg/m²)"
label(dat5$dairyintake_5y)  <- "Dairy intake at age 5"
label(dat5$medu_5y)         <- "Maternal education level"
label(dat5$Socioeco_5y)     <- "Average month family income(NT$)"
label(dat5$Appendicitis)    <- "Appendicitis"
label(dat5$breastfeeding)   <- "Breast feeding"
tab5<-table1( ~ B_SEX+BMI_5y+dairyintake_5y+medu_5y+Socioeco_5y+Appendicitis+breastfeeding | probioticintake, 
              data = dat5)
explanatory=c("probioticintake",
              "B_SEX",
              "BMI_5y",
              "dairyintake_5y",
              "breastfeeding",
              "medu_5y",
              "Socioeco_5y")
dependent='Appendicitis'

dat5%>%finalfit(dependent,explanatory,metric=TRUE)->T5

write.csv(T5[1],"Table5_negative_control_ORresult.csv",row.names = FALSE)
##Cox model(appendicitis)----
##Crude----
cox_model_ap<-coxph(Surv(ap_followup_time, ap_event) ~ probioticintake,
                    data = surv_data_clean)
##adjusted Cox model----
surv_data_labeled_ap<-surv_data_clean%>%
  mutate(probioticintake.factor = factor(probioticintake,levels = c(0, 1),
                                         labels = c("No","Yes")),
         sex.factor = factor(B_SEX,levels = c("1", "2"),
                             labels = c("Boy", "Girl")),
         breastfeeding.factor = factor(breastfeeding,levels = c(0, 1),
                                       labels = c("No breastfeeding","Breastfeeding")),
         dairyintake.factor = factor(dairyintake_5y,levels = c(0,1,2,3,4,5,8,9),
                                     labels = c("Never",
                                                "less than 1 time a week",
                                                "1-2 times a week",
                                                "3 to 5 times a week",
                                                "everyday/almost everyday",
                                                "Unsure","Not Applicable",
                                                "Unknown")),
         medu.factor = factor(medu_5y,levels = c(1,2,3),
                              labels = c ("Junior High&below",
                                          "Senior High/Vocational",
                                          "University&above")),
         socioeco.factor = factor(Socioeco_5y,levels = c(1,2,3,4,5,6,7,8,9,88,98,99),
                                  labels = c("<10,000",
                                             ">=10,000,<20,000",
                                             ">=20,000,<30,000",
                                             ">=30,000,<50,000",
                                             ">=50,000,<70,000",
                                             ">=70,000,<100,000",
                                             ">=100,000,<150,000",
                                             ">=150,000,<200,000",
                                             ">=200,000",
                                             "Not Appliable",
                                             "Refused/Don't Know",
                                             "Unknown")),
         BMI.numeric = as.numeric(BMI_5y))

explanatory=c("probioticintake.factor",
              "sex.factor",
              "dairyintake.factor",
              "breastfeeding.factor",
              "BMI.numeric",
              "socioeco.factor",
              "medu.factor")
dependent = "Surv(ap_followup_time, ap_event)"
surv_data_labeled_ap %>%
  finalfit(dependent, explanatory) -> t4
knitr::kable(t4, row.names=FALSE, align=c("l", "l", "r", "r", "r", "r"))

write.csv(t4, "Table4_HR_results_ap.csv", row.names = FALSE)
#Schoenfeld residual tests----
##cox for Schoenfeld residual tests----
cox_schoenfeld_ap<-coxph(Surv(ap_followup_time, ap_event) ~ 
                           probioticintake.factor +
                           sex.factor +
                           dairyintake.factor +
                           breastfeeding.factor +
                           BMI.numeric +
                           socioeco.factor +
                           medu.factor,
                         data = surv_data_labeled_ap)
##check PH assumption----
cox.zph(cox_schoenfeld_ap)
##plot----
png("Cox_PH_Check_ap.png", width=8, height=6, units="in", res=400)
plot(cox.zph(cox_schoenfeld_ap))
dev.off()

png("Cox_PH_Check_ap_V2.png", width = 8, height = 6, units = "in", res = 400)
plot(cox.zph(cox_schoenfeld_ap), 
     cex = 0.8,     #調整殘差點的大小
     resid = TRUE)  #確保顯示殘差點
abline(h = 0, col = "red", lty = 3) #y=0 reference
dev.off()
##HR plot----
png("HR_plot_final_ap.png", width = 12, height = 10, units = "in", res = 400)

print(
  surv_data_labeled_ap %>%
    hr_plot(dependent = "Surv(ap_followup_time, ap_event)",
            explanatory = c(
              "probioticintake.factor",
              "sex.factor",
              "dairyintake.factor",
              "breastfeeding.factor",
              "BMI.numeric",
              "socioeco.factor",
              "medu.factor"),
            dependent_label = "Appendicitis"))
dev.off()

#Instrumental variable----
##make EarlyPuberty numeric----
Finaldataset$EarlyPuberty <- as.numeric(as.character(Finaldataset$EarlyPuberty))

##establish analysis data----
dat_iv <- Finaldataset[, c("EarlyPuberty", "probioticintake", "gastroenteritis",
                           "B_SEX", "breastfeeding", "medu_5y",
                           "Socioeco_5y", "dairyintake_5y", "BMI_5y")]

##delete missing value----
dat_iv <- subset(dat_iv, is.finite(EarlyPuberty))
dat_iv <- na.omit(dat_iv)

##IV model----
iv_model <- ivreg(EarlyPuberty ~ 
    probioticintake + 
    B_SEX + 
    breastfeeding + 
    medu_5y + 
    Socioeco_5y + 
    dairyintake_5y + 
    BMI_5y
  |
    gastroenteritis + 
    B_SEX + 
    breastfeeding + 
    medu_5y + 
    Socioeco_5y + 
    dairyintake_5y + 
    BMI_5y,
  data = dat_iv)

sum_iv<-summary(iv_model, diagnostics = TRUE)
capture.output(sum_iv, file = "IV_Summary_Result.txt")
#Probiotics grouping----
pb_grouping <- exposureb45y

pb_grouping$probiotic_group <- with(pb_grouping, ifelse(
  probioticintake_6m == 0 & probioticintake_18m == 0 &
    probioticintake_3y == 0 & probioticintake_5y == 0,
  "Never",
  
  ifelse(
    probioticintake_6m == 1 & probioticintake_18m == 1 &
      probioticintake_3y == 1 & probioticintake_5y == 1,
    "Always",
    
    ifelse(
      (probioticintake_6m == 1 | probioticintake_18m == 1) &
        probioticintake_3y == 0 & probioticintake_5y == 0,
      "Early",
      
      ifelse(
        probioticintake_6m == 0 & probioticintake_18m == 0 &
          (probioticintake_3y == 1 | probioticintake_5y == 1),
        "Late",
        "Intermittent")))))

pb_grouping$probiotic_group <- factor(
  pb_grouping$probiotic_group,
  levels = c("Never", 
             "Early", 
             "Late", 
             "Intermittent", 
             "Always"))  #Define factor levels: "Never" as reference for regression & logical ordering for plots
                         #(R會預設拿levels第一個當對照組，在此設定never為第一個)

table(pb_grouping$probiotic_group, useNA = "ifany") #check distribution and handle missing values
##combine pb_grouping w finaldataset-----
Finaldataset<-Finaldataset%>%
  full_join(pb_grouping,by="Sampleid")
##logistic regression----
datpbgroup<-Finaldataset
explanatory=c("probiotic_group",
              "B_SEX",
              "BMI_5y",
              "dairyintake_5y",
              "breastfeeding",
              "medu_5y",
              "Socioeco_5y")
dependent='EarlyPuberty' 

datpbgroup%>%finalfit(dependent,explanatory,metric=TRUE)->pbgroup

write.csv(pbgroup[1],"Table_probiotic_group.csv",row.names = FALSE)