#sort data for cox----
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
         FUNC_DATE,
         EarlyPuberty,
         dairyintake_5y,
         breastfeeding,
         medu_5y,
         BMI_5y,
         Socioeco_5y)
#combine selected data----
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
         FUNC_DATE,
         cutoff_date,
         survey_date_8y)

surv_data_clean<-surv_data_clean%>%
  filter(!(EarlyPuberty == 1 & FUNC_DATE < start_date))
###define event----
surv_data_clean<-surv_data_clean %>%
  mutate(event = case_when(
    EarlyPuberty == 1 & FUNC_DATE >= start_date &
      FUNC_DATE <= cutoff_date ~ 1,
      TRUE ~ 0))
###calculate end date----
surv_data_clean<-surv_data_clean%>%
  mutate(end_date = case_when(
      event == 1 ~ FUNC_DATE,
      TRUE ~ cutoff_date),
    followup_time = as.numeric(
      end_date - start_date) / 30)
#Cox model----
##Crude----
cox_model<-coxph(Surv(followup_time, event) ~ probioticintake,
                 data = surv_data_clean)
##adjusted Cox model----
surv_data_labeled<-surv_data_clean%>%
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
dependent = "Surv(followup_time, event)"
surv_data_labeled %>%
  finalfit(dependent, explanatory) -> t4
knitr::kable(t4, row.names=FALSE, align=c("l", "l", "r", "r", "r", "r"))

write.csv(t4, "Table4_HR_results.csv", row.names = FALSE)
#Schoenfeld residual tests----
##cox for Schoenfeld residual tests----
cox_schoenfeld<-coxph(Surv(followup_time, event) ~ 
                        probioticintake.factor +
                        sex.factor +
                        dairyintake.factor +
                        breastfeeding.factor +
                        BMI.numeric +
                        socioeco.factor +
                        medu.factor,
                      data = surv_data_labeled)
## check PH assumption----
cox.zph(cox_model)
##plot----
plot(cox.zph(cox_model))
##HR plot----
surv_data_labeled %>%
  hr_plot(dependent = "Surv(followup_time, event)",
    explanatory = c(
      "probioticintake.factor",
      "sex.factor",
      "dairyintake.factor",
      "breastfeeding.factor",
      "BMI.numeric",
      "socioeco.factor",
      "medu.factor"),
    dependent_label = "Early puberty")
#negative control----
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

write.csv(T5[1],"Table5_negative_control_result.csv",row.names = FALSE)

#instrumental variable----

#make EarlyPuberty numeric 0/1
Finaldataset$EarlyPuberty <- as.numeric(as.character(Finaldataset$EarlyPuberty))

#establish analisys data
dat_iv <- Finaldataset[, c("EarlyPuberty", "probioticintake", "gastroenteritis",
                           "B_SEX", "breastfeeding", "medu_5y",
                           "Socioeco_5y", "dairyintake_5y", "BMI_5y")]

#delete missing value
dat_iv <- subset(dat_iv, is.finite(EarlyPuberty))
dat_iv <- na.omit(dat_iv)

#IV model
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

summary(iv_model, diagnostics = TRUE)

#probiotics grouping----
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

#設定類別順序(回歸分析時通常會拿Never當reference)
pb_grouping$probiotic_group <- factor(
  pb_grouping$probiotic_group,
  levels = c("Never", 
             "Early", 
             "Late", 
             "Intermittent", 
             "Always"))

#看每組人數
table(pb_grouping$probiotic_group, useNA = "ifany")