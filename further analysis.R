source("final_dataset.R")
#Adjustment covariate sets---- 
cov_A2 <- c("B_SEX","dairyintake_5y","breastfeeding","medu_5y","Socioeco_5y")
cov_A4 <- c("B_SEX","medu_5y","Socioeco_5y") #(no BMI:A2 adjusted all(AA)、adjusted 4(A4)

#logistic regression----
##loose AA----
dat <- analytic_loose %>%
  mutate(EarlyPuberty = factor(EarlyPuberty_loose, levels=c(0,1), labels=c("No","Yes")))
dat %>% finalfit("EarlyPuberty", c("probioticintake", cov_A2), metric=TRUE) -> logit_loose_A2
write.csv(logit_loose_A2, "Table_logistic_loose_AA.csv", row.names=FALSE)

##loose A4----
dat %>% finalfit("EarlyPuberty", c("probioticintake", cov_A4), metric=TRUE) -> logit_loose_A4
write.csv(logit_loose_A4, "Table_logistic_loose_A4.csv", row.names=FALSE)

##strict AA----
dat <- analytic_strict %>%
  mutate(EarlyPuberty = factor(EarlyPuberty_strict, levels=c(0,1), labels=c("No","Yes")))
dat %>% finalfit("EarlyPuberty", c("probioticintake", cov_A2), metric=TRUE) -> logit_strict_A2
write.csv(logit_strict_A2, "Table_logistic_strict_AA.csv", row.names=FALSE)

##strict A4----
dat %>% finalfit("EarlyPuberty", c("probioticintake", cov_A4), metric=TRUE) -> logit_strict_A4
write.csv(logit_strict_A4, "Table_logistic_strict_A4.csv", row.names=FALSE)

#Probiotic grouping----
##loose A2----
dat <- analytic_loose %>%
  mutate(EarlyPuberty = factor(EarlyPuberty_loose, levels=c(0,1), labels=c("No","Yes")))
dat %>% finalfit("EarlyPuberty", c("probiotic_group", cov_A2), metric=TRUE) -> pbgroup_loose_A2
write.csv(pbgroup_loose_A2, "Table_probiotic_group_loose_AA.csv", row.names=FALSE)

##loose A4----
dat %>% finalfit("EarlyPuberty", c("probiotic_group", cov_A4), metric=TRUE) -> pbgroup_loose_A4
write.csv(pbgroup_loose_A4, "Table_probiotic_group_loose_A4.csv", row.names=FALSE)

##strict A2----
dat <- analytic_strict %>%
  mutate(EarlyPuberty = factor(EarlyPuberty_strict, levels=c(0,1), labels=c("No","Yes")))
dat %>% finalfit("EarlyPuberty", c("probiotic_group", cov_A2), metric=TRUE) -> pbgroup_strict_A2
write.csv(pbgroup_strict_A2, "Table_probiotic_group_strict_AA.csv", row.names=FALSE)

##strict A4----
dat %>% finalfit("EarlyPuberty", c("probiotic_group", cov_A4), metric=TRUE) -> pbgroup_strict_A4
write.csv(pbgroup_strict_A4, "Table_probiotic_group_strict_A4.csv", row.names=FALSE)

#Sex-stratified----
run_sexstrat <- function(dat, covs, tag){
  covs_nosex <- setdiff(covs, "B_SEX")
  res <- bind_rows(lapply(c("Male","Female"), function(sx){
    dat %>% filter(B_SEX == sx) %>%
      finalfit("Surv(ep_followup_time, ep_event)", c("probioticintake", covs_nosex)) %>%
      rename(Variable = 1) %>%
      mutate(Variable = na_if(Variable, "")) %>%
      fill(Variable, .direction = "down") %>%
      filter(grepl("Probiotic", Variable, ignore.case = TRUE)) %>%
      mutate(Subgroup = sx)
  })) %>% select(Subgroup, everything())
  write.csv(res, paste0("Table_sexstrat_HR_", tag, ".csv"), row.names = FALSE)
}

run_sexstrat(analytic_loose,  cov_A2, "loose_A2")
run_sexstrat(analytic_loose,  cov_A4, "loose_A4")
run_sexstrat(analytic_strict, cov_A2, "strict_A2")
run_sexstrat(analytic_strict, cov_A4, "strict_A4")

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
                            labels = c("Never or less than 1 time a week",
                                       "Never or less than 1 time a week",
                                       "1-2 times a week",
                                       "3 to 5 times a week",
                                       "everyday/almost everyday",
                                       "everyday/almost everyday",
                                       "everyday/almost everyday",
                                       "everyday/almost everyday"))
dat5$medu_5y<-factor(dat5$medu_5y,
                     levels=c(1,2,3),
                     labels=c("Junior High&below",
                              "Senior High/Vocational",
                              "University&above"))
dat5$Socioeco_5y<-factor(dat5$Socioeco_5y,
                         levels=c(1,2,3,4,5,6,7,8,9,88,98,99),
                         labels=c("30,000",
                                  "30,000",
                                  "30,000",
                                  ">=30,000,<50,000",
                                  ">=50,000,<70,000",
                                  ">=70,000,<100,000",
                                  ">=100,000,<150,000",
                                  ">=150,000,<200,000",
                                  ">=200,000",
                                  ">=50,000,<70,000",
                                  ">=50,000,<70,000",
                                  ">=50,000,<70,000"))
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
##check event distribution----
ap_check <- surv_data_clean %>%
  mutate(
    probioticintake = factor(probioticintake, levels = c(0,1),
                             labels = c("Never-users","Ever-users")),
    ap_event = factor(ap_event, levels = c(0,1),
                      labels = c("No Appendicitis","Appendicitis")),
    B_SEX = factor(B_SEX, levels = c(1,2),
                   labels = c("Male","Female")))

tab_check <- table1(~ ap_event + B_SEX | probioticintake, data = ap_check)
save_html(tab_check, "appendicitis_check.html")
##Crude----
cox_model_ap<-coxph(Surv(ap_followup_time, ap_event) ~ probioticintake,
                    data = surv_data_clean)
##adjusted Cox model----
surv_data_labeled_ap<-surv_data_clean%>%
  mutate(probioticintake.factor = factor(probioticintake,levels = c(0, 1),
                                         labels = c("No","Yes")),
         sex.factor = factor(B_SEX,levels = c(1, 2),
                             labels = c("Boy", "Girl")),
         breastfeeding.factor = factor(breastfeeding,levels = c(0, 1),
                                       labels = c("No breastfeeding","Breastfeeding")),
         dairyintake.factor = factor(dairyintake_5y,levels = c(0,1,2,3,4,5,8,9),
                                     labels = c("Never or less than 1 time a week",
                                                "Never or less than 1 time a week",
                                                "1-2 times a week",
                                                "3 to 5 times a week",
                                                "everyday/almost everyday",
                                                "everyday/almost everyday",
                                                "everyday/almost everyday",
                                                "everyday/almost everyday")),
         medu.factor = factor(medu_5y,levels = c(1,2,3),
                              labels = c ("Junior High&below",
                                          "Senior High/Vocational",
                                          "University&above")),
         socioeco.factor = factor(Socioeco_5y,levels = c(1,2,3,4,5,6,7,8,9,88,98,99),
                                  labels = c("30,000",
                                             "30,000",
                                             "30,000",
                                             ">=30,000,<50,000",
                                             ">=50,000,<70,000",
                                             ">=70,000,<100,000",
                                             ">=100,000,<150,000",
                                             ">=150,000,<200,000",
                                             ">=200,000",
                                             ">=50,000,<70,000",
                                             ">=50,000,<70,000",
                                             ">=50,000,<70,000")),
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
par(mfrow = c(2, 4))
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
###Negative control(no BMI)----
surv_data_labeled_ap_noBMI<-surv_data_clean%>%
  mutate(probioticintake.factor = factor(probioticintake,levels = c(0, 1),
                                         labels = c("No","Yes")),
         sex.factor = factor(B_SEX,levels = c(1, 2),
                             labels = c("Boy", "Girl")),
         breastfeeding.factor = factor(breastfeeding,levels = c(0, 1),
                                       labels = c("No breastfeeding","Breastfeeding")),
         dairyintake.factor = factor(dairyintake_5y,levels = c(0,1,2,3,4,5,8,9),
                                     labels = c("Never or less than 1 time a week",
                                                "Never or less than 1 time a week",
                                                "1-2 times a week",
                                                "3 to 5 times a week",
                                                "everyday/almost everyday",
                                                "everyday/almost everyday",
                                                "everyday/almost everyday",
                                                "everyday/almost everyday")),
         medu.factor = factor(medu_5y,levels = c(1,2,3),
                              labels = c ("Junior High&below",
                                          "Senior High/Vocational",
                                          "University&above")),
         socioeco.factor = factor(Socioeco_5y,levels = c(1,2,3,4,5,6,7,8,9,88,98,99),
                                  labels = c("30,000",
                                             "30,000",
                                             "30,000",
                                             ">=30,000,<50,000",
                                             ">=50,000,<70,000",
                                             ">=70,000,<100,000",
                                             ">=100,000,<150,000",
                                             ">=150,000,<200,000",
                                             ">=200,000",
                                             ">=50,000,<70,000",
                                             ">=50,000,<70,000",
                                             ">=50,000,<70,000")))


explanatory_noBMI=c("probioticintake.factor",
                    "sex.factor",
                    "dairyintake.factor",
                    "breastfeeding.factor",
                    "socioeco.factor",
                    "medu.factor")
dependent = "Surv(ap_followup_time, ap_event)"
surv_data_labeled_ap_noBMI %>%
  finalfit(dependent, explanatory_noBMI) -> t4_ap_noBMI
knitr::kable(t4_ap_noBMI, row.names=FALSE, align=c("l", "l", "r", "r", "r", "r"))

write.csv(t4_ap_noBMI, "Table4_HR_results_ap_noBMI.csv", row.names = FALSE)
###cox for Schoenfeld residual tests(no BMI)----
cox_schoenfeld_ap_noBMI<-coxph(Surv(ap_followup_time, ap_event) ~ 
probioticintake.factor +
  sex.factor +
  dairyintake.factor +
  breastfeeding.factor +
  socioeco.factor +
  medu.factor,
data = surv_data_labeled_ap_noBMI)
####check PH assumption----
cox.zph(cox_schoenfeld_ap_noBMI)
####plot----
png("Cox_PH_Check_ap_noBMI.png", width=8, height=6, units="in", res=400)
plot(cox.zph(cox_schoenfeld_ap_noBMI))
dev.off()

png("Cox_PH_Check_ap_V2_noBMI.png", width = 8, height = 6, units = "in", res = 400)
par(mfrow = c(2, 4))
plot(cox.zph(cox_schoenfeld_ap_noBMI), 
     cex = 0.8,     #調整殘差點的大小
     resid = TRUE)  #確保顯示殘差點
abline(h = 0, col = "red", lty = 3) #y=0 reference
dev.off()
####HR plot----
png("HR_plot_final_ap_noBMI.png", width = 12, height = 10, units = "in", res = 400)

print(
  surv_data_labeled_ap_noBMI %>%
    hr_plot(dependent = "Surv(ap_followup_time, ap_event)",
            explanatory = c(
              "probioticintake.factor",
              "sex.factor",
              "dairyintake.factor",
              "breastfeeding.factor",
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
#Instrumental variable model (no BMI) ----
## make EarlyPuberty numeric ----
Finaldataset$EarlyPuberty <- as.numeric(as.character(Finaldataset$EarlyPuberty))

## establish analysis data ----
dat_iv_noBMI <- Finaldataset[, c(
  "EarlyPuberty",
  "probioticintake",
  "gastroenteritis",
  "B_SEX",
  "breastfeeding",
  "medu_5y",
  "Socioeco_5y",
  "dairyintake_5y"
)]

## delete missing value ----
dat_iv_noBMI <- subset(dat_iv_noBMI, is.finite(EarlyPuberty))
dat_iv_noBMI <- na.omit(dat_iv_noBMI)

## IV model without BMI ----
iv_model_noBMI <- ivreg(
  EarlyPuberty ~ 
    probioticintake + 
    B_SEX + 
    breastfeeding + 
    medu_5y + 
    Socioeco_5y + 
    dairyintake_5y
  |
    gastroenteritis + 
    B_SEX + 
    breastfeeding + 
    medu_5y + 
    Socioeco_5y + 
    dairyintake_5y,
  data = dat_iv_noBMI)

sum_iv_noBMI <- summary(iv_model_noBMI, diagnostics = TRUE)

capture.output(
  sum_iv_noBMI,
  file = "IV_Summary_Result_noBMI.txt")