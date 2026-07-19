source("final_dataset.R")
#Survival variables (early puberty)
analytic_loose <- analytic_loose %>%
  mutate(ep_event = case_when(EarlyPuberty_loose == 1 & EarlyPuberty_date >= start_date &
                                EarlyPuberty_date <= cutoff_date ~ 1, TRUE ~ 0),
         ep_end_date      = case_when(ep_event == 1 ~ EarlyPuberty_date, TRUE ~ cutoff_date),
         ep_followup_time = as.numeric(ep_end_date - start_date) / 30)

analytic_strict <- analytic_strict %>%
  mutate(ep_event = case_when(EarlyPuberty_strict == 1 & EarlyPuberty_date >= start_date &
                                EarlyPuberty_date <= cutoff_date ~ 1, TRUE ~ 0),
         ep_end_date      = case_when(ep_event == 1 ~ EarlyPuberty_date, TRUE ~ cutoff_date),
         ep_followup_time = as.numeric(ep_end_date - start_date) / 30)
#Table1 (ever/never, loose)----
dat1 <- analytic_loose
dat1$EarlyPuberty <- factor(dat1$EarlyPuberty_loose, levels=c(0,1),
                            labels=c("No Early Puberty","Early Puberty"))
label(dat1$EarlyPuberty) <- "Early Puberty"
tab1 <- table1( ~ B_SEX+BMI_5y+dairyintake_5y+medu_5y+Socioeco_5y+EarlyPuberty+breastfeeding | probioticintake, data = dat1)
explanatory = c("EarlyPuberty","B_SEX","BMI_5y","dairyintake_5y","breastfeeding","medu_5y","Socioeco_5y")
dependent = "probioticintake"
dat1 %>% summary_factorlist(dependent, explanatory, p=TRUE, add_dependent_label=TRUE,
                            add_col_totals=TRUE, total_col=TRUE) -> t1
write.csv(t1, "Table1_result_probiotic_loose.csv", row.names = FALSE)

#Table1 (ever/never, strict)----
dat1 <- analytic_strict
dat1$EarlyPuberty <- factor(dat1$EarlyPuberty_strict, levels=c(0,1),
                            labels=c("No Early Puberty","Early Puberty"))
label(dat1$EarlyPuberty) <- "Early Puberty"
tab1 <- table1( ~ B_SEX+BMI_5y+dairyintake_5y+medu_5y+Socioeco_5y+EarlyPuberty+breastfeeding | probioticintake, data = dat1)
explanatory = c("EarlyPuberty","B_SEX","BMI_5y","dairyintake_5y","breastfeeding","medu_5y","Socioeco_5y")
dependent = "probioticintake"
dat1 %>% summary_factorlist(dependent, explanatory, p=TRUE, add_dependent_label=TRUE,
                            add_col_totals=TRUE, total_col=TRUE) -> t1
write.csv(t1, "Table1_result_probiotic_strict.csv", row.names = FALSE)

#Table1 (5 patterns, loose)----
dat1 <- analytic_loose
dat1$EarlyPuberty <- factor(dat1$EarlyPuberty_loose, levels=c(0,1),
                            labels=c("No Early Puberty","Early Puberty"))
label(dat1$EarlyPuberty) <- "Early Puberty"
tab1 <- table1( ~ B_SEX+BMI_5y+dairyintake_5y+medu_5y+Socioeco_5y+EarlyPuberty+breastfeeding | probiotic_group, data = dat1)
save_html(tab1, "table1_pattern_loose.html")
explanatory = c("EarlyPuberty","B_SEX","BMI_5y","dairyintake_5y","breastfeeding","medu_5y","Socioeco_5y")
dependent = "probiotic_group"
dat1 %>% summary_factorlist(dependent, explanatory, p=TRUE, add_dependent_label=TRUE,
                            add_col_totals=TRUE, total_col=TRUE) -> t1
write.csv(t1, "Table1_result_pattern_loose.csv", row.names = FALSE)

#Table1 (5 patterns, strict)----
dat1 <- analytic_strict
dat1$EarlyPuberty <- factor(dat1$EarlyPuberty_strict, levels=c(0,1),
                            labels=c("No Early Puberty","Early Puberty"))
label(dat1$EarlyPuberty) <- "Early Puberty"
tab1 <- table1( ~ B_SEX+BMI_5y+dairyintake_5y+medu_5y+Socioeco_5y+EarlyPuberty+breastfeeding | probiotic_group, data = dat1)
explanatory = c("EarlyPuberty","B_SEX","BMI_5y","dairyintake_5y","breastfeeding","medu_5y","Socioeco_5y")
dependent = "probiotic_group"
dat1 %>% summary_factorlist(dependent, explanatory, p=TRUE, add_dependent_label=TRUE,
                            add_col_totals=TRUE, total_col=TRUE) -> t1
write.csv(t1, "Table1_result_pattern_strict.csv", row.names = FALSE)

#Table1 (boy/girl, loose)----
dat1 <- analytic_loose
dat1$EarlyPuberty <- factor(dat1$EarlyPuberty_loose, levels=c(0,1),
                            labels=c("No Early Puberty","Early Puberty"))
label(dat1$EarlyPuberty) <- "Early Puberty"
tab1 <- table1( ~ probioticintake+BMI_5y+dairyintake_5y+medu_5y+Socioeco_5y+EarlyPuberty+breastfeeding | B_SEX, data = dat1)
explanatory = c("EarlyPuberty","probioticintake","BMI_5y","dairyintake_5y","breastfeeding","medu_5y","Socioeco_5y")
dependent = "B_SEX"
dat1 %>% summary_factorlist(dependent, explanatory, p=TRUE, add_dependent_label=TRUE,
                            add_col_totals=TRUE, total_col=TRUE) -> t1
write.csv(t1, "Table1_result_sex_loose.csv", row.names = FALSE)

#Table1 (boy/girl, strict)----
dat1 <- analytic_strict
dat1$EarlyPuberty <- factor(dat1$EarlyPuberty_strict, levels=c(0,1),
                            labels=c("No Early Puberty","Early Puberty"))
label(dat1$EarlyPuberty) <- "Early Puberty"
tab1 <- table1( ~ probioticintake+BMI_5y+dairyintake_5y+medu_5y+Socioeco_5y+EarlyPuberty+breastfeeding | B_SEX, data = dat1)
explanatory = c("EarlyPuberty","probioticintake","BMI_5y","dairyintake_5y","breastfeeding","medu_5y","Socioeco_5y")
dependent = "B_SEX"
dat1 %>% summary_factorlist(dependent, explanatory, p=TRUE, add_dependent_label=TRUE,
                            add_col_totals=TRUE, total_col=TRUE) -> t1
write.csv(t1, "Table1_result_sex_strict.csv", row.names = FALSE)

#Main Cox Early puberty----
cov_A1 <- c("B_SEX","dairyintake_5y","breastfeeding","medu_5y","Socioeco_5y","BMI_5y")  # adjusted all+BMI
cov_A2 <- c("B_SEX","dairyintake_5y","breastfeeding","medu_5y","Socioeco_5y")           # adjusted all-BMI
cov_A3 <- c("B_SEX","medu_5y","Socioeco_5y","BMI_5y")                                   # adjusted4+BMI
cov_A4 <- c("B_SEX","medu_5y","Socioeco_5y")                                            # adjusted4-BMI
dependent <- "Surv(ep_followup_time, ep_event)"

##loose----
analytic_loose %>% finalfit(dependent, c("probioticintake", cov_A1)) -> cox_loose_A1
write.csv(cox_loose_A1, "Cox_ep_loose_AABMI.csv", row.names = FALSE)
analytic_loose %>% finalfit(dependent, c("probioticintake", cov_A2)) -> cox_loose_A2
write.csv(cox_loose_A2, "Cox_ep_loose_AAnoBMI.csv", row.names = FALSE)
analytic_loose %>% finalfit(dependent, c("probioticintake", cov_A3)) -> cox_loose_A3
write.csv(cox_loose_A3, "Cox_ep_loose_A4BMI.csv", row.names = FALSE)
analytic_loose %>% finalfit(dependent, c("probioticintake", cov_A4)) -> cox_loose_A4
write.csv(cox_loose_A4, "Cox_ep_loose_A4noBMI.csv", row.names = FALSE)

##strict----
analytic_strict %>% finalfit(dependent, c("probioticintake", cov_A1)) -> cox_strict_A1
write.csv(cox_strict_A1, "Cox_ep_strict_AABMI.csv", row.names = FALSE)
analytic_strict %>% finalfit(dependent, c("probioticintake", cov_A2)) -> cox_strict_A2
write.csv(cox_strict_A2, "Cox_ep_strict_AAnoBMI.csv", row.names = FALSE)
analytic_strict %>% finalfit(dependent, c("probioticintake", cov_A3)) -> cox_strict_A3
write.csv(cox_strict_A3, "Cox_ep_strict_A4BMI.csv", row.names = FALSE)
analytic_strict %>% finalfit(dependent, c("probioticintake", cov_A4)) -> cox_strict_A4
write.csv(cox_strict_A4, "Cox_ep_strict_A4noBMI.csv", row.names = FALSE)

#Schoenfeld PH check----
ph_formula <- Surv(ep_followup_time, ep_event) ~ probioticintake + B_SEX +
  dairyintake_5y + breastfeeding + medu_5y + Socioeco_5y + BMI_5y

##loose----
cox_ph_loose <- coxph(ph_formula, data = analytic_loose)
zph_loose <- cox.zph(cox_ph_loose)
zt_loose <- as.data.frame(zph_loose$table); zt_loose$term <- rownames(zt_loose)
write.csv(zt_loose, "Schoenfeld_pvalue_ep_loose_AABMI.csv", row.names = FALSE)
png("Schoenfeld_ep_loose_AABMI.png", width = 10, height = 8, units = "in", res = 300)
par(mfrow = c(2,4)); plot(zph_loose); dev.off()

##strict----
cox_ph_strict <- coxph(ph_formula, data = analytic_strict)
zph_strict <- cox.zph(cox_ph_strict)
zt_strict <- as.data.frame(zph_strict$table); zt_strict$term <- rownames(zt_strict)
write.csv(zt_strict, "Schoenfeld_pvalue_ep_strict_AABMI.csv", row.names = FALSE)
png("Schoenfeld_ep_strict_AABMI.png", width = 10, height = 8, units = "in", res = 300)
par(mfrow = c(2,4)); plot(zph_strict); dev.off()