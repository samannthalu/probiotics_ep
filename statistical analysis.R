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
##PHformula+HRplot variables----
ph_A1 <- Surv(ep_followup_time, ep_event) ~ probioticintake + B_SEX + dairyintake_5y + breastfeeding + medu_5y + Socioeco_5y + BMI_5y
ph_A2 <- Surv(ep_followup_time, ep_event) ~ probioticintake + B_SEX + dairyintake_5y + breastfeeding + medu_5y + Socioeco_5y
ph_A3 <- Surv(ep_followup_time, ep_event) ~ probioticintake + B_SEX + medu_5y + Socioeco_5y + BMI_5y
ph_A4 <- Surv(ep_followup_time, ep_event) ~ probioticintake + B_SEX + medu_5y + Socioeco_5y

hr_A1 <- c("probioticintake","B_SEX","dairyintake_5y","breastfeeding","medu_5y","Socioeco_5y","BMI_5y")
hr_A2 <- c("probioticintake","B_SEX","dairyintake_5y","breastfeeding","medu_5y","Socioeco_5y")
hr_A3 <- c("probioticintake","B_SEX","medu_5y","Socioeco_5y","BMI_5y")
hr_A4 <- c("probioticintake","B_SEX","medu_5y","Socioeco_5y")

#PH check----
run_ph <- function(dat, ph_formula, hr_expl, tag){
  m   <- coxph(ph_formula, data = dat, model = TRUE)
  zph <- cox.zph(m)
##PH p-value----
  zt <- as.data.frame(zph$table); zt$term <- rownames(zt)
  write.csv(zt, paste0("Schoenfeld_pvalue_ep_", tag, ".csv"), row.names = FALSE)
  
##PH residual plot
  np <- nrow(zph$table) - 1                 # 變項數(扣掉 GLOBAL)
  nc <- 3; nr <- ceiling(np / nc)           # 每列 3 格
  png(paste0("Cox_PH_Check_ep_", tag, ".png"),
      width = nc*4, height = nr*3.5, units = "in", res = 300)
  par(mfrow = c(nr, nc), mar = c(4,4,2,1))  # 每格邊界留白
  for (i in 1:np){                          # 一格一個變項,各自加參考線
    plot(zph[i], resid = TRUE, cex = 0.7)
    abline(h = 0, col = "red", lty = 3)
  }
  dev.off()
  
##HR forest plot----
  png(paste0("HR_plot_ep_", tag, ".png"), width = 12, height = 10, units = "in", res = 300)
  print(hr_plot(dat, dependent = "Surv(ep_followup_time, ep_event)",
                explanatory = hr_expl, dependent_label = "Early puberty"))
  dev.off()
}
##loose----
run_ph(analytic_loose,  ph_A1, hr_A1, "loose_AABMI")
run_ph(analytic_loose,  ph_A2, hr_A2, "loose_AAnoBMI")
run_ph(analytic_loose,  ph_A3, hr_A3, "loose_A4BMI")
run_ph(analytic_loose,  ph_A4, hr_A4, "loose_A4noBMI")

##strict----
run_ph(analytic_strict, ph_A1, hr_A1, "strict_AABMI")
run_ph(analytic_strict, ph_A2, hr_A2, "strict_AAnoBMI")
run_ph(analytic_strict, ph_A3, hr_A3, "strict_A4BMI")
run_ph(analytic_strict, ph_A4, hr_A4, "strict_A4noBMI")