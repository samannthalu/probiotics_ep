source("final_dataset.R")
#Adjustment covariate sets---- 
cov_A2 <- c("B_SEX","dairyintake_5y","breastfeeding","medu_5y","Socioeco_5y")
cov_A4 <- c("B_SEX","medu_5y","Socioeco_5y") #(no BMI:A2 adjusted all(AA)、adjusted 4(A4)

#Logistic regression----
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
##loose AA----
dat <- analytic_loose %>%
  mutate(EarlyPuberty = factor(EarlyPuberty_loose, levels=c(0,1), labels=c("No","Yes")))
dat %>% finalfit("EarlyPuberty", c("probiotic_group", cov_A2), metric=TRUE) -> pbgroup_loose_A2
write.csv(pbgroup_loose_A2, "Table_probiotic_group_loose_AA.csv", row.names=FALSE)

##loose A4----
dat %>% finalfit("EarlyPuberty", c("probiotic_group", cov_A4), metric=TRUE) -> pbgroup_loose_A4
write.csv(pbgroup_loose_A4, "Table_probiotic_group_loose_A4.csv", row.names=FALSE)

##strict AA----
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

run_sexstrat(analytic_loose,  cov_A2, "loose_AA")
run_sexstrat(analytic_loose,  cov_A4, "loose_A4")
run_sexstrat(analytic_strict, cov_A2, "strict_AA")
run_sexstrat(analytic_strict, cov_A4, "strict_A4")

#Instrumental Variable (gastro/AD)----
run_iv <- function(dat, outcome, instrument, covs, tag){
  d   <- dat %>% select(all_of(c(outcome, "probioticintake", instrument, covs))) %>% na.omit()
  rhs <- paste(covs, collapse = " + ")
  f   <- as.formula(paste0(outcome, " ~ probioticintake + ", rhs,
                           " | ", instrument, " + ", rhs))       # 工具只放一個
  m   <- ivreg(f, data = d)
  capture.output(summary(m, diagnostics = TRUE), file = paste0("IV_", tag, ".txt"))
}

##gastroenteritis----
run_iv(analytic_loose,  "EarlyPuberty_loose",  "gastroenteritis", cov_A2, "gastro_loose_AA")
run_iv(analytic_loose,  "EarlyPuberty_loose",  "gastroenteritis", cov_A4, "gastro_loose_A4")
run_iv(analytic_strict, "EarlyPuberty_strict", "gastroenteritis", cov_A2, "gastro_strict_AA")
run_iv(analytic_strict, "EarlyPuberty_strict", "gastroenteritis", cov_A4, "gastro_strict_A4")

##AD----
run_iv(analytic_loose,  "EarlyPuberty_loose",  "AD", cov_A2, "AD_loose_AA")
run_iv(analytic_loose,  "EarlyPuberty_loose",  "AD", cov_A4, "AD_loose_A4")
run_iv(analytic_strict, "EarlyPuberty_strict", "AD", cov_A2, "AD_strict_AA")
run_iv(analytic_strict, "EarlyPuberty_strict", "AD", cov_A4, "AD_strict_A4")

#Negative control(appendicitis)----
##exclude prevalent appendicitis----
ap_loose <- analytic_loose %>%
  mutate(ap_event = case_when(Appendicitis_loose == 1 & Appendicitis_date >= start_date &
                                Appendicitis_date <= cutoff_date ~ 1, TRUE ~ 0),
         ap_end_date      = case_when(ap_event == 1 ~ Appendicitis_date, TRUE ~ cutoff_date),
         ap_followup_time = as.numeric(ap_end_date - start_date) / 30) %>%
  filter(!(Appendicitis_loose == 1 & Appendicitis_date < start_date))   # 排 prevalent

ap_strict <- analytic_strict %>%
  mutate(ap_event = case_when(Appendicitis_strict == 1 & Appendicitis_date >= start_date &
                                Appendicitis_date <= cutoff_date ~ 1, TRUE ~ 0),
         ap_end_date      = case_when(ap_event == 1 ~ Appendicitis_date, TRUE ~ cutoff_date),
         ap_followup_time = as.numeric(ap_end_date - start_date) / 30) %>%
  filter(!(Appendicitis_strict == 1 & Appendicitis_date < start_date))

## Cox (loose/strict × A2/A4)----
depap <- "Surv(ap_followup_time, ap_event)"
write.csv(ap_loose  %>% finalfit(depap, c("probioticintake", cov_A2)), "NCO_ap_Cox_loose_AA.csv",  row.names=FALSE)
write.csv(ap_loose  %>% finalfit(depap, c("probioticintake", cov_A4)), "NCO_ap_Cox_loo se_A4.csv",  row.names=FALSE)
write.csv(ap_strict %>% finalfit(depap, c("probioticintake", cov_A2)), "NCO_ap_Cox_strict_AA.csv", row.names=FALSE)
write.csv(ap_strict %>% finalfit(depap, c("probioticintake", cov_A4)), "NCO_ap_Cox_strict_A4.csv", row.names=FALSE)

#Otitis media----
##exclude prevalent otitis media----
om_loose <- analytic_loose %>%
  mutate(om_event = case_when(OtitisMedia_loose == 1 & OtitisMedia_date >= start_date &
                                OtitisMedia_date <= cutoff_date ~ 1, TRUE ~ 0),
         om_end_date      = case_when(om_event == 1 ~ OtitisMedia_date, TRUE ~ cutoff_date),
         om_followup_time = as.numeric(om_end_date - start_date) / 30) %>%
  filter(!(OtitisMedia_loose == 1 & OtitisMedia_date < start_date))   # 排 prevalent

om_strict <- analytic_strict %>%
  mutate(om_event = case_when(OtitisMedia_strict == 1 & OtitisMedia_date >= start_date &
                                OtitisMedia_date <= cutoff_date ~ 1, TRUE ~ 0),
         om_end_date      = case_when(om_event == 1 ~ OtitisMedia_date, TRUE ~ cutoff_date),
         om_followup_time = as.numeric(om_end_date - start_date) / 30) %>%
  filter(!(OtitisMedia_strict == 1 & OtitisMedia_date < start_date))

## Cox (loose/strict × A2/A4)----
depom <- "Surv(om_followup_time, om_event)"
write.csv(om_loose  %>% finalfit(depom, c("probioticintake", cov_A2)), "NCO_om_Cox_loose_AA.csv",  row.names=FALSE)
write.csv(om_loose  %>% finalfit(depom, c("probioticintake", cov_A4)), "NCO_om_Cox_loose_A4.csv",  row.names=FALSE)
write.csv(om_strict %>% finalfit(depom, c("probioticintake", cov_A2)), "NCO_om_Cox_strict_AA.csv", row.names=FALSE)
write.csv(om_strict %>% finalfit(depom, c("probioticintake", cov_A4)), "NCO_om_Cox_strict_A4.csv", row.names=FALSE)

#NCO/IV event number----
count_table <- tibble::tibble(
  Variable = c("Appendicitis (loose)", "Appendicitis (strict)",
               "Otitis media (loose)", "Otitis media (strict)",
               "Gastroenteritis", "Atopic dermatitis (AD)"),
  N_yes = c(sum(analytic_loose$Appendicitis_loose  == 1),
            sum(analytic_loose$Appendicitis_strict == 1),
            sum(analytic_loose$OtitisMedia_loose   == 1),
            sum(analytic_loose$OtitisMedia_strict  == 1),
            sum(analytic_loose$gastroenteritis == "Yes", na.rm = TRUE),
            sum(analytic_loose$AD              == "Yes", na.rm = TRUE)),
  Total = nrow(analytic_loose))
count_table$Percent <- round(count_table$N_yes / count_table$Total * 100, 1)

print(count_table)
write.csv(count_table, "NCO_IV_counts.csv", row.names = FALSE)
#VIF----
## A1 (AA + BMI)----
cox_A1_loose <- coxph(Surv(ep_followup_time, ep_event) ~ probioticintake + B_SEX +
                        dairyintake_5y + breastfeeding + medu_5y + Socioeco_5y + BMI_5y,
                      data = analytic_loose)
v <- as.data.frame(car::vif(cox_A1_loose)); v$term <- rownames(v)
write.csv(v, "VIF_ep_loose_A1.csv", row.names = FALSE)

cox_A1_strict <- coxph(Surv(ep_followup_time, ep_event) ~ probioticintake + B_SEX +
                         dairyintake_5y + breastfeeding + medu_5y + Socioeco_5y + BMI_5y,
                       data = analytic_strict)
v <- as.data.frame(car::vif(cox_A1_strict)); v$term <- rownames(v)
write.csv(v, "VIF_ep_strict_A1.csv", row.names = FALSE)

## A2 (AA − BMI)----
cox_A2_loose <- coxph(Surv(ep_followup_time, ep_event) ~ probioticintake + B_SEX +
                        dairyintake_5y + breastfeeding + medu_5y + Socioeco_5y,
                      data = analytic_loose)
v <- as.data.frame(car::vif(cox_A2_loose)); v$term <- rownames(v)
write.csv(v, "VIF_ep_loose_AA.csv", row.names = FALSE)

cox_A2_strict <- coxph(Surv(ep_followup_time, ep_event) ~ probioticintake + B_SEX +
                         dairyintake_5y + breastfeeding + medu_5y + Socioeco_5y,
                       data = analytic_strict)
v <- as.data.frame(car::vif(cox_A2_strict)); v$term <- rownames(v)
write.csv(v, "VIF_ep_strict_AA.csv", row.names = FALSE)
#Stepwise----
run_mystep <- function(dat, include_BMI = TRUE, tag){
  vars <- c("probioticintake","B_SEX","dairyintake_5y","breastfeeding","medu_5y","Socioeco_5y")
  if (include_BMI) vars <- c(vars, "BMI_5y")
  
  d0 <- dat %>% select(ep_followup_time, ep_event, all_of(vars)) %>% na.omit()
  f  <- as.formula(paste("~", paste(vars, collapse = " + ")))
  mm <- model.matrix(f, data = d0)[, -1, drop = FALSE]
  colnames(mm) <- make.names(colnames(mm))
  d  <- data.frame(ep_followup_time = d0$ep_followup_time,
                   ep_event         = d0$ep_event, mm, check.names = FALSE)
  in_var <- grep("^probioticintake", colnames(mm), value = TRUE)
  cand   <- setdiff(colnames(mm), in_var)
  
  #save to txt
  proc <- capture.output(
    My.stepwise.coxph(Time = "ep_followup_time", Status = "ep_event",
                      variable.list = cand, in.variable = in_var,
                      data = d, sle = 0.05, sls = 0.05))
  writeLines(proc, paste0("Stepwise_ep_", tag, ".txt"))
}

run_mystep(analytic_loose,  include_BMI = TRUE,  "loose_A1")
run_mystep(analytic_strict, include_BMI = TRUE,  "strict_A1")
run_mystep(analytic_loose,  include_BMI = FALSE, "loose_A2")
run_mystep(analytic_strict, include_BMI = FALSE, "strict_A2")
#Change-in-estimate----
run_cie <- function(dat, tag){
  covs <- c("B_SEX","dairyintake_5y","breastfeeding","medu_5y","Socioeco_5y","BMI_5y")
  key  <- "probioticintakeEverusers"
  getrow <- function(m, label){
    s <- summary(m)
    data.frame(model = label,
               HR    = round(s$conf.int[key, "exp(coef)"], 3),
               CI    = paste0(round(s$conf.int[key, "lower .95"], 2), "-",
                              round(s$conf.int[key, "upper .95"], 2)),
               p     = signif(s$coefficients[key, "Pr(>|z|)"], 3))
  }
  res <- getrow(coxph(Surv(ep_followup_time, ep_event) ~ probioticintake, data = dat), "Crude")
  for (cv in covs){
    m <- coxph(as.formula(paste("Surv(ep_followup_time, ep_event) ~ probioticintake +", cv)),
               data = dat)
    res <- rbind(res, getrow(m, paste("+", cv)))
  }
  mf <- coxph(as.formula(paste("Surv(ep_followup_time, ep_event) ~ probioticintake +",
                               paste(covs, collapse = " + "))), data = dat)
  res <- rbind(res, getrow(mf, "Full adjusted"))
  rownames(res) <- NULL
  write.csv(res, paste0("ChangeInEstimate_probiotic_", tag, ".csv"), row.names = FALSE)
  res
}

run_cie(analytic_loose,  "loose")
run_cie(analytic_strict, "strict")