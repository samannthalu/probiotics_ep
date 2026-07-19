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

#Negative control----
