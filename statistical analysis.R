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
