#Table1----
dat1<-Finaldataset
dat1$probioticintake<-factor(dat1$probioticintake,
         levels = c(0, 1),
         labels = c("Neverusers", "Everusers"))
dat1$B_SEX<-factor(dat1$B_SEX,
         levels = c(1, 2),
         labels = c("Male", "Female"))
dat1$EarlyPuberty<-factor(dat1$EarlyPuberty,
                          levels = c(0,1),
                          labels = c("No Early Puberty","Early Puberty"))
dat1$breastfeeding<-factor(dat1$breastfeeding,
                           levels = c(0,1),
                           labels = c("No breastfeeding","breastfeeding"))
dat1$dairyintake_5y<-factor(dat1$dairyintake_5y,
                            levels = c(0,1,2,3,4,5,8,9),
                            labels = c("Never or less than 1 time a week",
                                     "Never or less than 1 time a week",
                                     "1-2 times a week",
                                     "3 to 5 times a week",
                                     "everyday/almost everyday",
                                     "everyday/almost everyday",
                                     "everyday/almost everyday",
                                     "everyday/almost everyday"))
dat1$medu_5y<-factor(dat1$medu_5y,
                     levels=c(1,2,3),
                     labels=c("Junior High&below",
                              "Senior High/Vocational",
                              "University&above"))
dat1$Socioeco_5y<-factor(dat1$Socioeco_5y,
                         levels=c(01,02,03,04,05,06,07,08,09,88,98,99),
                         labels=c("30,000",
                                  "30,000",
                                  "30,000",
                                  ">=30,000,<50,000",
                                  ">=50,000,<70,000",
                                  ">=70,000,<100,000",
                                  ">=100,000,<150,000",
                                  ">=150,000,<200,000",
                                  ">=200,000",
                                  "Unknown",
                                  "Unknown",
                                  "Unknown"))
dat1$BMI_5y<-as.numeric(dat1$BMI_5y)

label(dat1$probioticintake) <- "Probiotic intake"
label(dat1$B_SEX)           <- "Sex"
label(dat1$BMI_5y)          <- "BMI at age 5 (kg/m²)"
label(dat1$dairyintake_5y)  <- "Dairy intake at age 5"
label(dat1$medu_5y)         <- "Maternal education level"
label(dat1$Socioeco_5y)     <- "Average month family income(NT$)"
label(dat1$EarlyPuberty)    <- "Early Puberty"
label(dat1$breastfeeding)   <- "Breast feeding"
tab1<-table1( ~ B_SEX+BMI_5y+dairyintake_5y+medu_5y+Socioeco_5y+EarlyPuberty+breastfeeding | probioticintake, 
              data = dat1)
save_html(tab1, "table1.html")
#table1 p-value
explanatory = c("EarlyPuberty",
                "B_SEX","BMI_5y",
                "dairyintake_5y",
                "breastfeeding",
                "medu_5y",
                "Socioeco_5y")
dependent = 'probioticintake'
dat1 %>%
  summary_factorlist(dependent, explanatory, 
                     p=TRUE, add_dependent_label=TRUE) -> t1
write.csv(t1,"Table1_result.csv",row.names = FALSE)
##Table 1(no BMI version)----
dat1<-Finaldataset
dat1$probioticintake<-factor(dat1$probioticintake,
                             levels = c(0, 1),
                             labels = c("Neverusers", "Everusers"))
dat1$B_SEX<-factor(dat1$B_SEX,
                   levels = c(1, 2),
                   labels = c("Male", "Female"))
dat1$EarlyPuberty<-factor(dat1$EarlyPuberty,
                          levels = c(0,1),
                          labels = c("No Early Puberty","Early Puberty"))
dat1$breastfeeding<-factor(dat1$breastfeeding,
                           levels = c(0,1),
                           labels = c("No breastfeeding","breastfeeding"))
dat1$dairyintake_5y<-factor(dat1$dairyintake_5y,
                            levels = c(0,1,2,3,4,5,8,9),
                            labels = c("Never or less than 1 time a week",
                                       "Never or less than 1 time a week",
                                       "1-2 times a week",
                                       "3 to 5 times a week",
                                       "everyday/almost everyday",
                                       "everyday/almost everyday",
                                       "everyday/almost everyday",
                                       "everyday/almost everyday"))
dat1$medu_5y<-factor(dat1$medu_5y,
                     levels=c(1,2,3),
                     labels=c("Junior High&below",
                              "Senior High/Vocational",
                              "University&above"))
dat1$Socioeco_5y<-factor(dat1$Socioeco_5y,
                         levels=c(01,02,03,04,05,06,07,08,09,88,98,99),
                         labels=c("30,000",
                                  "30,000",
                                  "30,000",
                                  ">=30,000,<50,000",
                                  ">=50,000,<70,000",
                                  ">=70,000,<100,000",
                                  ">=100,000,<150,000",
                                  ">=150,000,<200,000",
                                  ">=200,000",
                                  "Unknown",
                                  "Unknown",
                                  "Unknown"))

label(dat1$probioticintake) <- "Probiotic intake"
label(dat1$B_SEX)           <- "Sex"
label(dat1$dairyintake_5y)  <- "Dairy intake at age 5"
label(dat1$medu_5y)         <- "Maternal education level"
label(dat1$Socioeco_5y)     <- "Average month family income(NT$)"
label(dat1$EarlyPuberty)    <- "Early Puberty"
label(dat1$breastfeeding)   <- "Breast feeding"
tab1_noBMI<-table1( ~ B_SEX+dairyintake_5y+medu_5y+Socioeco_5y+EarlyPuberty+breastfeeding | probioticintake, 
              data = dat1)
save_html(tab1_noBMI, "table1.html")
##table1 p-value(no BMI)
explanatory = c("EarlyPuberty",
                "B_SEX",
                "dairyintake_5y",
                "breastfeeding",
                "medu_5y",
                "Socioeco_5y")
dependent = 'probioticintake'
dat1 %>%
  summary_factorlist(dependent, explanatory, 
                     p=TRUE, add_dependent_label=TRUE) -> t1_noBMI
write.csv(t1_noBMI,"Table1_result_noBMI.csv",row.names = FALSE)
#Table2----
explanatory=c("probioticintake",
              "B_SEX",
              "BMI_5y",
              "dairyintake_5y",
              "breastfeeding",
              "medu_5y",
              "Socioeco_5y")
dependent='EarlyPuberty' 

dat1%>%finalfit(dependent,explanatory,metric=TRUE)->T2

write.csv(T2[1],"Table2_result.csv",row.names = FALSE)
#Regression plots----
explanatory=c("probioticintake",
              "B_SEX",
              "BMI_5y",
              "dairyintake_5y",
              "breastfeeding",
              "medu_5y",
              "Socioeco_5y")
dependent='EarlyPuberty' 

dat1%>%or_plot(dependent,explanatory)
##Table2(no BMI version)----
explanatory=c("probioticintake",
              "B_SEX",
              "dairyintake_5y",
              "breastfeeding",
              "medu_5y",
              "Socioeco_5y")
dependent='EarlyPuberty' 

dat1%>%finalfit(dependent,explanatory,metric=TRUE)->T2_noBMI

write.csv(T2_noBMI[1],"Table2_result_noBMI.csv",row.names = FALSE)
##Regression plots----
explanatory=c("probioticintake",
              "B_SEX",
              "dairyintake_5y",
              "breastfeeding",
              "medu_5y",
              "Socioeco_5y")
dependent='EarlyPuberty' 

dat1%>%or_plot(dependent,explanatory)

#Stratified analysis----
dependent = "EarlyPuberty"
explanatory_stratified = c("probioticintake", 
                           "BMI_5y", 
                           "dairyintake_5y", 
                           "breastfeeding", 
                           "medu_5y", 
                           "Socioeco_5y")
##boys----
t3_boys <- dat1 %>%
  filter(B_SEX == "Male") %>% 
  finalfit(dependent, explanatory_stratified) %>%
  rename(Variable = 1) %>%  
  mutate(Variable = na_if(Variable, "")) %>% 
  fill(Variable, .direction = "down") %>% 

filter(grepl("Probiotic", Variable, ignore.case = TRUE)) %>% 
  mutate(Subgroup = "Boys (Male)")
##girls----
t3_girls <- dat1 %>%
  filter(B_SEX == "Female") %>% 
  finalfit(dependent, explanatory_stratified) %>%
  rename(Variable = 1) %>%
  mutate(Variable = na_if(Variable, "")) %>%
  fill(Variable, .direction = "down") %>%

filter(grepl("Probiotic", Variable, ignore.case = TRUE)) %>%
  mutate(Subgroup = "Girls (Female)")
##combine boys and girls----
table3_sex_result <- rbind(t3_boys, t3_girls) %>%
  select(Subgroup, everything())

print(table3_sex_result)

write_csv(table3_sex_result, "Table3_Stratified_by_Sex.csv")
##Stratified analysis(no BMI)----
dependent = "EarlyPuberty"
explanatory_stratified = c("probioticintake", 
                           "BMI_5y", 
                           "dairyintake_5y", 
                           "breastfeeding", 
                           "medu_5y", 
                           "Socioeco_5y")
###boys----
t3_boys_noBMI <- dat1 %>%
  filter(B_SEX == "Male") %>% 
  finalfit(dependent, explanatory_stratified) %>%
  rename(Variable = 1) %>%  
  mutate(Variable = na_if(Variable, "")) %>% 
  fill(Variable, .direction = "down") %>% 
  
  filter(grepl("Probiotic", Variable, ignore.case = TRUE)) %>% 
  mutate(Subgroup = "Boys (Male)")
##girls----
t3_girls_noBMI <- dat1 %>%
  filter(B_SEX == "Female") %>% 
  finalfit(dependent, explanatory_stratified) %>%
  rename(Variable = 1) %>%
  mutate(Variable = na_if(Variable, "")) %>%
  fill(Variable, .direction = "down") %>%
  
  filter(grepl("Probiotic", Variable, ignore.case = TRUE)) %>%
  mutate(Subgroup = "Girls (Female)")
##combine boys and girls----
table3_sex_result_noBMI <- rbind(t3_boys_noBMI, t3_girls_noBMI) %>%
  select(Subgroup, everything())

print(table3_sex_result_noBMI)

write_csv(table3_sex_result_noBMI, "Table3_Stratified_by_Sex_noBMI.csv")