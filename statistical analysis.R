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
dat1$dairyintake_5y<-factor(dat1$dairyintake_5y,
                            levels=c(0,1,2,3,4,5,8,9),
                            labels=c("Never",
                                     "less than 1 time a week",
                                     "1-2 times a week",
                                     "3 to 5 times a week",
                                     "everyday/almost everyday",
                                     "Unsure","Not Applicable",
                                     "Unknown"))
dat1$medu_5y<-factor(dat1$medu_5y,
                     levels=c(1,2,3),
                     labels=c("Junior High&below",
                              "Senior High/Vocational",
                              "University&above"))
dat1$Socioeco_5y<-factor(dat1$Socioeco_5y,
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
dat1$BMI_5y<-as.numeric(dat1$BMI_5y)

label(dat1$probioticintake) <- "Probiotic intake"
label(dat1$B_SEX)           <- "Sex"
label(dat1$BMI_5y)          <- "BMI at age 5 (kg/m²)"
label(dat1$dairyintake_5y)  <- "Dairy intake at age 5"
label(dat1$medu_5y)         <- "Maternal education level"
label(dat1$Socioeco_5y)     <- "Average month family income(NT$)"
label(dat1$EarlyPuberty)    <- "Early Puberty"
tab1<-table1( ~ B_SEX+BMI_5y+dairyintake_5y+medu_5y+Socioeco_5y+EarlyPuberty | probioticintake, 
              data = dat1)
save_html(tab1, "table1.html")
#table1 pvalue
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
#Table2----
explanatory=c("probioticintake",
              "B_SEX","BMI_5y",
              "dairyintake_5y",
              "breastfeeding",
              "medu_5y",
              "Socioeco_5y")
dependent='EarlyPuberty' 

Finaldataset%>%finalfit(dependent,explanatory,metric=TRUE)->T2

write.csv(T2[1],"Table2_result.csv",row.names = FALSE)
