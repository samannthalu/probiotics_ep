#Select column from different dataset----
##exposure selection----
exposure<-exposureb45y_clean%>%
  select(Sampleid,probioticintake)
##covariate selection----
TBCSstimu5y<-TBCSstimu%>%
  select(Sampleid,B_SEX,height_5y,weight_5y,dairyintake_5y,fedu_5y,medu_5y)
###BMI counting----
TBCSstimu5y<-TBCSstimu5y%>%
  mutate(BMI_5y=weight_5y/(height_5y/100)^2)
##outcome selection----
IPD_outcome<-NHIRD_IPD%>%
  select(Sampleid,IN_DATE,EarlyPuberty_IPD)
OPD_outcome<-NHIRD_OPD%>%
  select(Sampleid,FUNC_DATE,EarlyPuberty_OPD)
###combine NHIRD OPD/IPD outcome(is this necessary?)----
outcome
#Combine final dataset----
TBCSstimu_fixed<-TBCSstimu5y%>%
  full_join(exposure,by="Sampleid")%>%
  full_join(IPD_outcome,by="Sampleid")%>%
  full_join(OPD_outcome,by="Sampleid")

##change factor----
TBCSstimu_fixed$probioticintake<-factor(TBCSstimu_fixed$probioticintake)

#table1----
dat1<-TBCSstimu_fixed

dat1$probioticintake<- 
  factor(dat1$probioticintake,
  levels = c(0, 1),
  labels = c("No", "Yes"))

dat1$B_SEX<- 
  factor(dat1$B_SEX,
  levels = c(1, 2),
  labels = c("Male", "Female"))

dat1$dairyintake_5y<-factor(dat1$dairyintake_5y)
dat1$medu_5y<-factor(dat1$medu_5y)
dat1$height_5y<-as.numeric(dat1$height_5y)
dat1$weight_5y<-as.numeric(dat1$weight_5y)
dat1$BMI_5y<-as.numeric(dat1$BMI_5y)

label(dat1$probioticintake) <- "Probiotic intake"
label(dat1$B_SEX)           <- "Sex"
label(dat1$height_5y)       <- "Height at age 5 (cm)"
label(dat1$weight_5y)       <- "Weight at age 5 (kg)"
label(dat1$BMI_5y)          <- "BMI at age 5 (kg/m²)"
label(dat1$dairyintake_5y)  <- "Dairy intake at age 5"
label(dat1$medu_5y)         <- "Maternal education level"

tab1<-table1( ~ B_SEX+BMI_5y+dairyintake_5y+medu_5y | probioticintake, 
        data = dat1)
save_html(tab1, "table1.html")