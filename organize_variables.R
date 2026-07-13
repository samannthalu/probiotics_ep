#install package----
install.packages("tidyverse")
install.packages("arsenal")
install.packages("table1")
install.packages("finalfit")
install.packages("survminer")
install.packages("lubridate")
install.packages("AER")
library(dplyr)
library(tidyverse)
library(readr)
library(table1)
library(arsenal)
library(htmltools)
library(finalfit)
library(survival)
library(survminer)
library(lubridate)
library(AER)
#input data----
sixmdata<-read_csv("/Users/samanthalu/Desktop/thesis/TBCS_stimulated_V2/TBCS_6m_simulated.csv")
eighteenmdata<-read_csv("/Users/samanthalu/Desktop/thesis/TBCS_stimulated_V2/TBCS_18m_simulated.csv")
threeydata<-read_csv("/Users/samanthalu/Desktop/thesis/TBCS_stimulated_V2/TBCS_3y_simulated.csv")
fiveydata<-read_csv("/Users/samanthalu/Desktop/thesis/TBCS_stimulated_V2/TBCS_5y_simulated.csv")
NHIRD_IPD<-read_csv("/Users/samanthalu/Desktop/thesis/TBCS_NHIRD_stimulated/NHIRD_IPD_simulated.csv")
NHIRD_OPD<-read_csv("/Users/samanthalu/Desktop/thesis/TBCS_NHIRD_stimulated/NHIRD_OPD_simulated.csv")
#select and rename variables----
##6month----
sixmdata_fixed<-sixmdata%>%
  select(Sampleid,B_SEX,C4a4,C2ad,C2aM,Medu,H13,
         CVR8,CVR9,CVR10,E5a_2,E5b2_2,E5a_3,E5b3_1)

sixmdata_fixed<-sixmdata_fixed%>%
  rename(Sex=B_SEX,
         probioticintake_6m=C4a4,
         breastfeedingmonths_6m=C2aM,
         breastfeedingdays_6m=C2ad,
         medu_6m=Medu,
         Socioeco_6m=H13,
         m6_year=CVR8,
         m6_month=CVR9,
         m6_day=CVR10,
         doc_gastro_6m=E5a_2,
         gastroenteritis_6m=E5b2_2,
         doc_AD_6m=E5a_3,
         AD_6m=E5b3_1)
##18month----
eighteenmdata_fixed<-eighteenmdata%>%
  select(Sampleid,D4a4,D2b,Medu,F10,E3a_2,E3b2_2,E3a_3,E3b3_2)

eighteenmdata_fixed<-eighteenmdata_fixed%>%
  rename(probioticintake_18m=D4a4,
         dairyintake_18m=D2b,
         medu_18m=Medu,
         Socioeco_18m=F10,
         doc_gastro_18m=E3a_2,
         gastroenteritis_18m=E3b2_2,
         doc_AD_18m=E3a_3,
         AD_18m=E3b3_2)
##3yeard-old----
threeydata_fixed<-threeydata%>%
  select(Sampleid,D6a4,Medu,F14,E3a2,E3b2_2,E4c_6)

threeydata_fixed<-threeydata_fixed%>%
  rename(probioticintake_3y=D6a4,
         medu_3y=Medu,
         Socioeco_3y=F14,
         physician_diagnosis_3y=E3a2,
         gastroenteritis_3y=E3b2_2,
         doc_AD_3y=E4c_6)
##5year-old----
fiveydata_fixed<-fiveydata%>%
  select(Sampleid,D5ad,A2_2L,A2_2W,D4_8,MEDU_5Y,G13,E4a2,E4b2_b,E5c_4)

fiveydata_fixed<-fiveydata_fixed%>%
  rename(probioticintake_5y=D5ad,
         dairyintake_5y=D4_8,
         weight_5y=A2_2W,
         height_5y=A2_2L,
         medu_5y=MEDU_5Y,
         Socioeco_5y=G13,
         physician_diagnosis_5y=E4a2,
         gastroenteritis_5y=E4b2_b,
         doc_AD_5y=E5c_4)
#join data----
TBCSstimu<-sixmdata_fixed%>%
  full_join(eighteenmdata_fixed, by="Sampleid")%>%
  full_join(threeydata_fixed, by="Sampleid")%>%
  full_join(fiveydata_fixed, by="Sampleid")