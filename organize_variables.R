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
eightydata<-read_csv("/Users/samanthalu/Desktop/thesis/TBCS_stimulated_V2/TBCS_8y_simulated.csv")
NHIRD_IPD<-read_csv("/Users/samanthalu/Desktop/thesis/TBCS_NHIRD_stimulated/NHIRD_IPD_simulated.csv")
NHIRD_OPD<-read_csv("/Users/samanthalu/Desktop/thesis/TBCS_NHIRD_stimulated/NHIRD_OPD_simulated.csv")
#select and rename variables----
##6month----
sixmdata_fixed<-sixmdata%>%
  select(Sampleid,B_SEX,C4a4,C2ad,C2aM,D1d1,D1d2,D1d3,Fedu,Medu,H13,
         CVR8,CVR9,CVR10,E5a_2,E5b2_2)

sixmdata_fixed<-sixmdata_fixed%>%
  rename(probioticintake_6m=C4a4,
         breastfeedingmonths_6m=C2aM,
         breastfeedingdays_6m=C2ad,
         weight_6m=D1d1,
         height_6m=D1d2,
         HC_6m=D1d3,fedu_6m=Fedu,medu_6m=Medu,Socioeco_6m=H13,
         m6_year=CVR8,m6_month=CVR9,m6_day=CVR10,
         physician_diagosis_6m=E5a_2,
         gastroenteritis_6m=E5b2_2)
##18month----
eighteenmdata_fixed<-eighteenmdata%>%
  select(Sampleid,D4a4,D2b,A2_3H,A2_3L,A2_3W,,Fedu,Medu,F10,E3a_2,E3b2_2)

eighteenmdata_fixed<-eighteenmdata_fixed%>%
  rename(probioticintake_18m=D4a4,
         dairyintake_18m=D2b,
         weight_18m=A2_3W,
         height_18m=A2_3L,
         HC_18m=A2_3H,fedu_18m=Fedu,medu_18m=Medu,Socioeco_18m=F10,
         physician_diagnosis_18m=E3a_2,
         gastroenteritis_18m=E3b2_2)
##3yeard-old----
threeydata_fixed<-threeydata%>%
  select(Sampleid,D6a4,A2_3L,A2_3W,Fedu,Medu,F14,E3a2,E3b2_2)

threeydata_fixed<-threeydata_fixed%>%
  rename(probioticintake_3y=D6a4,
         weight_3y=A2_3W,
         height_3y=A2_3L,
         fedu_3y=Fedu,
         medu_3y=Medu,
         Socioeco_3y=F14,
         physician_diagnosis_3y=E3a2,
         gastroenteritis_3y=E3b2_2)
##5year-old----
fiveydata_fixed<-fiveydata%>%
  select(Sampleid,D5ad,A2_2L,A2_2W,D4_8,FEDU_5Y,MEDU_5Y,G13,E4a2,E4b2_b)

fiveydata_fixed<-fiveydata_fixed%>%
  rename(probioticintake_5y=D5ad,
         dairyintake_5y=D4_8,
         weight_5y=A2_2W,
         height_5y=A2_2L,
         fedu_5y=FEDU_5Y,
         medu_5y=MEDU_5Y,
         Socioeco_5y=G13,
         physician_diagnosis_5y=E4a2,
         gastroenteritis_5y=E4b2_b)
##8year-old----
eightydata_fixed<-eightydata%>%
  select(Sampleid,D6ad,A1_1L,A1_1W,D5_8,FEDU_8Y,MEDU_8Y,H13,
         C_Y_18M,C_M_18M,C_D_18M,
         C_Y_3Y,C_M_3Y,C_D_3Y,
         C_Y_5Y,C_M_5Y,C_D_5Y,
         C_Y_8Y,C_M_8Y,C_D_8Y)

eightydata_fixed<-eightydata_fixed%>%
  rename(probioticintake_8y=D6ad,
         dairyintake_8y=D5_8,
         weight_8y=A1_1W,
         height_8y=A1_1L,
         fedu_8y=FEDU_8Y,
         medu_8y=MEDU_8Y,
         Socioeco_8y=H13,
         m18_year=C_Y_18M,
         m18_month=C_M_18M,
         m18_day=C_D_18M,
         y3_year=C_Y_3Y,
         y3_month=C_M_3Y,
         y3_day=C_D_3Y,
         y5_year=C_Y_5Y,
         y5_month=C_M_5Y,
         y5_day=C_D_5Y,
         y8_year=C_Y_8Y,
         y8_month=C_M_8Y,
         y8_day=C_D_8Y,)
#join data----
TBCSstimu<-sixmdata_fixed%>%
  full_join(eighteenmdata_fixed, by="Sampleid")%>%
  full_join(threeydata_fixed, by="Sampleid")%>%
  full_join(fiveydata_fixed, by="Sampleid")%>%
  full_join(eightydata_fixed, by="Sampleid")