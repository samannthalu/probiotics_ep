#install package
install.packages("tidyverse")
library(dplyr)
library(tidyverse)
#input data
sixmdata<-read_csv("/Users/samanthalu/Desktop/thesis/TBCS_stimulated_1/TBCS_6m_simulated.csv")
eighteenmdata<-read_csv("/Users/samanthalu/Desktop/thesis/TBCS_stimulated_1/TBCS_18m_simulated.csv")
threeydata<-read_csv("/Users/samanthalu/Desktop/thesis/TBCS_stimulated_1/TBCS_3y_simulated.csv")
fiveydata<-read_csv("/Users/samanthalu/Desktop/thesis/TBCS_stimulated_1/TBCS_5y_simulated.csv")
eightydata<-read_csv("/Users/samanthalu/Desktop/thesis/TBCS_stimulated_1/TBCS_8y_simulated.csv")

#select and rename variables----
##6month----
sixmdata_fixed<-sixmdata%>%
  select(Sampleid,wave,age_months,B_SEX,C4a4,C2ad,D1d1,D1d2,D1d3,Fedu,Medu,H13)

sixmdata_fixed<-sixmdata_fixed%>%
  rename(pribioticintake_6m=C4a4, 
         breastfeeding_6m=C2ad,
         weight_6m=D1d1,
         height_6m=D1d2,
         HC_6m=D1d3,fedu_6m=Fedu,medu_6m=Medu,Socioeco_6m=H13)
##18month----
eighteenmdata_fixed<-eighteenmdata%>%
  select(Sampleid,wave,age_months,B_SEX,D4a4,D1a1,D2b,A2_3H,A2_3L,A2_3W,,Fedu,Medu,F10)

eighteenmdata_fixed<-eighteenmdata_fixed%>%
  rename(probioticintake_18m=D4a4,
         breastfeeding_18m=D1a1,
         dairyintake_18m=D2b,
         weight_18m=A2_3W,
         height_18m=A2_3L,
         HC_18m=A2_3H,fedu_18m=Fedu,medu_18m=Medu,Socioeco_18m=F10)
##3yeard-old----
threeydata_fixed<-threeydata%>%
  select(Sampleid,wave,age_months,B_SEX,D6a4,A2_3L,A2_3W,Fedu,Medu,F14)

threeydata_fixed<-threeydata_fixed%>%
  rename(probioticintake_3y=D6a4,
         weight_3y=A2_3W,
         height_3y=A2_3L,
         fedu_3y=Fedu,
         medu_3y=Medu,
         Socioeco_3y=F14)
##5year-old----
fiveydata_fixed<-fiveydata%>%
  select(Sampleid,wave,age_months,B_SEX,D5ad,A2_2L,A2_2W,D4_8,FEDU_5Y,MEDU_5Y,G13)

fiveydata_fixed<-fiveydata_fixed%>%
  rename(probioticinkake_5y=D5ad,
         dairyintake_5y=D4_8,
         weight_5y=A2_2W,
         height_5y=A2_2L,
         fedu_5y=FEDU_5Y,
         medu_5y=MEDU_5Y,
         Socioeco_5y=G13)
##8year-old----
eightydata_fixed<-eightydata%>%
  select(Sampleid,wave,age_months,B_SEX,D6ad,A1_1L,A1_1W,D5_8,FEDU_8Y,MEDU_8Y,H13)

eightydata_fixed<-eightydata_fixed%>%
  rename(probioticintake_8y=D6ad,
         dairyintake_8y=D5_8,
         weight_8y=A1_1W,
         height_8y=A1_1L,
         fedu_8y=FEDU_8Y,
         medu_8y=MEDU_8Y,
         Socioeco_8y=H13)
#join data----
TBCSstimu<-sixmdata_fixed%>%
  full_join(eighteenmdata_fixed, by="Sampleid")%>%
  full_join(threeydata_fixed, by="Sampleid")%>%
  full_join(fiveydata_fixed, by="Sampleid")%>%
  full_join(eighteenmdata_fixed, by="Sampleid")