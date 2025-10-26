sixmdata<-read.csv("/Users/samanthalu/Desktop/thesis/TBCS_stimulated_1/TBCS_6m_simulated.csv")
eighteenmdata<-read.csv("/Users/samanthalu/Desktop/thesis/TBCS_stimulated_1/TBCS_18m_simulated.csv")
threeydata<-read.csv("/Users/samanthalu/Desktop/thesis/TBCS_stimulated_1/TBCS_3y_simulated.csv")
fiveydata<-read.csv("/Users/samanthalu/Desktop/thesis/TBCS_stimulated_1/TBCS_5y_simulated.csv")
eightydata<-read.csv("/Users/samanthalu/Desktop/thesis/TBCS_stimulated_1/TBCS_8y_simulated.csv")

head(sixmdata)
tail(sixmdata)
nrow(sixmdata)

head(eighteenmdata)
tail(eighteenmdata)
nrow(eighteenmdata)

nrow(threeydata)
nrow(fiveydata)
nrow(eightydata)

install.packages("dplyr")
library(dplyr)
sixmdata_fixed<-sixmdata%>%select(Sampleid,wave,age_months,B_SEX,C4a4,C2ad,D1d1,D1d2,D1d3,Fedu,Medu,H13)
sixmdata_fixed<-sixmdata_fixed%>%rename(pribioticintake=C4a4,breastfeeding=C2ad,weight=D1d1,height=D1d2,HC=D1d3,Socioeco=H13)
str(sixmdata_fixed)

eighteenmdata_fixed<-eighteenmdata%>%select(Sampleid,wave,age_months,B_SEX,D4a4,D1a1,D2b,A2_3H,A2_3L,A2_3W,,Fedu,Medu,F10)
eighteenmdata_fixed<-eighteenmdata_fixed%>%rename(probioticintake=D4a4,breastfeeding=D1a1,dairyintake=D2b,weight=A2_3W,height=A2_3L,HC=A2_3H,Socioeco=F10)