#sort data for cox----
##select survey date from each wave----
surv_select<-TBCSstimu%>%
  select(Sampleid,
         m6_year,m6_month,m6_day,
         m18_year,m18_month,m18_day,
         y3_year,y3_month,y3_day,
         y5_year,y5_month,y5_day,
         y8_year,y8_month,y8_day)
##select data from each dataset----
surv_select2<-Finaldataset%>%
  select(Sampleid,
         B_SEX,
         probioticintake,
         FUNC_DATE,
         EarlyPuberty)
#combine selected data----
surv_data<-surv_select%>%
  full_join(surv_select2,by="Sampleid")