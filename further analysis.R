#sort data for cox----
##select survey date from each wave----
surv_select<-TBCSstimu%>%
  select(Sampleid,
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
##organize date----
surv_data<-surv_data%>%
  mutate(start_date=make_date(y5_year, y5_month, y5_day),
         survey_date_8y=make_date(y8_year, y8_month, y8_day))
##classify event and set up end date----
###set cutoff date----
surv_data<-surv_data%>%
  mutate(cutoff_date = case_when(
    B_SEX == "1" ~ start_date + years(4),  #boys end at 9y 
    B_SEX == "2" ~ start_date + years(3))) #girls end at 8y
###eliminate child had early puberty before 5y----
surv_data_clean<-surv_data%>%
  filter(!(EarlyPuberty == 1 & FUNC_DATE < start_date))
###define event----
surv_data_clean<-surv_data_clean %>%
  mutate(event = case_when(
    EarlyPuberty == 1 & FUNC_DATE >= start_date &
      FUNC_DATE <= cutoff_date ~ 1,
      TRUE ~ 0))
###calculate end date----
surv_data_clean<-surv_data_clean%>%
  mutate(end_date = case_when(
      event == 1 ~ FUNC_DATE,
      TRUE ~ cutoff_date),
    followup_time = as.numeric(
      end_date - start_date) / 30)
#Cox model----
cox_model<-coxph(
  Surv(followup_time, event) ~ probioticintake,
  data = surv_data_clean)

summary(cox_model)

#Kaplan Mier----
KM<-survfit(
  Surv(followup_time, event) ~ probioticintake, 
  data = surv_data_clean)

plot(KM, col = c(1,2), lty = 1:2,
     xlab = "Months", ylab = "Survival probability")
legend("bottomleft", legend = c("No probiotic", "Probiotic"),
       col = c(1,2), lty = 1:2)
