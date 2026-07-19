#Select column from different dataset----
##exposure selection----
exposure<-exposureb45y%>%
  select(Sampleid,
         probioticintake,
         probiotic_score,
         probiotic_group)
##gastroenteritis selection----
gastro<-gastro_all%>%
  select(Sampleid,gastroenteritis)
##AD selection----
AD<-AD_all%>%
  select(Sampleid,AD)
##5y proboiitc available (exclusion)----
prob5y<-prob5y_available

##covariate selection----
TBCSstimu5y<-TBCSstimu%>%
  select(Sampleid,
         B_SEX,
         height_5y,
         weight_5y,
         breastfeedingdays_6m,
         breastfeedingmonths_6m,
         dairyintake_5y,
         medu_5y,
         Socioeco_5y,
         y5_year,
         y5_month,
         y5_day,
         y8_year,
         y8_month,
         y8_day)
###breastfeeding duration calculate---
TBCSstimu5y<-TBCSstimu5y%>%    
  mutate(breastfeeding=if_else((breastfeedingmonths_6m>1&breastfeedingmonths_6m<99)|
       (breastfeedingdays_6m>30&breastfeedingdays_6m<999),1,0))
###BMI calculate----
TBCSstimu5y<-TBCSstimu5y%>%
  mutate(BMI_5y=weight_5y/(height_5y/100)^2)
###Cox analysis date calculate----
###date calculate----
TBCSstimu5y<-TBCSstimu5y%>%
  mutate(start_date     = make_date(y5_year , y5_month, y5_day),   
         survey_date_8y = make_date(y8_year , y8_month, y8_day),   
         cutoff_date    = case_when(B_SEX=="1" ~ start_date + years(4),  
                                    B_SEX=="2" ~ start_date + years(3)))

#Combine Final dataset----
Finaldataset<-TBCSstimu5y%>%
  select(Sampleid,
         B_SEX,
         BMI_5y,
         height_5y,
         weight_5y,
         dairyintake_5y,
         breastfeeding,
         medu_5y,
         Socioeco_5y,
         start_date,
         survey_date_8y,
         cutoff_date)%>%
  left_join(exposure,by="Sampleid")%>%
  left_join(gastro,by="Sampleid")%>%
  left_join(AD,by="Sampleid")%>%
  left_join(prob5y,by="Sampleid")%>%
  left_join(ep_outcome,by="Sampleid")%>% 
  left_join(ap_outcome,by="Sampleid")%>%
  left_join(om_outcome,by="Sampleid")%>%
  mutate(across(c(EarlyPuberty_loose,EarlyPuberty_strict, # 沒配到NHIRD=沒事件=0
                  Appendicitis_loose,Appendicitis_strict,
                  OtitisMedia_loose, OtitisMedia_strict),
                ~ replace_na(.,0L)))
##change factor----
Finaldataset <- Finaldataset %>%
  mutate(
    probioticintake = factor(probioticintake, levels = c(0,1), labels = c("Neverusers","Everusers")),
    B_SEX = factor(B_SEX, levels = c(1,2), labels = c("Male","Female")),
    breastfeeding = factor(breastfeeding, levels = c(0,1), labels = c("No breastfeeding","breastfeeding")),
    dairyintake_5y = factor(dairyintake_5y, levels = c(0,1,2,3,4,5,8,9),
                            labels = c("Never or less than 1 time a week","Never or less than 1 time a week",
                                       "1-2 times a week","3 to 5 times a week",
                                       "everyday/almost everyday","everyday/almost everyday",
                                       "everyday/almost everyday","everyday/almost everyday")),
    medu_5y = factor(medu_5y, levels = c(1,2,3),
                     labels = c("Junior High&below","Senior High/Vocational","University&above")),
    Socioeco_5y = factor(Socioeco_5y, levels = c(1,2,3,4,5,6,7,8,9,88,98,99),
                         labels = c("30,000","30,000","30,000",">=30,000,<50,000",">=50,000,<70,000",
                                    ">=70,000,<100,000",">=100,000,<150,000",">=150,000,<200,000",">=200,000",
                                    ">=50,000,<70,000",">=50,000,<70,000",">=50,000,<70,000")),
    gastroenteritis = factor(gastroenteritis, levels = c(0,1), labels = c("No","Yes")),
    AD              = factor(AD, levels = c(0,1), labels = c("No","Yes")))

label(Finaldataset$probioticintake) <- "Probiotic intake"
label(Finaldataset$B_SEX)           <- "Sex"
label(Finaldataset$BMI_5y)          <- "BMI at age 5 (kg/m²)"
label(Finaldataset$dairyintake_5y)  <- "Dairy intake at age 5"
label(Finaldataset$medu_5y)         <- "Maternal education level"
label(Finaldataset$Socioeco_5y)     <- "Average month family income(NT$)"
label(Finaldataset$breastfeeding)   <- "Breastfeeding"

#Exclusion flow----
n_linked <- nrow(Finaldataset)

##共用前兩步:排除 無5歲追蹤 / 益生菌無資料----
base <- Finaldataset %>% filter(!is.na(start_date) & probiotic5y_available == 1)
n_base <- nrow(base)

##LOOSE 版:排除 5歲前性早熟(loose)----
analytic_loose <- base %>%
  filter(!(EarlyPuberty_loose == 1 & EarlyPuberty_date < start_date))
n_loose <- nrow(analytic_loose)

##STRICT 版:排除 5歲前性早熟(strict)----
analytic_strict <- base %>%
  filter(!(EarlyPuberty_strict == 1 & EarlyPuberty_date < start_date))
n_strict <- nrow(analytic_strict)

##flowchart(loose)----
exclusion_flow_loose <- tibble::tibble(
  step        = c("成功連結健保","排除:無5歲追蹤/益生菌無資料","排除:5歲前性早熟 loose(最終)"),
  n_remaining = c(n_linked, n_base, n_loose),
  n_excluded  = c(NA, n_linked - n_base, n_base - n_loose))

##flowchart(strict)----
exclusion_flow_strict <- tibble::tibble(
  step        = c("成功連結健保","排除:無5歲追蹤/益生菌無資料","排除:5歲前性早熟 strict(最終)"),
  n_remaining = c(n_linked, n_base, n_strict),
  n_excluded  = c(NA, n_linked - n_base, n_base - n_strict))

print(exclusion_flow_loose)
print(exclusion_flow_strict)
write.csv(exclusion_flow_loose,  "exclusion_flow_loose.csv",  row.names = FALSE)
write.csv(exclusion_flow_strict, "exclusion_flow_strict.csv", row.names = FALSE)

#Missing pattern (loose)----
missing_loose <- analytic_loose %>%
  missing_pattern(dependent   = "EarlyPuberty_loose",
                  explanatory = c("probioticintake",
                                  "B_SEX","BMI_5y","height_5y","weight_5y",
                                  "dairyintake_5y","breastfeeding",
                                  "medu_5y","Socioeco_5y"))
missing_loose <- as.data.frame(missing_loose); missing_loose$n <- rownames(missing_loose)
missing_loose <- missing_loose %>% relocate(n)
write.csv(missing_loose, "missing_pattern_loose.csv", row.names = FALSE)

#Missing pattern (strict)----
missing_strict <- analytic_strict %>%
  missing_pattern(dependent   = "EarlyPuberty_strict",
                  explanatory = c("probioticintake",
                                  "B_SEX","BMI_5y","height_5y","weight_5y",
                                  "dairyintake_5y","breastfeeding",
                                  "medu_5y","Socioeco_5y"))
missing_strict <- as.data.frame(missing_strict); missing_strict$n <- rownames(missing_strict)
missing_strict <- missing_strict %>% relocate(n)
write.csv(missing_strict, "missing_pattern_strict.csv", row.names = FALSE)

#BMI missing----
missing_summary <- Finaldataset %>%
  summarise(
    Total_N          = n(),
    Height_missing   = sum(is.na(height_5y)),
    Weight_missing   = sum(is.na(weight_5y)),
    Both_missing     = sum(is.na(height_5y) & is.na(weight_5y)),
    Only_height_miss = sum(is.na(height_5y) & !is.na(weight_5y)),
    Only_weight_miss = sum(!is.na(height_5y) & is.na(weight_5y)),
    BMI_missing      = sum(is.na(BMI_5y)))

missing_table <- data.frame(
  Variable = names(missing_summary),
  N        = as.numeric(missing_summary[1, ]),
  Percent  = round(as.numeric(missing_summary[1, ]) / missing_summary$Total_N * 100, 1))

print(missing_table)

write.csv(missing_table, "missing_height_weight_summary.csv", row.names = FALSE)