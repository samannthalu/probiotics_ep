#select column from different dataset----
##exposure selection----
exposure<-exposureb45y_clean%>%
  select(Sampleid,probioticintake)
##covariate selection----
