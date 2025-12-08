source("organize_variables.R")

str(TBCSstimu)

TBCSstimu$pribioticintake_6m<-as.factor(TBCSstimu$pribioticintake_6m)

table(TBCSstimu$medu_8y)
