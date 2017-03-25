library(data.table)
dtRTO <- fread("./RTO_Occupancy_2009_t_2015.csv")
dtRTO[,Year:=sapply(strsplit(V1,"M"),"[[",1)]
dtRTO[,Month:=sapply(strsplit(V1,"M"),"[[",2)]
head(dtRTO)
dtRTO <- dtRTO[,lapply(.SD, mean, na.rm=TRUE),by="Year",.SDcols=-c("V1","Month","Year")]
melt(dtRTO,id.vars = "Year")
