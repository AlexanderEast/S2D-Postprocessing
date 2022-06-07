

# This script requires an EXCEL of a pophouse file and a
# .csv chem all houses S2D File. Please enter here or in "Run" sourcing file.
# chemall <- _____.csv
# popgen  <- _____.xlsx

# loads popgen from excel
popgen  <- import(popgen)

# removes extraneous "num" comlumn and renames 'row' to 'house.num'
popgen<- popgen[,!colnames(popgen)%in% c("num")]
colnames(popgen)[colnames(popgen)=="row"]<-"house.num"

chemall <- fread(chemall)
chemall$units <- "Total Annual Use (mg)"
# combines chemall and popgen
both <- merge(chemall,popgen, by= "house.num", all = TRUE)
rm(chemall,popgen)




