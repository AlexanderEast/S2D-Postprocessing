

# Load required packages
requiredpackages<-  c("rio","stringr","plyr","dplyr","data.table")
invisible(lapply(requiredpackages,require, character.only = TRUE))
rm(requiredpackages)

rm(list=ls())

# Import daily files
dailyfolder   <- "./homepucsonly/Daily/"
files <- list.files(dailyfolder)
files <- paste(dailyfolder,files, sep="")
files <- lapply(files,fread) 
names(files)<- list.files(dailyfolder)
names(files)<- gsub("[_.csv]", "", names(files))
names(files)<- gsub("([y])", "\\1 \\2", names(files))
rm(dailyfolder)

# Get yearly avg sum(indirect/direct, inhal, ingest, dermal)
# intake per chemical
getdailyavg <- function(x){
y<-split(x,x$dtxsid)


dailysum <- function(z){
Daily  <- data.frame(sum(z$dir.derm.abs + z$dir.inhal.abs + z$dir.ingest.abs + 
              z$ind.derm.abs + z$ind.inhal.abs + z$ind.ingest.abs)/364)

colnames(Daily)[1]<- ""

return(Daily)
}

y <- data.frame(sapply(y,dailysum))
y <-cbind(data.frame(unique(x$household)),y)
colnames(y)[1]<- "Household"

return(y)
}

exposure <- rbind.fill(lapply(files,getdailyavg))
exposure<- exposure[order(exposure$Household),]
rm(getdailyavg)

# apply bw for mg/kg/bw/day. 
popgen  <- "./homeowners/pophouse.csv"
popgen  <- import(popgen)

colnames(popgen)[colnames(popgen)=="row"]<-"Household"
colnames(popgen)[colnames(popgen)=="weight"]<-"Weight"
colnames(popgen)[colnames(popgen)=="kownrent"]<-"Owner"


exposure <- merge(exposure,popgen[,c("Household","Weight","Owner")], by = "Household")
exposure[str_detect(colnames(exposure),"DTXSID")] <- exposure[str_detect(colnames(exposure),"DTXSID")]/exposure$Weight
rm(popgen,getdailyavg)
rm(files)


# Add units column 
exposure$Weight <- "mg/kg-bw/day"
colnames(exposure)[colnames(exposure)=="Weight"]<-"Units"

exposure$Owner[exposure$Owner != 1] <- 0


export(exposure,"CHEM Homeowners Only.csv")

