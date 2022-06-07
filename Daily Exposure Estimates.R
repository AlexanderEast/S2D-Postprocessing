# This script requires the folder name of the daily S2D diaries.
# Please enter here or in "Run" sourcing file.
# dailyfolder <- ___________

# Reads in every daily diary into a list of named dataframes.
files <- list.files(dailyfolder)
files <- paste(dailyfolder,files, sep="")
files <- lapply(files,fread) 
names(files)<- list.files(dailyfolder)
names(files)<- gsub("[_.csv]", "", names(files))
names(files)<- gsub("([y])", "\\1 \\2", names(files))


# Removes days with 0 exposure and households without exposure
removenonevents<- function(x){
  nums <- rowSums(x[,4:32])
  x <- x[(nums != 0),]
  rm(nums)
  return(x)
}
files<- lapply(files,removenonevents)
rm(dailyfolder,removenonevents)
files <- files[as.vector(sapply(files,nrow) != 0)]


yearlyexp <- function(x){

# total exposure (mg into body, see user guide)
x$`total exposure` <- (x$dir.derm.abs + x$dir.inhal.abs + x$dir.ingest.abs + 
                              x$ind.derm.abs + x$ind.inhal.abs + x$ind.ingest.abs)
y<-split(x, x$dtxsid)

# daily estimates (all events across year over 364 days, includes down the drain and waste)
sumdaily<- function(z){
return(colSums(z[,4:33])/364) # 4:33 are colnames with numbers
}

metric <- str_c(rownames(sapply(y,sumdaily))," (mg/day)")

return(data.frame(cbind(metric,sapply(y,sumdaily))))
}

# Cleaning dataframe before exporting
dailyexposures<- lapply(files,yearlyexp)
dailyexposures<- bind_rows(dailyexposures,.id = "house.num")

rownames(dailyexposures) <- NULL
dailyexposures$house.num <- as.numeric(gsub("Daily ", "", dailyexposures$house.num))
dailyexposures<- dailyexposures[order(dailyexposures$house.num),]
rm(files,yearlyexp)



