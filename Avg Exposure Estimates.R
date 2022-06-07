# Daily Average Exposure Estimates



# 1. Estimate /BW daily intake.

# Pull aggregate exposures
totbw <- dailyexposures[dailyexposures$metric == "total exposure (mg/day)",]
# Pull house numbers and bodyweights 
totbw<- merge(totbw,both[,c("house.num","weight")], by = "house.num")
# 

totbw[str_detect(colnames(totbw),"DTXSID")]<- sapply(totbw[str_detect(colnames(totbw),"DTXSID")],as.character)
totbw[str_detect(colnames(totbw),"DTXSID")]<- sapply(totbw[str_detect(colnames(totbw),"DTXSID")],as.numeric)
totbw[str_detect(colnames(totbw),"DTXSID")]<- totbw[str_detect(colnames(totbw),"DTXSID")]/totbw$weight
totbw$metric<- "exposure (mg/kg-bw/day)"

# 2. Estimate Average (for just exposed households and all households)
justexposed <- data.frame(colMeans(totbw[str_detect(colnames(totbw),"DTXSID")],na.rm = TRUE))

# set NAs to 0
totbw[is.na(totbw)] <- 0
allhouseholds <- data.frame(colMeans(totbw[str_detect(colnames(totbw),"DTXSID")],na.rm = FALSE))


# Combine into dataframe
avgdaily<- cbind(rownames(justexposed),justexposed,allhouseholds)

colnames(avgdaily)<- c("DTXSID","Average exposure (mg/kg-bw/day) across all households with product use",
                       "Average exposure (mg/kg-bw/day across all households in run (including no use)")
rm(justexposed,allhouseholds)


# 3. Import chemical properties file for names
chemprops<- import("./internal/chemical_properties_zero.csv")
chemprops<-chemprops[,c("dtxsid","cas","name")]
colnames(chemprops)<- toupper(colnames(chemprops))

# Subset by only chemicals in run
alldtxsids <- colnames(both)[str_detect(colnames(both),"DTXSID")]
chemprops<- chemprops[chemprops$DTXSID %in% alldtxsids,]

# 4. append chemical names
avgdaily <- merge(chemprops,avgdaily,by ="DTXSID", all.x = TRUE)
rm(chemprops,alldtxsids)

# O
avgdaily <- avgdaily[order(-avgdaily$`Average exposure (mg/kg-bw/day) across all households with product use`
                          ,-avgdaily$`Average exposure (mg/kg-bw/day across all households in run (including no use)`),]
