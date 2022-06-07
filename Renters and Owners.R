# Renters and Owners Postprocessing 
rm(list=ls())
source('Run.R')
library(graphics)
library(ggplot2)
library(reshape2)

chems <- colnames(both)[str_detect(colnames(both),"DTXSID")]
both  <- data.frame(both)
pop   <- both[!colnames(both) %in% chems]
pop   <- merge(pop,totbw, by = "house.num")


owners  <- pop[pop$kownrent %in% 1,]
renters <- pop[pop$kownrent %in% 2,]
legend  <- avgdaily[c("DTXSID","NAME")]


rm(list=setdiff(ls(), c("owners","renters","legend")))


# fxn that generates summary, frequency, and boxplots.

x <- owners
y <- "Home Owners"


exposuresummary <- function(x,y,legend){

x<- x[str_detect(colnames(x),"DTXSID")]
measure <- nrow(x)

summary<- sapply(x,summary)

freq   <- data.frame(colSums(x != 0))
freq   <- data.frame(DTXSID = row.names(freq),freq)
colnames(freq)[2]<- "Count"

maxima    <- round_any(max(summary),.1, f = ceiling)
int       <- c(0,maxima/10000000,maxima/1000000,maxima/100000,maxima/10000,
               maxima/1000,maxima/100,maxima/10,maxima)

x <- melt(x)
colnames(x)[1]<- "DTXSID"
x<- merge(x,legend, by= "DTXSID")

myplot<- ggplot(data = x, aes(x=DTXSID, y=value)) + geom_boxplot(aes(fill= NAME))+
  scale_y_continuous(trans = "log",limits = c(maxima/10000000, maxima),breaks = int,
                     labels= scales::scientific)+ 
  labs(title = str_c("Daily Exposure for ",y," in CHEM (1 Year)"),
       x = "Chemical ID",
       y = "Aggregate Intake (mg/kg-bw/day)",
       fill = "Chemical Name",
       caption = str_c("n= ",measure,". Not all households exposed."))

# Format for return
summary <- data.frame(summary)
summary <- data.frame(Metrics = row.names(summary),summary)

myplot

done <- list(Boxplots = myplot, Freqency = freq, Summary =summary, Raw = x)

return(done)
}

owners <- exposuresummary(owners,"Home Owners",legend)
renters<- exposuresummary(renters,"Renters",legend)

export(owners[2:4],"Owners.xlsx")
export(renters[2:4],"Renters.xlsx")

owners[1]
renters[1]












#### WORKS FOR OWNERS ########

owners<- owners[str_detect(colnames(owners),"DTXSID")]


summary<- sapply(owners,summary)

freq   <- data.frame(colSums(owners != 0))
freq$DTSXID<- rownames(freq)
colnames(freq)[1]<- "Count"

maxima    <- round_any(max(summary),.1, f = ceiling)
int       <- c(0,maxima/10000000,maxima/1000000,maxima/100000,maxima/10000,
                 maxima/1000,maxima/100,maxima/10,maxima)

owners <- melt(owners)
colnames(owners)[1]<- "DTXSID"
owners<- merge(owners,legend, by= "DTXSID")

ggplot(data = owners, aes(x=DTXSID, y=value)) + geom_boxplot(aes(fill= NAME))+
  scale_y_continuous(trans = "log",limits = c(maxima/10000000, maxima),breaks = int,
                     labels= scales::scientific)+ 
  labs(title = "Total Exposure for Home Owners",
       x = "Chemical ID",
       y = "Aggregate Intake (ng/kg-bw/day)",
       fill = "Chemical Name",
       caption = " n=49. Not all households exposed.")




