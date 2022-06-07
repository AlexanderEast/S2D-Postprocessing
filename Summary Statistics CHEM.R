rm(list=ls())

requiredpackages<-  c("rio","stringr","plyr","dplyr","data.table")
invisible(lapply(requiredpackages,require, character.only = TRUE))
rm(requiredpackages)

data <- import_list("Case Example RPGen 07272021 AE.xlsx")

data<- merge(data$`Daily Exposure by BW`,data$`Chemical Use and Popgen`[,c("house.num","weight","pool","gender","kownrent")], by = "house.num")

pool.reader<- function(x){
  
  # pool = 36*(location-1)+12*(housetyp-1)+3*(famcat-1)+inccat
  
  x<-as.numeric(x)  
  
  location.des <- function(x){
    
    setting0 <- "Rural"
    setting1 <- "Urban"
    
    reg1 <- " North East"
    reg2 <- " Midwest"
    reg3 <- " South"
    reg4 <- " West"
    
    if (x %% 2 == 0){
      des<- setting1
    } else {
      des<- setting0
    }
    
    if (x %in% 7:8){
      des <- str_c(des,reg4)
    } else if (x %in% 5:6){
      des <- str_c(des,reg3)
    } else if (x %in% 3:4){
      des <- str_c(des,reg2)
    } else if (x %in% 1:2)
      des<- str_c(des,reg1)
    
    return(des)
  }
  loc.div<- sort(rep(c(1:8),36))
  loc.pos <- sapply(loc.div,location.des)
  
  housetyp.des <- function(x){
    house1<- "Stand Alone"
    house2<- "Multi Unit Structure"
    house3<- "Other Housetype"
    
    if (x == 1){
      des <- house1
    } else if (x == 2){
      des <- house2
    } else if (x == 3){
      des<-house3
    }
    return(des)
  }
  housetyp.div <- rep(sort(rep(c(1:3),12)),8)
  housetyp.pos<- sapply(housetyp.div,housetyp.des)
  
  famcat.des <-function(x){
    
    famcat1<- "One Adult"
    famcat2<- "One Adult with Kids"
    famcat3<- "Adults"
    famcat4<- "Adults with Kids"
    
    if (x == 1){
      des <- famcat1
    } else if (x == 2){
      des <- famcat2
    } else if (x == 3){
      des<- famcat3
    } else if (x == 4){
      des<- famcat4
    }
    return(des)
  }
  famcat.div <- rep(sort(rep(c(1:4),3)),24)
  famcat.pos<- sapply(famcat.div,famcat.des)
  
  inccat.des <- function(x){
    inc1<- "Low Income"
    inc2<- "Middle Income"
    inc3<- "High Income"
    
    if (x == 1){
      des <- inc1
    } else if (x == 2){
      des <- inc2
    } else if (x == 3){
      des<-inc3
    }
    return(des)
  }
  inccat.div <-rep(c(1:3),96)
  inccat.pos <-sapply(inccat.div,inccat.des)
  
  register<- str_c(loc.pos," ",housetyp.pos," ",famcat.pos," ",inccat.pos)
  output<- register[x]
  
  return(output)
}

data$pool <- pool.reader(data$pool)
rm(pool.reader)

data <- data[data$kownrent %in% 1:2,]

sumstats <- function(x){

df <- t(c(mean(x),mean(x > 0),
       quantile(x,c(0,.10,.5,.75,.95,1))))

df<- cbind(df, Units = "mg/kg bw/day", Exposed = sum(x > 0), Houses = sum(x >= 0))
df<- data.frame(df)
colnames(df)[1:8]<-c("Mean","Mean (Just Exposed)","Min","10th%","Median","75th%","95th%","Max")
return(df)
}


ds<- rbind.fill(lapply(data[3:7],sumstats))
ds$Subset <- "All"
ds$Chemical <- colnames(data[3:7])


#### Now do for owners and renters

owners  <- data[data$kownrent %in% 1,]
own<- rbind.fill(lapply(owners[3:7],sumstats))
own$Subset <- "Owners"
own$Chemical <- colnames(data[3:7])



renters <- data[data$kownrent %in% 2,]
rent<- rbind.fill(lapply(renters[3:7],sumstats))
rent$Subset <- "Renters"
rent$Chemical <- colnames(renters[3:7])

results <- rbind(ds,rent,own)
rm(list=setdiff(ls(), "results"))

results[names(results)[1:8]] <- lapply(results[names(results)[1:8]], as.character)
results[names(results)[1:8]] <- lapply(results[names(results)[1:8]], as.numeric)
results[1:8] <- signif(results[1:8],3)
results<- results[,c(13:10,1:8,9)]



export(results,"CHEM Exposure Summary Stats AE 07272021.xlsx")
