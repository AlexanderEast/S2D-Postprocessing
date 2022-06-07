# S2D Postprocessing
# AE ORAU 5/18/2021
# Designed for S2D and RPGen Output from 
# EPA's Combined Human Exposure Model (CHEM).

rm(list = ls())

# Specify the three inputs
chemall <- "./homeowners/Chem/All_houses.csv"
popgen  <- "./homeowners/pophouse.csv"
dailyfolder   <- "./homeowners/Daily/"

# specify output file name (with .xlsx)
filename<- "Case Example Homeowners RPGen Final AE.xlsx"

# Load required packages
requiredpackages<-  c("rio","stringr","plyr","dplyr","data.table")
invisible(lapply(requiredpackages,require, character.only = TRUE))
rm(requiredpackages)

# source two function files (one for each sheet)
source("Merge Pophouse and All Houses.R")
source("Daily Exposure Estimates.R")
source("Avg Exposure Estimates.R")
# export result as .xlsx
export(list("Chemical Use and Popgen" = both, "Daily Exposure" = dailyexposures, 
            "Daily Exposure by BW" = totbw, "Average Exposure Estimates" = avgdaily),filename)



