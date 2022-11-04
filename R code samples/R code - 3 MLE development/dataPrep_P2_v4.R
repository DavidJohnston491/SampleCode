################
#This script is to load and prepare ML data sets for paper #2
# Data set created by 'v4' simulations (Hybrid v2)
#################

#clear the workspace
gc(verbose = F)
rm(list = ls())

#load the relevant libraries
suppressPackageStartupMessages({
  #library(readr)
  #library(xlsx)
  #library(openxlsx)  #read, write and edit XLSX files !!has problems writing to excel files
  
  library(ggpmisc)
  library(tidyxl)
  library(dplyr)
  
})
#Load private functions for normalisation
source("C:/Users/u1097253/Documents/PhD work (C)/CodeLibraries/R/DBJ_Lib.R")
#source("DBJ_Lib.R")


# define the datapaths / files / settings
uniDataSets <- "/uni/DataGen/DataSets/Paper2/APSIMX/"
dataSet <- ""
inputRaw <- "chickpea_5dTOS_OSFfactorial_EMv4_MLReport.csv"

#metStatsFile <- "Emerald_MnthSummary.csv"

strLocation <- "Emerald"
#strOutputFile <- "InputDataforML.xlsx"

strOutputCSV <- "InputDataSet_v4_Emerald.csv"  #Hybrid v2 

strWD <- paste0(uniDataSets, dataSet)
setwd(strWD)

#read in the raw results
rawSourceFile <- paste0(uniDataSets,inputRaw)

#rawData <- read.xlsx2(rawSourceFile, sheetName  = 'MLReport2', colClasses = c(rep('character',6), rep('numeric',36)))

#rawData <- read.csv(rawSourceFile,  colClasses = c(rep('character',6),rep('numeric',1),rep('character',4), rep('numeric',16),rep('character',1),rep('numeric',3),rep('character',1), rep('numeric',10)))
##v2#rawData <- read.csv(rawSourceFile,  colClasses = c(rep(NA,6),rep('numeric',1),rep('character',4), rep('numeric',16),rep('character',1),rep('numeric',3),rep('character',1), rep('numeric',10)))
##v3#rawData <- read.csv(rawSourceFile,  colClasses = c(rep('numeric',1),rep('character',2), rep('numeric',13),rep('character',1),rep('numeric',3),rep('character',1), rep('numeric',12)))
##v4#
    rawData <- read.csv(rawSourceFile,  colClasses = c(rep(NA,6),
                                                       rep('character',2),
                                                       rep('numeric',1),
                                                       rep(NA,2),
                                                       rep('numeric',13), 
                                                       rep(NA,1),
                                                       rep('numeric',3),
                                                       rep(NA,1),
                                                       rep('numeric',12)))


#Delete the unneeded data/descriptive columns (CheckpointName,CheckpointID, SimulationName,Experiment, FolderName, Zone, Clock.Today, SowProfileWater, SowingDate Cultivar, )
# giving 31 columns in RawData
##v2#rawData <- rawData[,-c(1:6, 10,11,27,28,32)]
##v3# delete columns(SowingDate, Cultivar) # rawData <- rawData[,-c(17,21)]
##v4# 
    rawData <- rawData[,-c(1:6, 10,11,25,27,28,29)]
    
    
# replace Cv string name with Cv Code
cvList <- unique(rawData$Cv)
levels(rawData$Cv) <- c(levels(rawData$Cv), "1", "2","3","4","5","6")
rawData$Cv[rawData$Cv == 'Seamer'] <- '1'
rawData$Cv[rawData$Cv == 'HatTrick'] <- '2'
rawData$Cv[rawData$Cv == 'CICA1521'] <- '3'
rawData$Cv[rawData$Cv == 'Monarch'] <- '4'
rawData$Cv[rawData$Cv == 'Almaz'] <- '5'
rawData$Cv[rawData$Cv == 'Kalkee'] <- '6'

# replace SW string name with Code for Low, Medium, High
levels(rawData$SW) <- c(levels(rawData$SW), "1", "2", "3")
rawData$SW[rawData$SW == 'L'] <- '1'
rawData$SW[rawData$SW == 'M'] <- '2'
rawData$SW[rawData$SW == 'H'] <- '3'

rawData$Cv <- as.numeric(rawData$Cv)
rawData$SW <- as.numeric(rawData$SW)

inputData <- rawData




# write out the combined data set to a csv file (for storing)
outputCSVFile = paste0(uniDataSets,strOutputCSV)
write.csv(inputData, outputCSVFile)


# create normalised data frame

df <- data.frame(inputData)  # backup copy

#length(unique(testnormargs[,"minVal"]))
colMax <- function(inputData) sapply(inputData, max, na.rm = TRUE)
colMin <- function(inputData) {
          sapply(inputData, min, na.rm = TRUE)
          } 

normargs <- data.frame("name" = colnames(inputData), 
                      # "minVal" = colMin(inputData), 
                       "minVal" = 0, 
                       "maxVal" = as.numeric(colMax(inputData)),
                       "minScale" = 0,
                       "maxScale" = 1)

#force Latitude to set values
# latDetails <- data.frame("name" = c("Lat"), "minVal" = -90.0, "maxVal" = 90.0, "minScale" = -1, "maxScale" = 1)
# normargs['Lat',] <- latDetails

normargs2 <- rows_update(normargs, tibble(name = "Lat", "minVal" = -90.0, "maxVal" = 90.0, "minScale" = -1, "maxScale" = 1))
normargs <- normargs2
rm(normargs2)


# write out the arguments for normalisation as data frame to a csv file (for storing) - NOTE These are by row not by column
outputCSVFile = paste0(uniDataSets,"NormalisationArguments_v4_Em.csv")
write.csv(normargs, outputCSVFile, row.names=FALSE)

#colDetailsPresent <- all(colnames(inputData) %in% rownames(normargs))  #This might be useful for testing if preloaded data matched stored normalisation details
colDetailsPresent <- all(colnames(inputData) %in% normargs$name)  #This might be useful for testing if preloaded data matched stored normalisation details
colDetailsPresent  #TRUE if all columns in inputData are available in normargs data set


#Now create the Normalised Sensitivity value columns 

normalisedInputData <- std_normalisation(inputData, normargs)  #std_normalisation() from "DBJ_Lib.R"
#randomise the normalised data set
set.seed(7)
randNormInputData <- normalisedInputData[sample(nrow(normalisedInputData)),]
write.csv(normalisedInputData, "normalisedInputData_v4_Em.csv", row.names=FALSE)
write.csv(randNormInputData, "randNormInputData_v4_Em.csv", row.names=FALSE)
  
  
  
