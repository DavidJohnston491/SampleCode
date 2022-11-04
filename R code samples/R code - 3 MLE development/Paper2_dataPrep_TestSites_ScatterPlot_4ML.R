# Prepare data for Ploting xy scatter plots of MARS, ELM, NNet and Random Forest Predicted vs Observed
################################################
#plotting 6 outputs X 4 MLs: xy scatter plots grouped by 2 TEST locations (Development Testing) 

rm(list = ls())
library(ggplot2)
library(ggpmisc)
library(gridExtra)


# define the datapaths / files / settings
dataDir <- "C:/Users/u1097253/Documents/PhD work (C)/Paper 2 - Emulators for BioPhysical Crop Models/Data and Code/R/Reports/"
#setwd("C:/Users/u1097253/Documents/PhD work (C)/Paper 2 - Emulators for BioPhysical Crop Models/Data and Code/R/Reports/")
strWD <- paste0(dataDir, " ")
setwd(strWD)

#Set up variable values
inputDataPath <- "C:/Users/u1097253/Documents/PhD work (C)/Paper 2 - Emulators for BioPhysical Crop Models/Data and Code/R/Reports/" 

RF_inputDataFile <- "Act_Pred_RF_TestSites_v2.csv"
ELM_inputDataFile <- "Act_Pred_ELM_TestSites_v2.csv"
nnet_inputDataFile <- "Act_Pred_nnet_TestSites_v2.csv"
MARS_inputDataFile <- "Act_Pred_MARS_TestSites_v2.csv"

outputDataPath <- "C:/Users/u1097253/Documents/PhD work (C)/Paper 2 - Emulators for BioPhysical Crop Models/Data and Code/R/Reports/"
outputCSVFile <- "Act_Pred_All_TestSites.csv"
#===========================================

#Load RF .csv data file
sourceFile <- paste0(inputDataPath,RF_inputDataFile)
RF_inputData <- read.csv(sourceFile)

#Load ELM .csv data file
sourceFile <- paste0(inputDataPath,ELM_inputDataFile)
ELM_inputData <- read.csv(sourceFile)

#Load NNet .csv data file
sourceFile <- paste0(inputDataPath,nnet_inputDataFile)
NNet_inputData <- read.csv(sourceFile)

#Load MARS .csv data file
sourceFile <- paste0(inputDataPath,MARS_inputDataFile)
MARS_inputData <- read.csv(sourceFile)

#------------------------------------------------------

# Setup variables list for looping
#
modelCodes <- c("RF", "ELM", "NNet","MARS")
## Loop Start - reference model dfs 
for (loopModel in modelCodes) {
  
    loopModel_df <- paste0(loopModel, "_inputData")
    thisModel_df <- get(loopModel_df)
    
    targetNames <- c("EmergenceDAS", "FloweringDAS", "PoddingDAS", "MaturityDAS","Biomass","GrainWt")
    target_loopCnt <- 0
    
    ## Loop Start - split model dfs into target specific dfs
    for (loopTarget in targetNames) {
    
      target_loopCnt <- target_loopCnt + 1
        loopTarget <- targetNames[[target_loopCnt]]
        out_dfName <- paste0(loopTarget,"_df")
        thisModel_target_df <- thisModel_df[, c(1,2,target_loopCnt*2 + 1,target_loopCnt*2 + 2)]
      
        # Add column "Output" = loopTarget  
        thisModel_target_df$Output <- loopTarget
        
        # Add column "Model" = loopModel  
        thisModel_target_df$Model <- loopModel
        
        # Ensure all names are consistent
        names(thisModel_target_df) <- c("Lat","PAWCmm","Actual","Predicted","Output","Model")
        
        # Save the dataframe as the "target" df
        assign(out_dfName, thisModel_target_df)

    }  # end of loop for output targets for one model
    
    # combine all output_df for this model
    thisModel_outdf <- rbind(EmergenceDAS_df, FloweringDAS_df, PoddingDAS_df, MaturityDAS_df,Biomass_df,GrainWt_df)
    
    # Save the dataframe as the "Model" df
    out_modelDFName <- paste0(loopModel,"_df")
    assign(out_modelDFName, thisModel_outdf)

}  # end of loop for a model


# Combine dataframes from each model
Actual_Pred_df <- rbind(RF_df, ELM_df, NNet_df, MARS_df)

# Add Location column by matching $Lat values

Actual_Pred_df$Location[Actual_Pred_df$Lat == -23.569] <- "1.Emerald"
Actual_Pred_df$Location[Actual_Pred_df$Lat == -27.600] <- "2.Bongeen"
Actual_Pred_df$Location[Actual_Pred_df$Lat == -28.979] <- "3.Mungindi"
Actual_Pred_df$Location[Actual_Pred_df$Lat == -29.191] <- "4.Mingenew"
Actual_Pred_df$Location[Actual_Pred_df$Lat == -30.954] <- "5.Gunnedah"
Actual_Pred_df$Location[ grep("-33.83",Actual_Pred_df$Lat)] <- "6.Clare"  # needed as stored as "-33.8359999999"
Actual_Pred_df$Location[Actual_Pred_df$Lat == -36.670] <- "7.Horsham"
Actual_Pred_df$Location[Actual_Pred_df$Lat == -28.521] <- "8.Goondiwindi"
Actual_Pred_df$Location[Actual_Pred_df$Lat == -34.236] <- "9.Mildura"


#Convert selected columns to Factors ready for plotting with group-by  
Actual_Pred_df$Location. <- as.factor(Actual_Pred_df$Location)    
Actual_Pred_df$Model. <- as.factor(Actual_Pred_df$Model)

# write out the csv file for RE-graphing if needed
outputCSVFile = paste0(outputDataPath,outputCSVFile)
write.csv(Actual_Pred_df, outputCSVFile, row.names=FALSE)

  
## End of Script

##  tempdf <- data.frame(inputData)
##  tempdf$Actual[tempdf$Output=="EmergenceDAS" & tempdf$Location=="1.Emerald" & tempdf$Actual > 25.0] <- 10
##  tempdf$Predicted[17018] <- 10.58086
##  tempdf2 <- data.frame(inputData)
## inputData <- data.frame(tempdf)

  #####==============================================================================================  

