# test code that loops through Random Forest models for testing
# -------------------------------------------------------------

rm(list = ls())

#strWD <- paste0(uniDataSets, dataSet)
#setwd("C:/Users/u1097253/Documents/PhD work (C)/Paper 2 - Emulators for BioPhysical Crop Models/Data and Code/R")
setwd("U:/Paper #2/Data and Code/R")

#load the relevant libraries
suppressPackageStartupMessages({

library(tidyverse)
library(randomForest)
source("evaluationStats.R")   #Stats to evaluate the model performance
    
})


#Set up variable values
#inputDataPath <- "/uni/DataGen/DataSets/Paper2/R_DataSets/" 
inputDataPath <- "U:/DataSets/Paper2/R_DataSets/" 
inputDataFile <- "randNormInputData_MultiLoc_v2.csv"

normalisationValuesFile <- "NormalisationArguments_MultiLoc_v1.csv"
datasetDescr <- " Six Locations ML v2"

numberOfTargets <- 6
currentTarget <- 1

reportFile <- "./Reports_K-Fold/RF_MultiLoc_v2_K-fold.rpt"
act_pred_outputDataFile <- "./Reports_K-Fold/Act_Pred_RF_MultiLoc_v2_K-fold.csv"

#modelPath <- "./Models/"
#modelBase <- "rf_model_"


#***************************************
#Load normalised .csv data file
sourceFile <- paste0(inputDataPath,inputDataFile)
inputData <- read.csv(sourceFile)

#Load normalisation arguments .csv data file ready for denormalisation of target values
sourceFile <- paste0(inputDataPath,normalisationValuesFile)
normargs <- read.csv(sourceFile)


#names(inputData) = NULL
totalCols <- ncol(inputData)
totalFactorCnt <- totalCols - numberOfTargets
outputCnt <- totalCols - totalFactorCnt   #output count should equal number of Targets!

#prepare report data frame
modelSummary_df <- data.frame("ModelNo" = 0,
                              "Target" = "",
                              "MB" = 0,
                              "MAE" = 0,
                              "RMSE" = 0,
                              "R2" = 0,
                              "IOA" = 0,
                              "COE" = 0)


#prepare report file
rptString <- paste("Report on RANDOM FOREST model development for ", datasetDescr, "\n \n")
write(rptString, reportFile)



KFoldMax <- 10
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
## Start KFold loop
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# for (KFold in 1:KFoldMax)
# {
#     
# }

# Randomise the inputData 
#------------------------

inputFactors <- inputData[, -c(totalFactorCnt+1:totalCols)]     #drop off the output target columns (6 outputs)
targetValues <- inputData[, -c(1:totalFactorCnt)]     #save the target values into their own dataframe

input_df <- data.frame(inputFactors)
# input_df <- transform(inputFactors, 
#                       TOS=as.factor(TOS),
#                       Cv=as.factor(Cv),
#                       #SW=as.factor(SW),
#                       Lat=as.numeric(Lat))
                      
                                 

X <- data.frame(input_df)
trainCnt <- nrow(X) * 0.8    #80/20 Train/Test split
testStart <- trainCnt + 1
xtrain <- as.matrix(X[1:trainCnt, ])
xtest <- as.matrix(X[testStart:nrow(X), ])


# prepare normalised target columns with names ready for de-normalisation
Yall_normalised <- targetValues
targetNames <- colnames(Yall_normalised)



##' Loop Start
##' ==========
##' 
##' Loop for each target (output) 
##' and within this loop for each of the rf model's parameter settings
##' -------------------------------------------------------------------
##' 
act_pred_df <- data.frame()



#modelConfiguration <- 0
#for (loopTarget in targetNames) {
    
   loopTarget = 'FloweringDAS'  #for testing without loop targets

    # convert the 'current' column of train-data-response to a one-column matrix
    #---------------------------------------------------------------------------
    #currentTargetName <- targetNames[loopTarget]
    ytrain <- matrix(Yall_normalised[1:trainCnt, loopTarget], nrow = trainCnt, ncol = 1)
    
    # Set up Loop values for each model parameter
    # currently no model parameters being varied
    # #for (loopBias in c(TRUE, FALSE)) {

                    # setup output to a variable for reporting (variable is 'stdoutText')
                    stdoutText <- vector('character')
                    con    <- textConnection('stdoutText', 'wr', local = TRUE)
                    sink(con)
                    
                    #modelConfiguration <- modelConfiguration + 1
                    
                    # perform a fit and predict [ randomForest ]
                    #----------------------------------------
                    start.time <- Sys.time()
                    
                    rf.model <- randomForest(x = xtrain, y = ytrain, ntree = 500, keep.forest=TRUE, importance=TRUE )
                    
                    end.time <- Sys.time()
                    time.taken_dev <- as.numeric(end.time - start.time, units = "secs")
                    time.taken_dev <- round(time.taken_dev, digits = 3)
                    rf_predicted <- NULL
                    rf_predicted <- predict(rf.model, newdata = xtest)

                    
                    # end output diversion and close temporary connection
                    #   (output is in 'stdoutText')
                    sink()
                    close(con)
                    
                    
                    # save the current model to disk
                    #---------------------------------
                 #??   modelFileName <- paste0(modelPath,modelBase,loopTarget,".RData")
                 #??   save(rf.model, file=modelFileName)
                    
                    
                    
                    # test data actual response values
                    #---------------------------------
                    ytestActual <- Yall_normalised[testStart:nrow(Yall_normalised), loopTarget]
                    
                    # maxValue from normalisation arguement values
                    #
                    r <- match(loopTarget, normargs$name)
                    maxValue <- normargs[r,"maxVal"]
                    
                    
                    #Denormalise the test data and the original values of the test data range
                    #
                    ytestActual_dn <- ytestActual * maxValue
                    rf_predicted_dn <- rf_predicted * maxValue
                    #which.max(rf_predicted_dn)
                    
                    # evaluation metrics (coded in 'evaluationStats.R')
                    #-------------------
                    
                    ## MB  : mean bias 
                    ## MAE : mean absolute error
                    ## RMSE : root mean squared error
                    ## Rsq  : R2 
                    ## IOA  : Index of Agreement
                    ## COE  : Coefficient of Efficiency (Legates/McCabe Index)
                    
                    datapoints <- length(rf_predicted_dn)
                    meanBias <- MB(ytestActual_dn,rf_predicted_dn)
                    meanAbsErr <- MAE(ytestActual_dn,rf_predicted_dn)
                    rmse <- RMSE(ytestActual_dn,rf_predicted_dn)
                    r2 <- Rsq(ytestActual_dn,rf_predicted_dn)
                    IdxOfAgree <- IOA(ytestActual_dn,rf_predicted_dn)
                    CoOfEff <- COE(ytestActual_dn,rf_predicted_dn)
                    
                    
                    # create a vector of the values and add to the summary dataframe
                    currentLoopModel <- data.frame(KFold,
                                                   loopTarget,
                                                   meanBias,
                                                   meanAbsErr,
                                                   rmse,
                                                   r2,
                                                   IdxOfAgree,
                                                   CoOfEff)
                    
                    # name the dataframe
                    names(currentLoopModel) <- c("ModelNo", "Target", "MB", "MAE", "RMSE","R2","IOA","COE")

                    modelSummary_df <- rbind(modelSummary_df, currentLoopModel)
                    # print the description of model number
                    rptString <- paste0("\n \nModel KFold # ", KFold, 
                                       "  with ", as.character(datapoints), 
                                       " datapoints used for output/validation ", loopTarget, ". \n", collapse = " ")
                    write(rptString, reportFile, append = TRUE)


                    # print the model training log
                    write(stdoutText, reportFile, append = TRUE)
                    
                    rptString <- paste("Model development time: ", time.taken_dev, "seconds.\n") 
                    write(rptString, reportFile, append = TRUE)                   
                                       
                    # print the model statistics from testing (currentLoop Model)
                    capture.output(currentLoopModel, file = reportFile , append = TRUE)
                    write("\n ", reportFile, append = TRUE)
                    

    
#} # end loop (target)

# Finally, write out the summary dataframe of performance for easy reference
#---------------------------------------------------------------------------

#delete the first 'dummy' row of modelSummary_df
if(modelSummary_df$Target[1] == "") {modelSummary_df <- modelSummary_df[-c(1),]}

#write(modelSummary_df, reportFile, append = TRUE)
write.table( modelSummary_df, reportFile  , append= T, col.names = T,sep = "\t\t" )
write("\n \nBEST MODEL IS ... \n", reportFile, append = TRUE)
capture.output(modelSummary_df[which.max(modelSummary_df$COE),], file = reportFile , append = TRUE)

write("\n \n ", reportFile, append = TRUE)

by_target <- modelSummary_df %>% group_by(Target)
capture.output(by_target %>% summarise(bestR2 = max(R2, na.rm = TRUE), bestCOE = max(COE)) ,
               file = reportFile , append = TRUE)



## end of script ####

