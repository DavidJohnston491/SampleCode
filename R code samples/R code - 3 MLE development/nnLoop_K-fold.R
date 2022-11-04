# test code that loops through nNet models for testing
# ----------------------------------------------------

rm(list = ls())

#strWD <- paste0(uniDataSets, dataSet)
#setwd("C:/Users/u1097253/Documents/PhD work (C)/Paper 2 - Emulators for BioPhysical Crop Models/Data and Code/R")
#setwd("U:/Paper #2/Data and Code/R")
setwd("C:/Uni/Paper #2/Data and Code/R")


#load the relevant libraries
suppressPackageStartupMessages({
    
library(tidyverse)
library(nnet)
library(vip)
source("evaluationStats.R")   #Stats to evaluate the model performance

})


#Set up variable values
#inputDataPath <- "/uni/DataGen/DataSets/Paper2/R_DataSets/" 
#inputDataPath <- "U:/DataSets/Paper2/R_DataSets/" 
inputDataPath <- "C:/uni/DataSets/Paper2/R_DataSets/"

#inputDataFile <- "randNormComboData.csv"
#inputDataFile <- "randNormComboData_Trimmed.csv"
inputDataFile <- "randNormInputData_MultiLoc_v2.csv"

normalisationValuesFile <- "NormalisationArguments_MultiLoc_v1.csv"
datasetDescr <- " Six Locations (nnet)"
numberOfTargets <- 6
currentTarget <- 1

reportFile <- "./Reports_K-Fold/nnet_MultiLoc_v2_VI_K-fold.rpt"
act_pred_outputDataFile <- "./Reports_K-Fold/Act_Pred_nnet_MultiLoc_v2_K-fold.csv"

#modelPath <- "./Models/"
#modelBase <- "nn_model_"


#***************************************
#Load normalised .csv data file
sourceFile <- paste0(inputDataPath,inputDataFile)
inputDataBase <- read.csv(sourceFile)

#Load normalisation arguments .csv data file ready for denormalisation of target values
sourceFile <- paste0(inputDataPath,normalisationValuesFile)
normargs <- read.csv(sourceFile)


#names(inputData) = NULL
totalCols <- ncol(inputDataBase)
totalFactorCnt <- totalCols - numberOfTargets
outputCnt <- totalCols - totalFactorCnt   #output count should equal number of Targets!
targetNames <- colnames(inputDataBase[, -c(1:totalFactorCnt)])


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
rptString <- paste("Report on nnet model development for ", datasetDescr, "\n \n")
write(rptString, reportFile)


##' Loop Start
##' ==========
##' 
##' Loop for each target (output) 
##' -----------------------------
##' 

for (loopTarget in targetNames) {
    #loopTargetName <- targetNames[loopTarget]
     #loopTarget = 'FloweringDAS'  #for testing without loop targets

    
    KFoldMax <- 3
    ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    ## Start KFold loop
    ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    for (KFold in 1:KFoldMax) {
     #KFold <- 1
    
        # Randomise the inputData 
        #------------------------
        inputDataRnd <- inputDataBase[sample(nrow(inputDataBase)),]
        
        inputFactors <- inputDataRnd[, -c(totalFactorCnt+1:totalCols)]     #drop off the output target columns (6 outputs)
        targetValues <- inputDataRnd[, -c(1:totalFactorCnt)]     #save the target values into their own dataframe
        
        
        X <- inputFactors
        trainCnt <- nrow(X) * 0.8    #80/20 Train/Test split
        testStart <- trainCnt + 1
        xtrain <- as.matrix(X[1:trainCnt, ])
        xtest <- as.matrix(X[testStart:nrow(X), ])
        
        # prepare normalised target columns with names ready for de-normalisation
        Yall_normalised <- targetValues
    
    
        # convert the 'current' column of train-data-response to a one-column matrix
        #---------------------------------------------------------------------------
        ytrain <- as.matrix(Yall_normalised[1:trainCnt, loopTarget], nrow = trainCnt, ncol = 1)
        
            # setup output to a variable for reporting (variable is 'stdoutText')
            stdoutText <- vector('character')
            con    <- textConnection('stdoutText', 'wr', local = TRUE)
            sink(con)
            

            # perform a fit and predict [ nnet ]
            #----------------------------------------
            start.time <- Sys.time()
            
            nn_model = nnet(xtrain, ytrain, 
                            size = 20,
                            maxit = 100)
            end.time <- Sys.time()
            time.taken_dev <- as.numeric(end.time - start.time, units = "secs")
            time.taken_dev <- round(time.taken_dev, digits = 3)
            
            predicted_nn <- NULL
            predicted_nn <- predict(nn_model, xtest)

            
            # end output diversion and close temporary connection
            #   (output is in 'stdoutText')
            sink()
            close(con)
            
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
            predicted_nn_dn <- predicted_nn * maxValue
            #which.max(predicted_nnet_dn)
            
            # evaluation metrics (coded in 'evaluationStats.R')
            #-------------------
            
            ## MB  : mean bias 
            ## MAE : mean absolute error
            ## RMSE : root mean squared error
            ## Rsq  : R2 
            ## IOA  : Index of Agreement
            ## COE  : Coefficient of Efficiency (Legates/McCabe Index)
            
            datapoints <- length(predicted_nn_dn)
            meanBias <- MB(ytestActual_dn,predicted_nn_dn)
            meanAbsErr <- MAE(ytestActual_dn,predicted_nn_dn)
            rmse <- RMSE(ytestActual_dn,predicted_nn_dn)
            r2 <- Rsq(ytestActual_dn,predicted_nn_dn)
            IdxOfAgree <- IOA(ytestActual_dn,predicted_nn_dn)
            CoOfEff <- COE(ytestActual_dn,predicted_nn_dn)
            
            
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
            rptString <- paste("\n Development Model KFold #", KFold, 
                               " with ", as.character(datapoints), 
                               "datapoints used for output/validation ", loopTarget, ". ", collapse = " ")
            write(rptString, reportFile, append = TRUE)


            # print the model training log
           # write(stdoutText, reportFile, append = TRUE)

            
            rptString <- paste("  Model development time: ", time.taken_dev, "seconds.") 
            write(rptString, reportFile, append = TRUE)                   
            
            
            # print the model statistics from testing (currentLoop Model)
            #capture.output(currentLoopModel, file = reportFile , append = TRUE)
           # write("\n ", reportFile, append = TRUE)
                    
         
     } # end loop (KFold)
                    
    
} # end loop (target)

# Finally, write out the summary dataframe of performance for easy reference
#---------------------------------------------------------------------------

#delete the first 'dummy' row of modelSummary_df
if(modelSummary_df$Target[1] == "") {modelSummary_df <- modelSummary_df[-c(1),]}

#write(modelSummary_df, reportFile, append = TRUE)
write.table( modelSummary_df, reportFile  , append= T, col.names = T,sep = "\t\t" )
# write("\n \nBEST MODEL IS ... \n", reportFile, append = TRUE)
# capture.output(modelSummary_df[which.max(modelSummary_df$COE),], file = reportFile , append = TRUE)

write("\n \n ", reportFile, append = TRUE)

# by_target <- modelSummary_df %>% group_by(Target)
# capture.output(by_target %>% summarise(bestR2 = max(R2, na.rm = TRUE), bestCOE = max(COE)) ,
#                file = reportFile , append = TRUE)

# Summarise using mean and SD by Target
by_target <- modelSummary_df %>% group_by(Target)
#capture.output(by_target %>% summarise(summaryStats = mean(R2, na.rm = TRUE), bestCOE = max(COE)) ,
#                               file = reportFile , append = TRUE)

write("\n \n SUMMARY \n", reportFile, append = TRUE)
by_target <- modelSummary_df %>% group_by(Target)
KFoldSummary <- data.frame(by_target %>% summarise(meanMB = mean(MB),
                                                   sdMB = sd(MB),
                                                   meanMAE = mean(MAE), 
                                                   sdMAE = sd(MAE), 
                                                   meanRMSE = mean(RMSE), 
                                                   sdRMSE = sd(RMSE), 
                                                   meanR2 = mean(R2), 
                                                   sdR2 = sd(R2), 
                                                   meanIOA = mean(IOA), 
                                                   sdIOA = sd(IOA), 
                                                   meanCOE = mean(COE), 
                                                   sdCOE = sd(COE)))


capture.output(KFoldSummary , file = reportFile , append = TRUE)
write("\n \n ", reportFile, append = TRUE)

## end of script ####
