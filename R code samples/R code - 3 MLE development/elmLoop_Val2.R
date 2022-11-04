# test code that loops through elm models for testing
# ---------------------------------------------------

rm(list = ls())

#strWD <- paste0(uniDataSets, dataSet)
setwd("C:/Users/u1097253/Documents/PhD work (C)/Paper 2 - Emulators for BioPhysical Crop Models/Data and Code/R")

#load the relevant libraries
suppressPackageStartupMessages({
    
library(tidyverse)
library(elmNNRcpp)
library(vip)
source("evaluationStats.R")   #Stats to evaluate the model performance

})


#Set up variable values
inputDataPath <- "/uni/DataGen/DataSets/Paper2/R_DataSets/" 
#inputDataFile <- "randNormComboData.csv"
#inputDataFile <- "randNormComboData_Trimmed.csv"

inputDataFile <- "randNormInputData_MultiLoc_v2.csv"


normalisationValuesFile <- "NormalisationArguments_MultiLoc_v1.csv"
datasetDescr <- " Six Locations (elm nhid = 1000)"
numberOfTargets <- 6
currentTarget <- 1

reportFile <- "./Reports/ELM_MultiLoc_v2_VI.rpt"
act_pred_outputDataFile <- "./Reports/Act_Pred_ELM_MultiLoc_v2.csv"

modelPath <- "./Models/"
modelBase <- "elm_model_"

validationData <- TRUE
validationInputFile <- "randNormInputData_MultiLoc_Val_v2.csv"
validataion_act_pred_outputDataFile <- "./Reports/Act_Pred_Validation_ELM_MultiLoc_v2.csv"


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

inputFactors <- inputData[, -c(totalFactorCnt+1:totalCols)]     #drop off the output target columns (6 outputs)
targetValues <- inputData[, -c(1:totalFactorCnt)]     #save the target values into their own dataframe


X <- inputFactors
trainCnt <- nrow(X) * 0.8    #80/20 Train/Test split
testStart <- trainCnt + 1
xtrain <- as.matrix(X[1:trainCnt, ])
xtest <- as.matrix(X[testStart:nrow(X), ])

# prepare normalised target columns with names ready for de-normalisation
Yall_normalised <- targetValues
targetNames <- colnames(Yall_normalised)

#Load and prepare validation data set(s)
#if (validationData){
    
    sourceFile <- paste0(inputDataPath,validationInputFile)
    inputValData <- read.csv(sourceFile)
    
    inputValFactors <- inputValData[, -c(totalFactorCnt+1:totalCols)]     #drop off the output target columns (6 outputs)
    xValidate <- as.matrix(inputValFactors[ , ])
    
    targetValidationValues <- inputValData[, -c(1:totalFactorCnt)]               #save the target values into their own dataframe
    Yall_validationNorm <- targetValidationValues
    
#}

#prepare report file
rptString <- paste("Report on elm model development for ", datasetDescr, "\n \n")
write(rptString, reportFile)

##' Loop Start
##' ==========
##' 
##' Loop for each target (output) 
##' and within this loop for each of the elm model's parameter settings
##' -------------------------------------------------------------------
##' 

modelSummary_df <- data.frame("ModelNo" = 0,
                              "Target" = "",
                              "MB" = 0,
                              "MAE" = 0,
                              "RMSE" = 0,
                              "R2" = 0,
                              "IOA" = 0,
                              "COE" = 0)

act_pred_df <- data.frame()

#if (validationData){
    validationSummary_df <- modelSummary_df
    validation_act_pred_df <- act_pred_df
#}


modelConfiguration <- 0
#for (loopTarget in targetNames) {

    loopTarget <- "EmergenceDAS"
    
    # convert the 'current' column of train-data-response to a one-column matrix
    #---------------------------------------------------------------------------
    #currentTargetName <- targetNames[loopTarget]
    ytrain <- matrix(Yall_normalised[1:trainCnt, loopTarget], nrow = trainCnt, ncol = 1)
    
    # Set up [Loop] values for each model parameter
    loopBias <- TRUE
    loopInitwts <- 'normal_gaussian'    
    loopActFun <- 'sin'
    loopnhid <- 1000
        
                    # setup output to a variable for reporting (variable is 'stdoutText')
                    stdoutText <- vector('character')
                    con    <- textConnection('stdoutText', 'wr', local = TRUE)
                    sink(con)
                    
                    modelConfiguration <- modelConfiguration + 1
                    
                    # perform a fit and predict [ elmNNRcpp ]
                    #----------------------------------------
                    
                    fit_elm = elm_train(xtrain, ytrain, verbose = T,
                                        nhid = loopnhid, 
                                        actfun = loopActFun,
                                        init_weights = loopInitwts, 
                                        bias = loopBias 
                    )
                    
                    predicted_elm <- NULL
                    predicted_elm <- elm_predict(fit_elm, xtest)

                    
                    # end output diversion and close temporary connection
                    #   (output is in 'stdoutText')
                    sink()
                    close(con)
                    
 
                    # save the current model to disk
                    #---------------------------------
                        
                        #save(fit_elm, file="elm_model_GrainYld.RData")
                        #fit_elm <- load("elm_model_GrainYld.RData")
                    modelFileName <- paste0(modelPath,modelBase,loopTarget,".RData")
                    save(fit_elm, file=modelFileName)
                    
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
                    predicted_elm_dn <- predicted_elm[, 1] * maxValue
                    #which.max(predicted_elm_dn)
                    
                    # evaluation metrics (coded in 'evaluationStats.R')
                    #-------------------
                    
                    ## MB  : mean bias 
                    ## MAE : mean absolute error
                    ## RMSE : root mean squared error
                    ## Rsq  : R2 
                    ## IOA  : Index of Agreement
                    ## COE  : Coefficient of Efficiency (Legates/McCabe Index)
                    
                    datapoints <- length(predicted_elm_dn)
                    meanBias <- MB(ytestActual_dn,predicted_elm_dn)
                    meanAbsErr <- MAE(ytestActual_dn,predicted_elm_dn)
                    rmse <- RMSE(ytestActual_dn,predicted_elm_dn)
                    r2 <- Rsq(ytestActual_dn,predicted_elm_dn)
                    IdxOfAgree <- IOA(ytestActual_dn,predicted_elm_dn)
                    CoOfEff <- COE(ytestActual_dn,predicted_elm_dn)
                    
                    
                    # create a vector of the values and add to the summary dataframe
                    currentLoopModel <- data.frame(modelConfiguration,
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
                    rptString <- paste("\n \nDevelopment Model Configuration #", modelConfiguration, 
                                       " with ", as.character(datapoints), 
                                       "datapoints used for output ", loopTarget, ". \n", collapse = " ")
                    write(rptString, reportFile, append = TRUE)

                    # print the model settings
                    rptString <- paste("  nhid = ", as.character(loopnhid),
                                       "  actfun = ", loopActFun, 
                                       "  init_weights = ", loopInitwts,
                                       "  bias = ", loopBias,
                                       " \n")
                    write(rptString, reportFile, append = TRUE)
                    
                    # print the model training log
                    write(stdoutText, reportFile, append = TRUE)
                    
                    # print the model statistics from testing (currentLoop Model)
                    capture.output(currentLoopModel, file = reportFile , append = TRUE)
                    write("\n ", reportFile, append = TRUE)
                    
                    
                    #=============================================
                    # Store the Actual and Predicted 'y' values for plotting later
                    #=============================================
                    actName <- paste0("Act_",loopTarget,"_",loopnhid,"_",loopActFun)
                    predName <- paste0("Pred_",loopTarget,"_",loopnhid,"_",loopActFun)
                    
                    if(nrow(act_pred_df) == 0) {
                        act_pred_df <- data.frame(ytestActual_dn, predicted_elm_dn)
                    } else {
                        act_pred_df <- data.frame(act_pred_df,ytestActual_dn,predicted_elm_dn)
                    }
                    
                    #set the correct column names
                    act_pred_df <- rename(act_pred_df, !!actName:= "ytestActual_dn")    #rename(x, !!varNewName:=oldName) from dplyr
                    act_pred_df <- rename(act_pred_df, !!predName:= "predicted_elm_dn")
                    
                    
                    ## IF validation data set available - run prediction and run stats
                    #if (validationData){
                        elm_validationPred <- elm_predict(fit_elm, xValidate)

                        # validation data actual response values
                        #---------------------------------
                        yValidateActuals <- Yall_validationNorm[, loopTarget]
                        
                        #Denormalise the validataion data and the original values of the validation data set
                        #
                        yValidateActual_dn <- yValidateActuals * maxValue
                        elm_validationPred_dn <- elm_validationPred[, 1] * maxValue
                        
                        
                        # evaluation metrics (coded in 'evaluationStats.R')
                        #-------------------
                        
                        datapoints <- length(elm_validationPred_dn)
                        meanBias <- MB(yValidateActual_dn,elm_validationPred_dn)
                        meanAbsErr <- MAE(yValidateActual_dn,elm_validationPred_dn)
                        rmse <- RMSE(yValidateActual_dn,elm_validationPred_dn)
                        r2 <- Rsq(yValidateActual_dn,elm_validationPred_dn)
                        IdxOfAgree <- IOA(yValidateActual_dn,elm_validationPred_dn)
                        CoOfEff <- COE(yValidateActual_dn,elm_validationPred_dn)
                        
                        
                        # create a vector of the values and add to the summary dataframe
                        currentValidateLoopModel <- data.frame(modelConfiguration,
                                                               loopTarget,
                                                               meanBias,
                                                               meanAbsErr,
                                                               rmse,
                                                               r2,
                                                               IdxOfAgree,
                                                               CoOfEff)
                        
                        # name the dataframe
                        names(currentValidateLoopModel) <- c("ModelNo", "Target", "MB", "MAE", "RMSE","R2","IOA","COE")
                        
                        validationSummary_df <- rbind(validationSummary_df, currentValidateLoopModel)
                        # print the description of model number
                        rptString <- paste0("\n \nValidation Model Configuration # ", modelConfiguration, 
                                            "  with ", as.character(datapoints), 
                                            " datapoints used for output ", loopTarget, ". \n", collapse = " ")
                        write(rptString, reportFile, append = TRUE)
                        
                        
                        # print the model training log
                        #write(stdoutText, reportFile, append = TRUE)
                        
                        # print the model statistics from Validation (currentLoop Model)
                        capture.output(currentValidateLoopModel, file = reportFile , append = TRUE)
                        write("\n ", reportFile, append = TRUE)
                        

                        # calculate and print the variable importance for the model
                        # VI scores using the permutation method with 5 Monte Carlo repetitions
                        #
                        featureNames <- colnames(xtrain)
                        pfun <- function(fit_elm, newdata) {elm_predict(fit_elm, newdata = newdata) }
                        
                        viList <- vip::vi(fit_elm, method = "permute", 
                                     train = xValidate[1:2000 , ] ,    # sample 1000 observations
                                     target = yValidateActuals[1:2000],   #= "y",
                                     feature_names = featureNames, 
                                     metric = "r2",
                                     pred_wrapper = pfun, 
                                     nsim = 5,
                                     scale = TRUE)
                        
                        utils::capture.output(print(viList,n = 30), file = reportFile , append = TRUE)
                        write("\n ", reportFile, append = TRUE)
                        
                        
                        #=============================================
                        # Store the Actual and Predicted 'y' values for plotting later
                        #=============================================
                        actName <- paste0("Act_",loopTarget)
                        predName <- paste0("Pred_",loopTarget)
                        if(nrow(validation_act_pred_df) == 0) {

                            maxValueLat <- normargs[which(normargs$name == "Lat"), "maxVal"]
                            maxValuePAWCmm <- normargs[which(normargs$name == "PAWCmm"), "maxVal"]
                            
                            Lat_dn <- xValidate[, "Lat"] * maxValueLat
                            PAWCmm_dn <- xValidate[, "PAWCmm"] * maxValuePAWCmm
                            
                            validation_act_pred_df <- data.frame(Lat_dn, 
                                                                 PAWCmm_dn,
                                                                 yValidateActual_dn,
                                                                 elm_validationPred_dn)
                            
                        } else {
                             validation_act_pred_df <- data.frame(validation_act_pred_df,yValidateActual_dn,elm_validationPred_dn)
                        }
                        
                        #set the correct column names
                        validation_act_pred_df <- rename(validation_act_pred_df, !!actName:= "yValidateActual_dn")    #rename(x, !!varNewName:=oldName) from dplyr
                        validation_act_pred_df <- rename(validation_act_pred_df, !!predName:= "elm_validationPred_dn")
                        
                        
                        
                    #} end if validation
                    


    
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

## how to add/report model details for each of the groups/outputs for max(r2) and max(COE)?????


#if(validationData){
    if(validationSummary_df$Target[1] == "") {
        validationSummary_df <- validationSummary_df[-c(1),]
    }
    #write(validationSummary_df, reportFile, append = TRUE)
    write("\n \nVALIDATION MODEL >>>>>>>>>>>>>>> \n", reportFile, append = TRUE)
    write.table( validationSummary_df, reportFile  , append= T, col.names = T,sep = "\t\t" )
    write("\n \nBEST VALIDATION MODEL IS ... \n", reportFile, append = TRUE)
    capture.output(validationSummary_df[which.max(validationSummary_df$COE),], file = reportFile , append = TRUE)
    
    write("\n \n ", reportFile, append = TRUE)
    
    by_target <- validationSummary_df %>% group_by(Target)
    capture.output(by_target %>% summarise(bestR2 = max(R2, na.rm = TRUE), bestCOE = max(COE)) ,
                   file = reportFile , append = TRUE)
    
    
#}



#write out the "actual_predicted_values" data for xy plots
write.csv(act_pred_df, act_pred_outputDataFile , row.names=FALSE)
if(validationData){write.csv(validation_act_pred_df, validataion_act_pred_outputDataFile, row.names=FALSE)}



## end of script ####
