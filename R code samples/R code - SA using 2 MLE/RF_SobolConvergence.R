# Test template : specify size of Sobol test data set, which data set, and which ML function to generate y values with.

rm(list = ls())

library(boot)
library(sensitivity)
library(randomForest)
library(dplyr)

#Load private functions for normalisation
source("DBJ_Lib.R")


# define the datapaths / files / settings
# project load directory = working directory ["C:/uni/Paper#3/Data and Code/R/"]
dataPath <- "./data/"
dataSet <- ""
inputBaseScenariosFile <- "RF_BaseScenarios_all.csv"
testScenario_No <- 2

normalisationValuesFile <- "normargs.csv"

# Settings for SOBOL Analysis
n <- 10 #sample set size. Simulation count = (p+2)*n   Sample sizes for convergence testing: 10, 100, 1000, 10000, 50000, 100000.
p <- 6 #number of paramters being varied and analysed by Sobol


# Output strings
model <- c("RF")
index <- c(" ")


#********************************************************
#Load normalisation arguments .csv data file ready for normalisation of target values
sourceFile <- paste0(dataPath,normalisationValuesFile)
normargs <- read.csv(sourceFile)

#Load denormalised .csv base scenarios file
sourceFile <- paste0(dataPath,inputBaseScenariosFile)
baseScenarios <- read.csv(sourceFile, header = TRUE)
basedata <- subset(baseScenarios, select = -c(1,26:44))




#Now create the Normalised Sensitivity value columns 
normalisedBaseData <- std_normalisation(basedata, normargs)  #std_normalisation() from "DBJ_Lib.R"

#select the base scenario for testing
testScenarioNorm <- normalisedBaseData[testScenario_No,]

#
# We now have 1 Normalised scenario to act as the base scenario.
# Next, Generate the sampling strategy by calling Sobol routine with model set to NULL.
# Then create a matrix of the varying input parameters by applying the sampling strategy to the base values.
# Then combine the varying parameters with the non-varying parameters (IN THE CORRECT SEQUENCE) 
# to pass as input matrix for the ML method.
#


#<<<<<<<<<<<<<<<<<<< LOOP START >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#
# using the Base Scenario: Loop through the 6 target outputs running convergence test for each sample level
# and save output from Sobol x list : x$S and x$T, after adding extra details for future analysis
#
convergenceTest <- c(10, 100, 1000, 10000, 50000, 100000)
for (sampleSize in convergenceTest ) {

    n <- sampleSize
    
    for (target in c("EmergenceDAS","FloweringDAS","PoddingDAS","MaturityDAS","Biomass","GrainWt")) {
    
      
      
    
    
    # The method of sobol requires 2 samples
    # There are p factors, all following the uniform distribution
    # on [0,1]
    
    X1 <- data.frame(matrix(runif(p * n), nrow = n))
    X2 <- data.frame(matrix(runif(p * n), nrow = n))
    
    
    # sensitivity analysis
    #x <- sobolSalt(model = sobol.fun, X1, X2, scheme="A", nboot = 100)
    
    # call sobol routine with no model to establish sampling regime. This will be returned in "x$X".
    x <- sobolSalt(model = NULL, X1, X2, nboot = 100)
    
    X_samplingPlan <- data.frame(x$X)   # (p+2)*n obs of p variables (900 * 7 in the base test)
    
    # the input parameters to be varied and the ranges of values are:
    #   ShootLag	    Field.Chickpea.Phenology.Emerging.ShootLag	                120	140
    #   VegTarget	    Field.Chickpea.Phenology.Vegetative.Target.FixedValue	    400	600
    #   LateVegTarget	Field.Chickpea.Phenology.LateVegetative.Target.FixedValue	0	250
    #   FloweringTarget	Field.Chickpea.Phenology.Flowering.Target.FixedValue	    100	200
    #   Population	    Field.Reset_and_Sowing.Script.Population	                30	40
    #   SowingFracPAWCmm	Field.Reset_and_Sowing.Script.FracPAWC	                0.2	1
    #   - [SowingDOY	    Field.Reset_and_Sowing.Script.dblSowDOY	                    90	200] #removed
    #
    
    # Sobol will need the 'de-normalised' matrices of parameter values in x$X and for outputs in y.
    #
    # ML needs normalised inputs and outputs, so conversion is required before input and following output being generated.
    #
    # First convert the X_samplingPlan into X_Values (non-normalised)
    #   X_Value = (value_Range * X_samplingPlan value) + Min_Value of range
    #   X_Value_norm = X_Value / Max_Value of range
    #
    x_ShootLag <- (X_samplingPlan[ ,1] * (140-120) + 120)
    x_VegTarget <- (X_samplingPlan[ ,2] * (600 - 400) + 400)
    x_LateVegTarget <- (X_samplingPlan[ ,3] * (250 - 0) + 0)
    x_FloweringTarget <- (X_samplingPlan[ ,4] * (200 - 100) + 100)
    x_Population <- (X_samplingPlan[ ,5] * (40 - 30) + 30)
    x_FracPAWCmm <- (X_samplingPlan[ ,6] * (1 - 0.2) + 0.2)
     # [x_SowingDOY <- (X_samplingPlan[ ,7] * (200 - 90) + 90)] removed from analysis list
    
    # Combine these non-normalised values into a matrix and copy it back into 'x' as x$X
    myX <- cbind(x_ShootLag,x_VegTarget,x_LateVegTarget,x_FloweringTarget,x_Population,x_FracPAWCmm)
    x$X <- myX  #these are the non-normalised values that Sobol will use to perform its analysis against the non-normalised outputs 'y'
    
    # Generate the full set of normalised input parameters required to run the ML emulators
    # Base data values are already in normalised form, so we need to normalise the variable input parameters, and combine them.
    #*********************
    #*
    myXorig_df <- data.frame(myX)
    myNormX_df <- data.frame(myX)
    
    i <- 0
    for (c in colnames(myX)) {
        i <- i + 1
        c_Trimmed <- unlist(strsplit(c, split='_', fixed=TRUE))[2]
        r <- match(c_Trimmed, normargs$name)
        thisMaxVal <- normargs[r,"maxVal"]
        
        myNormX_df[i] <- (myXorig_df[i] / thisMaxVal)  #assumes that all values are in a (0 - maxValue) range
    }
    
    #Combine testScenarioNorm with myNormX_df  to give the input set 'xtest'  for ML 
    # (using dplyr) generate the sampling number of rows from the test scenario
    sims <- nrow(x$X)
    xtest <- testScenarioNorm %>% slice(rep(row_number(1), sims))  
    
      xtest$FracPAWCmm      <- myNormX_df$x_FracPAWCmm
      xtest$Population      <- myNormX_df$x_Population
      xtest$FloweringTarget <- myNormX_df$x_FloweringTarget
      xtest$LateVegTarget   <- myNormX_df$x_LateVegTarget
      xtest$VegTarget       <- myNormX_df$x_VegTarget
      xtest$ShootLag        <- myNormX_df$x_ShootLag
    
    #*
    #*********************
    
    
      
      #Record the system time for running the analysis
      start.time <- Sys.time()
      
      
    # Run the ML emulator to generate the output values set (normalised values)
    ##y <- ishigami.fun(x$X)
    
      # retrieve the desired model from disk
      #-------------------------------------
      
      
      #save(rf_model, file="rf_model_GrainYld.RData")
      #rf_model <- load("rf_model_GrainYld.RData")

      modelStr <- paste0("./Models/rf_model_",target,".RData")
      
      load(modelStr)
      y_rf_target <- predict(rf.model, newdata = xtest)
    
    # De-normalise the output parameter set(s)
      maxValueTarget <- normargs[which(normargs$name == target), "maxVal"]
      
      y_deNorm <- y_rf_target * maxValueTarget
    
    # Pass the de-normalised outputs, along with the de-normalised variable input parameters to Sobol for analysis (using 'tell')
    tell(x,y_deNorm)
    
    
    #Record the system time for timing the analysis
    end.time <- Sys.time()
    time.taken_analysis <- as.numeric(end.time - start.time, units = "secs")
    time.taken_analysis <- round(time.taken_analysis, digits = 3)
    
    
    # Title
    titleStr <- paste("\n\n>>>New Analysis: - ", target, " Sample size: ", sampleSize)
    rptFile <- paste0("./Reports/rf_CaptureOut_",target,".txt")
    cat(titleStr, file = rptFile, append = TRUE)
    
    
    # Review and report Sobol statistics
    
    #print.sobolSalt
    print(x)
    #plot.sobolSalt()
    plot(x, choice=1)
    
   # rf_GrainWt_100000 <- x
    #save(x, file="./Analysis/rf_GrainWt_100000.sobol")
    capture.output(print(x), file = rptFile, append = TRUE)
    
    # add timing values
    timeStr <- paste("\n <<<< Time Taken for analysis: ",time.taken_analysis," seconds \n")
    cat(timeStr,file=rptFile, append=TRUE)

    myS1 <- x$S
    row.names(myS1) <- 1:nrow(myS1)
    myS1$model <- model
    myS1$sampleSize <- sampleSize
    myS1$target <- target
    myS1$index <- c("FirstOrder")
    myS1$input <- c("ShootLag","VegTarget","LateVegTarget","FloweringTarget","Population","FracPAWC")
    myS1$ciRange <- (myS1$`max. c.i.` - myS1$`min. c.i.`)
    myS1b <- data.frame(myS1[ ,6:ncol(myS1)],myS1[ ,1:5])
    myS1 <- myS1b
    
    
    mySt <- x$T
    row.names(mySt) <- 1:nrow(mySt)
    mySt$model <- model
    mySt$sampleSize <- sampleSize
    mySt$target <- target
    mySt$index <- c("TotalEffects")
    mySt$input <- c("ShootLag","VegTarget","LateVegTarget","FloweringTarget","Population","FracPAWC")
    mySt$ciRange <- (mySt$`max. c.i.` - mySt$`min. c.i.`)
    myStb <- data.frame(mySt[ ,6:ncol(mySt)],mySt[ ,1:5])
    mySt <- myStb
    
    mySobolIndicies <- rbind(myS1,mySt)

    
    csvFileName <- paste0("./Reports/rf_Convergence_",target,".csv")
     # write.csv(mySobolIndicies, file = csvFileName, row.names = FALSE, append = TRUE)
    if(sampleSize == convergenceTest[1]){
      write.table(mySobolIndicies, file = csvFileName, sep = ",",row.names = FALSE,)
    } else {
      write.table(mySobolIndicies, file = csvFileName, sep = ",",col.names = FALSE, row.names = FALSE, append = TRUE)
    }    
    #library(ggplot2)
    #ggplot(x, choice=1)
    
    
   }  #end target list loop    
} #end sample size loop
    
