# RF template for all inputs: specify size of Morris test data set, which data set, 
# and which ML function to generate y values with.

rm(list = ls())

library(boot)
library(sensitivity)
library(dplyr)
library(randomForest)


#Load private functions for normalisation
source("DBJ_Lib.R")


# define the datapaths / files / settings
# project load directory = working directory ["C:/uni/Paper#3/Data and Code/R/"]
dataPath <- "./data/"
dataSet <- ""
inputBaseScenariosFile <- "BaseScenarios_all.csv"   #9 scenarios
testScenario_No <- 1 
testScenarioSet <- c(1,2,3,4,5,6,7,8,9)    # Sowing #2 for Gunnedah base scenario set. Latitude being varied!!
#testScenarioSet <- c(2,5,8)    # Sowing #2 for each of the 3 locations in the base scenario set
versions <- c("E1","E2","E3","G1","G2","G3","H1","H2","H3")
#versions <- c("E2","G2","H2")
locnLats <- c(-23.569, -30.954, -36.670)

normalisationValuesFile <- "normargs.csv"


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
basedata <- subset(baseScenarios, select = -c(1,26:44))  #only select the ML input fields



#Now create the Normalised Sensitivity value columns 
normalisedBaseData <- std_normalisation(basedata, normargs)  #std_normalisation() from "DBJ_Lib.R"


#<<<<<<<<<<<<<<<<<<< LOOP START >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#
# using the Base Scenario: Loop through the 6 target outputs 
# Save output from morris x list : x$S and x$T, after adding extra details for future analysis (eg model, location, CI range )
#
loopCnt <- 0
for (testScenario_No in testScenarioSet ) {
   #select the base scenario for testing
   testScenarioNorm <- normalisedBaseData[testScenario_No,]
   loopCnt <- loopCnt + 1
   vers <- versions[loopCnt]

#
# We now have 1 Normalised scenario to act as the base scenario.
# Next, Generate the sampling strategy by calling morris routine with model set to NULL.
# Then create a matrix of the varying input parameters by applying the sampling strategy to the base values.
# Then combine the varying parameters with the non-varying parameters (IN THE CORRECT SEQUENCE) 
# to pass as input matrix for the ML method.
#


  for (target in c("EmergenceDAS","FloweringDAS","PoddingDAS","MaturityDAS","Biomass","GrainWt")) {
    

    
    # The method of morris requires 1 Monte Carlo sampling plan
    # There are p factors, all following the uniform distribution
    # on [0,1]
    
    # sensitivity analysis
    # Test case : the non-monotonic function of morris
    # x <- morris(model = morris.fun, factors = 20, r = 4,
    #             design = list(type = "oat", levels = 5, grid.jump = 3),
    #              binf = 0, bsup = 1, scale = TRUE)
    #
    # tell(x, y)
    #
    #
    # mu <- apply(x$ee, 2, mean)
    # mu.star <- apply(x$ee, 2, function(x) mean(abs(x)))
    # sigma <- apply(x$ee, 2, sd)
    #
    # print(x)
    # plot(x)
    
    # binf = c(120,400,0,100,30,0.2,260,50,90,0,0,0,10.7,10.9,10.6,0,0,0,152,152,168), 
    # bsup = c(140,600,250,200,40,1,340,340,200,18.8,16.8,19.2,33,33.2,36,295,378,379,633,743,803), 
    
    sampleSize <- 1000
    # call morris routine with no model to establish sampling regime. This will be returned in "x$X".
    x <- morris(model = NULL, factors = 22, r = 1000,
                design = list(type = "oat", levels = 10, grid.jump = 5),
                binf = c(-36.670,120,400,0,100,30,0.2,260,50,90,0,0,0,10.7,10.9,10.6,0,0,0,152,152,168), 
                bsup = c(-23.569,140,600,250,200,40,1,340,340,200,18.8,16.8,19.2,33,33.2,36,295,378,379,633,743,803), 
                scale = TRUE)
    
    X_samplingPlan <- data.frame(x$X)   # 0 - 1 values @ (p+1)*n paths of p variables (10000 samples * 21 variables in the mega test)
    
    # the input parameters to be varied and the ranges of values possible are: 
    #   ShootLag	        Field.Chickpea.Phenology.Emerging.ShootLag	              120	  140
    #   VegTarget	        Field.Chickpea.Phenology.Vegetative.Target.FixedValue	    400	  600
    #   LateVegTarget	    Field.Chickpea.Phenology.LateVegetative.Target.FixedValue	  0	  250
    #   FloweringTarget	  Field.Chickpea.Phenology.Flowering.Target.FixedValue	    100	  200
    #   Population	      Field.Reset_and_Sowing.Script.Population	                 30	   40
    #   SowingFracPAWCmm	Field.Reset_and_Sowing.Script.FracPAWC	                    0.2	  1
    #   PAWCmm                                                                      260   340
    #   [sowingESW           (calc from PAWCmm * FracPAWCmm)]
    #   SowingDOY	        Field.Reset_and_Sowing.Script.dblSowDOY	                   90	  200
    #   AvgMinT0_30                                                                  -1.2  18.8
    #   AvgMinT30_60                                                                 -1.2  16.8
    #   AvgMinT60_90                                                                 -1.4  19.2
    #   AvgMaxT0_30                                                                  10.7  33.0
    #   AvgMaxT30_60                                                                 10.9  33.2
    #   AvgMaxT60_90                                                                 10.6  36.0
    #   Rain0_30                                                                      0.0 295
    #   Rain30_60                                                                     0.0 378
    #   Rain60_90                                                                     0.0 378
    #   Radn0_30                                                                    152   633
    #   Radn30_60                                                                   152   743
    #   Radn60_90                                                                   168   803
    #   Latitutde                                                                  -36.670 -23.569
    #   
    
    # morris will need the 'de-normalised' matrices of parameter values in x$X and for outputs in y.
    #
    # ML needs normalised inputs and outputs, so conversion is required before input and following output being generated.
    #
    # First convert the X_samplingPlan into X_Values (non-normalised)
    #   X_Value = (value_Range * X_samplingPlan value) + Min_Value of range
    #   X_Value_norm = X_Value / Max_Value of range
    #

    x_Lat       <- (X_samplingPlan[ ,1])
    x_ShootLag <- (X_samplingPlan[ ,2] )
    x_VegTarget <- (X_samplingPlan[ ,3] )
    x_LateVegTarget <- (X_samplingPlan[ ,4] )
    x_FloweringTarget <- (X_samplingPlan[ ,5]) 
    x_Population <- (X_samplingPlan[ ,6] )
    x_FracPAWCmm <- (X_samplingPlan[ ,7])
    x_PAWCmm <- (X_samplingPlan[ ,8] )
     x_sowingESW <- (X_samplingPlan[ ,9] )   
    #    x_sowingESW <- (x_PAWCmm * x_FracPAWCmm)  #needs to be calculated
    x_SowingDOY <- (X_samplingPlan[ ,10] )
    x_AvgMinT0_30 <- (X_samplingPlan[ ,11] )
    x_AvgMinT30_60 <- (X_samplingPlan[ ,12] )
    x_AvgMinT60_90 <- (X_samplingPlan[ ,13] )
    x_AvgMaxT0_30 <- (X_samplingPlan[ ,14] )
    x_AvgMaxT30_60 <- (X_samplingPlan[ ,15])
    x_AvgMaxT60_90 <- (X_samplingPlan[ ,16] )
    x_Rain0_30 <- (X_samplingPlan[ ,17] )
    x_Rain30_60 <- (X_samplingPlan[ ,18] )
    x_Rain60_90 <- (X_samplingPlan[ ,19] )
    x_Radn0_30 <- (X_samplingPlan[ ,20] )
    x_Radn30_60 <- (X_samplingPlan[ ,21] )
    x_Radn60_90 <- (X_samplingPlan[ ,22] )

    #x_Lat       <- (X_samplingPlan[ ,22] * (0) + locnLats[loopCnt])
    # x_ShootLag <- (X_samplingPlan[ ,1] * (140-120) + 120)
    # x_VegTarget <- (X_samplingPlan[ ,2] * (600 - 400) + 400)
    # x_LateVegTarget <- (X_samplingPlan[ ,3] * (250 - 0) + 0)
    # x_FloweringTarget <- (X_samplingPlan[ ,4] * (200 - 100) + 100)
    # x_Population <- (X_samplingPlan[ ,5] * (40 - 30) + 30)
    # x_FracPAWCmm <- (X_samplingPlan[ ,6] * (1 - 0.2) + 0.2)
    # x_PAWCmm <- (X_samplingPlan[ ,7] * (340 - 260) + 260)
    # x_sowingESW <- (X_samplingPlan[ ,8] * (340 - 60) + 60)   #needs to be calculated
    #    x_sowingESW <- (x_PAWCmm * x_FracPAWCmm)
    # x_SowingDOY <- (X_samplingPlan[ ,9] * (200 - 90) + 90)  
    # x_AvgMinT0_30 <- (X_samplingPlan[ ,10] * (16 - 0) + 0)
    # x_AvgMinT30_60 <- (X_samplingPlan[ ,11] * (14 - 0) + 0)
    # x_AvgMinT60_90 <- (X_samplingPlan[ ,12] * (18 - 0) + 0)
    # x_AvgMaxT0_30 <- (X_samplingPlan[ ,13] * (33 - 10) + 10)
    # x_AvgMaxT30_60 <- (X_samplingPlan[ ,14] * (33 - 12) + 12)
    # x_AvgMaxT60_90 <- (X_samplingPlan[ ,15] * (36 - 11) + 11)
    # x_Rain0_30 <- (X_samplingPlan[ ,16] * (295 - 0) + 0)
    # x_Rain30_60 <- (X_samplingPlan[ ,17] * (380 - 0) + 0)
    # x_Rain60_90 <- (X_samplingPlan[ ,18] * (380 - 0) + 0)
    # x_Radn0_30 <- (X_samplingPlan[ ,19] * (630 - 150) + 150)
    # x_Radn30_60 <- (X_samplingPlan[ ,20] * (740 - 150) + 150)
    # x_Radn60_90 <- (X_samplingPlan[ ,21] * (800 - 170) + 170)

    
    
    # Combine these non-normalised values into a matrix and copy it back into 'x' as x$X
    myX <- cbind(x_Lat,
                 x_ShootLag,
                 x_VegTarget,
                 x_LateVegTarget,
                 x_FloweringTarget,
                 x_Population,
                 x_FracPAWCmm,
                  x_PAWCmm,
                  x_sowingESW,
                  x_SowingDOY,
                 x_AvgMinT0_30,
                 x_AvgMinT30_60,
                 x_AvgMinT60_90,
                 x_AvgMaxT0_30,
                 x_AvgMaxT30_60,
                 x_AvgMaxT60_90,
                 x_Rain0_30,
                 x_Rain30_60,
                 x_Rain60_90,
                 x_Radn0_30,
                 x_Radn30_60,
                 x_Radn60_90
           #      , x_Lat
    )
    
    x$X <- myX  #these are the non-normalised values that morris will use to perform its analysis against the non-normalised outputs 'y'
    
    # Generate the full set of normalised input parameters required to run the ML emulators
    # Base data values are already in normalised form, so we need to normalise the variable input parameters, and combine them.
    #*********************
    #*
    myXorig_df <- data.frame(myX)
    myNormX_df <- data.frame(myX)
    
    i <- 0
    for (c in colnames(myX)) {
      i <- i + 1
                        #  c <- c("x_Radn60_90")
      c_Trimmed <- unlist(strsplit(c, split='x_', fixed=TRUE))[2]
      r <- match(c_Trimmed, normargs$name)
      thisMaxVal <- normargs[r,"maxVal"]
      thisMinVal <- normargs[r,"minVal"]
      thisMaxScale <- normargs[r,"maxScale"]
      thisMinScale <- normargs[r,"minScale"]
      
      #myNormX_df[i] <- (myXorig_df[i] / thisMaxVal)  #assumes that all values are in a (0 - maxValue) range
      myNormX_df[i] <-  (thisMaxScale - thisMinScale) * ((myXorig_df[i] - thisMinVal) / (thisMaxVal - thisMinVal)) + thisMinScale #allows any normalisation range
    }
    
    #Combine testScenarioNorm with myNormX_df  to give the input set 'xtest'  for ML 
    # (using dplyr) generate the sampling number of rows from the test scenario
    sims <- nrow(x$X)
    xtest <- testScenarioNorm %>% slice(rep(row_number(1), sims))  
    
    xtest$Lat             <- myNormX_df$x_Lat
    xtest$ShootLag        <- myNormX_df$x_ShootLag
    xtest$VegTarget       <- myNormX_df$x_VegTarget
    xtest$LateVegTarget   <- myNormX_df$x_LateVegTarget
    xtest$FloweringTarget <- myNormX_df$x_FloweringTarget
    xtest$Population      <- myNormX_df$x_Population
    xtest$FracPAWCmm      <- myNormX_df$x_FracPAWCmm
     xtest$PAWCmm          <- myNormX_df$x_PAWCmm
     xtest$sowingESW       <- myNormX_df$x_sowingESW
     xtest$SowingDOY       <- myNormX_df$x_SowingDOY
    xtest$AvgMinT0_30     <- myNormX_df$x_AvgMinT0_30
    xtest$AvgMinT30_60    <- myNormX_df$x_AvgMinT30_60
    xtest$AvgMinT60_90    <- myNormX_df$x_AvgMinT60_90
    xtest$AvgMaxT0_30     <- myNormX_df$x_AvgMaxT0_30
    xtest$AvgMaxT30_60    <- myNormX_df$x_AvgMaxT30_60
    xtest$AvgMaxT60_90    <- myNormX_df$x_AvgMaxT60_90
    xtest$Rain0_30        <- myNormX_df$x_Rain0_30
    xtest$Rain30_60       <- myNormX_df$x_Rain30_60
    xtest$Rain60_90       <- myNormX_df$x_Rain60_90
    xtest$Radn0_30        <- myNormX_df$x_Radn0_30
    xtest$Radn30_60       <- myNormX_df$x_Radn30_60
    xtest$Radn60_90       <- myNormX_df$x_Radn60_90

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
    
    # Pass the de-normalised outputs, along with the de-normalised variable input parameters to morris for analysis (using 'tell')
    tell(x,y_deNorm)
    
    colnames(x$ee)<-sub('^x_','',colnames(x$ee))
    
    mu <- apply(x$ee, 2, mean)
    mu.star <- apply(x$ee, 2, function(x) mean(abs(x)))
    sigma <- apply(x$ee, 2, sd)
    #
    print(x)
    plot(x)
    
    #Record the system time for timing the analysis
    end.time <- Sys.time()
    time.taken_analysis <- as.numeric(end.time - start.time, units = "secs")
    time.taken_analysis <- round(time.taken_analysis, digits = 3)
    
    # Extract location and sowing event details
    sowingCode <- "S?"
    if(baseScenarios[testScenario_No,"SowingDOY"] == 100) {sowingCode <- "S1"}
    if(baseScenarios[testScenario_No,"SowingDOY"] == 150) {sowingCode <- "S2"}
    if(baseScenarios[testScenario_No,"SowingDOY"] == 201) {sowingCode <- "S3"}
    #scenarioStr <- paste0(baseScenarios[testScenario_No,"Location"],sowingCode)
    locnName <- baseScenarios[testScenario_No,"Location"]
    
    # Title
    titleStr <- paste("\n\n>>>New Analysis: - ", target, " Sample size: ", sampleSize, "  ", locnName, sowingCode)
    rptFile <- paste0("./Reports/RF_Mega_Morris_CaptureOut_",target,".txt")
    cat(titleStr, file = rptFile, append = TRUE)

    # 
    # # Review and report morris statistics
    capture.output(print(x), file = rptFile, append = TRUE)
    # 
    # add timing values
    timeStr <- paste("\n <<<< Time Taken for analysis: ",time.taken_analysis," seconds \n")
    cat(timeStr,file=rptFile, append=TRUE)
    
    #
    # Save the data for plotting later in ggplot2
    #
    my.mu <- data.frame(mu)
    my.mu.star <- data.frame(mu.star)
    my.sigma <- data.frame(sigma)
    myMorris <- data.frame(my.mu,my.mu.star,my.sigma)
    myMorris$model <- model
    myMorris$location <- locnName
    myMorris$sowing <- sowingCode
    myMorris$target <- target
    myMorris$input <- rownames(myMorris)
    
    mytemp <- data.frame(myMorris[ ,4:ncol(myMorris) ],myMorris[ ,1:3])
    myMorris <- mytemp
    rm(mytemp)
    
    
    
    csvFileName <- paste0("./Reports/",model,"_22MegaMorrisTest_",vers,".csv")
    # write.csv(myMorris, file = csvFileName, row.names = FALSE, append = TRUE)
    if(target == "EmergenceDAS")   {
         write.table(myMorris, file = csvFileName, sep = ",",row.names = FALSE,)
    } else {
          write.table(myMorris, file = csvFileName, sep = ",",col.names = FALSE, row.names = FALSE, append = TRUE)
    }



  }  #end target list loop    
} #end test scenario loop

