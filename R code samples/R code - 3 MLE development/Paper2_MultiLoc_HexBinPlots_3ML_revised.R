# HexBin plots of Prediction Counts for MARS, ANN and Random Forest Predicted vs Observed
################################################
#Plotting 6 outputs X 3 MLs: HexBin plots  (Development Testing) 

rm(list = ls())
library(ggplot2)
library(ggpmisc)
library(grid)
library(gridExtra)
library(hexbin)
library(viridis)

# define the datapaths / files / settings
#setwd("C:/Users/u1097253/Documents/PhD work (C)/Paper 2 - Emulators for BioPhysical Crop Models/Data and Code/R/Reports/")
dataDir <- "//HomeCloud/HomeFiles/H_Documents/Home_David/USQ/H_Uni/Paper #2/Data and Code/R/Reports/"
strWD <- paste0(dataDir, " ")
setwd(strWD)

#Set up variable values
inputDataPath <- "//HomeCloud/HomeFiles/H_Documents/Home_David/USQ/H_Uni/Paper #2/Data and Code/R/Reports/"

inputDataFile <- "Act_Pred_All_DevVal.csv"

outputDataPath <- "//HomeCloud/HomeFiles/H_Documents/Home_David/USQ/H_Uni/Paper #2/Data and Code/R/Reports/"
outputCSVFile <- "Act_Pred_All_DevVal_3ML.csv"
#===========================================

#Load  .csv data file
sourceFile <- paste0(inputDataPath,inputDataFile)
inputData <- read.csv(sourceFile)


#Set Model labels for report ordering
inputData$Model[inputData$Model == "NNet"] <- "(a) ANN"
#inputData$Model[inputData$Model == "ELM"]  <- "(b) ELM"  >>> deleted
inputData$Model[inputData$Model == "MARS"] <- "(b) MARS"
inputData$Model[inputData$Model == "RF"]   <- "(c) RF"

#Remove data for ELM model
selectedInputData <- inputData[!(inputData$Model == "ELM"),]
inputData <- selectedInputData

inputData$Output[inputData$Output == "EmergenceDAS"] <- "(i) EmergenceDAS"
inputData$Output[inputData$Output == "FloweringDAS"] <- "(ii) FloweringDAS"
inputData$Output[inputData$Output == "PoddingDAS"]   <- "(iii) PoddingDAS"
inputData$Output[inputData$Output == "MaturityDAS"]  <- "(iv) MaturityDAS"
inputData$Output[inputData$Output == "Biomass"]      <- "(v) Biomass"
inputData$Output[inputData$Output == "GrainWt"]      <- "(vi) GrainWt"


#Convert selected columns to Factors ready for plotting with group-by  
inputData$Location. <- as.factor(inputData$Location)    
inputData$Model. <- as.factor(inputData$Model)

##DEBUG ONLY ===============================================
# write out the csv file for RE-graphing if needed
# outputCSVFile = paste0(outputDataPath,outputCSVFile)
# write.csv(inputData, outputCSVFile, row.names=FALSE)
#============================================================

#------------------------------------------------------
# Split input dataframe by output -> facet wrap by model for each of (6) outputs
EmergenceDAS_df <- subset(inputData, Output=="(i) EmergenceDAS")
FloweringDAS_df <- subset(inputData, Output=="(ii) FloweringDAS")
PoddingDAS_df <- subset(inputData, Output=="(iii) PoddingDAS")
MaturityDAS_df <- subset(inputData, Output=="(iv) MaturityDAS")
Biomass_df <- subset(inputData, Output=="(v) Biomass")
GrainWt_df <- subset(inputData, Output=="(vi) GrainWt")



#####==============================================================================================  
#Plotting script
#####===================================================

#EmergenceDAS
p1hexbin <- ggplot(EmergenceDAS_df, aes(Actual, Predicted)) + 
  geom_hex(bins = 40) +
  labs(y = "(i) EmergenceDAS", x = "") +
  expand_limits(x = c(0, 30), y = c(0, 30)) +
  scale_fill_gradientn(colours = viridis(256, option = "B", direction = -1)) +
  theme_bw() +
  theme(legend.position = 'hidden') +
    geom_abline(intercept = 0, slope = 1.0)
pEmergence_HB <- p1hexbin + facet_wrap( vars(Model),  ncol = 3, scales = "fixed", strip.position = "top") +
                            theme(strip.background = element_blank(), strip.placement = "outside")

#FloweringDAS
p2hexbin <- ggplot(FloweringDAS_df, aes(Actual, Predicted)) + 
  geom_hex(bins = 40) +
  labs(y = "(ii) FloweringDAS", x = "") +
  expand_limits(x = c(0, 150), y = c(0, 150)) +
  scale_fill_gradientn(colours = viridis(256, option = "B", direction = -1)) +
  theme_bw() +
  theme(legend.position = 'hidden',
        strip.background = element_blank(),
        strip.text.x = element_blank() ) +
   geom_abline(intercept = 0, slope = 1.0)
pFlowering_HB <- p2hexbin + facet_wrap( vars(Model), ncol = 3, scales = "fixed") 

#PoddingDAS
p3hexbin <- ggplot(PoddingDAS_df, aes(Actual, Predicted)) + 
  geom_hex(bins = 40) +
  labs(y = "(iii) PoddingDAS", x = "") +
  scale_fill_gradientn(colours = viridis(256, option = "B", direction = -1)) +
    xlim(0, 300) + ylim(0, 300) +
  theme_bw() +
  theme(legend.position = 'hidden',
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
   geom_abline(intercept = 0, slope = 1.0)
pPodding_HB <- p3hexbin + facet_wrap( vars(Model), ncol = 3, scales = "fixed") 

#MaturityDAS
p4hexbin <- ggplot(MaturityDAS_df, aes(Actual, Predicted)) + 
  geom_hex(bins = 40) +
  labs(y = "(iv) MaturityDAS", x = "") +
  scale_fill_gradientn(colours = viridis(256, option = "B", direction = -1)) +
    xlim(0, 250) + ylim(0, 250) +
  theme_bw() +
  theme(legend.position = 'hidden',
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
   geom_abline(intercept = 0, slope = 1.0)
pMaturity_HB <- p4hexbin + facet_wrap( vars(Model), ncol = 3, scales = "fixed") 

#Biomass
p5hexbin <- ggplot(Biomass_df, aes(Actual, Predicted)) + 
  geom_hex(bins = 40) +
  labs(y = "(v) Biomass", x = "") +
  scale_fill_gradientn(colours = viridis(256, option = "B", direction = -1)) +
    xlim(0, 1500) + ylim(0, 1500) +
  theme_bw() +
  theme(legend.position = 'hidden',
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
   geom_abline(intercept = 0, slope = 1.0) 
pBiomass_HB <- p5hexbin + facet_wrap( vars(Model), ncol = 3, scales = "fixed") 

#GrainWt
p6hexbin <- ggplot(GrainWt_df, aes(Actual, Predicted)) + 
  geom_hex(bins = 40) +
  labs(y = "(vi) GrainWt", x = "") +
  scale_fill_gradientn(colours = viridis(256, option = "B", direction = -1)) +
    xlim(0, 700) + ylim(0, 700) +
  theme_bw() +
  theme(legend.position = 'hidden',
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
   geom_abline(intercept = 0, slope = 1.0)
pGrainWt_HB <- p6hexbin + facet_wrap( vars(Model), ncol = 3, scales = "fixed") 



png('HexBinPlots_Models_MultiSites_v4_3ML_revised.png', width=13, height=30, units='cm', res=300)
print(
  gridExtra::grid.arrange(pEmergence_HB, pFlowering_HB, pPodding_HB, pMaturity_HB, pBiomass_HB, pGrainWt_HB,
                          nrow = 6,
                         ## top = textGrob("  HexBin Plots of Prediction Counts",gp=gpar(fontsize=18,font=3)),
                          left = textGrob("ML Predicted Values",gp=gpar(fontsize=12,font=2), rot = 90),
                          bottom =  textGrob("APSIM Generated Values",gp=gpar(fontsize=12,font=2))
                          )
     )


dev.off()


######################################################################
# 
# LEDGEND CREATION
#GrainWt
p6lhexbin <- ggplot(GrainWt_df, aes(Actual, Predicted)) + 
  geom_hex(bins = 40) +
  labs(y = "(vi) GrainWt", x = "") +
  expand_limits(x = c(0, 1000), y = c(-1000, 1500)) +
  scale_fill_gradientn(colours = viridis(256, option = "B", direction = -1)) +
  theme_bw() +
  theme(legend.position = 'bottom',
        strip.background = element_blank(),
        strip.text.x = element_blank())
pGrainWtLegend_HB <- p6lhexbin + facet_wrap( vars(Model), ncol = 3, scales = "fixed") 



png('Legend_HexBinPlots_MultiStites_3ML.png', width=12, height=15, units='cm', res=300)
print(
  gridExtra::grid.arrange( pGrainWtLegend_HB,
                           nrow = 2,
                           ##top = textGrob(" HexBin Plots of Prediction Counts",gp=gpar(fontsize=18,font=3)),
                           left = textGrob("ML Predicted Values",gp=gpar(fontsize=12,font=2), rot = 90),
                           bottom =  textGrob("APSIM Generated Values",gp=gpar(fontsize=12,font=2))
  )
  
)


dev.off()

######################################################################


