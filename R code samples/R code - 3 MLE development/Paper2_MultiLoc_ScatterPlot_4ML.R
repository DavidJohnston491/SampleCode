# Ploting xy scatter plots of MARS, ELM, ANN and Random Forest Predicted vs Observed
################################################
#Plotting 6 outputs X 4 MLs: xy scatter plots grouped by 7 locations (Development Testing) 

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

inputDataFile <- "Act_Pred_All_DevVal.csv"

outputDataPath <- "C:/Users/u1097253/Documents/PhD work (C)/Paper 2 - Emulators for BioPhysical Crop Models/Data and Code/R/Reports/"
outputCSVFile <- "Act_Pred_All_DevVal.csv"
#===========================================

#Load  .csv data file
sourceFile <- paste0(inputDataPath,inputDataFile)
inputData <- read.csv(sourceFile)


#Set Model labels for report ordering
inputData$Model[inputData$Model == "NNet"] <- "(a) ANN"
inputData$Model[inputData$Model == "ELM"]  <- "(b) ELM"
inputData$Model[inputData$Model == "MARS"] <- "(c) MARS"
inputData$Model[inputData$Model == "RF"]   <- "(d) RF"

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
library(gridExtra)
library(grid)

#EmergenceDAS
p1 <- ggplot(EmergenceDAS_df, aes(Actual, Predicted, color = Location.)) + 
  geom_point() +
  labs(y = "(i) EmergenceDAS", x = "") +
  expand_limits(x = c(0, 30), y = c(0, 30)) +
  theme_bw() +
  theme(legend.position = 'hidden')
pEmergence <- p1 + facet_wrap( vars(Model),  ncol = 4, scales = "fixed", strip.position = "top") 

#FloweringDAS
p2 <- ggplot(FloweringDAS_df, aes(Actual, Predicted, color = Location.)) + 
  geom_point() +
  labs(y = "(ii) FloweringDAS", x = "") +
  expand_limits(x = c(0, 150), y = c(0, 150)) +
  theme_bw() +
  theme(legend.position = 'hidden',
        strip.background = element_blank(),
        strip.text.x = element_blank() )
pFlowering <- p2 + facet_wrap( vars(Model), ncol = 4, scales = "fixed") 

#PoddingDAS
p3 <- ggplot(PoddingDAS_df, aes(Actual, Predicted, color = Location.)) + 
  geom_point() +
  labs(y = "(iii) PoddingDAS", x = "") +
  theme_bw() +
  theme(legend.position = 'hidden',
        strip.background = element_blank(),
        strip.text.x = element_blank())
pPodding <- p3 + facet_wrap( vars(Model), ncol = 4, scales = "fixed") 

#MaturityDAS
p4 <- ggplot(MaturityDAS_df, aes(Actual, Predicted, color = Location.)) + 
  geom_point() +
  labs(y = "(iv) MaturityDAS", x = "") +
  theme_bw() +
  theme(legend.position = 'hidden',
        strip.background = element_blank(),
        strip.text.x = element_blank())
pMaturity <- p4 + facet_wrap( vars(Model), ncol = 4, scales = "fixed") 

#Biomass
p5 <- ggplot(Biomass_df, aes(Actual, Predicted, color = Location.)) + 
  geom_point() +
  labs(y = "(v) Biomass", x = "") +
  theme_bw() +
  theme(legend.position = 'hidden',
        strip.background = element_blank(),
        strip.text.x = element_blank())
pBiomass <- p5 + facet_wrap( vars(Model), ncol = 4, scales = "fixed") 

#GrainWt
p6 <- ggplot(GrainWt_df, aes(Actual, Predicted, color = Location.)) + 
  geom_point() +
  labs(y = "(vi) GrainWt", x = "") +
  theme_bw() +
  theme(legend.position = 'hidden',
        strip.background = element_blank(),
        strip.text.x = element_blank())
pGrainWt <- p6 + facet_wrap( vars(Model), ncol = 4, scales = "fixed") 



png('ScatterPlots_Models_All_v3.png', width=18, height=30, units='cm', res=300)
   print(
     gridExtra::grid.arrange(pEmergence, pFlowering, pPodding, pMaturity, pBiomass, pGrainWt,
                             nrow = 6,
                             top = textGrob("Scatterplots of Model Output Predictions",gp=gpar(fontsize=18,font=3)),
                             left = textGrob("ML Predicted Values",gp=gpar(fontsize=12,font=2), rot = 90),
                             bottom =  textGrob("APSIM Generated Values",gp=gpar(fontsize=12,font=2))
                             )
                           
     )


dev.off()

# 
# 
#   # Base Plot
#   g <- ggplot(mpg, aes(x=displ, y=hwy)) +
#     geom_point() +
#    # geom_smooth(method="lm", se=FALSE) +
#     theme_bw()  # apply bw theme
# 
#   # Facet wrap with free scales
#   g + facet_wrap( ~ class, scales = "free") + 
#       labs(title="hwy vs displ", caption = "Source: mpg", subtitle="Ggplot2 - Faceting - Multiple plots in one figure with free scales")  # Scales free
