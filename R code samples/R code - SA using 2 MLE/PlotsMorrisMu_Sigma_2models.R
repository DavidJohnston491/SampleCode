#Plots XY Morris indices (22 inputs) mu* and sigma 2 ML emulators
#============================================================================
# Random Forest and ANN emulators for 6 output targets by 22 Inputs
#   Latitude varied [Gunnedah Sowing 2 base]


rm(list = ls())

library(ggplot2)
library(grid)
library(gridExtra)
library(dplyr)

options(scipen=999)
#format(summary(caps), big.mark = ",") # using big.mark to add commas

# define the datapaths / files / settings
# project load directory = working directory ["U:/Paper #3/Data and Code/R"]
dataPath <- "./Reports/"

# sample name "ANN_22MegaMorrisTest_E1.csv"
ANN_megainputs <- "ANN_22MegaMorrisTest_"
RF_megainputs <- "RF_22MegaMorrisTest_"
#versions <- c("E1","E2","E3","G1","G2","G3","H1","H2","H3")
versions <- c("G2")
inputFileExt <- ".csv"


# Output strings
outputPath <- "./Reports/Figures/"

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Start loop of input file sets
for (loopCnt in (1:length(versions)))  {
  #Load  .csv data file of Morris data
  
  #ANN models
  sourceFile <- paste0(dataPath,ANN_megainputs,versions[loopCnt],inputFileExt)
  ann_mega_df <- read.csv(sourceFile)
  
  #RF models
  sourceFile <- paste0(dataPath,RF_megainputs,versions[loopCnt],inputFileExt)
  rf_mega_df <- read.csv(sourceFile)
  
  #Rename statistic columns to identify model
  ann_mega_df <- rename(ann_mega_df, ann.mu = mu
                                   , ann.mu.star = mu.star
                                   , ann.sigma = sigma
                        )

  rf_mega_df <- rename(rf_mega_df, rf.mu = mu
                                 , rf.mu.star = mu.star
                                 , rf.sigma = sigma
                      )
  
  
  # Combine the two ML models' dataframes into a single data source
  allScenarios_df <- data.frame(ann_mega_df[ ,-c(1)])
  allScenarios_df$rf.mu <- rf_mega_df$rf.mu
  allScenarios_df$rf.mu.star <- rf_mega_df$rf.mu.star
  allScenarios_df$rf.sigma <- rf_mega_df$rf.sigma

  
  # convert target names to report format
  allScenarios_df$target[allScenarios_df$target == "EmergenceDAS"] <- "(i) EmergenceDAS"
  allScenarios_df$target[allScenarios_df$target == "FloweringDAS"] <- "(ii) FloweringDAS"
  allScenarios_df$target[allScenarios_df$target == "PoddingDAS"]   <- "(iii) PoddingDAS"
  allScenarios_df$target[allScenarios_df$target == "MaturityDAS"]  <- "(iv) MaturityDAS"
  allScenarios_df$target[allScenarios_df$target == "Biomass"]      <- "(v) Biomass"
  allScenarios_df$target[allScenarios_df$target == "GrainWt"]      <- "(vi) GrainWt"
  
   Emergence_df <- subset(allScenarios_df, target == "(i) EmergenceDAS")
   Flowering_df <- subset(allScenarios_df, target == "(ii) FloweringDAS")
   Podding_df <- subset(allScenarios_df, target == "(iii) PoddingDAS")
   Maturity_df <- subset(allScenarios_df, target == "(iv) MaturityDAS")
   Biomass_df <- subset(allScenarios_df, target == "(v) Biomass")
   GrainWt_df <- subset(allScenarios_df, target == "(vi) GrainWt")
   
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  #Generate graphs
  #===============
  

  #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  # generate 1 XY plot for each Target for statistics mu.star and sigma.
  # Use Grid Arrange to create output plot
  #
  titleTxt <- paste0("Comparison of Morris statistics from two ML models ")
  
      # ggplot2 plot EmergenceDAS mustar
      morris_EmergenceDAS_mustar <- ggplot(Emergence_df, aes(x=ann.mu.star, y=rf.mu.star,  label=input )) +
              labs(title = "EmergenceDAS mu* (days)") +
              xlab("ANN model")+
              ylab("RF model")+
              geom_point()+
              geom_text(hjust=0,vjust=1,  size = 2.5, check_overlap = TRUE, nudge_y = 0.2)+
              scale_x_continuous(limits = c(0, 10)) +
              scale_y_continuous(limits = c(0, 10)) +
              theme_bw() +
              theme(legend.position = "none") +
              theme(plot.title = element_text(size = 10), axis.title = element_text(size = 8)) +
              geom_abline(intercept = 0, slope = 1)
      
      # ggplot2 plot EmergenceDAS sigma
      morris_EmergenceDAS_sigma <- ggplot(Emergence_df, aes(x=ann.sigma, y=rf.sigma,  label=input )) +
        labs(title = "EmergenceDAS sigma (days)") +
        xlab("ANN model")+
        ylab("RF model")+
        geom_point()+
        geom_text(hjust=0,vjust=1,  size = 2.5, check_overlap = TRUE, nudge_y = 0.2)+
        scale_x_continuous(limits = c(0, 6)) +
        scale_y_continuous(limits = c(0, 6)) +
        theme_bw() +
        theme(legend.position = "none") +
        theme(plot.title = element_text(size = 10), axis.title = element_text(size = 8)) +
        geom_abline(intercept = 0, slope = 1)
      
      
      # ggplot2 plot FloweringDAS mustar
      morris_FloweringDAS_mustar <- ggplot(Flowering_df, aes(x=ann.mu.star, y=rf.mu.star,  label=input )) +
        labs(title = "FloweringDAS mu* (days)") +
        xlab("ANN model")+
        ylab("RF model")+
        geom_point()+
        geom_text(hjust=0,vjust=1,  size = 2.5, check_overlap = TRUE, nudge_y = 0.2)+
        scale_x_continuous(limits = c(0, 25)) +
        scale_y_continuous(limits = c(0, 25)) +
        theme_bw() +
        theme(legend.position = "none") +
        theme(plot.title = element_text(size = 10), axis.title = element_text(size = 8)) +
        geom_abline(intercept = 0, slope = 1)
      
      # ggplot2 plot FloweringDAS sigma
      morris_FloweringDAS_sigma <- ggplot(Flowering_df, aes(x=ann.sigma, y=rf.sigma,  label=input )) +
        labs(title = "FloweringDAS sigma (days)") +
        xlab("ANN model")+
        ylab("RF model")+
        geom_point()+
        geom_text(hjust=0,vjust=1,  size = 2.5, check_overlap = TRUE, nudge_y = 0.2)+
        scale_x_continuous(limits = c(0, 18)) +
        scale_y_continuous(limits = c(0, 18)) +
        theme_bw() +
        theme(legend.position = "none") +
        theme(plot.title = element_text(size = 10), axis.title = element_text(size = 8)) +
        geom_abline(intercept = 0, slope = 1)
      
      
      # ggplot2 plot PoddingDAS mustar
      morris_PoddingDAS_mustar <- ggplot(Podding_df, aes(x=ann.mu.star, y=rf.mu.star,  label=input )) +
        labs(title = "PoddingDAS mu* (days)") +
        xlab("ANN model")+
        ylab("RF model")+
        geom_point()+
        geom_text(hjust=0,vjust=1,  size = 2.5, check_overlap = TRUE, nudge_y = 0.2)+
        scale_x_continuous(limits = c(0, 80)) +
        scale_y_continuous(limits = c(0, 80)) +
        theme_bw() +
        theme(legend.position = "none") +
        theme(plot.title = element_text(size = 10), axis.title = element_text(size = 8)) +
        geom_abline(intercept = 0, slope = 1)
      
      # ggplot2 plot PoddingDAS sigma
      morris_PoddingDAS_sigma <- ggplot(Podding_df, aes(x=ann.sigma, y=rf.sigma,  label=input )) +
        labs(title = "PoddingDAS sigma (days)") +
        xlab("ANN model")+
        ylab("RF model")+
        geom_point()+
        geom_text(hjust=0,vjust=1,  size = 2.5, check_overlap = TRUE, nudge_y = 0.2)+
        scale_x_continuous(limits = c(0, 50)) +
        scale_y_continuous(limits = c(0, 50)) +
        theme_bw() +
        theme(legend.position = "none") +
        theme(plot.title = element_text(size = 10), axis.title = element_text(size = 8)) +
        geom_abline(intercept = 0, slope = 1)
      
      
      
      # ggplot2 plot MaturityDAS mustar
      morris_MaturityDAS_mustar <- ggplot(Maturity_df, aes(x=ann.mu.star, y=rf.mu.star,  label=input )) +
        labs(title = "MaturityDAS mu* (days)") +
        xlab("ANN model")+
        ylab("RF model")+
        geom_point()+
        geom_text(hjust=0,vjust=1,  size = 2.5, check_overlap = TRUE, nudge_y = 0.2)+
        scale_x_continuous(limits = c(0, 60)) +
        scale_y_continuous(limits = c(0, 60)) +
        theme_bw() +
        theme(legend.position = "none") +
        theme(plot.title = element_text(size = 10), axis.title = element_text(size = 8)) +
        geom_abline(intercept = 0, slope = 1)
      
      # ggplot2 plot MaturityDAS sigma
      morris_MaturityDAS_sigma <- ggplot(Maturity_df, aes(x=ann.sigma, y=rf.sigma,  label=input )) +
        labs(title = "MaturityDAS sigma (days)") +
        xlab("ANN model")+
        ylab("RF model")+
        geom_point()+
        geom_text(hjust=0,vjust=1,  size = 2.5, check_overlap = TRUE, nudge_y = 0.2)+
        scale_x_continuous(limits = c(0, 40)) +
        scale_y_continuous(limits = c(0, 40)) +
        theme_bw() +
        theme(legend.position = "none") +
        theme(plot.title = element_text(size = 10), axis.title = element_text(size = 8)) +
        geom_abline(intercept = 0, slope = 1)
      
      
      
      
      # ggplot2 plot Biomass mustar
      morris_Biomass_mustar <- ggplot(Biomass_df, aes(x=ann.mu.star, y=rf.mu.star,  label=input )) +
        labs(title = "Biomass mu* (kg/ha)") +
        xlab("ANN model")+
        ylab("RF model")+
        geom_point()+
        geom_text(hjust=0,vjust=1,  size = 2.5, check_overlap = TRUE, nudge_y = 0.2)+
        scale_x_continuous(limits = c(0, 500)) +
        scale_y_continuous(limits = c(0, 500)) +
        theme_bw() +
        theme(legend.position = "none") +
        theme(plot.title = element_text(size = 10), axis.title = element_text(size = 8)) +
        geom_abline(intercept = 0, slope = 1)
      
      # ggplot2 plot Biomass sigma
      morris_Biomass_sigma <- ggplot(Biomass_df, aes(x=ann.sigma, y=rf.sigma,  label=input )) +
        labs(title = "Biomass sigma (kg/ha)") +
        xlab("ANN model")+
        ylab("RF model")+
        geom_point()+
        geom_text(hjust=0,vjust=1,  size = 2.5, check_overlap = TRUE, nudge_y = 0.2)+
        scale_x_continuous(limits = c(0, 400)) +
        scale_y_continuous(limits = c(0, 400)) +
        theme_bw() +
        theme(legend.position = "none") +
        theme(plot.title = element_text(size = 10), axis.title = element_text(size = 8)) +
        geom_abline(intercept = 0, slope = 1)
      
      
      
      # ggplot2 plot GrainWt mustar
      morris_GrainWt_mustar <- ggplot(GrainWt_df, aes(x=ann.mu.star, y=rf.mu.star,  label=input )) +
        labs(title = "GrainWt mu* (kg/ha)") +
        xlab("ANN model")+
        ylab("RF model")+
        geom_point()+
        geom_text(hjust=0,vjust=1,  size = 2.5, check_overlap = TRUE, nudge_y = 0.2)+
        scale_x_continuous(limits = c(0, 300)) +
        scale_y_continuous(limits = c(0, 300)) +
        theme_bw() +
        theme(legend.position = "none") +
        theme(plot.title = element_text(size = 10), axis.title = element_text(size = 8)) +
        geom_abline(intercept = 0, slope = 1)
      
      # ggplot2 plot GrainWt sigma
      morris_GrainWt_sigma <- ggplot(GrainWt_df, aes(x=ann.sigma, y=rf.sigma,  label=input )) +
        labs(title = "GrainWt sigma (kg/ha)") +
        xlab("ANN model")+
        ylab("RF model")+
        geom_point()+
        geom_text(hjust=0,vjust=1,  size = 2.5, check_overlap = TRUE, nudge_y = 0.2)+
        scale_x_continuous(limits = c(0, 200)) +
        scale_y_continuous(limits = c(0, 200)) +
        theme_bw() +
        theme(legend.position = "none") +
        theme(plot.title = element_text(size = 10), axis.title = element_text(size = 8)) +
        geom_abline(intercept = 0, slope = 1)
      

      outputPlot <- paste0("./Reports/Figures/MorrisCompare_Summary.png")
      
       png(outputPlot, width=12, height=28, units='cm', res=300)
       print(
      
            grid.arrange(morris_EmergenceDAS_mustar
                         , morris_EmergenceDAS_sigma
                         , morris_FloweringDAS_mustar
                         , morris_FloweringDAS_sigma
                         , morris_PoddingDAS_mustar
                         , morris_PoddingDAS_sigma
                         , morris_MaturityDAS_mustar
                         , morris_MaturityDAS_sigma
                         , morris_Biomass_mustar
                         , morris_Biomass_sigma
                         , morris_GrainWt_mustar
                         , morris_GrainWt_sigma
                         , ncol = 2
                        # , heights=unit(c(5,5,5,5,5,5),c("cm"))
                         , top="Comparison of Morris statistics \nfrom two Machine Learning emulators"
                        )
      
   )
   
   dev.off()
  

}


