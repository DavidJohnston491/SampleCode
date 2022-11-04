################
#This script is to consolidate morris and sobol results 
#################

#clear the workspace
gc(verbose = F)
rm(list = ls())

#load the relevant libraries
suppressPackageStartupMessages({
  #library(readr)
   library(xlsx)
   # library(openxlsx)  #read, write and edit XLSX files !!has problems writing to excel files
   library(ggplot2)
   library(ggpmisc)
  
})

# define the datapaths / files / settings
uniDataSets <- "/uni/DataGen/DataSets/"
dataSet <- "SAWarwick/"
inputMorris <- "Warwick_S3d2_Morris.xlsx"
inputSobol <- "Warwick_S3tight_Sobol.xls"
strLocation <- "Warwick"
strSowing <- "S3tight"
strOutputFile <- "Warwick_Merged_Morris_SobolAnalysis.xlsx"

strWD <- paste0(uniDataSets, dataSet)
setwd(strWD)

#read in the morris results
#mor <- read.csv('/uni/DataGen/DataSets/SAGundy/Gundy_S2_Morris.csv', header = T, sep = ',')

mSourceFile <- paste0(uniDataSets,dataSet,inputMorris)

mor <- read.xlsx2(mSourceFile, sheetName  = 'MorrisStatistics', colClasses = c(rep('character',4), rep('numeric',30)))


params.out <- grep(pattern = '.+.Mu$', colnames(mor), value = T)
params.out <- sub('.Mu', '', params.out)

params.out <- params.out[-c(1:3)]

params.in <- unique(mor$Parameter)

#create a dataframe of output, input
df <- data.frame('Output' = sort(rep(params.out, length(params.in))), 'Input' = rep(params.in, length(params.out)))

mor.out <- paste0('Morris_',c('Mu', 'Mustar', 'Sigma'))
df[mor.out] <- NA
for (i in params.out){
  df[,3:5][df$Output == i,] <- mor[grep(i, colnames(mor))]

}



#read in the sobol results, dropping the first 2 columns
#sob <- read.csv('Data/David/Gundy_S2_Sobol.csv')[,3:10]
options(java.parameters = '-Xmx16000m')
sSourceFile <- paste0(uniDataSets,dataSet,inputSobol)
sob <- read.xlsx2(sSourceFile, sheetName = 'SobolStatistics', colClasses = c(rep('character',2), rep('numeric',5), rep('character',3)))[,3:10]
colnames(sob) <- c('SobolResult', 'Bias', 'SE', 'minCI', 'maxCI', 'Input', 'Output', 'Indices')

sob.f <- sob[sob$Indices == 'FirstOrder',]

colnames(sob.f)[1:5] <- paste0('First_', colnames(sob.f)[1:5])

sob.t <- sob[sob$Indices == 'Total',]
colnames(sob.t)[1:5] <- paste0('Total_', colnames(sob.t)[1:5])
sob.f <- sob.f[, -8] #remove the indices column to allow merge
sob.t <- sob.t[,-8]

#df.final <- df.final[,c(16,17, 1:15)]


#df.final <- rbind.data.frame()
df.final <- merge(df, sob.f)
df.final <- merge(df.final, sob.t)
df.final$Site <- strLocation
df.final$Sowing <- strSowing
df.final <- df.final[,c(16:17, 1:15)]

#write.table(df.final, '/uni/DataGen/DataSets/SAGundy/Merged_Sobol_Morris_Goondiwindi_S1.csv',sep = ',', row.names = F)

#write the output dataframe to a sheet in a excel spreadsheet workbook
outFile <- paste0(uniDataSets,dataSet,strOutputFile)
strSheetName <- paste0("MorrisSobol_", strSowing)
#skip this write when Normalised value calcs below are working (they should us df.final instaed of reading in sheets)
#write.xlsx(df.final, outFile, sheetName = strSheetName, row.names = FALSE, append = T)


#Now create the Normalised Sensitivity value columns 
# This will have a value of each input variable, grouped by the output variable

# df <- read.xlsx2(outFile, sheetName = strSheetName, colClasses = 'numeric')

df <- data.frame(df.final)


#ROUND((F2-MIN(F2:F8))/(MAX(F2:F8)-MIN(F2:F8))*100,0)
aa <- seq(1, nrow(df), 7)
bb <- aa + 6

#NormalisedMuStar
for (i in (1:length(aa))) {
  r <- aa[i]:bb[i]
  df$NormalisedMuStar[r] <-((df$Morris_Mustar[r]-min(df$Morris_Mustar[r]))/(max(df$Morris_Mustar[r])-min(df$Morris_Mustar[r])))*100
  df$NormalisedMuStar[r] <-round(df$NormalisedMuStar[r], digits = 0)
}

#NormalisedSobol_FirstOrder
for (i in (1:length(aa))) {
  r <- aa[i]:bb[i]
  df$NormalisedS1[r] <-((df$First_SobolResult[r]-min(df$First_SobolResult[r]))/(max(df$First_SobolResult[r])-min(df$First_SobolResult[r])))*100
  df$NormalisedS1[r] <-round(df$NormalisedS1[r], digits = 0)
}

#NormalisedSobol_TotalOrder
for (i in (1:length(aa))) {
  r <- aa[i]:bb[i]
  df$NormalisedST[r] <-((df$Total_SobolResult[r]-min(df$Total_SobolResult[r]))/(max(df$Total_SobolResult[r])-min(df$Total_SobolResult[r])))*100
  df$NormalisedST[r] <-round(df$NormalisedST[r], digits = 0)
}

#Now write out the spreadsheet
write.xlsx(df, outFile, sheetName = strSheetName, row.names = FALSE, append = T)


#Now produce the XY plots of the NormalisedResults
graphName = paste0(strLocation,strSowing,'MorrisSobol_FirstOrd.png')
png(graphName, width=15, height=20, units='in', res=300)
print(
p1 <-  ggplot(data=df, mapping=aes(x=NormalisedMuStar, y=NormalisedS1)) +
    
    geom_point(aes(shape= Input, col = Input),size=4) +
     #scale_shape_manual(values=c(0,1,2,5,6,15,16 ))+
     scale_shape_manual(values=c(2,17,16,1,18,15,0 ))+
  
    coord_cartesian(ylim=c(0,110)) +
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=8,face="bold"),
          strip.text = element_text(size=8))+
    theme_bw() +
    ylab('Sobol FirstOrder')+
    xlab('MuStar Importance')+
    theme(text = element_text(size=10)) 
    #facet_wrap(~ Location , scales="free", ncol=6, dir = "h")
  
  
)
dev.off()

graphName = paste0(strLocation,strSowing,'MorrisSobol_FirstOrd.png')
png(graphName, width=15, height=20, units='in', res=300)
print(
p2 <-  ggplot(data=df, mapping=aes(x=NormalisedMuStar, y=NormalisedST)) +
  
  geom_point(aes(shape= Input, col = Input),size=4) +
  #scale_shape_manual(values=c(0,1,2,5,6,15,16 ))+
  scale_shape_manual(values=c(2,17,16,1,18,15,0 ))+
  
  coord_cartesian(ylim=c(0,110)) +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=8,face="bold"),
        strip.text = element_text(size=8))+
  theme_bw() +
  ylab('Sobol TotalOrder')+
  xlab('MuStar Importance')+
  theme(text = element_text(size=10)) 
#facet_wrap(~ Location , scales="free", ncol=6, dir = "h")


)
dev.off()

ggsave(filename = graphName, plot = p1, width = 15, height = 10, units = 'cm' , dpi = 600 )

