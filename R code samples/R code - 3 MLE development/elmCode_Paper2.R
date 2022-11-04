
# load the data and split it in two parts
#----------------------------------------
rm(list = ls())

library(elmNNRcpp)
source("modStats.R")

#Set up variable values
inputDataPath <- "/uni/DataGen/DataSets/Paper2/R_DataSets/" 
inputDataFile <- "randNormComboData.csv"
numberOfTargets <- 6
currentTarget <- 1
normalisationValuesFile <- "NormalisationArguments.csv"

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


# prepare / convert the train-data-response to a one-column matrix
#-----------------------------------------------------------------

Yall_normalised <- targetValues
targetNames <- colnames(Yall_normalised)
currentTargetName <- targetNames[currentTarget]
ytrain <- matrix(Yall_normalised[1:trainCnt, currentTarget], nrow = trainCnt, ncol = 1)


# output to a variable for reporting
stdoutText <- vector('character')
con    <- textConnection('stdoutText', 'wr', local = TRUE)
sink(con)

# perform a fit and predict [ elmNNRcpp ]
#----------------------------------------

fit_elm = elm_train(xtrain, ytrain, verbose = T,
                    nhid = 1000, 
                    actfun = 'purelin',
                    init_weights = "uniform_negative", 
                    bias = TRUE 
                    )

predicted_elm = elm_predict(fit_elm, xtest)

# end output diversion and close temporary connection
sink()
close(con)


# test data actual response values
#---------------------------------
ytestActual <- Yall_normalised[testStart:nrow(Yall_normalised), currentTarget]

# maxValue from normalisation arguement values
#
r <- match(currentTargetName, normargs$name)
maxValue <- normargs[r,"maxVal"]


#Denormalise the test data and the original values of the test data range
#
ytestActual_dn <- ytestActual * maxValue
predicted_elm_dn <- predicted_elm[, 1] * maxValue
#which.max(predicted_elm_dn)

# evaluation metric
#------------------

#RMSE
#-----------------------------------------------------
rmse = function (y_true, y_pred) {
    out = sqrt(mean((y_true - y_pred)^2))
    out
}

#R2
#-----------------------------------------------------
rsq <- function (x,y) cor(x,y) ^ 2


##  Coefficient of Efficiency (Legates/McCabe)
#-----------------------------------------------------
COE <- function(y_true, y_pred) {
    res <- 1 - sum(abs(y_pred - y_true)) / sum(abs(y_true - mean(y_true)))
    res
}


# statistical measures for 'elm' 
#-------------------------------

cat('the rmse error for extreme-learning-machine is :', rmse(ytestActual_dn, predicted_elm_dn), '\n')
cat('the rsq for extreme-learning-machine is :', rsq(ytestActual_dn, predicted_elm_dn), '\n')
cat('the COE for extreme-learning-machine is :', COE(ytestActual_dn, predicted_elm_dn), '\n')

#write(stdoutText, 'testTxtOutput.txt', append = TRUE)

