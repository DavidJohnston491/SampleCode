var1 <- runif(7, min = 0, max = 1)
var2 <- runif(7, min = 0, max = 10)
var3 <- runif(7, min = 50, max = 100)
var4 <- runif(7, min = 2050, max = 10000)
var5 <- runif(7, min = -1, max = 1)

testdf <- data.frame(var1,var2,var3,var4,var5)


testnormargs <- data.frame("name" = colnames(testdf), 
                       # "minVal" = colMin(comboData), 
                       "minVal" = 0, 
                       "maxVal" = colMax(testdf),
                       "minScale" = 0,
                       "maxScale" = 1)

#force var5 to set values
var5Details <- data.frame("name" = c("var5"), "minVal" = -5.0, "maxVal" = 5.0, "minScale" = -1, "maxScale" = 1)
testnormargs['var5',] <- var5Details

#define functions
my_normalise <- function(x,minScale,maxScale,minVal,maxVal){
    result <-  (maxScale - minScale) %*% ((x - minVal) / (maxVal - minVal)) + minScale 
    return(result)
}

# test 'my_normalise()' function  (passes)
#z <- my_normalise(5,0,1,0,200)
#z


#std_normalisation <- function(data.df, norm.args){
 data.df <- newSowdf
 norm.args <- normargs
 testnormargs <- normargs
 
    mylist <- data.frame ()
    normdata <- data.df
    for (c in colnames(data.df)) {
        currentFactor <- data.df[c]
        r <- match(c, testnormargs$name)
        #print(r)
        #print(norm.args[r]$maxVal)
        minScale <- testnormargs[r,"minScale"]
        maxScale <- testnormargs[r,"maxScale"]
        minVal <- testnormargs[r,"minVal"]
        maxVal <- testnormargs[r,"maxVal"]

        for (i in 1:nrow(currentFactor)) {
            x <- currentFactor[i,1]
            normalisedValue <- my_normalise(x ,        
                                      minScale ,
                                      maxScale ,
                                      minVal ,
                                      maxVal )
            #print(normalisedValue)                     
            mylist[i,1] <- normalisedValue      
        }
        normdata[c] <- mylist
    }
    return (normdata)
#}  



testdf2 <- testdf
normalised_df <- std_normalisation(testdf, testnormargs)
normalised_df
set.seed(42)
random_df <- normalised_df[sample(nrow(normalised_df)),]
random_df

write.csv(random_df, "testrandom_df.csv", row.names=FALSE)
