



#define functions


colMax <- function(comboData) sapply(comboData, max, na.rm = TRUE)
colMin <- function(comboData) {
    sapply(comboData, min, na.rm = TRUE)
    
} 

my_normalise <- function(x,minScale,maxScale,minVal,maxVal){
    # print(x)
    result <-  (maxScale - minScale) %*% ((x - minVal) / (maxVal - minVal)) + minScale 
    return(result)
}


std_normalisation <- function(data.df, norm.args){
    mylist <- data.frame ()
    normdata <- data.df
    for (c in colnames(data.df)) {
        currentFactor <- data.df[c]
        r <- match(c, norm.args$name)
        #print(r)
        #print(norm.args[r]$maxVal)
        minScale <- norm.args[r,"minScale"]
        maxScale <- norm.args[r,"maxScale"]
        minVal <- norm.args[r,"minVal"]
        maxVal <- norm.args[r,"maxVal"]
        
       
        for (i in 1:nrow(currentFactor)) {
            x <- currentFactor[i,1]
            print(i)
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
}  


