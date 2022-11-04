
# evaluation metrics for measures of agreement for predicted vs observed values
#------------------------------------------------------------------------------

## mean bias
#-----------------------------------------------------
MB <- function(y_true, y_pred) {
    out <- mean(y_pred - y_true)
    out
}

## mean absolute error
#-----------------------------------------------------
MAE <- function(y_true, y_pred) {
    out <- mean(abs(y_pred - y_true))
    out
}

#RMSE
#-----------------------------------------------------
RMSE = function (y_true, y_pred) {
    out = sqrt(mean((y_true - y_pred)^2))
    out
}

#R2
#-----------------------------------------------------
Rsq <- function (x,y) cor(x,y) ^ 2



##  Index of Agreement
#--------------------------------
IOA <- function(y_true, y_pred) {
    LHS <- sum(abs(y_pred - y_true))
    RHS <- 2 * sum(abs(y_true - mean(y_true)))
    
    if (LHS <= RHS) res <- 1 - LHS / RHS else res <- RHS / LHS - 1
    
    res
}
##  Coefficient of Efficiency (Legates/McCabe)
#-----------------------------------------------------
COE <- function(y_true, y_pred) {
    res <- 1 - sum(abs(y_pred - y_true)) / sum(abs(y_true - mean(y_true)))
    res
}
