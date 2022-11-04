# VIP example of vip::permute for vi_permute: Permutation-based variable importance
#
#   Description
#   ===========
#   Compute permutation-based variable importance scores for the predictors in a model.
#

# Load required packages 
library(vip)
library(ggplot2)  # for ggtitle() function
library(nnet)     # for fitting neural networks

# Simulate training data
trn <- vip::gen_friedman(500, seed = 101)  # ?vip::gen_friedman

# Inspect data
tibble::as_tibble(trn)

# Fit PPR and NN models (hyperparameters were chosen using the caret package
# with 5 repeats of 5-fold cross-validation)
pp <- ppr(y ~ ., data = trn, nterms = 11)
set.seed(0803) # for reproducibility
nn <- nnet(y ~ ., data = trn, size = 7, decay = 0.1, linout = TRUE,
           maxit = 500)

# Plot VI scores
set.seed(2021)  # for reproducibility
p1 <- vip(pp, method = "permute", target = "y", metric = "rsquared",
          pred_wrapper = predict) + ggtitle("PPR")
p2 <- vip(nn, method = "permute", target = "y", metric = "rsquared",
          pred_wrapper = predict) + ggtitle("NN")
grid.arrange(p1, p2, ncol = 2)

# Mean absolute error
mae <- function(actual, predicted) {
    mean(abs(actual - predicted))
}

# Permutation-based VIP with user-defined MAE metric
set.seed(1101)  # for reproducibility
vip(pp, method = "permute", target = "y", metric = mae,
    smaller_is_better = TRUE,
    pred_wrapper = function(object, newdata) predict(object, newdata)
) + ggtitle("PPR")

