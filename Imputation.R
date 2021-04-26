require(mice)
require(lattice)
library(dplyr)
set.seed(123)


# To impute the missing values, we will use mice R package.
# Lets select the columns that we want to impute
impt_data <- df_replaced %>% select(w1c1_L1_21:w5_life_sumptsdworst)


# We will use two passes of imputation because in first pass some columns are
# not imputed due to co-linearity. So we need to set some column off,
# means that column will not be used for imputing other columns

# First pass
cor_imp <- mice(impt_data, pred=quickpred(impt_data, mincor=.9), print=F)
cor_imp_comp <- complete(cor_imp)

# Plot some columns for first pass
stripplot(cor_imp, w3_NN1~.imp,
          xlab = "correlation Imputation number", pch=20, cex=2)
stripplot(cor_imp, w3_U8B~.imp,
          xlab= "Correlation  Imputation number",  pch=20, cex=2)

stripplot(cor_imp, w1c1_L1_27~.imp,
          xlab= "Correlation  Imputation number",  pch=20, cex=2)


# plot(cor_imp)
View(cor_imp_comp)
imputed_data <- cor_imp_comp
colSums(is.na(imputed_data)/ nrow(imputed_data) *100)



# Change the predictor matrix. The predictor matrix is a square
# matrix that specifies the variables that are used to impute each
# incomplete variable. Let us have a look at the predictor matrix that was used
View(cor_imp$predictorMatrix)


#' The object pred contains the predictor matrix from an initial run of mice
#' with zero iterations, specified by maxit = 0. Altering the predictor matrix
#' and returning it to the mice algorithm is very simple. For example, the
#' following code removes the variable w3_stress2_drugs_alcohol from the set of predictors,
#' but still leaves it to be predicted by the other variables.
#'
pred <- cor_imp$predictorMatrix
pred[, "w3_stress2"] <- 0
View(pred)


# Second pass
#' Use your new predictor matrix in mice() as follows
imp <- mice(impt_data, pred=pred, print=F)
new_imputed <- complete(imp)
colSums(is.na(new_imputed)/ nrow(new_imputed) *100)
stopifnot(!any(is.na(new_imputed)))

stripplot(imp, w3_NN1~.imp,
          xlab = "correlation Imputation number", pch=20, cex=2)

stripplot(imp, w3_U8B~.imp,
          xlab= "Correlation  Imputation number",  pch=20, cex=2)



# save output
write.csv(new_imputed, "path/to/file.csv")

# End ------------------------------------------------------
