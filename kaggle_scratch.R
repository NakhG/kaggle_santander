##GEORGE NAKHLEH
##SANTANDER KAGGLE COMPETITION
##PREDICTING CUSTOMER DISSATISFACTION

#Retry v2 after discussion

#Step 1: Prep for cleaning
# Since we can't use 'testing' data anyway (meant for validation on kaggle)
# ... why not just work w/ the 'training' set, and split after cleaning?
install.packages("dplyr")
library(dplyr)
df <- read.csv("C:\\Users\\gnakhleh\\Documents\\kaggle_santander\\train.csv")


#Step 2: remove constant variables
df_noconstants <- df[ , apply(df, 2, var, na.rm=TRUE) > 0]


#Step 3: remove variables w/ more than x% of values == 0
#first, get a histogram that has percent of values that are 0 as x...
# ... and number of variables as y

#new df for this?
hist_df <- data.frame(colnames(df_noconstants))
head(hist_df)

#BUSTED
percent_zeros <- c()
zero_finder <- function(d){
  for (column in 1:ncol(d)){
    perc <- sum(d[, column][d[, column] ==0]) / length(d[, column])
    percent_zeros[0+column] <- perc
  } 
  return (percent_zeros)
}

#doesnt work 
#list_of_constants <- zero_finder(df_noconstants)



#Step 4: create the training/testing split from the 'training' data


#Step 5: attempt decision tree


#Step 6: perform PCA on numeric data


#Step 7: use components to perform logistic regression