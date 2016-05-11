install.packages('dplyr')
library(dplyr)

#Kaggle
#Project to predict customer dissatisfaction ...
#from a dataset provided by Santander bank

#read in the dataset and make a smaller sample ...
#to review in Excel

df <- read.csv("C:\\Users\\gnakhleh\\Documents\\train.csv")
df_sample <- sample_n(df, 1000, replace=TRUE)
write.csv(df_sample, "train_sample.csv")

#load in test dataset too, to inspect

df_test <- read.csv('C:\\Users\\gnakhleh\\Documents\\test.csv')
dim(df_test)
df_test_sample <- sample_n(df_test, 1000, replace=TRUE)
write.csv(df_test_sample, 'test_sample.csv')
#there is one less variable in the test dataset
#this is because this test is to be submitted to Kaggle for verification
#so we're really going to just split the training data into training and testing

# - - -

#combine the training and testing in order to ...
#clean them equally

df_train <- df
remove(df)

#we will append, after we clarify what dataset they came from

df_train$dataset <- 'train'
df_test$dataset <- 'test'

#append and inspect
combined_df <- bind_rows(df_train, df_test)
combined_sample <- sample_n(combined_df, 1000, replace=TRUE)
write.csv(combined_sample, 'combined_sample.csv')

#begin to clean

#drop rows that are constant
#what's it mean mathematically to be a constant row?
#variance is zero

#combined_noconstants <- combined_df[ , apply(combined_df, 2, var, na.rm=TRUE) != 0]

constant_remover <- function(dat){
  for (i in colnames(dat)){
    print(var(dat$i))
  }
}

constant_remover(combined_df) #doesn't work
#THIS IS WHERE I STOPPED: Trying to remove constants in a way that was clear to me
#if that doesn't work, the commented out function above works


#turn all binaries into 