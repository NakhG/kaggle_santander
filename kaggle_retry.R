install.packages('dplyr')
library(dplyr)

#Kaggle
#Project to predict customer dissatisfaction ...
#from a dataset provided by Santander bank

#read in the dataset and make a smaller sample ...
#to review in Excel

df <- read.csv("C:\\Users\\gnakhleh\\Documents\\kaggle_santander\\train.csv")
df_sample <- sample_n(df, 1000, replace=TRUE)
write.csv(df_sample, "train_sample.csv")

#load in test dataset too, to inspect

df_test <- read.csv('C:\\Users\\gnakhleh\\Documents\\kaggle_santander\\test.csv')
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

#this returns a warning: NAs introduced by coercion
combined_noconstants <- combined_df[ , apply(combined_df, 2, var, na.rm=TRUE) > 0]
#verify
colnames(combined_df[ , apply(combined_df, 2, var, na.rm=TRUE) == 0])

combined_noconstants <- data.frame(combined_noconstants)
class(combined_noconstants)

install.packages("caret") ; library(caret)

#OK
#so column w/ 'train' and 'test' messes up 
# ... after removing constants
#corr table won't work on non-numeric data

#PLAN:
#make a df of just numeric data of no_constants

numeric_columns <- combined_noconstants %>% select(which(sapply(., is.numeric)))


#use this thing to make the corr
corr_mat <- cor(numeric_columns[,-337])
cols_to_remove <- findCorrelation(corr_mat, cutoff=0.75)

combined_nocorr <- combined_noconstants[, -c(cols_to_remove)]

#so we went from no constants, to no correlation
#so NOW we do the factorization

#if the only unique values in a column are 0 and 1, make it a factor variable
#pseudo code: what are the columns that have 2 unique values? cast those columns to be factors
#how do i do this?

#where are the columns w/ less than 25 unique values?
column_factorizer <- function(d){
  for (column in 1:ncol(d)){
    if (class(d[,column]) == "integer"){
      if (length(unique(d[,column])) < 20){
        d[,column] <- as.factor(d[,column])
      }
    }
  }
  return (d)
}

combined_factorized <- column_factorizer(combined_nocorr)

combined_df.id_dataset <- select(combined_df, ID, dataset)
dim(combined_df.id_dataset)

combined_factorized.update <- merge(x=combined_df.id_dataset, y=combined_factorized, by="ID")

#many columns where 99% of values are 0. 0 is effectively "NA"
#don't do this for TARGET, tho
combined_factorized.update[,-126][combined_factorized.update[,-126] == 0] <- NA

#THERE we go: the dataset we want is combined_facotrized.update
training_cleaned <- filter(combined_factorized.update, dataset=="train")

#remove so much of this
remove(combined_nocorr, combined_noconst_factorized, combined_df, numeric_columns, cor_mat, corr_mat)

#make a sample for analysis
set.seed(501)
train_index <- createDataPartition(training_cleaned$TARGET, p=0.7, list=FALSE, times=1)
santander_train <- training_cleaned[train_index, ]
santander_test <- training_cleaned[-train_index, ]

head(santander_train)
write.csv(santander_train, "cleaned_training_kaggle.csv")

#ok, so we have the data we want
#how can we determine the best predictors?
# PCA?

#this is busted again?
#numeric_columns <- santander_train %>% select(which(sapply(., is.numeric)))

#merge numerics w/ target var
#santander_train.target_id <- select(santander_train, ID, TARGET)
#numeric_columns.with_target <- merge(x=santander_train.target_id, y=numeric_columns, by="ID")

#do stepwise selection on these numeric columns

#DOESN"T WORK
library(MASS)
full_model <- glm(TARGET ~ ., family=binomial(logit), data=numeric_columns.with_target)

library(ggplot2)
library(reshape2)
d <- melt(numeric_columns.with_target[,-1])

ggplot(data=d, aes(x=value)) +
  facet_wrap(~variable, scales = "free_x") +
  geom_histogram()


install.packages("rpart")
library(rpart)

#doesn't work
#fit <- rpart(TARGET ~., method="class", data=santander_train)
