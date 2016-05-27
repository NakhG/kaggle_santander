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

#another try, with sapply
apply_attempt <- sapply(df_noconstants,
                        function(x) sum(x == 0)/length(x))

#worked!
apply_attempt <- data.frame(apply_attempt)

head(apply_attempt)

apply_attempt$column <- rownames(apply_attempt)
rownames(apply_attempt) <- 1:nrow(apply_attempt)
apply_attempt[, c(1,2)] <- apply_attempt[, c(2,1)]

apply_attempt <- rename(apply_attempt, percentzero = column)
apply_attempt <- rename(apply_attempt, variable = apply_attempt)

#histogram of frequency of variabls w/ different % of all values equal to 0
#(...wording that is tough)
#basically, you can see that most variables are almost all 0 values
install.packages("ggplot2")
library(ggplot2)

ggplot(data=apply_attempt, aes(percentzero)) +
  geom_histogram(binwidth=0.05) + 
  labs(x="Percent of variable values that = 0", 
       y="Number of variables", 
       title="Amount of variables \n w/ frequency of valued = 0")

#so now we want to remove all those variables
#and be left w/ a dataframe of variables w/ ...
# ... more than less than 95% of values ==0

#select all the columns w/ < 0.95
apply_attempt.to_keep <- filter(apply_attempt, percentzero < 0.95)
head(apply_attempt.to_keep)

#and make a new dataframe w/ just those
#need to explicitly keep TARGET in, cuz guess what, it has very few of the "1" class!
df_noconstants_nosparse <- select(df_noconstants, ID, TARGET, one_of(apply_attempt.to_keep$variable))

#proof of the TARGET distribution problem
ggplot(data=df_noconstants_nosparse, aes(x=TARGET)) +
  geom_histogram(binwidth=1) + scale_x_discrete(breaks=c(0,1)) + 
  labs(title="Customer satisfaction distribution", x="Customer satisfaction\n (0 = Satisfied, 1 = Dissatisfied)")

#turn TARGET into a factor
df_noconstants_nosparse$TARGET <- as.factor(df_noconstants_nosparse$TARGET)

#Step 4: create the training/testing split from the 'training' data
install.packages("caret")
library("caret")

set.seed(182)
train_index <- createDataPartition(df_noconstants_nosparse$TARGET, p=0.7, list=FALSE, times=1)
santander_train <- df_noconstants_nosparse[train_index, ]
santander_test <- df_noconstants_nosparse[-train_index, ]

table(santander_train$TARGET)

#going to make a training set w/o ID, so it isn't used in the models
santander_train.noID <- select(santander_train, -ID)


#Step 5: attempt decision tree
install.packages("rpart")
library(rpart)

model <- rpart(TARGET ~., data=santander_train.noID, method="class")
plot(model)
#hey! it's not making a tree, it's basically saying nothing is a good predictor??

install.packages("RWeka")
library(RWeka)

fit_c45 <- J48(TARGET ~., data=santander_train.noID)
fit_part <- PART(TARGET ~., data=santander_train.noID)

install.packages("ipred")
library(ipred)
fit_bagging <- bagging(TARGET ~., data=santander_train.noID, coob=TRUE)
summary(fit_bagging)
print(fit_bagging)

#test that bagging model
santander_test$bagging_predictions <- predict(object=fit_bagging, newdata=santander_test[,c(1, 3:88)])
table(santander_test$bagging_predictions)

install.packages("e1071")
confusionMatrix(data=santander_test$bagging_predictions,
                reference=santander_test$TARGET)

nrow(filter(santander_test, santander_test$bagging_predictions == santander_test$TARGET)) / nrow(santander_test)
#95% accuracy

#try bagging again, but using diff package: adabag
#adabag has nice pre-built in options, but for some reason is too slow??
#NOTE: installing adabag after ipred will make this the adabag version of "bagging" function
install.packages("adabag")
library(adabag)
fit_adabag <- bagging.cv(TARGET ~., data=santander_train.noID, v=10, mfinal=5,
                      control=rpart.control(maxdepth=10, minsplit=20))

fit_adaboost <- boosting(TARGET ~., data=santander_train.noID, mfinal=5)


#QUESTIONS:
# 1: why did single decision trees fail, but bagging works?
# 2: why is adabag's bagging/boosting infinitely slower than ipred's?
# 3: what were the most useful predictors in the ipred bagging model?

#apparently, the "ranger" package is very fast for random forst
install.packages("ranger")
library(ranger)

#make model
rf <- ranger(TARGET ~ ., data=santander_train.noID, 
             num.trees = 300, importance = "impurity", write.forest=TRUE)

#get variable importance
class(importance(rf))
importances <- data.frame(importance(rf))

importances$variables <- rownames(importances)
importances <- rename(importances, var_importance = importance.rf.)
rownames(importances) <- 1:nrow(importances)

importances <- arrange(importances, desc(var_importance)) 
#whoops turns out ID was one of the most important predictors (edit: fixed)
#most important var's: var15, var38. big gap btw those two and most important's 3-5: saldo_medio_var5_ult3, saldo_var30, saldo_medio_var5_hace2


ggplot(importances, aes(x=variables, y=var_importance)) +
  geom_bar(stat="identity")

#make predictions on the testing
santander_test$ranger_preds <- predictions(predict(rf, santander_test, predict.all = FALSE))

#test accuracy
nrow(filter(santander_test, 
            santander_test$ranger_preds == santander_test$TARGET)) / nrow(santander_test)

#96% accuracy




#Step 6: perform PCA on numeric data


#Step 7: use components to perform logistic regression