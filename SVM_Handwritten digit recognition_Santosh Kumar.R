
################################################################################################
# Santosh Kumar Assignment : Date 23 Sep 18
################################################################################################
### Handwritten digit recognition
# Image of a digit submitted by a user via a scanner/tablet/other digital devices
# The goal is to develop a model that can correctly identify the digit (between 0-9) written in an image. 
## Objective is to develop a model using Support Vector Machine which should correctly classify the handwritten digits based on the pixel values given as features

# ----Setting Up required packages-----#
install.packages("caret")
install.packages("kernlab")
install.packages("dplyr")
install.packages("readr")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("h2o")
install.packages("data.table")
install.packages("deepnet")
library(ggplot2)
library(gridExtra)
library(kernlab)
library(readr)
library(caret)
library(caTools)
library(dplyr)
library(h2o)
h2o.init(nthreads = 4)
#java version "1.8.0_131"
#Java(TM) SE Runtime Environment (build 1.8.0_131-b11)
#Java HotSpot(TM) 64-Bit Server VM (build 25.131-b11, mixed mode)
#Starting H2O JVM and connecting: .. Connection successful!
#R is connected to the H2O cluster: 
#H2O cluster uptime:         11 seconds 184 milliseconds 
#H2O cluster timezone:       Asia/Kolkata 
#H2O data parsing timezone:  UTC 
#H2O cluster version:        3.20.0.2 
#H2O cluster version age:    3 months and 7 days  
#H2O cluster name:           H2O_started_from_R_Ranjita_vad332 
#H2O cluster total nodes:    1 
#H2O cluster total memory:   3.52 GB 
#H2O cluster total cores:    4 
#H2O cluster allowed cores:  4 
#H2O cluster healthy:        TRUE 
#H2O Connection ip:          localhost 
#H2O Connection port:        54321 
#H2O Connection proxy:       NA 
#H2O Internal Security:      FALSE 
#H2O API Extensions:         Algos, AutoML, Core V3, Core V4 
#R Version:                  R version 3.5.1 (2018-07-02) 
library(deepnet)
library(parallel)


################################################################################################
## Loading the Train and the Test data set
################################################################################################
setwd("C:/Users/Ranjita/Desktop/Data Analytics_IITM/section V/5.3 SVM Assignment")
train <- read.delim("mnist_train.csv",sep=",",stringsAsFactors = FALSE,header = FALSE)
test <- read.delim("mnist_test.csv",sep=",",stringsAsFactors = FALSE,header = FALSE)

################################################################################################
###   1.Business Understanding  ###
################################################################################################
# To succesfully classify handwritten digits (0-9) using pixel values 
# Use Support Vector Machines

################################################################################################
###   2.Data Understanding  ###
################################################################################################
View(train) # The Data has no column names- 60000 obs. of  786 variables:
View(test) # The Data has no column names- 10000 obs. of  786 variables:

################################################################################################
###   3.Data Cleaning   ###
################################################################################################
# Changing the column name of the first column for both train and test data set

names(train)[1] <-"label"
names(test)[1] <-"label"

# Adding a column signifying train and test data in each data frame
train$type <- "train"
test$type <- "test"

# Duplicated rows
sum(duplicated(test)) # no duplicate rows
sum(duplicated(train)) # no duplicate rows

# Checking for NAs
sum(sapply(test, function(x) sum(is.na(x)))) # There are no NAs
sum(sapply(train, function(x) sum(is.na(x)))) # There are no NAs

str(train) # all dependant variables are integers:'data.frame':	60000 obs. of  786 variables:
str(test) # all dependant variables integers: 'data.frame':	10000 obs. of  786 variables:

# Combining test+train together for data undestanding/analysing the data
combinedData <- rbind(train,test)

# Checking again  for any NAs in the combined Data Set
which(sapply(combinedData,function(x) sum(is.na(x))) != 0) # No NAs on the combined data. We have already checked individually for both Train and Test data

# Checking for columns if some of them have the same features
cols_samefeatures <- which(sapply(combinedData,function(x) length(unique(x)) == 1))
length(cols_samefeatures) # 65 columns have same feature 
cols_samefeatures # There are 65 column which have same value present in all data points (test & train) combined.
# This will not add on to understanding any variance for prediction. Therefore, removing those columns

combinedData_new <- combinedData %>% select(-cols_samefeatures)

## Scaling data 

# The maximum pixel value is 0 to 255. If the value is more than 255, then  scale the entire data set within 255

which(combinedData_new %>% select(-type,-label) > 255) # integer(0):No  pixel value  is greater than 255

which((combinedData_new %>% select(label)) > 9 | (combinedData_new %>% select(label)) < 0) # integer(0):No number given is more than 9 or less than zero

# Getting the data back in train and test sets.

train <- combinedData_new %>% filter(type == "train") %>% select(-type)
test <- combinedData_new %>% filter(type == "test") %>% select(-type)

################################################################################################
###   4.Data preparation ###
################################################################################################
# Converting label variable into factor
train$label <- as.factor(train$label)
summary(train)
test$label <- as.factor(test$label)
summary(test)

dim(train) # 60000X720: The Computation time for such data set would be very high for such a large dataset
# Since the train data set is very large, we will have to sample some data out of actual for training of the model
# The rule of the thumb indicates 70% training and 30% Testing data set. For a test data set of 10000, around 20000-25000 dataset should be enough.
# We would check distribution of data for any abnormality.

set.seed(100)

# Check distribution for 70% train data

train_sampled_big <- sample.split(train$label,SplitRatio = 0.7)
train_set_big <- train[train_sampled_big,] # 42001 observations of 720 variables

# Lets see distribution for 30% train data
train_sampled_small <- sample.split(train$label,SplitRatio = 0.3)
train_set_small <- train[train_sampled_small,] # 18000 observations of 720 variables

plot1 <- ggplot(train_set_big) + 
  geom_bar(aes(x=as.factor(label)),fill="red",col="black") + labs(title="Distribution Set Big\nTrainSet = 42001 rows",x="Numbers",y="Count")
plot1

plot2 <- ggplot(train_set_small) + 
  geom_bar(aes(x=as.factor(label)),fill="red",col="black") +
  labs(title="Distribution Set Small\nTrainSet = 18000 rows",x="Numbers",y="Count")
plot2

grid.arrange(plot1,plot2, nrow=2) # The  distribution in both the plots seems to be similar. Therefore, we can afford to take the smaller data set

# Also check the distributon between the total Combined,train and test data set

plot3<- ggplot(combinedData_new, aes(x = label, y = (..count..)/sum(..count..))) + geom_bar()+ theme_get() +  labs(y = "Distribution", title = "Combined data set") + 
  scale_y_continuous(labels=scales::percent, limits = c(0 , 0.20)) +  geom_text(stat = "count",aes(label = scales:: percent((..count..)/sum(..count..)), vjust = -1))
plot3

plot4<- ggplot(train, aes(x = label, y = (..count..)/sum(..count..))) + geom_bar() + theme_get() +  labs(y = "Distribution", title = "Train Dataset") + 
  scale_y_continuous(labels=scales::percent, limits = c(0 , 0.20)) +  geom_text(stat = "count", aes(label = scales:: percent((..count..)/sum(..count..)), vjust = -1))


plot5<- ggplot(test, aes(x = label, y = (..count..)/sum(..count..))) + geom_bar() + theme_get() +  labs(y = "Distribution", title = "Test Dataset") + 
  scale_y_continuous(labels=scales::percent, limits = c(0 , 0.20)) +  geom_text(stat = "count", aes(label = scales:: percent((..count..)/sum(..count..)), vjust = -1))


grid.arrange(plot3, plot4, plot5, nrow = 3) # the frequency distribution seems to be similar

# Assigning the smallar data set as train data for reducting computational time
train_set <- train_set_small
test_set <- test

# However, with this 18000 observations of 720 variables itself, it is taking very long computational time. Therefore I am extracting only 5000 train and 1500 test samples for modelling.
# Taking 5000 data sets from train data view high computational time
train_sample_random <- sample(1: nrow(train_set_small),5000) 
train_set <- train_set_small[train_sample_random, ]

# Taking 1500 data sets from test data view high computational time
test_sample_random <- sample(1: nrow(test),1500) 
test_set <- test[test_sample_random, ]

################################################################################################
###    5. Model Preparation  ###
################################################################################################


################################################################################################
###     (A) LINEAR SVM      ###
################################################################################################
# Building a standard SVM model first and checking the performance #
SVM_linear_model_1 <- ksvm(label~.,
                           data=train_set,
                           scale=FALSE,
                           kernel="vanilladot", C=1)
SVM_linear_model_1
# SV type: C-svc  (classification) 
# parameter : cost C = 1 
# Linear (vanilla) kernel function. 
# Number of Support Vectors : 1678 

# Checking the training accuracy for model 1 #
eval_train_linear_model_1 <- predict(SVM_linear_model_1,train_set)
confusionMatrix(eval_train_linear_model_1,train_set$label)
##  Accuracy : 1          
#  95% CI : (0.9993, 1)
#  No Information Rate : 0.113   
#  P-Value [Acc > NIR] : < 2.2e-16  

# Checking test accuracy for model 1
eval_test_linear_model_1 <- predict(SVM_linear_model_1,test_set)
confusionMatrix(eval_test_linear_model_1,test_set$label)
## Accuracy : 0.9187     
# 95% CI :  (0.8965, 0.9259)
# No Information Rate : 0.112        
# P-Value [Acc > NIR] : < 2.2e-16 



## Linear kernel using stricter C(C=10)
SVM_linear_model_2 <- ksvm(label~.,
                           data=train_set,
                           scale=FALSE,
                           kernel="vanilladot",C=10)
SVM_linear_model_2
# SV type: C-svc  (classification) 
# parameter : cost C = 10
# Linear (vanilla) kernel function. 
# Number of Support Vectors : 1678

# Checking the training accuracy for model 2 
eval_train_linear_model_2 <- predict(SVM_linear_model_2,train_set)
confusionMatrix(eval_train_linear_model_2,train_set$label)
##  Accuracy : 1          
#  95% CI : (0.9998, 1)
#  No Information Rate : 0.113   
#  P-Value [Acc > NIR] : < 2.2e-16 

# Checking test accuracy for model 2
eval_test_linear_model_2 <- predict(SVM_linear_model_2,test_set)
confusionMatrix(eval_test_linear_model_2,test_set$label)
## Accuracy : 0.9187  # Model performance has remained the same wit c=10         
# 95% CI :  (0.9037, 0.932)
# No Information Rate : 0.112         
# P-Value [Acc > NIR] : < 2.2e-16   

##  Remarks: With stricker C also, the Test accuracy remains the same 91.8%

## Using cross validation to optimise C in Liear Model

grid_linear <- expand.grid(C= c(0.001, 0.1 ,1 ,10 ,100)) # defining range of C
fiting_train_linear <- train(label ~ ., data = train_set, metric = "Accuracy", method = "svmLinear",
                    tuneGrid = grid_linear, preProcess = NULL,
                    trControl = trainControl(method = "cv", number = 5))
fiting_train_linear
plot(fiting_train_linear)
#    C      Accuracy  Kappa    
#   1e-03  0.9067997  0.8963859
#   1e-01  0.9067997  0.8963859
#   1e+00  0.9067997  0.8963859
#   1e+01  0.9067997  0.8963859
#   1e+02  0.9067997  0.8963859
# Accuracy was used to select the optimal model using the largest value. The final value used for the model was C = 0.001.
# Max Accuracy 90.6% % with C= 0.001. Higher values of C would overfit and lower values would give simple models.

eval_cv_linear <- predict(fiting_train_linear, newdata = test_set)
confusionMatrix(eval_cv_linear, test_set$label)
# Accuracy : 0.9187  ## Accuracy remains by in large the same, maybe slightly improved(91.8%)
# 95% CI : (0.9037, 0.932)
# No Information Rate : 0.112     
# P-Value [Acc > NIR] : < 2.2e-16       

## Remarks: There is large gap between training accuracy and test accuracy data in the linear model

################################################################################################
###   (B) NON-LINEAR (RBF - Kernel) SVM ###
################################################################################################
##  building a RBF kernel model with default parameters and checking the performance
SVM_non_linear_RBF_model_1 <- ksvm(label~.,
                                   data=train_set,
                                   scale=FALSE,
                                   kernel="rbfdot")
SVM_non_linear_RBF_model_1
# SV type: C-svc  (classification) 
# parameter : cost C = 1 
# Gaussian Radial Basis kernel function. 
# Hyperparameter : sigma =  1.63257331025707e-07 
# Number of Support Vectors : 2353

# Lets check the training accuracy #
eval_train_non_linear_model_1 <- predict(SVM_non_linear_RBF_model_1,train_set)
confusionMatrix(eval_train_non_linear_model_1,train_set$label)
# Accuracy : 0.9818     
# 95% CI : (0.9777, 0.9853)
# No Information Rate : 0.113        
# P-Value [Acc > NIR] : < 2.2e-16      
# Lets Check test accuracy #

eval_test_non_linear_RBF_model_1 <- predict(SVM_non_linear_RBF_model_1,test_set)
confusionMatrix(eval_test_non_linear_RBF_model_1,test_set$label)
# Accuracy : 0.9467       
# 95% CI :  (0.9341, 0.9575)
# No Information Rate : 0.112       
# P-Value [Acc > NIR] : < 2.2e-16   

# Remarks:  The RBF model is performing better than Linear Model(Accuracy 94.67 for test data as against 91.8% in optimised linear model)
# Remarks:  Also, the gap is smaller in case of RBF between trainning and test data as compared to linear model

## Using cross validation to optimisethe RBF Model

##  Model Evaluation - Cross Validation 

# We will doing cross validation for RBF Model, with following login
# Cross Validation folds = 2
# The Sigma built by default RBF model was sigma =  1.63257331025707e-07. We would substitute the same range of data
# Range of sigma = 00.63e-7,1.63e-7,2.63e-7
# Range of C = 1,2,3
## Using cross validation to optimise C and sigma

set.seed(80)
grid <- expand.grid(.sigma = c(0.63e-7,1.63e-7,2.63e-7),.C=c(1,2,3))
SVM_non_linear_RBF_CV <- train(label ~ ., data = train_set, metric = "Accuracy", method = "svmRadial",tuneGrid = grid,
                  trControl = trainControl(method = "cv", number = 2,verboseIter=TRUE), preProcess = NULL)
SVM_non_linear_RBF_CV
#   Support Vector Machines with Radial Basis Function Kernel 
#   5000 samples
#   719 predictor
#   10 classes: '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' 
#   No pre-processing
#   Resampling: Cross-Validated (2 fold) 
#   Summary of sample sizes: 2500, 2500 
#   Resampling results across tuning parameters:
#   sigma     C  Accuracy  Kappa    
#   6.30e-08  1  0.9154    0.9059413
#   6.30e-08  2  0.9216    0.9128378
#   6.30e-08  3  0.9246    0.9161747
#   1.63e-07  1  0.9330    0.9255133
#   1.63e-07  2  0.9382    0.9312965
#   1.63e-07  3  0.9418    0.9352988
#   2.63e-07  1  0.9410    0.9344091
#   2.63e-07  2  0.9482    0.9424134
#   2.63e-07  3  0.9476    0.9417461
#   Accuracy was used to select the optimal model using the largest value.
#   The final values used for the model were sigma = 2.63e-07 and C = 2. Accuracy=94.82%
SVM_non_linear_RBF_model_final <- ksvm(label~.,
                                       data=train_set,
                                       kernel="rbfdot",
                                       scale=FALSE,
                                       C=2,
                                       kpar=list(sigma=2.63e-07))
SVM_non_linear_RBF_model_final
# SV type: C-svc  (classification) 
# parameter : cost C = 2 
# Gaussian Radial Basis kernel function. 
# Hyperparameter : sigma =  2.63e-07 
# Number of Support Vectors : 2508 

# Lets check training accuracy #
eval_train_non_linear_RBF_model_final <- predict(SVM_non_linear_RBF_model_final,train_set)
confusionMatrix(eval_train_non_linear_RBF_model_final,train_set$label)
#   Accuracy : 0.998          
#   95% CI : (0.9963, 0.999)
#   No Information Rate : 0.113          
#   P-Value [Acc > NIR] : < 2.2e-16      

# Lets check the test accuracy #
eval_test_non_linear_RBF_model_final <- predict(SVM_non_linear_RBF_model_final,test_set)
confusionMatrix(eval_test_non_linear_RBF_model_final,test_set$label)
# Accuracy : 0.956           
# 95% CI : (0.9444, 0.9658)
# No Information Rate : 0.112           
# P-Value [Acc > NIR] : < 2.2e-16    

# Remarks: There is  less difference between train and test accuracy. Therefore -no over-fitting
# Remarks: Value of hyper parameter sigma is very less  sigma = 2.63e-07- No Non-linearity in the data set. 
# Remarks: Cross validation for RBF kernel, indicates that  maximum accuracy can be seen with sigma = 2.63e-07 and C = 2. Accuracy=95.6%, the train accuracy is very good.
# Remarks: The test accuracy is comparable with train accuracy, eliminating over fitting.
# Remarks: Specificity and sensitivity across different levels  is very good for both train and test.

################################################################################################
###   (C) Polynomial Kernel 
################################################################################################

## Polynomial kernel with degree 2, default scale and offset
SVM_poly_model <- ksvm(label ~ ., data = train_set, kernel = "polydot", scaled = FALSE, C = 1, 
                    kpar = list(degree = 2, scale = 1, offset = 1))
SVM_poly_model
#   Support Vector Machine object of class "ksvm" 
#   SV type: C-svc  (classification) 
#   parameter : cost C = 1 
#   Polynomial kernel function. 
#   Hyperparameters : degree =  2  scale =  1  offset =  1 
#   Number of Support Vectors : 1712 

eval_train_SVM_poly_model<- predict(SVM_poly_model, newdata =train_set )
confusionMatrix(eval_train_SVM_poly_model, train_set$label)
#   Accuracy : 1          
#   95% CI : (0.9993, 1)
#   No Information Rate : 0.113      
#   P-Value [Acc > NIR] : < 2.2e-16  

eval_test_SVM_poly_model<- predict(SVM_poly_model, newdata =test_set )
confusionMatrix(eval_test_SVM_poly_model, test_set$label)
#   Accuracy : 0.95            
#   95% CI : (0.9377, 0.9605)
#   No Information Rate : 0.112           
#   P-Value [Acc > NIR] : < 2.2e-16       

# Remarks: Good accuracy of 95%. Almost, Similar performance to radial kernel
# Remarks: High Sensitivities and specificities


## Polynomial kernel with varied scale
SVM_poly_model_variedscale <- ksvm(label ~ ., data = train_set, kernel = "polydot", scaled = FALSE, C = 1, 
                    kpar = list(degree = 2, scale = -2, offset = 1))
SVM_poly_model_variedscale
#   SV type: C-svc  (classification) 
#   parameter : cost C = 1 
#   Polynomial kernel function. 
#   Hyperparameters : degree =  2  scale =  -2  offset =  1 
#   Number of Support Vectors : 1712 

eval_SVM_poly_model_variedscale<- predict(SVM_poly_model_variedscale, newdata = test_set)
confusionMatrix(eval_SVM_poly_model_variedscale, test_set$label)
#   Accuracy : 0.95            
#   95% CI : (0.9377, 0.9605)
#   No Information Rate : 0.112           
#   P-Value [Acc > NIR] : < 2.2e-16  

# Remarks: Accuracy of 95%. Almost, Similar as in previous case.

## Polynomial kernel with varied offset
SVM_poly_model_vaiedoffset <- ksvm(label ~ ., data = train_set, kernel = "polydot", scaled = FALSE, C = 1, 
                    kpar = list(degree = 2, scale = 1, offset = 10))
SVM_poly_model_vaiedoffset
#   parameter : cost C = 1 
#   Polynomial kernel function. 
#   Hyperparameters : degree =  2  scale =  1  offset =  10 
#   Number of Support Vectors : 1712 

eval_SVM_poly_model_vaiedoffset<- predict(SVM_poly_model_vaiedoffset, newdata = test_set)
confusionMatrix(eval_SVM_poly_model_vaiedoffset, test_set$label)
#   Accuracy : 0.95            
#   95% CI : (0.9377, 0.9605)
#   No Information Rate : 0.112           
#   P-Value [Acc > NIR] : < 2.2e-16     

# Remarks: Accuracy of 95%. Almost, Similar as in previous case.

## Polynomial kernel with higher C
SVM_poly_model_highC <- ksvm(label ~ ., data = train_set, kernel = "polydot", scaled = FALSE, C = 3, 
                    kpar = list(degree = 2, scale = 1, offset = 1))
SVM_poly_model_highC
#SV type: C-svc  (classification) 
#parameter : cost C = 3 
#Polynomial kernel function. 
#Hyperparameters : degree =  2  scale =  1  offset =  1 
#Number of Support Vectors : 1712 

eval_SVM_poly_model_highC <- predict(SVM_poly_model_highC, newdata = test_set)
confusionMatrix(eval_SVM_poly_model_highC, test_set$label)
#   Accuracy : 0.95            
#   95% CI : (0.9377, 0.9605)
#   No Information Rate : 0.112           
#   P-Value [Acc > NIR] : < 2.2e-16      

## Remarks: Accuracy of 95%. Almost, Similar as in previous case.

## Grid search to optimise hyperparameters
poly_grid = expand.grid(C= c(0.01, 0.1, 1, 10), degree = c(1, 2, 3, 4), 
                        scale = c(-10, -1, 1, 10))
SVM_poly_model <- train(label ~ ., data = train_set, metric = "Accuracy", method = "svmPoly",tuneGrid = poly_grid ,
                  trControl = trainControl(method = "cv", number = 2), preProcess = NULL)
SVM_poly_model
#5000 samples
#719 predictor
#10 classes: '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' 
#No pre-processing
#Resampling: Cross-Validated (2 fold) 
#Summary of sample sizes: 2501, 2499 
#Resampling results across tuning parameters:
#  C      degree  scale  Accuracy    Kappa     
#0.01  1       -10    0.00180008  -0.1212737
#0.01  1        -1    0.00180008  -0.1212737
#0.01  1         1    0.89959982   0.8883749
#0.01  1        10    0.89959982   0.8883749
#0.01  2       -10    0.93840079   0.9315157
#0.01  2        -1    0.93840079   0.9315157
#0.01  2         1    0.93840079   0.9315157
#0.01  2        10    0.93840079   0.9315157
#0.01  3       -10    0.00180008  -0.1203377
#0.01  3        -1    0.00180008  -0.1203377
#0.01  3         1    0.92800111   0.9199456
#0.01  3        10    0.92800111   0.9199456
#0.01  4       -10    0.90240222   0.8914362
#0.01  4        -1    0.90240222   0.8914362
#0.01  4         1    0.90240222   0.8914362
#0.01  4        10    0.90240222   0.8914362
#0.10  1       -10    0.00180008  -0.1212737
#0.10  1        -1    0.00180008  -0.1212737
#0.10  1         1    0.89959982   0.8883749
#0.10  1        10    0.89959982   0.8883749
#0.10  2       -10    0.93840079   0.9315157
#0.10  2        -1    0.93840079   0.9315157
#0.10  2         1    0.93840079   0.9315157
#0.10  2        10    0.93840079   0.9315157
#0.10  3       -10    0.00180008  -0.1203377
#0.10  3        -1    0.00180008  -0.1203377
#0.10  3         1    0.92800111   0.9199456
#0.10  3        10    0.92800111   0.9199456
#0.10  4       -10    0.90240222   0.8914362
#0.10  4        -1    0.90240222   0.8914362
#0.10  4         1    0.90240222   0.8914362
#0.10  4        10    0.90240222   0.8914362
#1.00  1       -10    0.00180008  -0.1212737
#1.00  1        -1    0.00180008  -0.1212737
#1.00  1         1    0.89959982   0.8883749
#1.00  1        10    0.89959982   0.8883749
#1.00  2       -10    0.93840079   0.9315157
#1.00  2        -1    0.93840079   0.9315157
#1.00  2         1    0.93840079   0.9315157
#1.00  2        10    0.93840079   0.9315157
#1.00  3       -10    0.00180008  -0.1203377
#1.00  3        -1    0.00180008  -0.1203377
#1.00  3         1    0.92800111   0.9199456
#1.00  3        10    0.92800111   0.9199456
#1.00  4       -10    0.90240222   0.8914362
#1.00  4        -1    0.90240222   0.8914362
#1.00  4         1    0.90240222   0.8914362
#1.00  4        10    0.90240222   0.8914362
#10.00  1       -10    0.00180008  -0.1212737
#10.00  1        -1    0.00180008  -0.1212737
#10.00  1         1    0.89959982   0.8883749
#10.00  1        10    0.89959982   0.8883749
#10.00  2       -10    0.93840079   0.9315157
#10.00  2        -1    0.93840079   0.9315157
#10.00  2         1    0.93840079   0.9315157
#10.00  2        10    0.93840079   0.9315157
#10.00  3       -10    0.00180008  -0.1203377
#10.00  3        -1    0.00180008  -0.1203377
#10.00  3         1    0.92800111   0.9199456
#10.00  3        10    0.92800111   0.9199456
#10.00  4       -10    0.90240222   0.8914362
#10.00  4        -1    0.90240222   0.8914362
#10.00  4         1    0.90240222   0.8914362
#10.00  4        10    0.90240222   0.8914362
#Accuracy was used to select the optimal model using the largest value.
#Remarks: The final values used for the model were degree = 2, scale = -10 and C = 0.01  Accuracy= 0.9384007901.

plot(SVM_poly_model)

eval_cv_SVM_poly_model <- predict(SVM_poly_model , newdata = test_set)
confusionMatrix(eval_cv_SVM_poly_model, test_set$label)
#   Accuracy : 0.95            
#   95% CI : (0.9377, 0.9605)
#   No Information Rate : 0.112           
#   P-Value [Acc > NIR] : < 2.2e-16    

# Remarks: Best model obtained for C = 0.01, degree = 2, scale = -10. degrees higher than 2 are overfitting
# Remarks: Accuracy of 93.84%, sensitivities and specificities are high

## Implementing optmised polynomial model 
SVM_poly_model_optmodel <- ksvm(label ~ ., data = train_set, kernel = "polydot", scaled = FALSE, C = 0.01, 
                    kpar = list(degree = 2, scale = -10, offset = 10))
SVM_poly_model_optmodel
#SV type: C-svc  (classification) 
#parameter : cost C = 0.01 
#Polynomial kernel function. 
#Hyperparameters : degree =  2  scale =  -10  offset =  10 
#Number of Support Vectors : 1712 

eval_SVM_poly_model_optmodel <- predict(SVM_poly_model_optmodel, newdata = test_set)
confusionMatrix(eval_SVM_poly_model_optmodel, test_set$label)
#Accuracy : 0.95            
#95% CI : (0.9377, 0.9605)
#No Information Rate : 0.112           
#P-Value [Acc > NIR] : < 2.2e-16   

# Remarks: Offset is not making any difference in the results.
# Remarks: Best accuracy of polynomial kernels 95%



################################################################################################
### Final Remarks
################################################################################################
# Remarks: Reduced training and testing data set of 5000 and 1500 obs.(extracted using random sampling) has been used to avoid high computational time.
# Remarks: The final model for maximum accuracy in predicting digits is of SVM_non_linear_RBF_model_final using RBF kernel
# Remarks: The maximum accuracy can be seen with sigma = 2.63e-07 and C = 2. Accuracy=95.6%. The train accuracy is also very good
# Remarks: Accuracy = 95.6%
# Remarks: High Sensitivites and Specificities 
# Remarks: Polynomial kernel (C = 0.01, degree = 2, scale = -10, offset = -10)  is also equally good with accuracy of 95%. Its run time is better than that of radial kernel



