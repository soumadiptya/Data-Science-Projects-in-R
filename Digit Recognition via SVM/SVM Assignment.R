############################ Digit Recognition using SVM #################################
# 1. Business Understanding
# 2. Data Understanding
# 3. Data Preparation
# 4. Model Building 
# 5 Hyperparameter tuning and cross validation

#####################################################################################

# 1. Business Understanding: 

#The objective is to identify handwritten digits from 0-9 using SVM and choose the best 
#predictive model that makes the best predictions on the test data set

#####################################################################################

#### 2. Data Understanding ####
# Note: Since nothing is mentioned in the assignment itself about the data, data understanding was
# obtained by googling about the dataset. Original Sources:-
# 1) Kaggle- https://www.kaggle.com/c/digit-recognizer/data
# 2) MNIST database- http://yann.lecun.com/exdb/mnist/
# Note- Kaggle has been only used to obtain an understanding of the dataset and attribute values.
# There is absolutely NO attempt to plagiarize any of the code.
# Data points:-
# a) The first column contains the digits 0-9. This is the target column and what we will try to predict.
# b) The columns 2:785 each contain a single pixel value. Higher values indicate darker pixels 
# while 0 indicates the particular pixel is empty. Pixel values are between 0 and 255 inclusive.
# c) We will try to use these pixel values to plot a few of the digits for a better data understanding.

#### 3. Data Preparation ####
# Read the dataset
setwd("H:/Career Development/Analytics Diploma/Predictive Analytics 2/SVMs/SVM Assignment")
train <- read.csv("mnist_train.csv", header = F)
test <- read.csv("mnist_test.csv", header = F)
View(train)
# Rename the first column to Labels in both train and test
colnames(train)[1] <- "Labels"
colnames(test)[1] <- "Labels"

# Rename the rest of the columns to pixel_1, pixel_2 etc.
colnames(train)[2:785] <-  paste("pixel", 1:784, sep = "_")
colnames(test)[2:785] <-  paste("pixel", 1:784, sep = "_")

# Take a subset of the first 10 values
top_10 <- train[1:10,]
top_10.lab <- top_10[,1] # Dataset labels
top_10.pix <- top_10[,-1] # Dataset pixel values

row.1 <- unlist(top_10.pix[1,]) # Take the first row of pixel values to plot it
# We need to convert row.1 to a matrix so that we can plot the pixel values
matrix1 <- t(matrix((1.0-row.1/256), nrow = 28))
rotate <- function(x) t(apply(x, 2, rev))

# install.packages("reshape2") ---- If needed
# install.packages("ggplot2") ---- if needed
library(reshape2)
library(ggplot2)
theme_raster <- theme(legend.position = "none", axis.title.x = element_blank(), 
                      axis.title.y = element_blank(), axis.text.x = element_blank(), 
                      axis.text.y = element_blank(), axis.ticks = element_blank())
p1 <- ggplot(melt(rotate(matrix1)), aes(Var1,Var2, fill=value)) + geom_raster() + theme_raster

# Do similarly for the other 9 values
row.2 <- unlist(top_10.pix[2,]) 
matrix2 <- t(matrix((1.0-row.2/256), nrow = 28))
p2 <- ggplot(melt(rotate(matrix2)), aes(Var1,Var2, fill=value)) + geom_raster() + theme_raster

row.3 <- unlist(top_10.pix[3,]) 
matrix3 <- t(matrix((1.0-row.3/256), nrow = 28))
p3 <- ggplot(melt(rotate(matrix3)), aes(Var1,Var2, fill=value)) + geom_raster() + theme_raster

row.4 <- unlist(top_10.pix[4,]) 
matrix4 <- t(matrix((1.0-row.4/256), nrow = 28))
p4 <- ggplot(melt(rotate(matrix4)), aes(Var1,Var2, fill=value)) + geom_raster() + theme_raster

row.5 <- unlist(top_10.pix[2,]) 
matrix5 <- t(matrix((1.0-row.2/256), nrow = 28))
p5 <- ggplot(melt(rotate(matrix2)), aes(Var1,Var2, fill=value)) + geom_raster() + theme_raster

row.6 <- unlist(top_10.pix[6,]) 
matrix6 <- t(matrix((1.0-row.6/256), nrow = 28))
p6 <- ggplot(melt(rotate(matrix6)), aes(Var1,Var2, fill=value)) + geom_raster() + theme_raster

row.7 <- unlist(top_10.pix[7,]) 
matrix7 <- t(matrix((1.0-row.7/256), nrow = 28))
p7 <- ggplot(melt(rotate(matrix7)), aes(Var1,Var2, fill=value)) + geom_raster() + theme_raster

row.8 <- unlist(top_10.pix[8,]) 
matrix8 <- t(matrix((1.0-row.8/256), nrow = 28))
p8 <- ggplot(melt(rotate(matrix8)), aes(Var1,Var2, fill=value)) + geom_raster() + theme_raster

row.9 <- unlist(top_10.pix[9,]) 
matrix9 <- t(matrix((1.0-row.9/256), nrow = 28))
p9 <- ggplot(melt(rotate(matrix9)), aes(Var1,Var2, fill=value)) + geom_raster() + theme_raster

row.10 <- unlist(top_10.pix[10,]) 
matrix10 <- t(matrix((1.0-row.10/256), nrow = 28))
p10 <- ggplot(melt(rotate(matrix10)), aes(Var1,Var2, fill=value)) + geom_raster() + theme_raster

library(gridExtra)
grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10, nrow = 2, ncol = 5)

# Thus we have got an understanding of how the pixel values translate to an image

# Check for NA values
sum(is.na(train))
# No NA's are present
# Check the structure of the data set
str(train)
# All atributes are numeric so 
# Convert the labels to factors
train$Labels <- as.factor(train$Labels)
test$Labels <- as.factor(test$Labels)

#### 4. Model Building ####
# Since train and test datasets are already separate we don't have to do any split
# Let us first apply SVM with different kernel types on the whole data set
# 1) Linear kernel with C=1
library(readr)
library(kernlab)
library(caret)
library(caTools)
# Split the train data set to get 10% of the data as it takes a huge time 
# to run the algorithm on the whole dataset
set.seed(100)
split_indices <- sample.split(train$Labels, SplitRatio = 0.2)
train_split <- train[split_indices,]
# p = as.vector(which(sapply(train_split[,-1], function(x) sum(x)==0)))
# p=p+1
# train_split <- train_split[,-p]
# This takes almost 10 minutes
linear_pred<- ksvm(Labels ~ ., data = train_split,scale = FALSE,C=1)
# Warnings are generated due to all values being 0 in few columns. This is not a problem.

# Using the model to make predictions
evaluate_1<- predict(linear_pred, test)
# Confusion Matrix - Finding accuracy, Sensitivity and specificity
conf <- confusionMatrix(evaluate_1, test$Labels)
acc <- conf$overall[1]
acc
sensitivity <- conf$byClass[1]
sensitivity
specificity <- conf$byClass[2]
specificity
# Accuracy    : 0.9521
# Sensitivity : 0.9877551         
# Specificity : 0.9876652
# Thus even with the vanilla kernel it gives a very good prediction and virtually no overfitting
# Let's see if it improves with Polynomial and RBF kernels

# With a polynomial kernel
poly_pred<- ksvm(Labels ~ ., data = train_split,scale = FALSE,kernel = "polydot")
# Using the model to make predictions
evaluate_2<- predict(poly_pred, test[,-1])
# Confusion Matrix - Finding accuracy, Sensitivity and specificity
conf <- confusionMatrix(evaluate_2, test$Labels)
acc <- conf$overall[1]
acc
sensitivity <- conf$byClass[1]
sensitivity
specificity <- conf$byClass[2]
specificity
# Accuracy    : 0.9164
# Sensitivity : 0.977551         
# Specificity : 0.9823789
# All metrics have actually decreased which indicates that a Linear kernel is actually better.
# However let us still try the RBF dot kernel
# With a RBF kernel
RBF_pred <- ksvm(Labels~., data = train_split,scale = FALSE, kernel = "rbfdot")
# Using the model to make predictions
evaluate_3<- predict(RBF_pred, test[,-1])
# Confusion Matrix - Finding accuracy, Sensitivity and specificity
conf <- confusionMatrix(evaluate_3, test$Labels)
acc <- conf$overall[1]
acc
sensitivity <- conf$byClass[1]
sensitivity
specificity <- conf$byClass[2]
specificity
specificity
# Accuracy    : 0.9523
# Sensitivity : 0.9877551         
# Specificity : 0.9876652
# All metrics obtained are almost same as those with the linear kernel. So we will use linear kernel
# with cross validation to find the best Hyper Parameters (C)

#####################################################################
# Hyperparameter tuning and Cross Validation  - Linear - SVM 
######################################################################
# We will perform 5 fold cross-validation
trainControl <- trainControl(method = "cv", number = 3)
metric <- "Accuracy"
set.seed(100)
# making a grid of C values. 
grid <- expand.grid(C=seq(1, 5, by=2))
grid
fit.svm <- caret::train(Labels~., data = train_split, method = "svmLinear", metric = metric,
                 tuneGrid=grid, trControl=trainControl)
plot(fit.svm)
print(fit.svm)

evaluate_linear_test<- predict(fit.svm, test)
confusionMatrix(evaluate_linear_test, test$Labels)
test <- read.csv("test.csv", header = T)

