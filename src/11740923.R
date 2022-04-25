library(dplyr)
library(forecast)
library(caret)
library(ggplot2)
library(GGally)
library(patchwork)

data = read.csv("~/Downloads/assn-4/assets/data/11740923.csv", header = F)
data$V1 = as.Date(data$V1, format="%Y-%m-%d")

head(data$V1)
dim(data)

p1 = data %>%
      ggplot(aes(x=V1, y=V3)) + 
      geom_line() +
      labs(x = "Date", y = "Quantity",title = "Trend in Quantity") 

p2 = data %>%
      ggplot(aes(x=V3)) +
      geom_histogram(aes(y=..density..), colour="black", fill="gray") +
      geom_density() + 
      labs(x = "Quanity", y = "Frequency",title = "Histogram of Quantity") 

p1 / p2

mape <- function(actual, predicted){
  mean(abs((actual - predicted)/actual))
}
mapeSummary <- function (data,
                         lev = NULL,
                         model = NULL) {
  c(MAPE = mape(data$obs, data$pred),
    RMSE = sqrt(mean((data$obs - data$pred)^2)),
    MAE = mean(abs(data$obs - data$pred)))
}


tdata = read.csv("~/Downloads/assn-4/assets/data/transpose-11740923.csv", header = F)
head(tdata)

tdata = tdata[tdata$V8 > 3, ]

folds = trainControl(method = "cv", number = 10, summaryFunction = mapeSummary)

# GLM
set.seed(42)
glmfit = train(x = tdata[-8],
               y = tdata$V8,
               method = "glm",
               trControl = folds)
glmfit

# SVM
set.seed(42)
svmfit = train(x = tdata[-8],
               y = tdata$V8,
               method = "svmLinear",
               trControl = folds)
svmfit

set.seed(42)
svmfit = train(x = tdata[-8],
               y = tdata$V8,
               method = "svmPoly",
               trControl = folds)
svmfit

set.seed(42)
svmfit = train(x = tdata[-8],
               y = tdata$V8,
               method = "svmRadial",
               trControl = folds)
svmfit

# NN
set.seed(42)
nn = train(x = tdata[-8],
           y = tdata$V8,
           method = "neuralnet",
           trControl = folds)
nn

