library(arules)
library(plyr)
library(dplyr)
library(caret)
library(ggplot2)
library(GGally)
library(patchwork)


df <- read.csv("~/Downloads/assn-4/assets/data/transpose-11743201.csv", header = FALSE, 
               sep = ',')
train <- df[1:31,]

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

set.seed(42)
myCvControl <- trainControl(method = "cv",
                            number=10, summaryFunction = mapeSummary)

set.seed(42)
glm <- train(V8 ~ .,
             data = train,
             method = "glm",
             preProc = c("center", "scale"),
             tuneLength = 10,
             trControl = myCvControl)

glm

set.seed(42)
svmLinear <- train(V8 ~ .,
                   data = train,
                   method = "svmLinear",
                   tuneLength = 10,
                   trControl = myCvControl)
svmLinear

set.seed(42)
svmPoly <- train(V8 ~ .,
                 data = train,
                 method = "svmPoly",
                 trControl = myCvControl)
svmPoly


set.seed(42)
svmRadial <- train(V8 ~ .,
                   data = train,
                   method = "svmRadial",
                   trControl = myCvControl)
svmRadial


set.seed(42)
nn <- train(V8 ~ .,
            data = train,
            method = "neuralnet",
            trControl = myCvControl)
nn


ts <- read.csv("~/Downloads/assn-4/assets/data/11743201.csv", header = FALSE, 
               sep = ',')

ts$V1 <- as.Date(ts$V1, format = "%Y-%m-%d")

p1 = ts %>%
  ggplot(aes(x=V1, y=V3)) +
  geom_line() +
  labs(x = "Date", y = "Quantity",title = "Trend in Quantity")



p2 = ts %>%
  ggplot(aes(x=V3)) +
  geom_histogram(aes(y=..density..), colour="black", fill="gray") +
  geom_density() +
  labs(x = "Quanity", y = "Frequency",title = "Histogram of Quantity")

p1 / p2
