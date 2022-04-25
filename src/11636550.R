library(dplyr)
library(forecast)
library(caret)

data = read.csv("~/Downloads/assn-4/assets/data/daily11636550.csv", header = F)
data$V1 = as.Date(data$V1, format="%Y-%m-%d")

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

tdata = read.csv("~/Downloads/assn-4/assets/data/transpose-daily11636550.csv", header = F)
head(tdata)

folds = trainControl(method = "cv", number = 10)

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

# NN
set.seed(42)
nn = train(x = tdata[-8],
               y = tdata$V8,
               method = "neuralnet",
               trControl = folds)
nn