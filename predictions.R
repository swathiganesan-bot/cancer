library(rpart)
library(rpart.plot)
library(randomForest)

data <- data.frame(
  Age = c(45,50,60,38,55),
  Tumor_Size = c(3.2,2.1,4.5,1.8,3.9),
  Symptoms_Score = c(7,5,8,4,7),
  Diagnosis = c("Malignant","Benign","Malignant","Benign","Malignant")
)

data$Diagnosis <- as.factor(data$Diagnosis)

trainData <- data[1:3, ]
testData  <- data[4:5, ]

dt_model <- rpart(Diagnosis ~ Age + Tumor_Size + Symptoms_Score,
                  data = trainData,
                  method = "class")

rpart.plot(dt_model)

dt_pred <- predict(dt_model, testData, type = "class")

cat("Decision Tree Result:\n")
print(table(Predicted = dt_pred, Actual = testData$Diagnosis))

rf_model <- randomForest(Diagnosis ~ Age + Tumor_Size + Symptoms_Score,
                         data = trainData,
                         ntree = 100)

rf_pred <- predict(rf_model, testData)

cat("Random Forest Result:\n")
print(table(Predicted = rf_pred, Actual = testData$Diagnosis))