
library(tidyverse)
library(plotrix)
library(cluster)
library(MASS)
library(pROC)
library(caret)
library(reshape2)


# read the data 
data<-read.csv("airlinesData120.csv")


# dimensions of the data 
dim(data)

total.deleted.values<-0

zero.col<-c("Inflight.wifi.service","Ease.of.Online.booking","Food.and.drink","Online.boarding","Inflight.entertainment","Leg.room.service", "Cleanliness")

#check for the zero values : 



colSums(data[zero.col]==0) 






# delete zero values : 

preprocessed.data<- filter (data, Inflight.wifi.service!= 0 & 
                              Departure.Arrival.time.convenient!=0 &
                              Ease.of.Online.booking!= 0 & 
                              Food.and.drink!= 0 & 
                              Online.boarding!= 0 & 
                              Inflight.entertainment!= 0 & 
                              Leg.room.service!= 0 &
                              Cleanliness!= 0 )


# check if zero values are deleted :

colSums(preprocessed.data[zero.col]==0) 


total.deleted.values<- total.deleted.values +sum(colSums(data[zero.col]==0)) 

total.deleted.values

total.deleted.values<- total.deleted.values +sum(is.na(data))  

total.deleted.values

# Delete NA from Arrival.Delay.in.minutes
preprocessed.data<- na.omit(preprocessed.data)#


# check if Na ARE deleted : 

#sum(is.na(preprocessed.data)) 




preprocessed.data.b<-preprocessed.data  

preprocessed.data$satisfaction <- 1*(preprocessed.data$satisfaction=="satisfied")


preprocessed.data$satisfaction<- as.integer(preprocessed.data$satisfaction)




head(preprocessed.data)



preprocessed.data$Gender<- as.factor(preprocessed.data$Gender)
preprocessed.data$Customer.Type<- as.factor(preprocessed.data$Customer.Type)
preprocessed.data$Type.of.Travel<- as.factor(preprocessed.data$Type.of.Travel)
preprocessed.data$Class<- as.factor(preprocessed.data$Class)





#preprocessed.data.DissimMatrix <- daisy(preprocessed.data, metric="gower")
DissimMatrix <- daisy(subset(preprocessed.data, select = -satisfaction), metric="gower")
View(DissimMatrix )


data.MDS <- cmdscale(DissimMatrix, k=2)
colnames(data.MDS) <- c("D1","D2")
plot(data.MDS, pch="*")
data.MDS2<-cbind(data.MDS,preprocessed.data.b$satisfaction)
data.MDS2<-as.data.frame(data.MDS2)
colnames(data.MDS2) <- c("D1","D2","satisfaction")
data.MDS2$satisfaction <- 1*(data.MDS2$satisfaction=="satisfied")








#########""
data.MDS2$satisfaction<-as.integer(data.MDS2$satisfaction)




data.MDS2$satisfaction<-as.factor(data.MDS2$satisfaction)
plot(data.MDS2$D1, data.MDS2$D2,col=data.MDS2$satisfaction)

data.MDS2$satisfaction <- 1*(data.MDS2$satisfaction=="satisfied")





data.MDS2$satisfaction<-as.factor(data.MDS2$satisfaction)

View(data.MDS2)
# Split the data into training and testing sets
set.seed(123)
# Define the control for cross-validation
control <- trainControl(method = "cv", number = 5,classProbs = TRUE,
                        summaryFunction = twoClassSummary)

trainIndex <- createDataPartition(data.MDS2$satisfaction, p = 0.8, list = FALSE)
trainData <- data.MDS2[trainIndex, ]
testData <- data.MDS2[-trainIndex, ]

# Logistic Regression
logisticModel <- train(satisfaction ~ D1+D2, data = trainData, method = "glm", family = "binomial" , trControl = control)
summary(logisticModel)
logisticPredictions <- predict(logisticModel, newdata = testData)
logisticAccuracy <- confusionMatrix(logisticPredictions, testData$satisfaction)$overall['Accuracy']

# K-nearest Neighbors (knn)
parameters <- data.frame(k = c(5, 7, 9)) 
names(trainData)

levels(trainData$satisfaction)


knnModel <- train(satisfaction ~ D1+D2, data = trainData, method = "knn", trControl = control ,tuneGrid = parameters)
knnModel$bestTune
knnPredictions <- predict(knnModel, newdata = testData)
knnAccuracy <- confusionMatrix(knnPredictions, testData$satisfaction)$overall['Accuracy']

# Decision Tree
treeModel <- train(satisfaction ~ ., data = trainData, method = "rpart", trControl = control)
treePredictions <- predict(treeModel, newdata = testData)
treeAccuracy <- confusionMatrix(treePredictions, testData$satisfaction)$overall['Accuracy']

# Compare the model accuracies
accuracyTable1 <- data.frame(Model = c("Logistic Regression", "k-Nearest Neighbors", "Decision Tree"),
                             Accuracy = c(logisticAccuracy, knnAccuracy, treeAccuracy))
accuracyTable1



# Calculate sensitivity (recall) and specificity
logisticSensitivity <- confusionMatrix(logisticPredictions, testData$satisfaction)$byClass['Sensitivity']
logisticSpecificity <- confusionMatrix(logisticPredictions, testData$satisfaction)$byClass['Specificity']
knnSensitivity <- confusionMatrix(knnPredictions, testData$satisfaction)$byClass['Sensitivity']
knnSpecificity <- confusionMatrix(knnPredictions, testData$satisfaction)$byClass['Specificity']
treeSensitivity <- confusionMatrix(treePredictions, testData$satisfaction)$byClass['Sensitivity']
treeSpecificity <- confusionMatrix(treePredictions, testData$satisfaction)$byClass['Specificity']

# Calculate the positive predictive value (PPV) and negative predictive value (NPV)
logisticPPV <- confusionMatrix(logisticPredictions, testData$satisfaction)$byClass['Pos Pred Value']
logisticNPV <- confusionMatrix(logisticPredictions, testData$satisfaction)$byClass['Neg Pred Value']
knnPPV <- confusionMatrix(knnPredictions, testData$satisfaction)$byClass['Pos Pred Value']
knnNPV <- confusionMatrix(knnPredictions, testData$satisfaction)$byClass['Neg Pred Value']
treePPV <- confusionMatrix(treePredictions, testData$satisfaction)$byClass['Pos Pred Value']
treeNPV <- confusionMatrix(treePredictions, testData$satisfaction)$byClass['Neg Pred Value']




# Create a dataframe for sensitivity, specificity, PPV, and NPV
performanceData <- data.frame(Model = rep(c("Logistic Regression", "k-Nearest Neighbors", "Decision Tree"), each = 2),
                              Metric = rep(c("Sensitivity", "Specificity", "PPV", "NPV"), times = 3),
                              Value = c(logisticSensitivity, logisticSpecificity, logisticPPV, logisticNPV,
                                        knnSensitivity, knnSpecificity, knnPPV, knnNPV,
                                        treeSensitivity, treeSpecificity, treePPV, treeNPV))

# Plotting the trade-off
ggplot(performanceData, aes(x = Metric, y = Value, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Trade-off between Correctly Identifying Satisfaction and Dissatisfaction",
       x = "Metric", y = "Value") +
  scale_fill_manual(values = c("#FFA500", "#FFD700", "#FF6347")) +
  theme_minimal()








# Calculate sensitivity and specificity
logisticSensitivity <- confusionMatrix(logisticPredictions, testData$satisfaction)$byClass['Sensitivity']
logisticSpecificity <- confusionMatrix(logisticPredictions, testData$satisfaction)$byClass['Specificity']
knnSensitivity <- confusionMatrix(knnPredictions, testData$satisfaction)$byClass['Sensitivity']
knnSpecificity <- confusionMatrix(knnPredictions, testData$satisfaction)$byClass['Specificity']
treeSensitivity <- confusionMatrix(treePredictions, testData$satisfaction)$byClass['Sensitivity']
treeSpecificity <- confusionMatrix(treePredictions, testData$satisfaction)$byClass['Specificity']

# Create a summary table of accuracy, sensitivity, and specificity
summaryTable.1 <- data.frame(Model = c("Logistic Regression", "k-Nearest Neighbors", "Decision Tree"),
                             Accuracy = c(logisticAccuracy, knnAccuracy, treeAccuracy),
                             Sensitivity = c(logisticSensitivity, knnSensitivity, treeSensitivity),
                             Specificity = c(logisticSpecificity, knnSpecificity, treeSpecificity))
print(summaryTable.1)




# Summary table
summaryTable_1 <- data.frame(
  Model = c("Logistic Regression", "k-Nearest Neighbors", "Decision Tree"),
  Accuracy = c(logisticAccuracy, knnAccuracy, treeAccuracy),
  Sensitivity = c(logisticSensitivity, knnSensitivity, treeSensitivity),
  Specificity = c(logisticSpecificity, knnSpecificity, treeSpecificity),
  PPV = c(logisticPPV, knnPPV, treePPV),
  NPV = c(logisticNPV, knnNPV, treeNPV)
)

summaryTable_1

#####################
# Calculate ROC curves and AUC

# ROC Curve
roc_obj_logistic <- roc(as.numeric(testData$satisfaction), as.numeric(logisticPredictions))
roc_obj_knn <- roc(as.numeric(testData$satisfaction), as.numeric(knnPredictions))
roc_obj_tree <- roc(as.numeric(testData$satisfaction), as.numeric(treePredictions))

# Plotting ROC curves
plot(roc_obj_logistic, col = "blue", print.auc=TRUE)
lines(roc_obj_knn, col = "red", print.auc=TRUE)
lines(roc_obj_tree, col = "green", print.auc=TRUE)
legend("bottomright", legend = c("Logistic Regression", "k-Nearest Neighbors", "Decision Tree"), 
       col = c("blue", "red", "green"), lwd = 2)

# Creating a summary table
summaryTable <- data.frame(Model = c("Logistic Regression(MDS)", "k-Nearest Neighbors(MDS)", "Decision Tree(MDS)"),
                           Accuracy = c(logisticAccuracy, knnAccuracy, treeAccuracy),
                           Sensitivity = c(logisticSensitivity, knnSensitivity, treeSensitivity),
                           Specificity = c(logisticSpecificity, knnSpecificity, treeSpecificity),
                           AUC = c(auc(roc_obj_logistic), auc(roc_obj_knn), auc(roc_obj_tree)))

summaryTable




############################################################################





trainIndex2 <- createDataPartition(preprocessed.data$satisfaction, p = 0.8, list = FALSE)
trainData2 <- preprocessed.data[trainIndex, ]
testData2 <- preprocessed.data[-trainIndex, ]

# Logistic Regression
logisticModel2 <- train(satisfaction ~  Customer.Type + Age + Type.of.Travel + 
                          Class +  Inflight.wifi.service + Departure.Arrival.time.convenient + 
                          Ease.of.Online.booking + Gate.location + Online.boarding + Seat.comfort + On.board.service + 
                          Leg.room.service + Baggage.handling + Checkin.service + Inflight.service + Cleanliness + 
                          Arrival.Delay.in.Minutes, data = trainData2, method = "glm", family = "binomial" , trControl = control)
summary(logisticModel2)
logisticPredictions2 <- predict(logisticModel2, newdata = testData2)
logisticAccuracy2 <- confusionMatrix(logisticPredictions2, testData2$satisfaction)$overall['Accuracy']

# K-nearest Neighbors (knn)
knnModel2 <- train(satisfaction ~  Customer.Type + Age + Type.of.Travel + 
                     Class +  Inflight.wifi.service + Departure.Arrival.time.convenient + 
                     Ease.of.Online.booking + Gate.location + Online.boarding + Seat.comfort + On.board.service + 
                     Leg.room.service + Baggage.handling + Checkin.service + Inflight.service + Cleanliness + 
                     Arrival.Delay.in.Minutes, data = trainData2, method = "knn", trControl = control)
knnModel2$bestTune
knnPredictions2 <- predict(knnModel2, newdata = testData2)
knnAccuracy2<- confusionMatrix(knnPredictions2, testData2$satisfaction)$overall['Accuracy']

# Decision Tree
treeModel2 <- train(satisfaction ~  Customer.Type + Age + Type.of.Travel + 
                      Class +  Inflight.wifi.service + Departure.Arrival.time.convenient + 
                      Ease.of.Online.booking + Gate.location + Online.boarding + Seat.comfort + On.board.service + 
                      Leg.room.service + Baggage.handling + Checkin.service + Inflight.service + Cleanliness + 
                      Arrival.Delay.in.Minutes, data = trainData2, method = "rpart", trControl = control)
treePredictions2 <- predict(treeModel2, newdata = testData2)
treeAccuracy2 <- confusionMatrix(treePredictions2, testData2$satisfaction)$overall['Accuracy']

# Compare the model accuracies
accuracyTable.2 <- data.frame(Model = c("Logistic Regression", "k-Nearest Neighbors", "Decision Tree"),
                              Accuracy = c(logisticAccuracy2, knnAccuracy2, treeAccuracy2))
accuracyTable.2



# Calculate sensitivity (recall) and specificity
logisticSensitivity2 <- confusionMatrix(logisticPredictions2, testData2$satisfaction)$byClass['Sensitivity']
logisticSpecificity2<- confusionMatrix(logisticPredictions2, testData2$satisfaction)$byClass['Specificity']
knnSensitivity2 <- confusionMatrix(knnPredictions2, testData2$satisfaction)$byClass['Sensitivity']
knnSpecificity2 <- confusionMatrix(knnPredictions2, testData2$satisfaction)$byClass['Specificity']
treeSensitivity2 <- confusionMatrix(treePredictions2, testData2$satisfaction)$byClass['Sensitivity']
treeSpecificity2 <- confusionMatrix(treePredictions2, testData2$satisfaction)$byClass['Specificity']

# Calculate the positive predictive value (PPV) and negative predictive value (NPV)
logisticPPV2 <- confusionMatrix(logisticPredictions2, testData2$satisfaction)$byClass['Pos Pred Value']
logisticNPV2 <- confusionMatrix(logisticPredictions2, testData2$satisfaction)$byClass['Neg Pred Value']
knnPPV2<- confusionMatrix(knnPredictions2, testData2$satisfaction)$byClass['Pos Pred Value']
knnNPV2 <- confusionMatrix(knnPredictions2, testData2$satisfaction)$byClass['Neg Pred Value']
treePPV2 <- confusionMatrix(treePredictions2, testData2$satisfaction)$byClass['Pos Pred Value']
treeNPV2 <- confusionMatrix(treePredictions2, testData2$satisfaction)$byClass['Neg Pred Value']



# Create a dataframe for sensitivity, specificity, PPV, and NPV
performanceData2 <- data.frame(Model = rep(c("Logistic Regression", "k-Nearest Neighbors", "Decision Tree"), each = 2),
                               Metric = rep(c("Sensitivity", "Specificity", "PPV", "NPV"), times = 3),
                               Value = c(logisticSensitivity2, logisticSpecificity2, logisticPPV2, logisticNPV2,
                                         knnSensitivity2, knnSpecificity2, knnPPV2, knnNPV2,
                                         treeSensitivity2, treeSpecificity2, treePPV2, treeNPV2))

# Plotting the trade-off
ggplot(performanceData2, aes(x = Metric, y = Value, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Trade-off between Correctly Identifying Satisfaction and Dissatisfaction",
       x = "Metric", y = "Value") +
  scale_fill_manual(values = c("#FFA500", "#FFD700", "#FF6347")) +
  theme_minimal()








# Calculate sensitivity and specificity
logisticSensitivity2 <- confusionMatrix(logisticPrediction2s, testData2$satisfaction)$byClass['Sensitivity']
logisticSpecificity2 <- confusionMatrix(logisticPredictions2, testData2$satisfaction)$byClass['Specificity']
knnSensitivity2 <- confusionMatrix(knnPredictions2, testData2$satisfaction)$byClass['Sensitivity']
knnSpecificity2 <- confusionMatrix(knnPredictions2, testData2$satisfaction)$byClass['Specificity']
treeSensitivity2 <- confusionMatrix(treePredictions2, testData2$satisfaction)$byClass['Sensitivity']
treeSpecificity2 <- confusionMatrix(treePredictions2, testData2$satisfaction)$byClass['Specificity']

# Create a summary table of accuracy, sensitivity, and specificity
summaryTable.2 <- data.frame(Model = c("Logistic Regression", "k-Nearest Neighbors", "Decision Tree"),
                             Accuracy = c(logisticAccuracy2, knnAccuracy2, treeAccuracy2),
                             Sensitivity = c(logisticSensitivity2, knnSensitivity2, treeSensitivity2),
                             Specificity = c(logisticSpecificity2, knnSpecificity2, treeSpecificity2))
print(summaryTable.2)



# ROC Curve
roc_obj_logistic2 <- roc(as.numeric(testData2$satisfaction), as.numeric(logisticPredictions2))
roc_obj_knn2 <- roc(as.numeric(testData2$satisfaction), as.numeric(knnPredictions2))
roc_obj_tree2 <- roc(as.numeric(testData2$satisfaction), as.numeric(treePredictions2))

roc_data2 <- data.frame(
  model = c(rep("Logistic Regression", length(roc_obj_logistic2$sensitivities)), 
            rep("k-Nearest Neighbors", length(roc_obj_knn2$sensitivities)),
            rep("Decision Tree", length(roc_obj_tree2$sensitivities))),
  sensitivity = c(roc_obj_logistic2$sensitivities, roc_obj_knn2$sensitivities, roc_obj_tree2$sensitivities),
  1-specificity = c(roc_obj_logistic2$specificities, roc_obj_knn2$specificities, roc_obj_tree2$specificities)
)

ggplot(roc_data2, aes(x = `1-specificity`, y = sensitivity, color = model)) +
  geom_path() +
  labs(title = "ROC Curve for Logistic Regression, k-Nearest Neighbors, and Decision Tree",
       x = "1 - Specificity",
       y = "Sensitivity") +
  scale_color_manual(values = c("#FFA500", "#FFD700", "#FF6347")) +
  theme_minimal()

# Summary table
summaryTable_2<- data.frame(
  Model = c("Logistic Regression", "k-Nearest Neighbors", "Decision Tree"),
  Accuracy = c(logisticAccuracy2, knnAccuracy2, treeAccuracy2),
  Sensitivity = c(logisticSensitivity2, knnSensitivity2, treeSensitivity2),
  Specificity = c(logisticSpecificity2, knnSpecificity2, treeSpecificity2),
  PPV = c(logisticPPV2, knnPPV2, treePPV2),
  NPV = c(logisticNPV2, knnNPV2, treeNPV2)
)

summaryTable_2

#ROC Curve
roc_obj_logistic2 <- roc(as.numeric(testData2$satisfaction), as.numeric(logisticPredictions2))
roc_obj_knn2 <- roc(as.numeric(testData2$satisfaction), as.numeric(knnPredictions2))
roc_obj_tree2 <- roc(as.numeric(testData2$satisfaction), as.numeric(treePredictions2))

# Plotting ROC curves
plot(roc_obj_logistic2, col = "blue", print.auc=TRUE)
lines(roc_obj_knn2, col = "red", print.auc=TRUE)
lines(roc_obj_tree2, col = "green", print.auc=TRUE)
legend("bottomright", legend = c("Logistic Regression", "k-Nearest Neighbors", "Decision Tree"), 
       col = c("blue", "red", "green"), lwd = 2)

# Creating a summary table
summaryTable2 <- data.frame(Model = c("Logistic Regression", "k-Nearest Neighbors", "Decision Tree"),
                            Accuracy = c(logisticAccuracy2, knnAccuracy2, treeAccuracy2),
                            Sensitivity = c(logisticSensitivity2, knnSensitivity2, treeSensitivity2),
                            Specificity = c(logisticSpecificity2, knnSpecificity2, treeSpecificity2),
                            AUC = c(auc(roc_obj_logistic2), auc(roc_obj_knn2), auc(roc_obj_tree2)))

summaryTable2



#######################################################"
library(glmnet)
X<-data.frame(subset(trainData2 , select = -(satisfaction)))
y<-trainData2$satisfaction


# Define the regularization strengths
lambda_l1 <- 0.2  # L1 regularization parameter
lambda_l2 <- 0.2  # L2 regularization parameter

# Create and train the logistic regression model with L1 regularization
logreg_l1 <- glmnet(X, y, family = "binomial", alpha = 1, lambda = lambda_l1)
summary(logreg_l1)
# Create and train the logistic regression model with L2 regularization
logreg_l2 <- glmnet(X, y, family = "binomial", alpha = 0, lambda = lambda_l2)

# Print the coefficients and intercepts
cat("L1 Regularization:\n")
print(coef(logreg_l1))
cat("\nL2 Regularization:\n")
print(coef(logreg_l2))

# Generate predictions
pred_l1 <- predict(logreg_l1, newx = as.matrix(subset(testData2 , select = -(satisfaction))), type = "response")
pred_l2 <- predict(logreg_l2, newx = as.matrix(subset(testData2 , select = -(satisfaction))), type = "response")

# Convert predictions to binary class labels
pred_labels_l1 <- ifelse(pred_l1 > 0.5, 1, 0)
pred_labels_l2 <- ifelse(pred_l2 > 0.5, 1, 0)

# Compute accuracy, specificity, and sensitivity
accuracy_l1 <- sum(pred_labels_l1 == y) / length(y)
specificity_l1 <- sum(pred_labels_l1[y == 0] == 0) / sum(y == 0)
sensitivity_l1 <- sum(pred_labels_l1[y == 1] == 1) / sum(y == 1)

accuracy_l2 <- sum(pred_labels_l2 == y) / length(y)
specificity_l2 <- sum(pred_labels_l2[y == 0] == 0) / sum(y == 0)
sensitivity_l2 <- sum(pred_labels_l2[y == 1] == 1) / sum(y == 1)

# Print summary table
summary_table <- data.frame(
  Regularization = c("L1", "L2"),
  Accuracy = c(accuracy_l1, accuracy_l2),
  Specificity = c(specificity_l1, specificity_l2),
  Sensitivity = c(sensitivity_l1, sensitivity_l2)
)
print(summary_table)

# Plot ROC curves
roc_l1 <- roc(y, pred_l1)
roc_l2 <- roc(y, pred_l2)

plot(roc_l1, col = "blue", main = "ROC Curves")
lines(roc_l2, col = "red")
legend("bottomright", legend = c("L1", "L2"), col = c("blue", "red"), lty = 1)

#############################################"""





library(tidyverse)
library(plotrix)
library(cluster)
library(MASS)
library(pROC)
library(caret)
library(reshape2)


# read the data 
data<-read.csv("airlinesData120.csv")


# dimensions of the data 
dim(data)

total.deleted.values<-0

zero.col<-c("Inflight.wifi.service","Ease.of.Online.booking","Food.and.drink","Online.boarding","Inflight.entertainment","Leg.room.service", "Cleanliness")

#check for the zero values : 



colSums(data[zero.col]==0) 






# delete zero values : 

preprocessed.data<- filter (data, Inflight.wifi.service!= 0 & 
                              Departure.Arrival.time.convenient!=0 &
                              Ease.of.Online.booking!= 0 & 
                              Food.and.drink!= 0 & 
                              Online.boarding!= 0 & 
                              Inflight.entertainment!= 0 & 
                              Leg.room.service!= 0 &
                              Cleanliness!= 0 )


# check if zero values are deleted :

colSums(preprocessed.data[zero.col]==0) 


total.deleted.values<- total.deleted.values +sum(colSums(data[zero.col]==0)) 

total.deleted.values

total.deleted.values<- total.deleted.values +sum(is.na(data))  

total.deleted.values

# Delete NA from Arrival.Delay.in.minutes
preprocessed.data<- na.omit(preprocessed.data)#


# check if Na ARE deleted : 

#sum(is.na(preprocessed.data)) 




preprocessed.data.b<-preprocessed.data  

preprocessed.data$satisfaction <- 1*(preprocessed.data$satisfaction=="satisfied")


preprocessed.data$satisfaction<- as.integer(preprocessed.data$satisfaction)




head(preprocessed.data)



preprocessed.data$Gender<- as.factor(preprocessed.data$Gender)
preprocessed.data$Customer.Type<- as.factor(preprocessed.data$Customer.Type)
preprocessed.data$Type.of.Travel<- as.factor(preprocessed.data$Type.of.Travel)
preprocessed.data$Class<- as.factor(preprocessed.data$Class)



preprocessed.data$satisfaction<- as.factor(preprocessed.data$satisfaction)








#preprocessed.data.DissimMatrix <- daisy(preprocessed.data, metric="gower")
DissimMatrix <- daisy(subset(preprocessed.data, select = -satisfaction), metric="gower")
View(DissimMatrix )


data.MDS <- cmdscale(DissimMatrix, k=2)
colnames(data.MDS) <- c("D1","D2")
plot(data.MDS, pch="*")
data.MDS2<-cbind(data.MDS,preprocessed.data.b$satisfaction)
data.MDS2<-as.data.frame(data.MDS2)
colnames(data.MDS2) <- c("D1","D2","satisfaction")

data.MDS2$satisfaction<-as.factor(data.MDS2$satisfaction)
plot(data.MDS2$D1, data.MDS2$D2,col=data.MDS2$satisfaction)

data.MDS2$satisfaction <- 1*(data.MDS2$satisfaction=="satisfied")








# Split the data into training and testing sets
set.seed(123)
# Define the control for cross-validation
control <- trainControl(method = "cv", number = 5)

trainIndex <- createDataPartition(data.MDS2$satisfaction, p = 0.8, list = FALSE)
trainData <- data.MDS2[trainIndex, ]
testData <- data.MDS2[-trainIndex, ]

# Logistic Regression
logisticModel <- train(satisfaction ~ ., data = trainData, method = "glm", family = "binomial" )
summary(logisticModel)
logisticPredictions <- predict(logisticModel, newdata = testData)
logisticAccuracy <- confusionMatrix(logisticPredictions, testData$satisfaction)$overall['Accuracy']

# K-nearest Neighbors (knn)
knnModel <- train(satisfaction ~ ., data = trainData, method = "knn")
knnModel$bestTune
knnPredictions <- predict(knnModel, newdata = testData)
knnAccuracy <- confusionMatrix(knnPredictions, testData$satisfaction)$overall['Accuracy']

# Decision Tree
treeModel <- train(satisfaction ~ ., data = trainData, method = "rpart")
treePredictions <- predict(treeModel, newdata = testData)
treeAccuracy <- confusionMatrix(treePredictions, testData$satisfaction)$overall['Accuracy']

# Compare the model accuracies
accuracyTable1 <- data.frame(Model = c("Logistic Regression", "k-Nearest Neighbors", "Decision Tree"),
                             Accuracy = c(logisticAccuracy, knnAccuracy, treeAccuracy))
accuracyTable1



# Calculate sensitivity (recall) and specificity
logisticSensitivity <- confusionMatrix(logisticPredictions, testData$satisfaction)$byClass['Sensitivity']
logisticSpecificity <- confusionMatrix(logisticPredictions, testData$satisfaction)$byClass['Specificity']
knnSensitivity <- confusionMatrix(knnPredictions, testData$satisfaction)$byClass['Sensitivity']
knnSpecificity <- confusionMatrix(knnPredictions, testData$satisfaction)$byClass['Specificity']
treeSensitivity <- confusionMatrix(treePredictions, testData$satisfaction)$byClass['Sensitivity']
treeSpecificity <- confusionMatrix(treePredictions, testData$satisfaction)$byClass['Specificity']

# Calculate the positive predictive value (PPV) and negative predictive value (NPV)
logisticPPV <- confusionMatrix(logisticPredictions, testData$satisfaction)$byClass['Pos Pred Value']
logisticNPV <- confusionMatrix(logisticPredictions, testData$satisfaction)$byClass['Neg Pred Value']
knnPPV <- confusionMatrix(knnPredictions, testData$satisfaction)$byClass['Pos Pred Value']
knnNPV <- confusionMatrix(knnPredictions, testData$satisfaction)$byClass['Neg Pred Value']
treePPV <- confusionMatrix(treePredictions, testData$satisfaction)$byClass['Pos Pred Value']
treeNPV <- confusionMatrix(treePredictions, testData$satisfaction)$byClass['Neg Pred Value']




# Create a dataframe for sensitivity, specificity, PPV, and NPV
performanceData <- data.frame(Model = rep(c("Logistic Regression", "k-Nearest Neighbors", "Decision Tree"), each = 2),
                              Metric = rep(c("Sensitivity", "Specificity", "PPV", "NPV"), times = 3),
                              Value = c(logisticSensitivity, logisticSpecificity, logisticPPV, logisticNPV,
                                        knnSensitivity, knnSpecificity, knnPPV, knnNPV,
                                        treeSensitivity, treeSpecificity, treePPV, treeNPV))

# Plotting the trade-off
ggplot(performanceData, aes(x = Metric, y = Value, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Trade-off between Correctly Identifying Satisfaction and Dissatisfaction",
       x = "Metric", y = "Value") +
  scale_fill_manual(values = c("#FFA500", "#FFD700", "#FF6347")) +
  theme_minimal()








# Calculate sensitivity and specificity
logisticSensitivity <- confusionMatrix(logisticPredictions, testData$satisfaction)$byClass['Sensitivity']
logisticSpecificity <- confusionMatrix(logisticPredictions, testData$satisfaction)$byClass['Specificity']
knnSensitivity <- confusionMatrix(knnPredictions, testData$satisfaction)$byClass['Sensitivity']
knnSpecificity <- confusionMatrix(knnPredictions, testData$satisfaction)$byClass['Specificity']
treeSensitivity <- confusionMatrix(treePredictions, testData$satisfaction)$byClass['Sensitivity']
treeSpecificity <- confusionMatrix(treePredictions, testData$satisfaction)$byClass['Specificity']

# Create a summary table of accuracy, sensitivity, and specificity
summaryTable.1 <- data.frame(Model = c("Logistic Regression", "k-Nearest Neighbors", "Decision Tree"),
                             Accuracy = c(logisticAccuracy, knnAccuracy, treeAccuracy),
                             Sensitivity = c(logisticSensitivity, knnSensitivity, treeSensitivity),
                             Specificity = c(logisticSpecificity, knnSpecificity, treeSpecificity))
print(summaryTable.1)




# Summary table
summaryTable_1 <- data.frame(
  Model = c("Logistic Regression", "k-Nearest Neighbors", "Decision Tree"),
  Accuracy = c(logisticAccuracy, knnAccuracy, treeAccuracy),
  Sensitivity = c(logisticSensitivity, knnSensitivity, treeSensitivity),
  Specificity = c(logisticSpecificity, knnSpecificity, treeSpecificity),
  PPV = c(logisticPPV, knnPPV, treePPV),
  NPV = c(logisticNPV, knnNPV, treeNPV)
)

summaryTable_1

#####################
# Calculate ROC curves and AUC

# ROC Curve
roc_obj_logistic <- roc(as.numeric(testData$satisfaction), as.numeric(logisticPredictions))
roc_obj_knn <- roc(as.numeric(testData$satisfaction), as.numeric(knnPredictions))
roc_obj_tree <- roc(as.numeric(testData$satisfaction), as.numeric(treePredictions))

# Plotting ROC curves
plot(roc_obj_logistic, col = "blue", print.auc=TRUE)
lines(roc_obj_knn, col = "red", print.auc=TRUE)
lines(roc_obj_tree, col = "green", print.auc=TRUE)
legend("bottomright", legend = c("Logistic Regression", "k-Nearest Neighbors", "Decision Tree"), 
       col = c("blue", "red", "green"), lwd = 2)

# Creating a summary table
summaryTable <- data.frame(Model = c("Logistic Regression(MDS)", "k-Nearest Neighbors(MDS)", "Decision Tree(MDS)"),
                           Accuracy = c(logisticAccuracy, knnAccuracy, treeAccuracy),
                           Sensitivity = c(logisticSensitivity, knnSensitivity, treeSensitivity),
                           Specificity = c(logisticSpecificity, knnSpecificity, treeSpecificity),
                           AUC = c(auc(roc_obj_logistic), auc(roc_obj_knn), auc(roc_obj_tree)))

summaryTable











##########################################"














