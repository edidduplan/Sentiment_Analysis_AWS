#============ 1) Import libraries ===================
pacman::p_load(readr, tidyverse, plotly)
pacman::p_load(rstudioapi)
pacman::p_load(caret, caretEnsemble, corrplot, data.table)
pacman::p_load(kknn, e1071, caTools)

# ============= 2) Splitting the data ================
set.seed(45)

fun_sample <- function(df){
  df[sample(1:nrow(df), 100, replace = F),]
}

iphone_list <- list(iphone, iphone_low250, iphone_nzv, iphonerfe)
iphonesample <- lapply(iphone_list, fun_sample)

# trainset <- list()
# testset <- list()
# j <- 1
# for (i in iphone_list){
#   trainsize <- createDataPartition(y = i$iphonesentiment, p = .80, list = F)
#   trainset[[j]] <- i[trainsize,]
#   testset[[j]] <- i[-trainsize,]
#   j <- j + 1
# }

#============= 3.1) Training "C5.0", "rf", "kknn" =================
fit <- c()
prediction <- c()
performance <- c()
error_test_compare  <- c()

models <- c("C5.0", "rf", "kknn")
ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 1)

time_start <- Sys.time()
for (i in models){
  fit <- train(iphonesentiment ~ . , 
               data = trainset[[1]], 
               method = i,
               tuneLength = 3,
               trControl = ctrl,
               preProc = c("center", "scale"))
  prediction_test <- predict(fit, newdata = testset[[1]])
  
  performance_test <- postResample(prediction_test, 
                                   testset[[1]]$iphonesentiment)
  
  error_test_compare <- cbind(error_test_compare, performance_test)
}
time_end <- Sys.time()

colnames(error_test_compare) <- c("C5.0", "rf", "kknn")
error_table1 <- error_test_compare

#============== 3.2) Training LogitBoost, svmRadial, svmPoly =============
models <- c("LogitBoost", "svmRadial", "svmPoly")
ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 1)

time_start <- Sys.time()
for (i in models){
  fit <- train(iphonesentiment ~ . , 
               data = trainset[[1]], 
               method = i,
               tuneLength = 3,
               trControl = ctrl,
               preProc = c("center", "scale"))
  prediction_test <- predict(fit, newdata = testset[[1]])
  
  performance_test <- postResample(prediction_test, 
                                   testset[[1]]$iphonesentiment)
  
  error_test_compare <- cbind(error_test_compare, performance_test)
}
time_end <- Sys.time()

colnames(error_test_compare) <- c("LogitBoost", "svmRadial", "svmPoly")
error_table2 <- error_test_compare

#============= 4.1) Training "C5.0", "rf", "kknn" =================
fit <- c()
prediction <- c()
performance <- c()
error_test_compare  <- c()

models <- c("C5.0", "rf", "kknn")
ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 1)

time_start <- Sys.time()
for (i in models){
  fit <- train(iphonesentiment ~ . , 
               data = trainset[[2]], 
               method = i,
               tuneLength = 3,
               trControl = ctrl,
               preProc = c("center", "scale"))
  prediction_test <- predict(fit, newdata = testset[[2]])
  
  performance_test <- postResample(prediction_test, 
                                   testset[[2]]$iphonesentiment)
  
  error_test_compare <- cbind(error_test_compare, performance_test)
}
time_end <- Sys.time()

colnames(error_test_compare) <- c("C5.0", "rf", "kknn")
error_table3 <- error_test_compare

#============== 4.2) Training LogitBoost, svmRadial, svmPoly =============
models <- c("LogitBoost", "svmRadial", "svmPoly")
ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 1)

time_start <- Sys.time()
for (i in models){
  fit <- train(iphonesentiment ~ . , 
               data = trainset[[2]], 
               method = i,
               tuneLength = 3,
               trControl = ctrl,
               preProc = c("center", "scale"))
  prediction_test <- predict(fit, newdata = testset[[2]])
  
  performance_test <- postResample(prediction_test, 
                                   testset[[2]]$iphonesentiment)
  
  error_test_compare <- cbind(error_test_compare, performance_test)
}
time_end <- Sys.time()

colnames(error_test_compare) <- c("LogitBoost", "svmRadial", "svmPoly")
error_table4 <- error_test_compare

#============= 5.1) Training "C5.0", "rf", "kknn" =================
fit <- c()
prediction <- c()
performance <- c()
error_test_compare  <- c()

models <- c("C5.0", "rf", "kknn")
ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 1)

time_start <- Sys.time()
for (i in models){
  fit <- train(iphonesentiment ~ . , 
               data = trainset[[3]], 
               method = i,
               tuneLength = 3,
               trControl = ctrl,
               preProc = c("center", "scale"))
  prediction_test <- predict(fit, newdata = testset[[3]])
  
  performance_test <- postResample(prediction_test, 
                                   testset[[3]]$iphonesentiment)
  
  error_test_compare <- cbind(error_test_compare, performance_test)
}
time_end <- Sys.time()

colnames(error_test_compare) <- c("C5.0", "rf", "kknn")
error_table5 <- error_test_compare

#============== 5.2) Training LogitBoost, svmRadial, svmPoly =============
models <- c("LogitBoost", "svmRadial", "svmPoly")
ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 1)

time_start <- Sys.time()
for (i in models){
  fit <- train(iphonesentiment ~ . , 
               data = trainset[[2]], 
               method = i,
               tuneLength = 3,
               trControl = ctrl,
               preProc = c("center", "scale"))
  prediction_test <- predict(fit, newdata = testset[[2]])
  
  performance_test <- postResample(prediction_test, 
                                   testset[[2]]$iphonesentiment)
  
  error_test_compare <- cbind(error_test_compare, performance_test)
}
time_end <- Sys.time()

colnames(error_test_compare) <- c("LogitBoost", "svmRadial", "svmPoly")
error_table6 <- error_test_compare

#============= 6.1) Training "C5.0", "rf", "kknn" =================
fit <- c()
prediction <- c()
performance <- c()
error_test_compare  <- c()

models <- c("C5.0", "rf", "kknn")
ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 1)

time_start <- Sys.time()
for (i in models){
  fit <- train(iphonesentiment ~ . , 
               data = trainset[[4]], 
               method = i,
               tuneLength = 3,
               trControl = ctrl,
               preProc = c("center", "scale"))
  prediction_test <- predict(fit, newdata = testset[[4]])
  
  performance_test <- postResample(prediction_test, 
                                   testset[[4]]$iphonesentiment)
  
  error_test_compare <- cbind(error_test_compare, performance_test)
}
time_end <- Sys.time()

colnames(error_test_compare) <- c("C5.0", "rf", "kknn")
error_table7 <- error_test_compare

#============== 6.2) Training LogitBoost, svmRadial, svmPoly =============
models <- c("LogitBoost", "svmRadial", "svmPoly")
ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 1)

time_start <- Sys.time()
for (i in models){
  fit <- train(iphonesentiment ~ . , 
               data = trainset[[4]], 
               method = i,
               tuneLength = 3,
               trControl = ctrl,
               preProc = c("center", "scale"))
  prediction_test <- predict(fit, newdata = testset[[4]])
  
  performance_test <- postResample(prediction_test, 
                                   testset[[4]]$iphonesentiment)
  
  error_test_compare <- cbind(error_test_compare, performance_test)
}
time_end <- Sys.time()

colnames(error_test_compare) <- c("LogitBoost", "svmRadial", "svmPoly")
error_table8 <- error_test_compare

#=========== 7) Class Imbalance study =====================
prediction_5 <- rep("5", nrow(iphone))
prediction_5 <- as.ordered(prediction_5)
postResample(prediction_5, testset[[1]]$iphonesentiment)

#============ Modelling with down/up-sampling ==================
trainsize <- createDataPartition(y = iphone_low250$iphonesentiment, 
                                 p = .80, list = F)
trainset <- iphone_low250[trainsize,]
testset <- iphone_low250[-trainsize,]

fit <- c()
prediction <- c()
performance <- c()
error_test_compare  <- c()

models <- c("C5.0", "rf")

ctrl <- trainControl(
  method = "repeatedcv", 
  number = 5, 
  repeats = 1,
  sampling = "down")

time_start <- Sys.time()
for (i in models){
  fit <- train(iphonesentiment ~ . , 
               data = trainset, 
               method = i,
               tuneLength = 3,
               trControl = ctrl,
               preProc = c("center", "scale"))
  prediction_test <- predict(fit, newdata = testset)
  
  performance_test <- postResample(prediction_test, 
                                   testset$iphonesentiment)
  
  error_test_compare <- cbind(error_test_compare, performance_test)
}
time_end <- Sys.time()

colnames(error_test_compare) <- c("C5.0", "rf")
error_table9 <- error_test_compare

#============= Error metrics of best model ==================
# The best results are obtained with pre-processing iphone_low250 
# (no outliers) and down sampling in the training set to account for
# class imbalance. 
# Best model is C5.0 with auto tuning. The error metrics are
# shown below:
#   
# acc: 0.83
# kappa: 0.60
