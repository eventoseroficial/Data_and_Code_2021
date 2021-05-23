# Auxiliary functions to be passed to the GA function -----------------------

# Fitness score 
fitness_score <- function(vars, features, target, sampling_prob = NA, metrics = 'ROC'){
  
  if(!is.na(sampling_prob)){
    sample_index <- sample(nrow(target), replace = FALSE, size = round(nrow(target) * sampling_prob))
    features <- features[sample_index,]
    target <- target[sample_index,]
  }
  
  # keep variables that were selected by the GA algorithm
  names <- colnames(features[vars == 1])
  features_used <- features[, names]
  
  # training and test set using 80/20 split
  partitions <- get_train_test_partition(features_used, target)
  
  
  # get the accuracy from the created model
  accuracy <- get_performance_metrics(partitions$training, partitions$test, metrics)
  
  # the fitness value will be the highest accuracy value divided by the number of features used
  accuracy / sum(vars)
}

get_performance_metrics <- function(training, test, metrics = 'ROC') {
  # Cross-Validation
  fitControl <- caret::trainControl(method = "cv",
                                    number = 10,
                                    summaryFunction = twoClassSummary,
                                    classProbs = TRUE)
  
  mtry <-  sqrt(ncol(training))
  tunegrid <- expand.grid(.mtry=round(mtry))
  
  rf_model <-  caret::train(label ~ .,
                            data = training,
                            method = "rf",
                            tuneGrid = tunegrid,
                            trControl = fitControl,
                            metric = metrics)
  
  predictions <- predict(rf_model, test)
  
  accuracy <- sum(predictions == test$label) / length(predictions)
  
  return(accuracy)
}
