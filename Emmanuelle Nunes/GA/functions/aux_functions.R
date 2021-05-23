# Auxiliary functions ----------------------------------------------------------

# Train/testing split
get_train_test_partition <- function(feature, target, training_size = 0.8){
  
  df <- feature %>%
    dplyr::bind_cols(target)
  
  index <- sample(nrow(target), 
                  replace = FALSE, 
                  size = round(nrow(target) * training_size))
  
  training <- df[index, ]
  
  test <- df[-index, ]
  
  return(list(training = training, test = test))
}