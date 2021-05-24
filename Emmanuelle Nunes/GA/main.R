library(GA)      # Genetic Algorithms
library(tictoc)
library(caret)
library(randomForest)
library(dplyr)
library(janitor)

source('functions/fitness_function.R')
source('functions/aux_functions.R')

cancer_df <- readr::read_csv(file = 'data/cancer.csv') %>%
  janitor::clean_names() %>%
  dplyr::mutate(label = as.factor(ifelse(label == 1, 'B', 'M')))

cancer_features <- cancer_df %>%
  dplyr::select(-label)

cancer_label <- cancer_df %>%
  dplyr::select(label)

# GA parameters
param_nBits <- ncol(cancer_features)
col_names <- colnames(cancer_features)

# Executing the GA 
tictoc::tic()

ga_feature_selection <-  GA::ga(
  # custom fitness function
  fitness = function(vars){
    fitness_score(vars = vars, 
                  features =  cancer_features, 
                  target = cancer_label, 
                  sampling_prob = 0.6)
  },
  # crossover method
  crossover = gabin_uCrossover,
  # optimization data type
  type = "binary",
  # best N individuals to pass to next generation
  elitism = 3,
  # mutation rate
  pmutation = 0.3, 
  # the number of individuals/solutions
  popSize = 50, 
  # total number of variables
  nBits = param_nBits, 
  # variable name
  names = col_names, 
  # max iterations without improvement (stopping criteria)
  run = 5, 
  # generations
  maxiter = 20, 
  # plot the results
  monitor = TRUE, 
  # keep the best solution
  keepBest = TRUE, 
  # allow for parallel processing
  parallel = TRUE, 
  # reproducibility 
  seed = 8888 
)

tictoc::toc()

# summary of the algorithm
summary(ga_feature_selection)

# Final and best solution
best_vars_ga <- col_names[ga_feature_selection@solution[1,] == 1]

# Checking the variables of the best solution...
best_vars_ga

# Checking and comparing the performance of the model
partition <-  get_train_test_partition(cancer_features, cancer_label)
get_performance_metrics(training = partition$training[c(best_vars_ga, 'label')], 
                        test = partition$test[c(best_vars_ga, 'label')])

get_performance_metrics(training = partition$training,
                        test = partition$test)
