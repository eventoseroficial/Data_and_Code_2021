# Defining various steps required for the genetic algorithm -------------------

library(glm)

# Initialise population
initial_population <- function(size, n_features){
  population <- vector('list', size)
  
  random_seed <- round(runif(1), 1)
  
  for(individual in seq(size)){
    chromossome <- sample(c(TRUE, FALSE), 
                          n_features, 
                          replace = TRUE, 
                          prob = c(1 - random_seed, random_seed))
    
    population[[individual]] <- chromossome
  }
  
  population
}

# Custom fitness score
fitness_score <- function(population, training, test){
  scores <- vector('double', length(population))
  
  for(chromossome in seq(length(population))){
    if(sum(population[[chromossome]]) == 0)
      next
    
    glm_fit <- caret::train(
      form = label ~ .,
      data = training[,c(population[[chromossome]], TRUE)],
      #trControl = trainControl(method = "cv", number = 5),
      method = "glm",
      family = "binomial"
    )
    
    predictions <- predict(glm_fit, test[, c(population[[chromossome]], FALSE)])
    accuracy <- sum(predictions == test$label) / length(predictions)
    
    scores[chromossome] <- accuracy
  }
  
  sorted_score <- sort(scores, decreasing = TRUE, index.return = TRUE)$x
  sorted_index <- sort(scores, decreasing = TRUE, index.return = TRUE)$ix
  
  return(list(score = sorted_score, pop = population[sorted_index]))
}
 
# Select the best n individuals to mate    
selection <- function(population_after_fit, n_parents){
  population_next_gen <- vector('list', n_parents)
  
  for(i in seq(n_parents)){
    population_next_gen[i] <- population_after_fit[i]
  }
  
  population_next_gen
}

# Crossover to generate the children
crossover <- function(pop_after_selection){
  population_next_gen <- vector('list', length(pop_after_selection))
  
  size <- length(pop_after_selection[[1]])
  
  crossover_point <- round(runif(1, 2, size) - 1)
  
  for(i in seq_along(pop_after_selection)){
    
    child <- pop_after_selection[[i]]
    child[crossover_point:size] <- !pop_after_selection[[i]][crossover_point:size]
    
    population_next_gen[[i]] <- child
  }
  
  population_next_gen
}

# Mutation to diversify the population
mutation <- function(pop_after_crossover, mutation_rate = 0.3){
  population_next_gen <- vector('list', length(pop_after_crossover))
  
  for(i in seq_along(pop_after_crossover)){
    chromossome <- pop_after_crossover[[i]]
    for(j in length(chromossome)){
      if(runif(1) < mutation_rate){
        chromossome[j] <- !chromossome[j]
      } 
    }
    population_next_gen[[i]] <- chromossome
  }
  
  population_next_gen
}
  
# Crete a new generation  
generations <- function(size, n_features, n_parents, mutation_rate = 0.3, n_generations, training,
                        test){
  best_chromossome <- vector('list', n_generations)
  best_score <- vector('double', n_generations)
  
  population_next_generation <- initial_population(size, n_features)
  
  for(i in seq(n_generations)){
    fitness <- fitness_score(population_next_generation, training, test)
    score <- fitness[['score']]
    population_after_fit <- fitness[['pop']]
    population_after_selection <- selection(population_after_fit, n_parents)
    population_after_crossover <- crossover(population_after_selection)
    population_next_generation <- mutation(population_after_crossover)
    
    best_chromossome[i] <- population_after_fit[1]
    best_score[i] <- score[1]
  }
  
  sorted_score <- sort(best_score, decreasing = TRUE, index.return = TRUE)$x[1]
  sorted_index <- sort(best_score, decreasing = TRUE, index.return = TRUE)$ix[1]
  
  return(list(best_score = sorted_score, pop = best_chromossome[[sorted_index]]))
}

# Sample usage
ga_solution <- generations(size = 20,n_features = 30, n_parents = 10, mutation_rate = 0.10, 
                           n_generations = 8, training, test)  

