n_vars <- function(importance) {
  vars <- importance
  vv <- names(vars)[vars > 0]
  length(vv)
}

acc_test <- function(rf, test){
  
  pp <- ranger:::predict.ranger(rf$fit, test)$predictions
  
  pred_test <- 
    pp %>% as.data.frame() %>% 
    pivot_longer(cols = c(good, poor)) %>% 
    mutate(ind = rep(1:nrow(test), each = 2)) %>% 
    group_by(ind) %>% 
    filter(value == max(value)) %>% 
    pull(name)
  
  round(sum(pred_test == test$class)/nrow(test), 3)
}


'%!in%' <- function(x,y)!('%in%'(x,y))


get_formula <- function(importance){
  vars <- importance
  vv <- names(vars)[vars > 0]
  form <- paste("class ~ ", paste0(vv, collapse = ' + ')) %>%
    as.formula()
  form  
}

get_vars <- function(importance){
  vars <- importance
  vv <- names(vars)[vars > 0]
  vv
}

modelling_reev <- function(train, forms, 
                           reg_factor = 1,  depth = FALSE){
  mtry <- round(sqrt(ncol(model.matrix(forms, train)) - 1))
  
  rf_mod <- 
    rand_forest(trees = 500, mtry = mtry) %>% 
    set_engine("ranger", importance = "impurity", 
               regularization.factor = reg_factor, 
               regularization.usedepth = depth) %>% 
    set_mode("classification") %>% 
    parsnip::fit(forms, data = train)
  return(rf_mod)
}
