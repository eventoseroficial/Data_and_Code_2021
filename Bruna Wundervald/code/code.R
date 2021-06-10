library(tidyverse)
library(tidymodels)
library(infotheo)
library(patchwork)
set.seed(2021)

# Loading data ---------
data('gravier', package = 'datamicroarray')
gravier <- data.frame(
  class = gravier$y, 
  gravier$x
)

folds <- rsample::vfold_cv(gravier, v = 5) %>% 
  dplyr::mutate(train =  map(splits, training),
                test  = map(splits, testing))

# Checking class proportions --- 
folds %>% 
  pull(train) %>%
  .[[1]] %>% 
  janitor::tabyl(class)

folds %>% 
  pull(test) %>%
  .[[1]] %>% 
  janitor::tabyl(class)

#----------------------------------------------
modelling <- function(train, reg_factor = 1, mtry = 1){
  rf_mod <- 
    rand_forest(trees = 500, mtry = (mtry * ncol(train)) - 1) %>% 
    set_engine("ranger", importance = "impurity", 
               regularization.factor = reg_factor) %>% 
    set_mode("classification") %>% 
    parsnip::fit(class ~ ., data = train)
  return(rf_mod)
}

penalization <- function(gamma, lambda_0,  
                         data = NULL, imps = NULL, type = "rf"){
  if(type == "rf"){
    imps <- imps/max(imps)
    imp_mixing <- imps * gamma + (1 - gamma) * lambda_0
    return(imp_mixing)
  } else if(type == "MI"){
    mi <- function(data, var){
      mutinformation(c(data$class), data %>% pull(var))
    }
    # Calculating the mutual information values
    disc_data  <- infotheo::discretize(data) 
    disc_data$class <- as.factor(data$class)
    names_data <- names(data)[-1]
    mi_vars <- names_data  %>% map_dbl(~{ mi(data = disc_data, var = .x) })
    mi_mixing <- (1 - gamma) * lambda_0 + gamma * (mi_vars/max(mi_vars))
    return(mi_mixing)  
  }
}

# --------------------------------------------------------------
# Setting all parameters ------------------------ 
mtry <-  tibble(mtry = c(0.20, 0.45, 0.85))  
gamma_f  <-  c(0.3, 0.5, 0.8)
lambda_0_f <- c(0.35, 0.75)

parameters <- mtry %>% 
  tidyr::crossing(lambda_0_f, gamma_f)

# Adds gamma_f and lambda_0_f and run the functions with them ------
folds_imp <- folds %>% 
  dplyr::mutate(
    # Run the standard random forest model for the 5 folds
    model = purrr::map(train, modelling), 
    importances_std = purrr::map(model, ~{.x$fit$variable.importance}))  %>% 
  tidyr::expand_grid(parameters) %>% 
  dplyr::mutate(imp_rf = purrr::pmap(
    list(gamma_f, lambda_0_f, train, importances_std), type = "rf", 
    penalization), 
    imp_mi = purrr::pmap(
      list(gamma_f, lambda_0_f, train, importances_std), type = "MI", penalization)) 

saveRDS(folds_imp, file = "results/folds_imp.rds")

folds_imp %>% 
  dplyr::select(2, 6:11) %>% 
  dplyr::slice(1:3) %>% 
  saveRDS(file = "results/folds_imp_head.rds")

# Running models with all the new importance values ------
run_all_models <-  folds_imp %>%   
  dplyr::select(id, model, train, test,  imp_rf, imp_mi, mtry, lambda_0_f, gamma_f) %>% 
  tidyr::gather(type, importance, -train, -test, -mtry,
                -id, -model, -lambda_0_f, -gamma_f) %>% 
  dplyr::mutate(fit_penalized_rf = 
                  purrr::pmap(list(train, importance, mtry), modelling)) 

dim(run_all_models)
saveRDS(run_all_models, file = "results/run_all_models.rds")

# Evaluating all models ------
# Extract:
# Important variables, number used, test accuracy 

# First, for the standard random forests we have:
metric_std_rf <- folds_imp %>% 
  dplyr::group_by(id) %>% 
  dplyr::slice(1) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(id, model, train, test) %>% 
  dplyr::mutate(
    model_importance = purrr::map(model, ~{.x$fit$variable.importance}),
    n_var = purrr::map_dbl(model_importance, n_vars), 
    accuracy_test_std = purrr::map2_dbl(
      .x = model, .y = test, ~{ acc_test(.x, test = .y)}),
    accuracy_std = 1 -purrr::map_dbl(model, ~{ .x$fit$prediction.error})
  ) %>% 
  dplyr::select(id, n_var, accuracy_test_std, accuracy_std) 

saveRDS(metric_std_rf, "results/metric_std_rf.rds")

results <- run_all_models %>% 
  dplyr::mutate(
    model_importance = purrr::map(fit_penalized_rf, ~{.x$fit$variable.importance}),
    n_var = purrr::map_dbl(model_importance, n_vars),
    accuracy = 1 - purrr::map_dbl(fit_penalized_rf, ~{ .x$fit$prediction.error}),
    accuracy_test = purrr::map2_dbl(
      .x = fit_penalized_rf, .y = test, ~{ acc_test(.x, .y)})) 

results %>% 
  dplyr::select(-train, -test, -importance, -model, 
                -fit_penalized_rf, -model_importance) %>% 
  dplyr::arrange(id, desc(accuracy_test), 
                 desc(accuracy), n_var) %>% 
  saveRDS("results/results_table.rds")

# Plots ----------------------------
p1 <- results %>% 
  group_by(mtry, type, gamma_f) %>%
  ggplot(aes(y = accuracy_test, x = factor(mtry))) +
  facet_wrap(~type + gamma_f, 
             labeller= label_bquote(gamma~"="~.(gamma_f)~", g("~x[i]~") ="~.(type))) +
  geom_boxplot(fill = "#e68c7c") +
  labs(y = "Test accuracy", x = "mtry (%)") +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  theme_bw(18)

p1

p2 <- results %>% 
  ggplot(aes(y = n_var, x = factor(mtry))) +
  facet_wrap(~type + gamma_f, 
             labeller= label_bquote(gamma~"="~.(gamma_f)~", g("~x[i]~") ="~.(type))) +
  geom_boxplot(fill = "#e68c7c") +
  labs(y = "Number of variables used", x = "mtry (%)") +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  theme_bw(18)

p2
p1 + p2 + plot_layout(nrow = 1)

saveRDS(results, "results/results.rds")
# --------------------------------------------------------
best_models <- results %>% 
  arrange(desc(accuracy_test), desc(accuracy), n_var) %>% 
  group_by(id) %>% 
  slice(1:3) %>% 
  ungroup() %>% 
  mutate(new_formula = map(model_importance, get_formula))

# Re-evaluating selected variables -----------------
reev <- tibble(
  forms = best_models$new_formula
) %>% 
  tidyr::expand_grid(folds) %>% 
  dplyr::mutate(
    reev_models = purrr::map2(train, forms, modelling_reev))
#
results_reev <- reev %>% 
  dplyr::mutate(
    feat_importance = purrr::map(reev_models, ~{.x$fit$variable.importance}),
    n_var = purrr::map_dbl(feat_importance, n_vars),
    accuracy = 1 - purrr::map_dbl(reev_models, ~{ .x$fit$prediction.error}),
    accuracy_test = purrr::map2_dbl(
      .x = reev_models, .y = test, ~{ acc_test(.x, test = .y)})) 


saveRDS(results_reev, "results/results_reev.rds")

selected_vars <- results_reev %>% 
  arrange(desc(accuracy_test), desc(accuracy), n_var) %>% 
  slice(1:30) %>% 
  mutate(
    ind = 1:n(), 
    vars = map(feat_importance, get_vars)) %>% 
  dplyr::select(ind, vars) %>% 
  unnest() %>% 
  group_by(vars) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

final_vars <- selected_vars %>% slice(1:15) %>% pull(vars)
final_form <- paste("class ~ ", paste0(final_vars, collapse = ' + ')) %>%
  as.formula()

saveRDS(final_vars, "results/final_vars.rds")

set.seed(2021)
folds_20 <- rsample::vfold_cv(gravier, v = 20) %>% 
  dplyr::mutate(train =  map(splits, training),
                test  = map(splits, testing))

# Test it in all splits ---
final_results <- folds_20$splits %>% map(~{
  train <-  training(.x)
  test <-  testing(.x)
  
  rf <- 
    rand_forest(trees = 500, mtry = 7) %>%
    set_engine("ranger", importance = "impurity") %>% 
    set_mode("classification") %>% 
    parsnip::fit(final_form, data = train)
  
  accuracy_test <- acc_test(rf, test = test)
  list(accuracy_test = accuracy_test, 
       accuracy = 1 - rf$fit$prediction.error, 
       imp = rf$fit$variable.importance)
})

saveRDS(final_results, "results/final_results.rds")

# Final accuracies and importance values 
data.frame(accuracy_test = final_results %>% map_dbl("accuracy_test"), 
           accuracy = final_results %>% map_dbl("accuracy")) %>% 
  gather(type, value) %>% 
  group_by(type) %>% 
  summarise(mean = mean(value), 
            median = median(value))

final_results %>% 
  map("imp") %>% 
  bind_rows() %>% 
  gather(vars, value) %>% 
  group_by(vars) %>% 
  summarise(value = mean(value)) %>% 
  arrange(desc(value)) %>% 
  ggplot(aes(x = reorder(vars, value), value)) +
  geom_linerange(
    aes(ymin = min(value), ymax = value),
    position = position_dodge(width = 0.5), size = 1.5, 
    colour = 'wheat1') + 
  geom_point(colour = "#f5c04a", size = 3) + 
  ylab("Average importance values") +
  xlab("Variables") +
  theme_bw(18) +
  coord_flip() 
# ----------------------------------------------------------------------