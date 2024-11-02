knots <- c(1, 2, 3, 4, 5)

#### AGE ####

age_varying <- c()

for (k in knots) {
  brier_scores <- c()
  for (fold in 1:10) {
    
    # Define test and training data for this fold of cross validation
    train_indices <- folds[[fold]]
    test_indices <- setdiff(1:nrow(modelling_data), train_indices)
    train <- modelling_data[train_indices, ]
    test <- modelling_data[test_indices, ]
    
    train <- train %>% 
      as.data.frame() %>% 
      select(-seq_id)
    test <- test %>% 
      as.data.frame() %>% 
      select(-seq_id)
    
    # Define model  formula around which to vary splines
    model.formula <- as.formula(paste0("Surv(time, offer) ~ 
                                           ns(age, df =",  k, ") +
                                           bloodgroup*ns(mpra, df = 1) + 
                                           bloodgroup*ns(waiting_months, df = 2) +
                                           bloodgroup*year +
                                           female +
                                           graftno + 
                                           bloodgroup*comorbcount + 
                                           bloodgroup*kidneydisease + 
                                           ethnicity" ))
    
    
    model_timetonextoffer <- flexsurvreg(model.formula,
                                         data = train,
                                         dist = 'llogis',
                                         hess.control = list(tol.evalues = 1),
                                         method = "Nelder-Mead" # makes model robust to poor initialising values
    )                                      
    
    
    exp <- explain(model_timetonextoffer,
                   data=train,
                   y = Surv(train$time, 
                            train$offer)
    )
    
    y <- Surv(test$time, 
              test$offer)
    times <- exp$times
    surv <- exp$predict_survival_function(model_timetonextoffer, test, times)
    # calculating directly
    ibs <- integrated_brier_score(y, surv = surv, times = times)
    brier_scores <- c(brier_scores, ibs)
  }
  
  age_varying[[k]] <- mean(brier_scores)
}

## Test linear
for (fold in 1:10) {
  
  # Define test and training data for this fold of cross validation
  train_indices <- folds[[fold]]
  test_indices <- setdiff(1:nrow(modelling_data), train_indices)
  train <- modelling_data[train_indices, ]
  test <- modelling_data[test_indices, ]
  
  train <- train %>% 
    as.data.frame() %>% 
    select(-seq_id)
  test <- test %>% 
    as.data.frame() %>% 
    select(-seq_id)
  
  model_timetonextoffer <- flexsurvreg(Surv(time, offer) ~ 
                                         age +
                                         bloodgroup*ns(mpra, df = 1) + 
                                         bloodgroup*ns(waiting_months, df = 2) +
                                         bloodgroup*year +
                                         female +
                                         graftno + 
                                         bloodgroup*comorbcount + 
                                         bloodgroup*kidneydisease + 
                                         ethnicity,
                                       data = train,
                                       dist = 'llogis',
                                       hess.control = list(tol.evalues = 1),
                                       method = "Nelder-Mead" # makes model robust to poor initialising values
  )                                      
  
  
  exp <- explain(model_timetonextoffer,
                 data=train,
                 y = Surv(train$time, 
                          train$offer)
  )
  
  y <- Surv(test$time, 
            test$offer)
  times <- exp$times
  surv <- exp$predict_survival_function(model_timetonextoffer, test, times)
  # calculating directly
  ibs <- integrated_brier_score(y, surv = surv, times = times)
  brier_scores <- c(brier_scores, ibs)
}

age_varying[[6]] <- mean(brier_scores)


#### PRA ####

pra_varying <- c()

for (k in knots) {
  brier_scores <- c()
  for (fold in 1:10) {
    
    # Define test and training data for this fold of cross validation
    train_indices <- folds[[fold]]
    test_indices <- setdiff(1:nrow(modelling_data), train_indices)
    train <- modelling_data[train_indices, ]
    test <- modelling_data[test_indices, ]
    
    train <- train %>% 
      as.data.frame() %>% 
      select(-seq_id)
    test <- test %>% 
      as.data.frame() %>% 
      select(-seq_id)
    
    model.formula <- as.formula(paste0("Surv(time, offer) ~ 
                                           ns(age, df = 2) +
                                           bloodgroup*ns(mpra, df = ",k,") + 
                                           bloodgroup*ns(waiting_months, df = 2) +
                                           bloodgroup*year +
                                           female +
                                           graftno + 
                                           bloodgroup*comorbcount + 
                                           bloodgroup*kidneydisease + 
                                           ethnicity" ))
    
    
    model_timetonextoffer <- flexsurvreg(model.formula,
                                         data = train,
                                         dist = 'llogis',
                                         hess.control = list(tol.evalues = 1),
                                         method = "Nelder-Mead" # makes model robust to poor initialising values
    )                                      
    
    
    exp <- explain(model_timetonextoffer,
                   data=train,
                   y = Surv(train$time, 
                            train$offer)
    )
    
    y <- Surv(test$time, 
              test$offer)
    times <- exp$times
    surv <- exp$predict_survival_function(model_timetonextoffer, test, times)
    # calculating directly
    ibs <- integrated_brier_score(y, surv = surv, times = times)
    brier_scores <- c(brier_scores, ibs)
  }
  
  pra_varying[[k]] <- mean(brier_scores)
}

for (fold in 1:10) {
  
  # Define test and training data for this fold of cross validation
  train_indices <- folds[[fold]]
  test_indices <- setdiff(1:nrow(modelling_data), train_indices)
  train <- modelling_data[train_indices, ]
  test <- modelling_data[test_indices, ]
  
  train <- train %>% 
    as.data.frame() %>% 
    select(-seq_id)
  test <- test %>% 
    as.data.frame() %>% 
    select(-seq_id)
  
  
  model_timetonextoffer <- flexsurvreg(Surv(time, offer) ~ 
                                         ns(age, df = 2) +
                                         bloodgroup*mpra + 
                                         bloodgroup*ns(waiting_months, df = 2) +
                                         bloodgroup*year +
                                         female +
                                         graftno + 
                                         bloodgroup*comorbcount + 
                                         bloodgroup*kidneydisease + 
                                         ethnicity,
                                       data = train,
                                       dist = 'llogis',
                                       hess.control = list(tol.evalues = 1),
                                       method = "Nelder-Mead" # makes model robust to poor initialising values
  )                                      
  
  
  exp <- explain(model_timetonextoffer,
                 data=train,
                 y = Surv(train$time, 
                          train$offer)
  )
  
  y <- Surv(test$time, 
            test$offer)
  times <- exp$times
  surv <- exp$predict_survival_function(model_timetonextoffer, test, times)
  # calculating directly
  ibs <- integrated_brier_score(y, surv = surv, times = times)
  brier_scores <- c(brier_scores, ibs)
}

pra_varying[[6]] <- mean(brier_scores)


#### WAITING MONTHS ####

months_varying <- c()

for (k in knots) {
  brier_scores <- c()
  for (fold in 1:10) {
    
    # Define test and training data for this fold of cross validation
    train_indices <- folds[[fold]]
    test_indices <- setdiff(1:nrow(modelling_data), train_indices)
    train <- modelling_data[train_indices, ]
    test <- modelling_data[test_indices, ]
    
    train <- train %>% 
      as.data.frame() %>% 
      select(-seq_id)
    test <- test %>% 
      as.data.frame() %>% 
      select(-seq_id)
    
    model.formula <- as.formula(paste0("Surv(time, offer) ~ 
                                           ns(age, df = 2) +
                                           bloodgroup*ns(mpra, df = 1) + 
                                           bloodgroup*ns(waiting_months, df = ",k,") +
                                           bloodgroup*year +
                                           female +
                                           graftno + 
                                           bloodgroup*comorbcount + 
                                           kidneydisease*kidneydisease +
                                           ethnicity" ))
    
    
    model_timetonextoffer <- flexsurvreg(model.formula,
                                         data = train,
                                         dist = 'llogis',
                                         hess.control = list(tol.evalues = 1),
                                         method = "Nelder-Mead" # makes model robust to poor initialising values
    )                                      
    
    
    exp <- explain(model_timetonextoffer,
                   data=train,
                   y = Surv(train$time, 
                            train$offer)
    )
    
    y <- Surv(test$time, 
              test$offer)
    times <- exp$times
    surv <- exp$predict_survival_function(model_timetonextoffer, test, times)
    # calculating directly
    ibs <- integrated_brier_score(y, surv = surv, times = times)
    brier_scores <- c(brier_scores, ibs)
  }
  
  months_varying[[k]] <- mean(brier_scores)
}

for (fold in 1:10) {
  
  # Define test and training data for this fold of cross validation
  train_indices <- folds[[fold]]
  test_indices <- setdiff(1:nrow(modelling_data), train_indices)
  train <- modelling_data[train_indices, ]
  test <- modelling_data[test_indices, ]
  
  train <- train %>% 
    as.data.frame() %>% 
    select(-seq_id)
  test <- test %>% 
    as.data.frame() %>% 
    select(-seq_id)
  
  
  model_timetonextoffer <- flexsurvreg(Surv(time, offer) ~ 
                                         ns(age, df = 2) +
                                         bloodgroup*ns(mpra, df = 1) + 
                                         bloodgroup*ns(waiting_months, df = 2) +
                                         bloodgroup*year +
                                         female +
                                         graftno + 
                                         bloodgroup*comorbcount + 
                                         bloodgroup*kidneydisease + 
                                         ethnicity,
                                       data = train,
                                       dist = 'llogis',
                                       hess.control = list(tol.evalues = 1),
                                       method = "Nelder-Mead" # makes model robust to poor initialising values
  )                                      
  
  
  exp <- explain(model_timetonextoffer,
                 data=train,
                 y = Surv(train$time, 
                          train$offer)
  )
  
  y <- Surv(test$time, 
            test$offer)
  times <- exp$times
  surv <- exp$predict_survival_function(model_timetonextoffer, test, times)
  # calculating directly
  ibs <- integrated_brier_score(y, surv = surv, times = times)
  brier_scores <- c(brier_scores, ibs)
}

months_varying[[6]] <- mean(brier_scores)

#### YEAR ####

year_varying <- c()

for (k in knots) {
  brier_scores <- c()
  for (fold in 1:10) {
    
    # Define test and training data for this fold of cross validation
    train_indices <- folds[[fold]]
    test_indices <- setdiff(1:nrow(modelling_data), train_indices)
    train <- modelling_data[train_indices, ]
    test <- modelling_data[test_indices, ]
    
    train <- train %>% 
      as.data.frame() %>% 
      select(-seq_id)
    test <- test %>% 
      as.data.frame() %>% 
      select(-seq_id)
    
    model.formula <- as.formula(paste0("Surv(time, offer) ~ 
                                           ns(age, df = 2) +
                                           bloodgroup*ns(mpra, df = 1) + 
                                           bloodgroup*ns(waiting_months, df = 2) +
                                           bloodgroup*ns(year, df = ", k, ") +
                                           graftno + 
                                           female +
                                           bloodgroup*comorbcount + 
                                           bloodgroup*kidneydisease + 
                                           ethnicity" ))
    
    
    model_timetonextoffer <- flexsurvreg(model.formula,
                                         data = train,
                                         dist = 'llogis',
                                         hess.control = list(tol.evalues = 1),
                                         method = "Nelder-Mead" # makes model robust to poor initialising values
    )                                      
    
    
    exp <- explain(model_timetonextoffer,
                   data=train,
                   y = Surv(train$time, 
                            train$offer)
    )
    
    y <- Surv(test$time, 
              test$offer)
    times <- exp$times
    surv <- exp$predict_survival_function(model_timetonextoffer, test, times)
    # calculating directly
    ibs <- integrated_brier_score(y, surv = surv, times = times)
    brier_scores <- c(brier_scores, ibs)
  }
  
  year_varying[[k]] <- mean(brier_scores)
}

for (fold in 1:10) {
  
  # Define test and training data for this fold of cross validation
  train_indices <- folds[[fold]]
  test_indices <- setdiff(1:nrow(modelling_data), train_indices)
  train <- modelling_data[train_indices, ]
  test <- modelling_data[test_indices, ]
  
  train <- train %>% 
    as.data.frame() %>% 
    select(-seq_id)
  test <- test %>% 
    as.data.frame() %>% 
    select(-seq_id)
  
  
  model_timetonextoffer <- flexsurvreg(Surv(time, offer) ~ 
                                         ns(age, df = 2) +
                                         bloodgroup*ns(mpra, df = 1) + 
                                         bloodgroup*ns(waiting_months, df = 2) +
                                         bloodgroup*year +
                                         female +
                                         graftno + 
                                         bloodgroup*comorbcount + 
                                         bloodgroup*kidneydisease + 
                                         ethnicity,
                                       data = train,
                                       dist = 'llogis',
                                       hess.control = list(tol.evalues = 1),
                                       method = "Nelder-Mead" # makes model robust to poor initialising values
  )                                      
  
  
  exp <- explain(model_timetonextoffer,
                 data=train,
                 y = Surv(train$time, 
                          train$offer)
  )
  
  y <- Surv(test$time, 
            test$offer)
  times <- exp$times
  surv <- exp$predict_survival_function(model_timetonextoffer, test, times)
  # calculating directly
  ibs <- integrated_brier_score(y, surv = surv, times = times)
  brier_scores <- c(brier_scores, ibs)
}

year_varying[[6]] <- mean(brier_scores)

#### GRAFT NUMBER ####

graftno_varying <- c()

brier_scores <- c()
for (fold in 1:10) {
  
  # Define test and training data for this fold of cross validation
  train_indices <- folds[[fold]]
  test_indices <- setdiff(1:nrow(modelling_data), train_indices)
  train <- modelling_data[train_indices, ]
  test <- modelling_data[test_indices, ]
  
  train <- train %>% 
    as.data.frame() %>% 
    select(-seq_id)
  test <- test %>% 
    as.data.frame() %>% 
    select(-seq_id)
  
  model_timetonextoffer <- flexsurvreg(Surv(time, offer) ~ 
                                         ns(age, df = 2) +
                                         bloodgroup*ns(mpra, df = 1) + 
                                         bloodgroup*ns(waiting_months, df = 2) +
                                         bloodgroup*year +
                                         female +
                                         graftno + 
                                         bloodgroup*comorbcount + 
                                         bloodgroup*kidneydisease + 
                                         ethnicity,
                                       data = train,
                                       dist = 'llogis',
                                       hess.control = list(tol.evalues = 1),
                                       method = "Nelder-Mead" # makes model robust to poor initialising values
  )                                      
  
  
  exp <- explain(model_timetonextoffer,
                 data=train,
                 y = Surv(train$time, 
                          train$offer)
  )
  
  y <- Surv(test$time, 
            test$offer)
  times <- exp$times
  surv <- exp$predict_survival_function(model_timetonextoffer, test, times)
  # calculating directly
  ibs <- integrated_brier_score(y, surv = surv, times = times)
  brier_scores <- c(brier_scores, ibs)
}

graftno_varying[[1]] <- mean(brier_scores)


## Prev_tx binary

modelling_data_graftno_bin <- modelling_data %>% 
  mutate(graftno = case_when(graftno == 1 ~ 1,
                                 TRUE ~ 2)) 


brier_scores <- c()
for (fold in 1:10) {
  
  # Define test and training data for this fold of cross validation
  train_indices <- folds[[fold]]
  test_indices <- setdiff(1:nrow(modelling_data), train_indices)
  train <- modelling_data_graftno_bin[train_indices, ]
  test <- modelling_data_graftno_bin[test_indices, ]
  
  train <- train %>% 
    as.data.frame() %>% 
    select(-seq_id)
  test <- test %>% 
    as.data.frame() %>% 
    select(-seq_id)
  
  model_timetonextoffer <- flexsurvreg(Surv(time, offer) ~ 
                                         ns(age, df = 2) +
                                         bloodgroup*ns(mpra, df = 1) + 
                                         bloodgroup*ns(waiting_months, df = 2) +
                                         bloodgroup*year +
                                         female +
                                         graftno + 
                                         bloodgroup*comorbcount + 
                                         bloodgroup*kidneydisease + 
                                         ethnicity,
                                       data = train,
                                       dist = 'llogis',
                                       hess.control = list(tol.evalues = 1),
                                       method = "Nelder-Mead" # makes model robust to poor initialising values
  )                                      
  
  
  exp <- explain(model_timetonextoffer,
                 data=train,
                 y = Surv(train$time, 
                          train$offer)
  )
  
  y <- Surv(test$time, 
            test$offer)
  times <- exp$times
  surv <- exp$predict_survival_function(model_timetonextoffer, test, times)
  # calculating directly
  ibs <- integrated_brier_score(y, surv = surv, times = times)
  brier_scores <- c(brier_scores, ibs)
}

graftno_varying[[2]] <- mean(brier_scores)
graftno_varying[[3]] <- NA
graftno_varying[[4]] <- NA
graftno_varying[[5]] <- NA
graftno_varying[[6]] <- NA

#### MERGE ####

total_spline_sens <- cbind(age_varying,
                           pra_varying,
                           months_varying,
                           year_varying,
                           graftno_varying)

total_spline_sens # Most optimal: age 2 knots, PRA 1 knot, months 2 knots, year 4 knots

