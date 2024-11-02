## Generate prediction plots and predictions for results section utilising final model

## Uses objects and libraries initiated in 02_wpp_modelling

#### Start with full model ####

times = c(1:365)

 used_model <- flexsurvreg(Surv(time, offer) ~
                             ns(age, df = 2) +
                             bloodgroup*ns(mpra, df = 1) +
                             bloodgroup*ns(waiting_months, df = 2) +
                             bloodgroup*ns(year, df = 4) +
                             graftno +
                             female +
                             bloodgroup*comorbcount +
                             kidneydisease*bloodgroup +
                             ethnicity,
                           data = modelling_data %>%
                             mutate(graftno = case_when(graftno == 1 ~ 1,
                                                        TRUE ~ 2)),
                           dist = 'llogis',
                           hess.control = list(tol.evalues = 1),
                           method = "Nelder-Mead")

## Baseline prediction from whom everything will vary

pred_set_baseline <- data.frame(
  age = 50,                            
  female = 0,                           
  graftno = 1,                         
  bloodgroup = factor("O", levels = levels(modelling_data$bloodgroup)),  
  comorbcount = 0,
  ethnicity = factor("White", levels = levels(modelling_data$ethnicity)), 
  kidneydisease = factor("Glomerular disease", levels = levels(modelling_data$kidneydisease)),
  year = 2019,
  mpra = 0,
  waiting_months = 0  
)

# 1 - survival prediction is prediction of kidney offer by this time (upper and lower confidence intervals)
preds_baseline_upper <-  1 - predict(used_model,
                               newdata = pred_set_baseline,
                               type = "survival",
                               times = times,
                               conf.int = TRUE)$.pred[[1]]$.pred_lower

preds_baseline <-  1 - predict(used_model,
                               newdata = pred_set_baseline,
                               type = "survival",
                               times = times,
                               conf.int = TRUE)$.pred[[1]]$.pred_survival

preds_baseline_lower <-  1 - predict(used_model,
                               newdata = pred_set_baseline,
                               type = "survival",
                               times = times,
                               conf.int = TRUE)$.pred[[1]]$.pred_upper

## Now generate predictions varying each covariate

#### AGE ####

# Make prediction sets with different ages
pred_set_30 <- pred_set_baseline %>% 
  mutate(age = 30)

pred_set_50 <- pred_set_baseline %>% 
  mutate(age = 50) # baseline

pred_set_70 <- pred_set_baseline %>% 
  mutate(age = 70)

pred_set_80 <- pred_set_baseline %>% 
  mutate(age = 80)

## Prediction curves with confidence intervals for each age 

preds_30 <- 1 - predict(used_model,
                        newdata = pred_set_30,
                        type = "survival",
                        times = times,
                        conf.int = TRUE)$.pred[[1]]$.pred_survival

preds_30_upper <- 1 - predict(used_model,
                              newdata = pred_set_30,
                              type = "survival",
                              times = times,
                              conf.int = TRUE)$.pred[[1]]$.pred_lower

preds_30_lower <- 1 - predict(used_model,
                              newdata = pred_set_30,
                              type = "survival",
                              times = times,
                              conf.int = TRUE)$.pred[[1]]$.pred_upper

preds_50 <- 1 - predict(used_model,
                        newdata = pred_set_50,
                        type = "survival",
                        times = times)$.pred[[1]]$.pred_survival

preds_50_upper <- 1 - predict(used_model,
                              newdata = pred_set_50,
                              type = "survival",
                              times = times,
                              conf.int = TRUE)$.pred[[1]]$.pred_lower

preds_50_lower <- 1 - predict(used_model,
                              newdata = pred_set_50,
                              type = "survival",
                              times = times,
                              conf.int = TRUE)$.pred[[1]]$.pred_upper

preds_70 <- 1 - predict(used_model,
                        newdata = pred_set_70,
                        type = "survival",
                        times = times)$.pred[[1]]$.pred_survival

preds_70_upper <- 1 - predict(used_model,
                              newdata = pred_set_70,
                              type = "survival",
                              times = times,
                              conf.int = TRUE)$.pred[[1]]$.pred_lower

preds_70_lower <- 1 - predict(used_model,
                              newdata = pred_set_70,
                              type = "survival",
                              times = times,
                              conf.int = TRUE)$.pred[[1]]$.pred_upper

preds_80 <- 1 - predict(used_model,
                        newdata = pred_set_80,
                        type = "survival",
                        times = times)$.pred[[1]]$.pred_survival

preds_80_upper <- 1 - predict(used_model,
                              newdata = pred_set_80,
                              type = "survival",
                              times = times,
                              conf.int = TRUE)$.pred[[1]]$.pred_lower

preds_80_lower <- 1 - predict(used_model,
                              newdata = pred_set_80,
                              type = "survival",
                              times = times,
                              conf.int = TRUE)$.pred[[1]]$.pred_upper

# Arrange predictions into one dataset, upper/lower CIs into a different dataset

data_age <- data.frame(
  times = times,
  age_50 = preds_baseline,  # Assuming preds_baseline corresponds to age 50
  age_30 = preds_30,
  age_70 = preds_70,
  age_80 = preds_80
)

data_age_upper <- data.frame(
  times = times,
  age_50 = preds_baseline_upper,
  age_30 = preds_30_upper,
  age_70 = preds_70_upper,
  age_80 = preds_80_upper
)

data_age_lower <- data.frame(
  times = times,
  age_50 = preds_baseline_lower,
  age_30 = preds_30_lower,
  age_70 = preds_70_lower,
  age_80 = preds_80_lower
)

# Arrange data in long to prepare for plotting

data_long_age <- pivot_longer(data_age, 
                              cols = c(age_50, age_30, age_70, age_80), 
                              names_to = "Age", 
                              values_to = "preds")

data_long_age_lower <- pivot_longer(data_age_lower, 
                                    cols = c(age_50, age_30, age_70, age_80), 
                                    names_to = "Age", 
                                    values_to = "lower")

data_bounds_age <- pivot_longer(data_age_upper, 
                                cols = c(age_50, age_30, age_70, age_80), 
                                names_to = "Age", 
                                values_to = "upper") %>% 
  left_join(data_long_age_lower);

# Create the line plot for age
age_preds <- ggplot(data_long_age, aes(x = times, y = preds, color = Age)) +
  geom_line() +   
# Create a ribbon plot for 95% confidence intervals
  geom_ribbon(data = data_bounds_age, aes(ymin = lower, 
                                          ymax = upper, 
                                          fill = Age, 
                                          x = times), 
              alpha = 0.2,
              inherit.aes = FALSE,
              color = NA) +  
  labs(title = "Age",
       x = "",                 
       y = "") + 
  theme_minimal() +         
# colour coding 
  scale_color_manual(values = c("age_50" = "blue", "age_30" = "forestgreen", "age_70" = "red",
                                "age_80" = "purple"),
                     labels = c("age_30" = "30", "age_50" = "50", 
                                "age_70" = "70", "age_80" = "80")) +
  scale_fill_manual(values = c("age_50" = "blue", "age_30" = "forestgreen", "age_70" = "red",
                               "age_80" = "purple"),
                    labels = c("age_30" = "30", "age_50" = "50", 
                               "age_70" = "70", "age_80" = "80")) +
  theme(legend.position = c(0.8, 0.6),  # legend on the plot for space efficiency
        legend.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        legend.title = element_blank())


#### BLOOD GROUP ####

pred_set_O <- pred_set_baseline

pred_set_AB <- pred_set_baseline %>% 
  mutate(bloodgroup = factor("AB", levels = levels(modelling_data$bloodgroup)))

pred_set_A <- pred_set_baseline %>% 
  mutate(bloodgroup = factor("A", levels = levels(modelling_data$bloodgroup)))

pred_set_B <- pred_set_baseline %>% 
  mutate(bloodgroup = factor("B", levels = levels(modelling_data$bloodgroup)))

preds_O <- 1 - predict(used_model,
                       newdata = pred_set_O,
                       type = "survival",
                       times = times,
                       conf.int = TRUE)$.pred[[1]]$.pred_survival

preds_O_upper <- 1 - predict(used_model,
                             newdata = pred_set_O,
                             type = "survival",
                             times = times,
                             conf.int = TRUE)$.pred[[1]]$.pred_lower

preds_O_lower <- 1 - predict(used_model,
                             newdata = pred_set_O,
                             type = "survival",
                             times = times,
                             conf.int = TRUE)$.pred[[1]]$.pred_upper

preds_A <- 1 - predict(used_model,
                       newdata = pred_set_A,
                       type = "survival",
                       times = times)$.pred[[1]]$.pred_survival

preds_A_upper <- 1 - predict(used_model,
                             newdata = pred_set_A,
                             type = "survival",
                             times = times,
                             conf.int = TRUE)$.pred[[1]]$.pred_lower

preds_A_lower <- 1 - predict(used_model,
                             newdata = pred_set_A,
                             type = "survival",
                             times = times,
                             conf.int = TRUE)$.pred[[1]]$.pred_upper

preds_B <- 1 - predict(used_model,
                       newdata = pred_set_B,
                       type = "survival",
                       times = times)$.pred[[1]]$.pred_survival

preds_B_upper <- 1 - predict(used_model,
                             newdata = pred_set_B,
                             type = "survival",
                             times = times,
                             conf.int = TRUE)$.pred[[1]]$.pred_lower

preds_B_lower <- 1 - predict(used_model,
                             newdata = pred_set_B,
                             type = "survival",
                             times = times,
                             conf.int = TRUE)$.pred[[1]]$.pred_upper

preds_AB <- 1 - predict(used_model,
                        newdata = pred_set_AB,
                        type = "survival",
                        times = times)$.pred[[1]]$.pred_survival

preds_AB_upper <- 1 - predict(used_model,
                              newdata = pred_set_AB,
                              type = "survival",
                              times = times,
                              conf.int = TRUE)$.pred[[1]]$.pred_lower

preds_AB_lower <- 1 - predict(used_model,
                              newdata = pred_set_AB,
                              type = "survival",
                              times = times,
                              conf.int = TRUE)$.pred[[1]]$.pred_upper

data_bloodgroup <- data.frame(
  times = times,
  bloodgroup_O = preds_baseline,  # Assuming preds_baseline corresponds to blood group O
  bloodgroup_A = preds_A,
  bloodgroup_B = preds_B,
  bloodgroup_AB = preds_AB
)

data_bloodgroup_upper <- data.frame(
  times = times,
  bloodgroup_O = preds_baseline_upper,
  bloodgroup_A = preds_A_upper,
  bloodgroup_B = preds_B_upper,
  bloodgroup_AB = preds_AB_upper
)

data_bloodgroup_lower <- data.frame(
  times = times,
  bloodgroup_O = preds_baseline_lower,
  bloodgroup_A = preds_A_lower,
  bloodgroup_B = preds_B_lower,
  bloodgroup_AB = preds_AB_lower
)

data_long_bloodgroup <- pivot_longer(data_bloodgroup, 
                                     cols = c(bloodgroup_O, bloodgroup_A, bloodgroup_B, bloodgroup_AB), 
                                     names_to = "Bloodgroup", 
                                     values_to = "preds")

data_long_bloodgroup_lower <- pivot_longer(data_bloodgroup_lower, 
                                           cols = c(bloodgroup_O, bloodgroup_A, bloodgroup_B, bloodgroup_AB), 
                                           names_to = "Bloodgroup", 
                                           values_to = "lower")

data_bounds_bloodgroup <- pivot_longer(data_bloodgroup_upper, 
                                       cols = c(bloodgroup_O, bloodgroup_A, bloodgroup_B, bloodgroup_AB), 
                                       names_to = "Bloodgroup", 
                                       values_to = "upper") %>% 
  left_join(data_long_bloodgroup_lower);

# Create the line plot for blood group
blood_group_preds <- ggplot(data_long_bloodgroup, aes(x = times, y = preds, color = Bloodgroup)) +
  geom_line() +   
  geom_ribbon(data = data_bounds_bloodgroup, aes(ymin = lower, 
                                                 ymax = upper, 
                                                 fill = Bloodgroup, 
                                                 x = times), 
              alpha = 0.2,
              inherit.aes = FALSE,
              color = NA) +  
  labs(title = "Blood group",    
       x = "",                 
       y = "") + 
  theme_minimal() +                    
  scale_color_manual(values = c("bloodgroup_O" = "blue", "bloodgroup_A" = "forestgreen", 
                                "bloodgroup_B" = "red", "bloodgroup_AB" = "purple"),
                     labels = c("bloodgroup_O" = "O", "bloodgroup_A" = "A", 
                                "bloodgroup_B" = "B", "bloodgroup_AB" = "AB")) +
  scale_fill_manual(values = c("bloodgroup_O" = "blue", "bloodgroup_A" = "forestgreen", 
                               "bloodgroup_B" = "red", "bloodgroup_AB" = "purple"),
                    labels = c("bloodgroup_O" = "O", "bloodgroup_A" = "A", 
                               "bloodgroup_B" = "B", "bloodgroup_AB" = "AB")) +
  theme(legend.position = c(0.5, 0.4),  # Adjust x and y coordinates as needed
        legend.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        legend.title = element_blank())

#### KIDNEY DISEASE ####

# Create prediction sets for each kidney disease category
pred_set_Glomerular <- pred_set_baseline %>% 
  mutate(kidneydisease = factor("Glomerular disease", levels = levels(modelling_data$kidneydisease)))

pred_set_Reflux <- pred_set_baseline %>% 
  mutate(kidneydisease = factor("Reflux nephropathy", levels = levels(modelling_data$kidneydisease)))

pred_set_Diabetes <- pred_set_baseline %>% 
  mutate(kidneydisease = factor("Diabetes", levels = levels(modelling_data$kidneydisease)))

pred_set_Hypertension <- pred_set_baseline %>% 
  mutate(kidneydisease = factor("Hypertension", levels = levels(modelling_data$kidneydisease)))

pred_set_Other <- pred_set_baseline %>% 
  mutate(kidneydisease = factor("Other", levels = levels(modelling_data$kidneydisease)))

pred_set_Polycystic <- pred_set_baseline %>% 
  mutate(kidneydisease = factor("Polycystic disease", levels = levels(modelling_data$kidneydisease)))

# Predictions for each kidney disease category
preds_Glomerular <- 1 - predict(used_model,
                                newdata = pred_set_Glomerular,
                                type = "survival",
                                times = times)$.pred[[1]]$.pred_survival

preds_Glomerular_upper <- 1 - predict(used_model,
                                      newdata = pred_set_Glomerular,
                                      type = "survival",
                                      times = times,
                                      conf.int = TRUE)$.pred[[1]]$.pred_lower

preds_Glomerular_lower <- 1 - predict(used_model,
                                      newdata = pred_set_Glomerular,
                                      type = "survival",
                                      times = times,
                                      conf.int = TRUE)$.pred[[1]]$.pred_upper

preds_Reflux <- 1 - predict(used_model,
                            newdata = pred_set_Reflux,
                            type = "survival",
                            times = times)$.pred[[1]]$.pred_survival

preds_Reflux_upper <- 1 - predict(used_model,
                                  newdata = pred_set_Reflux,
                                  type = "survival",
                                  times = times,
                                  conf.int = TRUE)$.pred[[1]]$.pred_lower

preds_Reflux_lower <- 1 - predict(used_model,
                                  newdata = pred_set_Reflux,
                                  type = "survival",
                                  times = times,
                                  conf.int = TRUE)$.pred[[1]]$.pred_upper

preds_Diabetes <- 1 - predict(used_model,
                              newdata = pred_set_Diabetes,
                              type = "survival",
                              times = times)$.pred[[1]]$.pred_survival

preds_Diabetes_upper <- 1 - predict(used_model,
                                    newdata = pred_set_Diabetes,
                                    type = "survival",
                                    times = times,
                                    conf.int = TRUE)$.pred[[1]]$.pred_lower

preds_Diabetes_lower <- 1 - predict(used_model,
                                    newdata = pred_set_Diabetes,
                                    type = "survival",
                                    times = times,
                                    conf.int = TRUE)$.pred[[1]]$.pred_upper

preds_Hypertension <- 1 - predict(used_model,
                                  newdata = pred_set_Hypertension,
                                  type = "survival",
                                  times = times)$.pred[[1]]$.pred_survival

preds_Hypertension_upper <- 1 - predict(used_model,
                                        newdata = pred_set_Hypertension,
                                        type = "survival",
                                        times = times,
                                        conf.int = TRUE)$.pred[[1]]$.pred_lower

preds_Hypertension_lower <- 1 - predict(used_model,
                                        newdata = pred_set_Hypertension,
                                        type = "survival",
                                        times = times,
                                        conf.int = TRUE)$.pred[[1]]$.pred_upper

preds_Other <- 1 - predict(used_model,
                           newdata = pred_set_Other,
                           type = "survival",
                           times = times)$.pred[[1]]$.pred_survival

preds_Other_upper <- 1 - predict(used_model,
                                 newdata = pred_set_Other,
                                 type = "survival",
                                 times = times,
                                 conf.int = TRUE)$.pred[[1]]$.pred_lower

preds_Other_lower <- 1 - predict(used_model,
                                 newdata = pred_set_Other,
                                 type = "survival",
                                 times = times,
                                 conf.int = TRUE)$.pred[[1]]$.pred_upper

preds_Polycystic <- 1 - predict(used_model,
                                newdata = pred_set_Polycystic,
                                type = "survival",
                                times = times)$.pred[[1]]$.pred_survival

preds_Polycystic_upper <- 1 - predict(used_model,
                                      newdata = pred_set_Polycystic,
                                      type = "survival",
                                      times = times,
                                      conf.int = TRUE)$.pred[[1]]$.pred_lower

preds_Polycystic_lower <- 1 - predict(used_model,
                                      newdata = pred_set_Polycystic,
                                      type = "survival",
                                      times = times,
                                      conf.int = TRUE)$.pred[[1]]$.pred_upper

# Compile predictions into data frames
data_kidneydisease <- data.frame(
  times = times,
  disease_Glomerular = preds_Glomerular,
  disease_Reflux = preds_Reflux,
  disease_Diabetes = preds_Diabetes,
  disease_Hypertension = preds_Hypertension,
  disease_Other = preds_Other,
  disease_Polycystic = preds_Polycystic
)

data_kidneydisease_upper <- data.frame(
  times = times,
  disease_Glomerular = preds_Glomerular_upper,
  disease_Reflux = preds_Reflux_upper,
  disease_Diabetes = preds_Diabetes_upper,
  disease_Hypertension = preds_Hypertension_upper,
  disease_Other = preds_Other_upper,
  disease_Polycystic = preds_Polycystic_upper
)

data_kidneydisease_lower <- data.frame(
  times = times,
  disease_Glomerular = preds_Glomerular_lower,
  disease_Reflux = preds_Reflux_lower,
  disease_Diabetes = preds_Diabetes_lower,
  disease_Hypertension = preds_Hypertension_lower,
  disease_Other = preds_Other_lower,
  disease_Polycystic = preds_Polycystic_lower
)

# Transform to long format for ggplot
data_long_kidneydisease <- pivot_longer(data_kidneydisease, 
                                        cols = c(disease_Glomerular, disease_Reflux, disease_Diabetes, 
                                                 disease_Hypertension, disease_Other, disease_Polycystic), 
                                        names_to = "KidneyDisease", 
                                        values_to = "preds")

data_long_kidneydisease_lower <- pivot_longer(data_kidneydisease_lower, 
                                              cols = c(disease_Glomerular, disease_Reflux, disease_Diabetes, 
                                                       disease_Hypertension, disease_Other, disease_Polycystic), 
                                              names_to = "KidneyDisease", 
                                              values_to = "lower")

data_bounds_kidneydisease <- pivot_longer(data_kidneydisease_upper, 
                                          cols = c(disease_Glomerular, disease_Reflux, disease_Diabetes, 
                                                   disease_Hypertension, disease_Other, disease_Polycystic), 
                                          names_to = "KidneyDisease", 
                                          values_to = "upper") %>% 
  left_join(data_long_kidneydisease_lower);

# Create the line plot for kidney disease categories
kidney_disease_preds <- ggplot(data_long_kidneydisease, aes(x = times, y = preds, color = KidneyDisease)) +
  geom_line() +   
  geom_ribbon(data = data_bounds_kidneydisease, aes(ymin = lower, 
                                                    ymax = upper, 
                                                    fill = KidneyDisease, 
                                                    x = times), 
              alpha = 0.2,
              inherit.aes = FALSE,
              color = NA) +  
  labs(title = "Kidney Disease",    
       x = "",                 
       y = "") + 
  theme_minimal() +                    
  scale_color_manual(values = c("disease_Glomerular" = "blue", 
                                "disease_Diabetes" = "red", 
                                "disease_Polycystic" = "cyan", 
                                "disease_Hypertension" = "purple", 
                                "disease_Reflux" = "forestgreen", 
                                "disease_Other" = "orange"),
                     labels = c("disease_Glomerular" = "Glomerular disease", 
                                "disease_Diabetes" = "Diabetes", 
                                "disease_Polycystic" = "Polycystic disease", 
                                "disease_Hypertension" = "Hypertension", 
                                "disease_Reflux" = "Reflux nephropathy", 
                                "disease_Other" = "Other"),
                     breaks = c("disease_Glomerular", "disease_Diabetes", 
                                "disease_Polycystic", "disease_Hypertension",
                                "disease_Reflux", "disease_Other")) +
  scale_fill_manual(values = c("disease_Glomerular" = "blue", 
                                "disease_Diabetes" = "red", 
                                "disease_Polycystic" = "cyan", 
                                "disease_Hypertension" = "purple", 
                                "disease_Reflux" = "forestgreen", 
                                "disease_Other" = "orange"),
                     labels = c("disease_Glomerular" = "Glomerular disease", 
                                "disease_Diabetes" = "Diabetes", 
                                "disease_Polycystic" = "Polycystic disease", 
                                "disease_Hypertension" = "Hypertension", 
                                "disease_Reflux" = "Reflux nephropathy", 
                                "disease_Other" = "Other"),
                    breaks = c("disease_Glomerular", "disease_Diabetes", 
                               "disease_Polycystic", "disease_Hypertension",
                               "disease_Reflux", "disease_Other")) +
  theme(legend.position = c(0.5, 0.4),  # Adjust x and y coordinates as needed
        legend.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        legend.title = element_blank())

#### YEAR ####

# Create prediction sets for 2019 and 2024 only
pred_set_2019 <- pred_set_baseline %>% 
  mutate(year = 2019)

pred_set_2024 <- pred_set_baseline %>% 
  mutate(year = 2024)

# Make predictions for 2019
preds_2019 <- 1 - predict(used_model,
                          newdata = pred_set_2019,
                          type = "survival",
                          times = times,
                          conf.int = TRUE)$.pred[[1]]$.pred_survival

preds_2019_upper <- 1 - predict(used_model,
                                newdata = pred_set_2019,
                                type = "survival",
                                times = times,
                                conf.int = TRUE)$.pred[[1]]$.pred_lower

preds_2019_lower <- 1 - predict(used_model,
                                newdata = pred_set_2019,
                                type = "survival",
                                times = times,
                                conf.int = TRUE)$.pred[[1]]$.pred_upper

# Make predictions for 2024
preds_2024 <- 1 - predict(used_model,
                          newdata = pred_set_2024,
                          type = "survival",
                          times = times,
                          conf.int = TRUE)$.pred[[1]]$.pred_survival

preds_2024_upper <- 1 - predict(used_model,
                                newdata = pred_set_2024,
                                type = "survival",
                                times = times,
                                conf.int = TRUE)$.pred[[1]]$.pred_lower

preds_2024_lower <- 1 - predict(used_model,
                                newdata = pred_set_2024,
                                type = "survival",
                                times = times,
                                conf.int = TRUE)$.pred[[1]]$.pred_upper

preds_2024_point <- predict(used_model,
                            newdata = pred_set_2024,
                            type = "quantile",
                            p = c(0.25, 0.5, 0.75))$.pred

# Create data frames for only 2019 and 2024
data <- data.frame(
  times = times,
  year_2019 = preds_2019,
  year_2024 = preds_2024
)

data_upper <- data.frame(
  times = times,
  year_2019 = preds_2019_upper,
  year_2024 = preds_2024_upper
)

data_lower <- data.frame(
  times = times,
  year_2019 = preds_2019_lower,
  year_2024 = preds_2024_lower
)

# Pivot longer for 2019 and 2024 only
data_long <- pivot_longer(data, 
                          cols = c(year_2019, year_2024), 
                          names_to = "Year", 
                          values_to = "preds")

data_long_lower <- pivot_longer(data_lower, 
                                cols = c(year_2019, year_2024), 
                                names_to = "Year", 
                                values_to = "lower")

data_bounds <- pivot_longer(data_upper, 
                            cols = c(year_2019, year_2024), 
                            names_to = "Year", 
                            values_to = "upper") %>% 
  left_join(data_long_lower)

# Create the line plot comparing 2019 and 2024
year_preds <- ggplot(data_long, aes(x = times, y = preds, color = Year)) +
  geom_line() +   
  geom_ribbon(data = data_bounds, aes(ymin = lower, 
                                      ymax = upper, 
                                      fill = Year, 
                                      x = times), 
              alpha = 0.2,
              inherit.aes = FALSE,
              color = NA) +  
  labs(title = "Year",    
       x = "Time (days)",                 
       y = "Predicted Probability of Kidney Offer") + 
  theme_minimal() +                    
  scale_color_manual(values = c("year_2019" = "blue", "year_2024" = "red"),
                     labels = c("year_2019" = "2019", 
                                "year_2024" = "2024")) +
  scale_fill_manual(values = c("year_2019" = "blue", "year_2024" = "red"),
                    labels = c("year_2019" = "2019", 
                               "year_2024" = "2024")) +
  theme(legend.position = c(0.5, 0.4),  # Adjust x and y coordinates as needed
        legend.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        legend.title = element_blank())



#### WAITING MONTHS ####

# Prepare prediction sets for each waiting month level
pred_set_0 <- pred_set_baseline %>% 
  mutate(waiting_months = 0)

pred_set_12 <- pred_set_baseline %>% 
  mutate(waiting_months = 12)

pred_set_24 <- pred_set_baseline %>% 
  mutate(waiting_months = 24)

pred_set_96 <- pred_set_baseline %>% 
  mutate(waiting_months = 96)

# Get predictions for each waiting month level
preds_0 <- 1 - predict(used_model,
                       newdata = pred_set_0,
                       type = "survival",
                       times = times,
                       conf.int = TRUE)$.pred[[1]]$.pred_survival

preds_0_upper <- 1 - predict(used_model,
                             newdata = pred_set_0,
                             type = "survival",
                             times = times,
                             conf.int = TRUE)$.pred[[1]]$.pred_lower

preds_0_lower <- 1 - predict(used_model,
                             newdata = pred_set_0,
                             type = "survival",
                             times = times,
                             conf.int = TRUE)$.pred[[1]]$.pred_upper

preds_12 <- 1 - predict(used_model,
                        newdata = pred_set_12,
                        type = "survival",
                        times = times)$.pred[[1]]$.pred_survival

preds_12_upper <- 1 - predict(used_model,
                              newdata = pred_set_12,
                              type = "survival",
                              times = times,
                              conf.int = TRUE)$.pred[[1]]$.pred_lower

preds_12_lower <- 1 - predict(used_model,
                              newdata = pred_set_12,
                              type = "survival",
                              times = times,
                              conf.int = TRUE)$.pred[[1]]$.pred_upper

preds_24 <- 1 - predict(used_model,
                        newdata = pred_set_24,
                        type = "survival",
                        times = times)$.pred[[1]]$.pred_survival

preds_24_upper <- 1 - predict(used_model,
                              newdata = pred_set_24,
                              type = "survival",
                              times = times,
                              conf.int = TRUE)$.pred[[1]]$.pred_lower

preds_24_lower <- 1 - predict(used_model,
                              newdata = pred_set_24,
                              type = "survival",
                              times = times,
                              conf.int = TRUE)$.pred[[1]]$.pred_upper

preds_96 <- 1 - predict(used_model,
                        newdata = pred_set_96,
                        type = "survival",
                        times = times)$.pred[[1]]$.pred_survival

preds_96_upper <- 1 - predict(used_model,
                              newdata = pred_set_96,
                              type = "survival",
                              times = times,
                              conf.int = TRUE)$.pred[[1]]$.pred_lower

preds_96_lower <- 1 - predict(used_model,
                              newdata = pred_set_96,
                              type = "survival",
                              times = times,
                              conf.int = TRUE)$.pred[[1]]$.pred_upper

# Combine predictions into data frames
data_waiting_months <- data.frame(
  times = times,
  waiting_0 = preds_0,
  waiting_12 = preds_12,
  waiting_24 = preds_24,
  waiting_96 = preds_96
)

data_waiting_months_upper <- data.frame(
  times = times,
  waiting_0 = preds_0_upper,
  waiting_12 = preds_12_upper,
  waiting_24 = preds_24_upper,
  waiting_96 = preds_96_upper
)

data_waiting_months_lower <- data.frame(
  times = times,
  waiting_0 = preds_0_lower,
  waiting_12 = preds_12_lower,
  waiting_24 = preds_24_lower,
  waiting_96 = preds_96_lower
)

# Transform data for ggplot
data_long_waiting_months <- pivot_longer(data_waiting_months, 
                                         cols = c(waiting_0, waiting_12, waiting_24, waiting_96), 
                                         names_to = "WaitingMonths", 
                                         values_to = "preds")

data_long_waiting_months_lower <- pivot_longer(data_waiting_months_lower, 
                                               cols = c(waiting_0, waiting_12, waiting_24, waiting_96), 
                                               names_to = "WaitingMonths", 
                                               values_to = "lower")

data_bounds_waiting_months <- pivot_longer(data_waiting_months_upper, 
                                           cols = c(waiting_0, waiting_12, waiting_24, waiting_96), 
                                           names_to = "WaitingMonths", 
                                           values_to = "upper") %>% 
  left_join(data_long_waiting_months_lower);

# Create the line plot for waiting months
waitmonths_preds <- ggplot(data_long_waiting_months, aes(x = times, y = preds, color = WaitingMonths)) +
  geom_line() +   
  geom_ribbon(data = data_bounds_waiting_months, aes(ymin = lower, 
                                                     ymax = upper, 
                                                     fill = WaitingMonths, 
                                                     x = times), 
              alpha = 0.2,
              inherit.aes = FALSE,
              color = NA) +  
  labs(title = "Waiting time",    
       x = "Time (days)",                 
       y = "") + 
  theme_minimal() +                    
  scale_color_manual(values = c("waiting_0" = "blue", "waiting_12" = "forestgreen", 
                                "waiting_24" = "red", "waiting_96" = "purple"),
                     labels = c("waiting_0" = "0 years", "waiting_12" = "1 year", 
                                "waiting_24" = "2 years", "waiting_96" = "4 years"),
                     breaks = c("waiting_0", "waiting_12", "waiting_24", "waiting_96")) +
  scale_fill_manual(values = c("waiting_0" = "blue", "waiting_12" = "forestgreen", 
                               "waiting_24" = "red", "waiting_96" = "purple"),
                    labels = c("waiting_0" = "0 years", "waiting_12" = "1 year", 
                               "waiting_24" = "2 years", "waiting_96" = "4 years"),
                    breaks = c("waiting_0", "waiting_12", "waiting_24", "waiting_96")) +
  theme(legend.position = c(0.6, 0.5),  # Adjust x and y coordinates as needed
        legend.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        legend.title = element_blank())



#### ETHNICITY ####

# Prepare prediction sets for each ethnicity
pred_set_white <- pred_set_baseline %>% 
  mutate(ethnicity = factor("White", levels = levels(modelling_data$ethnicity)))

pred_set_asian <- pred_set_baseline %>% 
  mutate(ethnicity = factor("Asian", levels = levels(modelling_data$ethnicity)))

pred_set_indigenous <- pred_set_baseline %>% 
  mutate(ethnicity = factor("Indigenous", levels = levels(modelling_data$ethnicity)))

pred_set_other <- pred_set_baseline %>% 
  mutate(ethnicity = factor("Other", levels = levels(modelling_data$ethnicity)))

# Get predictions for each ethnicity
preds_white <- 1 - predict(used_model,
                           newdata = pred_set_white,
                           type = "survival",
                           times = times,
                           conf.int = TRUE)$.pred[[1]]$.pred_survival

preds_white_upper <- 1 - predict(used_model,
                                 newdata = pred_set_white,
                                 type = "survival",
                                 times = times,
                                 conf.int = TRUE)$.pred[[1]]$.pred_lower

preds_white_lower <- 1 - predict(used_model,
                                 newdata = pred_set_white,
                                 type = "survival",
                                 times = times,
                                 conf.int = TRUE)$.pred[[1]]$.pred_upper

preds_asian <- 1 - predict(used_model,
                           newdata = pred_set_asian,
                           type = "survival",
                           times = times)$.pred[[1]]$.pred_survival

preds_asian_upper <- 1 - predict(used_model,
                                 newdata = pred_set_asian,
                                 type = "survival",
                                 times = times,
                                 conf.int = TRUE)$.pred[[1]]$.pred_lower

preds_asian_lower <- 1 - predict(used_model,
                                 newdata = pred_set_asian,
                                 type = "survival",
                                 times = times,
                                 conf.int = TRUE)$.pred[[1]]$.pred_upper

preds_indigenous <- 1 - predict(used_model,
                                newdata = pred_set_indigenous,
                                type = "survival",
                                times = times)$.pred[[1]]$.pred_survival

preds_indigenous_upper <- 1 - predict(used_model,
                                      newdata = pred_set_indigenous,
                                      type = "survival",
                                      times = times,
                                      conf.int = TRUE)$.pred[[1]]$.pred_lower

preds_indigenous_lower <- 1 - predict(used_model,
                                      newdata = pred_set_indigenous,
                                      type = "survival",
                                      times = times,
                                      conf.int = TRUE)$.pred[[1]]$.pred_upper

preds_other <- 1 - predict(used_model,
                           newdata = pred_set_other,
                           type = "survival",
                           times = times)$.pred[[1]]$.pred_survival

preds_other_upper <- 1 - predict(used_model,
                                 newdata = pred_set_other,
                                 type = "survival",
                                 times = times,
                                 conf.int = TRUE)$.pred[[1]]$.pred_lower

preds_other_lower <- 1 - predict(used_model,
                                 newdata = pred_set_other,
                                 type = "survival",
                                 times = times,
                                 conf.int = TRUE)$.pred[[1]]$.pred_upper

# Combine predictions into data frames
data_ethnicity <- data.frame(
  times = times,
  white = preds_white,
  asian = preds_asian,
  indigenous = preds_indigenous,
  other = preds_other
)

data_ethnicity_upper <- data.frame(
  times = times,
  white = preds_white_upper,
  asian = preds_asian_upper,
  indigenous = preds_indigenous_upper,
  other = preds_other_upper
)

data_ethnicity_lower <- data.frame(
  times = times,
  white = preds_white_lower,
  asian = preds_asian_lower,
  indigenous = preds_indigenous_lower,
  other = preds_other_lower
)

# Transform data for ggplot
data_long_ethnicity <- pivot_longer(data_ethnicity, 
                                    cols = c(white, asian, indigenous, other), 
                                    names_to = "Ethnicity", 
                                    values_to = "preds")

data_long_ethnicity_lower <- pivot_longer(data_ethnicity_lower, 
                                          cols = c(white, asian, indigenous, other), 
                                          names_to = "Ethnicity", 
                                          values_to = "lower")

data_bounds_ethnicity <- pivot_longer(data_ethnicity_upper, 
                                      cols = c(white, asian, indigenous, other), 
                                      names_to = "Ethnicity", 
                                      values_to = "upper") %>% 
  left_join(data_long_ethnicity_lower);

# Create the line plot for ethnicity
ethnicity_preds <- ggplot(data_long_ethnicity, aes(x = times, y = preds, color = Ethnicity)) +
  geom_line() +   
  geom_ribbon(data = data_bounds_ethnicity, aes(ymin = lower, 
                                                ymax = upper, 
                                                fill = Ethnicity, 
                                                x = times), 
              alpha = 0.2,
              inherit.aes = FALSE,
              color = NA) +  
  labs(title = "Ethnicity",    
       x = "",                 
       y = "Predicted Probability of Kidney Offer") + 
  theme_minimal() +                    
  scale_color_manual(values = c("white" = "blue", "asian" = "forestgreen", 
                                "indigenous" = "red", "other" = "purple"),
                     labels = c("white" = "White", "asian" = "Asian", 
                                "indigenous" = "Indigenous", "other" = "Other"),
                     breaks = c("indigenous", "white", "asian", "other")) +
  scale_fill_manual(values = c("white" = "blue", "asian" = "forestgreen", 
                               "indigenous" = "red", "other" = "purple"),
                    labels = c("white" = "White", "asian" = "Asian", 
                               "indigenous" = "Indigenous", "other" = "Other"),
                    breaks = c("indigenous", "white", "asian", "other")) +
  theme(legend.position = c(0.6, 0.5),  # Adjust x and y coordinates as needed
        legend.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        legend.title = element_blank())

#### GRAFT NUMBER ####

# Prepare prediction sets for each graft number
pred_set_graft1 <- pred_set_baseline %>% 
  mutate(graftno = 1)

pred_set_graft2 <- pred_set_baseline %>% 
  mutate(graftno = 2)


# Get predictions for each graft number
preds_graft1 <- 1 - predict(used_model,
                            newdata = pred_set_graft1,
                            type = "survival",
                            times = times,
                            conf.int = TRUE)$.pred[[1]]$.pred_survival

preds_graft1_upper <- 1 - predict(used_model,
                                  newdata = pred_set_graft1,
                                  type = "survival",
                                  times = times,
                                  conf.int = TRUE)$.pred[[1]]$.pred_lower

preds_graft1_lower <- 1 - predict(used_model,
                                  newdata = pred_set_graft1,
                                  type = "survival",
                                  times = times,
                                  conf.int = TRUE)$.pred[[1]]$.pred_upper

preds_graft2 <- 1 - predict(used_model,
                            newdata = pred_set_graft2,
                            type = "survival",
                            times = times)$.pred[[1]]$.pred_survival

preds_graft2_upper <- 1 - predict(used_model,
                                  newdata = pred_set_graft2,
                                  type = "survival",
                                  times = times,
                                  conf.int = TRUE)$.pred[[1]]$.pred_lower

preds_graft2_lower <- 1 - predict(used_model,
                                  newdata = pred_set_graft2,
                                  type = "survival",
                                  times = times,
                                  conf.int = TRUE)$.pred[[1]]$.pred_upper


# Combine predictions into data frames
data_graft <- data.frame(
  times = times,
  graft1 = preds_graft1,
  graft2 = preds_graft2
)

data_graft_upper <- data.frame(
  times = times,
  graft1 = preds_graft1_upper,
  graft2 = preds_graft2_upper
)

data_graft_lower <- data.frame(
  times = times,
  graft1 = preds_graft1_lower,
  graft2 = preds_graft2_lower
)

# Transform data for ggplot
data_long_graft <- pivot_longer(data_graft, 
                                cols = c(graft1, graft2), 
                                names_to = "GraftNo", 
                                values_to = "preds")

data_long_graft_lower <- pivot_longer(data_graft_lower, 
                                      cols = c(graft1, graft2), 
                                      names_to = "GraftNo", 
                                      values_to = "lower")

data_bounds_graft <- pivot_longer(data_graft_upper, 
                                  cols = c(graft1, graft2), 
                                  names_to = "GraftNo", 
                                  values_to = "upper") %>% 
  left_join(data_long_graft_lower);

# Create the line plot for graft number
graftno_preds <- ggplot(data_long_graft, aes(x = times, y = preds, color = GraftNo)) +
  geom_line() +   
  geom_ribbon(data = data_bounds_graft, aes(ymin = lower, 
                                            ymax = upper, 
                                            fill = GraftNo, 
                                            x = times), 
              alpha = 0.2,
              inherit.aes = FALSE,
              color = NA) +  
  labs(title = "",    
       x = "",                 
       y = "") + 
  theme_minimal() +                    
  scale_color_manual(values = c("graft1" = "blue", "graft2" = "red"),
                     labels = c("graft1" = "No", 
                                "graft2" = "Yes"),
                     breaks = c("graft1", "graft2")) +
  scale_fill_manual(values = c("graft1" = "blue", "graft2" = "red"),
                    labels = c("graft1" = "No", 
                               "graft2" = "Yes"),
                    breaks = c("graft1", "graft2")) +
  theme(legend.position = c(0.7, 0.4),  # Adjust x and y coordinates as needed
        legend.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        legend.title = element_blank()) +
  labs(title = "Prior transplants?")

#### SEX ####

# Prepare prediction sets for female and male groups
pred_set_female <- pred_set_baseline %>% 
  mutate(female = 1)

pred_set_male <- pred_set_baseline %>% 
  mutate(female = 0)

# Get predictions for each group
preds_female <- 1 - predict(used_model,
                            newdata = pred_set_female,
                            type = "survival",
                            times = times,
                            conf.int = TRUE)$.pred[[1]]$.pred_survival

preds_female_upper <- 1 - predict(used_model,
                                  newdata = pred_set_female,
                                  type = "survival",
                                  times = times,
                                  conf.int = TRUE)$.pred[[1]]$.pred_lower

preds_female_lower <- 1 - predict(used_model,
                                  newdata = pred_set_female,
                                  type = "survival",
                                  times = times,
                                  conf.int = TRUE)$.pred[[1]]$.pred_upper

preds_male <- 1 - predict(used_model,
                          newdata = pred_set_male,
                          type = "survival",
                          times = times)$.pred[[1]]$.pred_survival

preds_male_upper <- 1 - predict(used_model,
                                newdata = pred_set_male,
                                type = "survival",
                                times = times,
                                conf.int = TRUE)$.pred[[1]]$.pred_lower

preds_male_lower <- 1 - predict(used_model,
                                newdata = pred_set_male,
                                type = "survival",
                                times = times,
                                conf.int = TRUE)$.pred[[1]]$.pred_upper

# Combine predictions into data frames
data_gender <- data.frame(
  times = times,
  female = preds_female,
  male = preds_male
)

data_gender_upper <- data.frame(
  times = times,
  female = preds_female_upper,
  male = preds_male_upper
)

data_gender_lower <- data.frame(
  times = times,
  female = preds_female_lower,
  male = preds_male_lower
)

# Transform data for ggplot
data_long_gender <- pivot_longer(data_gender, 
                                 cols = c(female, male), 
                                 names_to = "Gender", 
                                 values_to = "preds")

data_long_gender_lower <- pivot_longer(data_gender_lower, 
                                       cols = c(female, male), 
                                       names_to = "Gender", 
                                       values_to = "lower")

data_bounds_gender <- pivot_longer(data_gender_upper, 
                                   cols = c(female, male), 
                                   names_to = "Gender", 
                                   values_to = "upper") %>% 
  left_join(data_long_gender_lower);

# Create the line plot for gender
sex_preds <- ggplot(data_long_gender, aes(x = times, y = preds, color = Gender)) +
  geom_line() +   
  geom_ribbon(data = data_bounds_gender, aes(ymin = lower, 
                                             ymax = upper, 
                                             fill = Gender, 
                                             x = times), 
              alpha = 0.2,
              inherit.aes = FALSE,
              color = NA) +  
  labs(title = "Sex",    
       x = "Time (days)",                 
       y = "") + 
  theme_minimal() +                    
  scale_color_manual(values = c("female" = "red", "male" = "blue"),
                     labels = c("female" = "Female", "male" = "Male"),
                     breaks = c("female", "male")) +
  scale_fill_manual(values = c("female" = "red", "male" = "blue"),
                    labels = c("female" = "Female", "male" = "Male"),
                    breaks = c("female", "male")) +
  theme(legend.position = c(0.5, 0.4),  # Adjust x and y coordinates as needed
        legend.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        legend.title = element_blank())

#### COMORBIDITIES ####

# Prepare prediction sets for each comorbcount level
pred_set_comorb0 <- pred_set_baseline %>% 
  mutate(comorbcount = 0)

pred_set_comorb1 <- pred_set_baseline %>% 
  mutate(comorbcount = 1)

# Get predictions for each comorbcount level
preds_comorb0 <- 1 - predict(used_model,
                             newdata = pred_set_comorb0,
                             type = "survival",
                             times = times,
                             conf.int = TRUE)$.pred[[1]]$.pred_survival

preds_comorb0_upper <- 1 - predict(used_model,
                                   newdata = pred_set_comorb0,
                                   type = "survival",
                                   times = times,
                                   conf.int = TRUE)$.pred[[1]]$.pred_lower

preds_comorb0_lower <- 1 - predict(used_model,
                                   newdata = pred_set_comorb0,
                                   type = "survival",
                                   times = times,
                                   conf.int = TRUE)$.pred[[1]]$.pred_upper

preds_comorb1 <- 1 - predict(used_model,
                             newdata = pred_set_comorb1,
                             type = "survival",
                             times = times)$.pred[[1]]$.pred_survival

preds_comorb1_upper <- 1 - predict(used_model,
                                   newdata = pred_set_comorb1,
                                   type = "survival",
                                   times = times,
                                   conf.int = TRUE)$.pred[[1]]$.pred_lower

preds_comorb1_lower <- 1 - predict(used_model,
                                   newdata = pred_set_comorb1,
                                   type = "survival",
                                   times = times,
                                   conf.int = TRUE)$.pred[[1]]$.pred_upper


# Combine predictions into data frames
data_comorb <- data.frame(
  times = times,
  comorb0 = preds_comorb0,
  comorb1 = preds_comorb1
)

data_comorb_upper <- data.frame(
  times = times,
  comorb0 = preds_comorb0_upper,
  comorb1 = preds_comorb1_upper
)

data_comorb_lower <- data.frame(
  times = times,
  comorb0 = preds_comorb0_lower,
  comorb1 = preds_comorb1_lower
)

# Transform data for ggplot
data_long_comorb <- pivot_longer(data_comorb, 
                                 cols = c(comorb0, comorb1), 
                                 names_to = "ComorbCount", 
                                 values_to = "preds")

data_long_comorb_lower <- pivot_longer(data_comorb_lower, 
                                       cols = c(comorb0, comorb1), 
                                       names_to = "ComorbCount", 
                                       values_to = "lower")

data_bounds_comorb <- pivot_longer(data_comorb_upper, 
                                   cols = c(comorb0, comorb1), 
                                   names_to = "ComorbCount", 
                                   values_to = "upper") %>% 
  left_join(data_long_comorb_lower);

# Create the line plot for comorbcount
comorb_preds <- ggplot(data_long_comorb, aes(x = times, y = preds, color = ComorbCount)) +
  geom_line() +   
  geom_ribbon(data = data_bounds_comorb, aes(ymin = lower, 
                                             ymax = upper, 
                                             fill = ComorbCount, 
                                             x = times), 
              alpha = 0.2,
              inherit.aes = FALSE,
              color = NA) +  
  labs(title = "Comorbidities",    
       x = "",                 
       y = "") + 
  theme_minimal() +                    
  scale_color_manual(values = c("comorb0" = "blue", "comorb1" = "red"),
                     labels = c("comorb0" = "0", 
                                "comorb1" = "1+"),
                     breaks = c("comorb0", "comorb1")) +
  scale_fill_manual(values = c("comorb0" = "blue", "comorb1" = "red"),
                    labels = c("comorb0" = "0", 
                               "comorb1" = "1+"),
                    breaks = c("comorb0", "comorb1")) +
  theme(legend.position = c(0.5, 0.6),  # Adjust x and y coordinates as needed
        legend.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        legend.title = element_blank())

#### PRA CATEGORIES ####

pred_set_pra0 <- pred_set_baseline %>% 
  mutate(mpra = 0)

pred_set_pra50 <- pred_set_baseline %>% 
  mutate(mpra = 50)  # PRA 50

pred_set_pra80 <- pred_set_baseline %>% 
  mutate(mpra = 80)  # Changed from 90 to 80

pred_set_pra99 <- pred_set_baseline %>% 
  mutate(mpra = 99)

# Get predictions for each PRA level
preds_pra0 <- 1 - predict(used_model,
                          newdata = pred_set_pra0,
                          type = "survival",
                          times = times,
                          conf.int = TRUE)$.pred[[1]]$.pred_survival

preds_pra0_upper <- 1 - predict(used_model,
                                newdata = pred_set_pra0,
                                type = "survival",
                                times = times,
                                conf.int = TRUE)$.pred[[1]]$.pred_lower

preds_pra0_lower <- 1 - predict(used_model,
                                newdata = pred_set_pra0,
                                type = "survival",
                                times = times,
                                conf.int = TRUE)$.pred[[1]]$.pred_upper

preds_pra50 <- 1 - predict(used_model,
                           newdata = pred_set_pra50,
                           type = "survival",
                           times = times)$.pred[[1]]$.pred_survival

preds_pra50_upper <- 1 - predict(used_model,
                                 newdata = pred_set_pra50,
                                 type = "survival",
                                 times = times,
                                 conf.int = TRUE)$.pred[[1]]$.pred_lower

preds_pra50_lower <- 1 - predict(used_model,
                                 newdata = pred_set_pra50,
                                 type = "survival",
                                 times = times,
                                 conf.int = TRUE)$.pred[[1]]$.pred_upper

preds_pra80 <- 1 - predict(used_model,
                           newdata = pred_set_pra80,
                           type = "survival",
                           times = times)$.pred[[1]]$.pred_survival

preds_pra80_upper <- 1 - predict(used_model,
                                 newdata = pred_set_pra80,
                                 type = "survival",
                                 times = times,
                                 conf.int = TRUE)$.pred[[1]]$.pred_lower

preds_pra80_lower <- 1 - predict(used_model,
                                 newdata = pred_set_pra80,
                                 type = "survival",
                                 times = times,
                                 conf.int = TRUE)$.pred[[1]]$.pred_upper

preds_pra99 <- 1 - predict(used_model,
                           newdata = pred_set_pra99,
                           type = "survival",
                           times = times)$.pred[[1]]$.pred_survival

preds_pra99_upper <- 1 - predict(used_model,
                                 newdata = pred_set_pra99,
                                 type = "survival",
                                 times = times,
                                 conf.int = TRUE)$.pred[[1]]$.pred_lower

preds_pra99_lower <- 1 - predict(used_model,
                                 newdata = pred_set_pra99,
                                 type = "survival",
                                 times = times,
                                 conf.int = TRUE)$.pred[[1]]$.pred_upper

# Combine predictions into data frames
data_pra <- data.frame(
  times = times,
  pra0 = preds_pra0,
  pra50 = preds_pra50,  # Updated to include 50
  pra80 = preds_pra80,  # Updated to include 80
  pra99 = preds_pra99
)

data_pra_upper <- data.frame(
  times = times,
  pra0 = preds_pra0_upper,
  pra50 = preds_pra50_upper,  # Updated to include 50
  pra80 = preds_pra80_upper,  # Updated to include 80
  pra99 = preds_pra99_upper
)

data_pra_lower <- data.frame(
  times = times,
  pra0 = preds_pra0_lower,
  pra50 = preds_pra50_lower,  # Updated to include 50
  pra80 = preds_pra80_lower,  # Updated to include 80
  pra99 = preds_pra99_lower
)

# Transform data for ggplot
data_long_pra <- pivot_longer(data_pra, 
                              cols = c(pra0, pra50, pra80, pra99), 
                              names_to = "PRA", 
                              values_to = "preds")

data_long_pra_lower <- pivot_longer(data_pra_lower, 
                                    cols = c(pra0, pra50, pra80, pra99), 
                                    names_to = "PRA", 
                                    values_to = "lower")

data_bounds_pra <- pivot_longer(data_pra_upper, 
                                cols = c(pra0, pra50, pra80, pra99), 
                                names_to = "PRA", 
                                values_to = "upper") %>% 
  left_join(data_long_pra_lower);

# Create the line plot for PRA categories
pra_preds <- ggplot(data_long_pra, aes(x = times, y = preds, color = PRA)) +
  geom_line() +   
  geom_ribbon(data = data_bounds_pra, aes(ymin = lower, 
                                          ymax = upper, 
                                          fill = PRA, 
                                          x = times), 
              alpha = 0.2,
              inherit.aes = FALSE,
              color = NA) +  
  labs(title = "PRA",    
       x = "Time (days)",                 
       y = "") + 
  theme_minimal() +                    
  scale_color_manual(values = c("pra0" = "blue", "pra50" = "forestgreen", 
                                "pra80" = "red", "pra99" = "purple"),
                     labels = c("pra0" = "0", 
                                "pra50" = "50",  # Updated label
                                "pra80" = "80",  # Updated label
                                "pra99" = "99"),
                     breaks = c("pra0", "pra50", "pra80", "pra99")) +
  scale_fill_manual(values = c("pra0" = "blue", "pra50" = "forestgreen", 
                               "pra80" = "red", "pra99" = "purple"),
                    labels = c("pra0" = "0", 
                               "pra50" = "50",  # Updated label
                               "pra80" = "80",  # Updated label
                               "pra99" = "99"),
                    breaks = c("pra0", "pra50", "pra80", "pra99")) +
  theme(legend.position = c(0.6, 0.4),  # Adjust x and y coordinates as needed
        legend.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        legend.title = element_blank())





#### PUT THEM ALL TOGETHER (except year, gets its own plot) ####

png("~/Documents/masters/wpp/prediction_plots.png", width = 1000, 
    height = 1000, res = 100)


ggarrange(age_preds, blood_group_preds, comorb_preds, 
          ethnicity_preds, graftno_preds, kidney_disease_preds,
          pra_preds, sex_preds, waitmonths_preds, 
          ncol = 3, 
          nrow = 3)

dev.off()

## Year plot

png("~/Documents/masters/wpp/year_pred_plot.png", width = 1000, 
    height = 1000, res = 100)


year_preds

dev.off()

#### 'EXTREME' OBSERVATIONS ####
# observations found from searching the data

## Obs 1

pred_set_obs_1 <- data.frame(
  age = 65,                            
  female = 1,                           
  graftno = 1,                         
  bloodgroup = factor("O", levels = levels(modelling_data$bloodgroup)),
  kidneydisease = factor("Other", levels = levels(modelling_data$kidneydisease)),
  comorbcount = 0,
  ethnicity = factor("Indigenous", levels = levels(modelling_data$ethnicity)), 
  year = 2019,
  mpra = 98.3,
  waiting_months = 29.6  
)
# prediction curves with CIs for this obs
preds_1_upper <-  1 - predict(used_model,
                                     newdata = pred_set_obs_1,
                                     type = "survival",
                                     times = times,
                                     conf.int = TRUE)$.pred[[1]]$.pred_lower

preds_1 <-  1 - predict(used_model,
                               newdata = pred_set_obs_1,
                               type = "survival",
                               times = times,
                               conf.int = TRUE)$.pred[[1]]$.pred_survival

preds_1_lower <-  1 - predict(used_model,
                                     newdata = pred_set_obs_1,
                                     type = "survival",
                                     times = times,
                                     conf.int = TRUE)$.pred[[1]]$.pred_upper

## Obs 2

pred_set_obs_2 <- data.frame(
  age = 51,                            
  female = 1,                           
  graftno = 2,                         
  bloodgroup = factor("A", levels = levels(modelling_data$bloodgroup)),
  kidneydisease = factor("Glomerular disease", levels = levels(modelling_data$kidneydisease)),
  comorbcount = 1,
  ethnicity = factor("Asian", levels = levels(modelling_data$ethnicity)), 
  year = 2019,
  mpra = 90,
  waiting_months = 44  
)

preds_2_upper <-  1 - predict(used_model,
                                     newdata = pred_set_obs_2,
                                     type = "survival",
                                     times = times,
                                     conf.int = TRUE)$.pred[[1]]$.pred_lower

preds_2 <-  1 - predict(used_model,
                               newdata = pred_set_obs_2,
                               type = "survival",
                               times = times,
                               conf.int = TRUE)$.pred[[1]]$.pred_survival

preds_2_lower <-  1 - predict(used_model,
                                     newdata = pred_set_obs_2,
                                     type = "survival",
                                     times = times,
                                     conf.int = TRUE)$.pred[[1]]$.pred_upper

## Obs 3

pred_set_obs_3 <- data.frame(
  age = 29,                            
  female = 1,                           
  graftno = 1,                         
  bloodgroup = factor("AB", levels = levels(modelling_data$bloodgroup)),  
  comorbcount = 0,
  kidneydisease = factor("Reflux nephropathy", levels = levels(modelling_data$kidneydisease)),
  ethnicity = factor("Asian", levels = levels(modelling_data$ethnicity)), 
  year = 2019,
  mpra = 3,
  waiting_months = 11  
)

preds_3_upper <-  1 - predict(used_model,
                              newdata = pred_set_obs_3,
                              type = "survival",
                              times = times,
                              conf.int = TRUE)$.pred[[1]]$.pred_lower

preds_3 <-  1 - predict(used_model,
                        newdata = pred_set_obs_3,
                        type = "survival",
                        times = times,
                        conf.int = TRUE)$.pred[[1]]$.pred_survival

preds_3_lower <-  1 - predict(used_model,
                              newdata = pred_set_obs_3,
                              type = "survival",
                              times = times,
                              conf.int = TRUE)$.pred[[1]]$.pred_upper

# Create data frames for predictions
data_preds <- data.frame(
  times = times,
  baseline = preds_baseline,
  obs_1 = preds_1,
  obs_2 = preds_2,
  obs_3 = preds_3
)

data_preds_upper <- data.frame(
  times = times,
  baseline = preds_baseline_upper,
  obs_1 = preds_1_upper,
  obs_2 = preds_2_upper,
  obs_3 = preds_3_upper
)

data_preds_lower <- data.frame(
  times = times,
  baseline = preds_baseline_lower,
  obs_1 = preds_1_lower,
  obs_2 = preds_2_lower,
  obs_3 = preds_3_lower
)

# Reshape data to long format
data_long_preds <- pivot_longer(data_preds, 
                                cols = c(baseline, obs_1, obs_2, obs_3), 
                                names_to = "Category", 
                                values_to = "preds")

data_long_preds_lower <- pivot_longer(data_preds_lower, 
                                      cols = c(baseline, obs_1, obs_2, obs_3), 
                                      names_to = "Category", 
                                      values_to = "lower")

data_bounds_preds <- pivot_longer(data_preds_upper, 
                                  cols = c(baseline, obs_1, obs_2, obs_3), 
                                  names_to = "Category", 
                                  values_to = "upper") %>% 
  left_join(data_long_preds_lower, by = c("times", "Category"));

# Create the prediction plot
png("~/Documents/masters/wpp/prediction_plot_obs.png", width = 1000, 
    height = 1000, res = 100)

ggplot(data_long_preds, aes(x = times, y = preds, color = Category)) +
  geom_line() +   
  geom_ribbon(data = data_bounds_preds, aes(ymin = lower, 
                                            ymax = upper, 
                                            fill = Category, 
                                            x = times), 
              alpha = 0.2,
              inherit.aes = FALSE,
              color = NA) +  
  labs(title = "",
       x = "Days",                 
       y = "Predicted probability of transplant") + 
  theme_minimal() +                    
  scale_color_manual(values = c("baseline" = "blue", "obs_1" = "forestgreen", 
                                "obs_2" = "red", "obs_3" = "purple"),
                     labels = c("baseline" = "Baseline", "obs_1" = "Obs 1", 
                                "obs_2" = "Obs 2", "obs_3" = "Obs 3")) +
  scale_fill_manual(values = c("baseline" = "blue", "obs_1" = "forestgreen", 
                               "obs_2" = "red", "obs_3" = "purple"),
                    labels = c("baseline" = "Baseline", "obs_1" = "Obs 1", 
                               "obs_2" = "Obs 2", "obs_3" = "Obs 3")) +
  theme(legend.position = c(0.8, 0.6),  # Adjust x and y coordinates as needed
        legend.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        legend.title = element_blank())

 dev.off()
 
 ## Point estimates
 
 preds_baseline_point <-  predict(used_model,
                           newdata = pred_set_baseline,
                           type = "quantile",
                           p = c(0.25, 0.5, 0.75))$.pred # 25.4 (11.4 - 56.7)
 
 preds_1_point <-  predict(used_model,
                           newdata = pred_set_obs_1,
                           type = "quantile",
                           p = c(0.25, 0.5, 0.75))$.pred # 13.1 (5.87 - 29.2)
 
 preds_2_point <-  predict(used_model,
                           newdata = pred_set_obs_2,
                           type = "quantile",
                           p = c(0.25, 0.5, 0.75))$.pred # 488 (219 - 1088)
 
  preds_3_point <-  predict(used_model,
                           newdata = pred_set_obs_3,
                           type = "quantile",
                           p = c(0.25, 0.5, 0.75))$.pred # 5.06 (2.27 - 11.3)

                           
