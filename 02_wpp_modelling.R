## Load libraries for modelling
library(survival)
library(dplyr)
library(lubridate)
library(tidyr)
library(randomForestSRC) # Required for mlr3 random forest model
library(caret) # cross validation for AFT modelling
library(ggsurvfit)
library(splines)
library(ggplot2)
library(gtsummary)
library(skimr)
library(survex)
library(ggpubr)
library(flexsurv)

## Read in analysis data
analysis_data_raw <- cohort_timetonextoffer

summary_dat <- analysis_data_raw %>% 
  na.omit()

dim(summary_dat)
table(summary_dat$offer)
table(summary_dat$outcome)
km <- survfit(Surv(time, offer) ~ 1,
              data = summary_dat)
# Median time to event
summary(km)$table["median"]
quantile(km$time, probs = c(0.25, 0.75))


## Table 1

analysis_data_raw %>% 
  select(ppn, age, mpra, female, bloodgroup, kidneydisease, comorbcount,
         prevtxcount, ethnicity) %>% 
  group_by(ppn) %>% 
  # One obs per person, at their youngest entry into waiting list in df
  mutate(min_age = min(age)) %>% 
  filter(min_age == age) %>% 
  ungroup() %>% 
  na.omit() %>% 
  select(-ppn, -min_age) %>% 
  tbl_summary


# Blood group/PRA missing in 166 no default, but account for only 5%, exclude
# Perform sensitivity to see if imputing blood group changes results
# Ethnicity missing in 27, <1% exclude


## Create analysis data
analysis_data <- analysis_data_raw %>%
  na.omit() %>% 
  mutate(startnum = as.numeric(start),
         endnum = as.numeric(end)) %>%
  # Drop NAs and select relevant columns
  select(ppn, time, offer, age, female, bloodgroup, kidneydisease,
         comorbcount, start, end, mpra,
         startnum, endnum, waiting_months, outcome, graftno, ethnicity) %>%
  # Add seq variable
  group_by(ppn) %>%
  arrange(start) %>%
  mutate(waitseq = row_number()) %>%
  ungroup()

## Median follow-up time across individuals
follow_up_time_by_ppn <- analysis_data %>% 
  select(ppn, time) %>% 
  group_by(ppn) %>% 
  mutate(total_followup = sum(time)) %>% 
  ungroup() %>% 
  select(ppn, total_followup) %>% 
  distinct()

median(follow_up_time_by_ppn$total_followup)

#### Get survival summary stats ####

fit <- survfit(Surv(analysis_data$time, analysis_data$offer)~1)
summary(fit)

##### Now split by year #####

# Create a vector of dates
cut_dates <- as.numeric(seq(from = as.Date("2006-01-01"), 
                            to = as.Date("2020-01-01"), 
                            by = "year"))

## Split into year categories (one row per year)

modelling_data <- survSplit(Surv(endnum,
                                      offer) ~ .,
                                 analysis_data,
                                 cut = cut_dates,
                                 start = "startnum",
                                 end = "endnum") %>% 
  # Survsplit function included times 0 - 1 jan 2006, remove 
  # start num of 0
  filter(startnum != 0) %>%
  # Format dates
  mutate(startnum = as.Date(startnum),
          endnum = as.Date(endnum)) %>%
  # Survsplit function assumed start date was always 1 jan, replace as actual
  # start date or 1 jan of subsequent years
  mutate(startnum = pmax(start, startnum)) %>%
  # People who started post 2006 had extra years before start date, remove these
  filter(endnum >= startnum) %>%
  # Only remove records where start and end are on same days if that day
  # was 1 Jan, as 1 Jan was the cutpoint day (but only if they had subsequent
  # rows for this waiting period)
  group_by(ppn, waitseq) %>%
  mutate(yearseq = row_number()) %>%
  mutate(waitseqcount = n()) %>%
  ungroup() %>%
  filter(!(startnum == endnum & day(endnum) == 1 & month(endnum) == 1 & 
             waitseqcount != 1 & yearseq == 1)) %>%
  # Replace start and end dates
  mutate(start = startnum,
         end = endnum) %>%
  select(-startnum, -endnum) %>%
  # Add year variable
  mutate(year = year(start)) %>%
  # Update time-varying variables
  group_by(ppn, waitseq) %>%
  mutate(waitseq_start = min(start)) %>%
  ungroup() %>%
  mutate(added_years = as.numeric(start - waitseq_start)/365.25,
          age = age + added_years,,
          waiting_months = waiting_months + 12*added_years) %>%
# Re-calculate time
  mutate(time = as.numeric(end - start)+1) %>% 
  # Change comorbidity count to factor
  mutate(comorbcount = case_when(comorbcount == 0 ~ 0,
                                           TRUE ~ 1)) %>% 
  # Reduce to modelling variables - using splines instead of cont. variables
  select(-waitseq_start, -waitseq, -waitseqcount, -added_years, -yearseq, -start, -end, -outcome)  %>% 
  mutate(graft_str = as.character(graftno),
         seq_id = paste(ppn, graft_str, sep = "_")) %>% 
  select(-ppn, -graft_str)

##### Likely covariates: full cox model #####

full_cox <- coxph(data = modelling_data,
                     formula = Surv(time,
                                    offer) ~
                       age*bloodgroup + female*bloodgroup + kidneydisease*bloodgroup +
                       waiting_months*bloodgroup + ethnicity +
                       year*bloodgroup + mpra*bloodgroup +
                    bloodgroup*comorbcount + bloodgroup*graftno)

full_cox

Anova(full_cox)


#### KM plot ####

png("~/Documents/masters/wpp/km_plot.png", width = 800, height = 600, res = 100)

  
survfit(Surv(time, offer)
               ~ 1,
               data = na.omit(analysis_data_raw)) %>% 
  ggsurvfit() +
  add_risktable() +
  labs(x = "Days",
       y = "Proportion with no kidney offer") +
  scale_x_continuous(expand=c(0, 0), limits=c(0, 365)) +
  scale_y_continuous(expand=c(0, 0), limits=c(0, 1)) +  # Add labels every 30 days
  xlim(0, 365) +
  ylim(0, 1)

dev.off()

  
##### Visualise assessment of days per offer by factors #####

### Cox regression comparing ###

## Make a binned age variable
breaks <- seq(15, 85, by = 10)
labels <- paste0(breaks[-length(breaks)], "-", breaks[-1])

modelling_data_age_bins <- na.omit(modelling_data) %>%
  mutate(age_group = factor(cut(age, 
                                breaks = breaks, 
                                labels = labels, 
                                right = FALSE),
                            levels = labels)
         )

## Run cox model
age_bin_cox <- coxph(data = modelling_data_age_bins,
                     formula = Surv(time,
                                    offer) ~
                       age_group + female + bloodgroup +
                       waiting_months + kidneydisease +
                       comorbcount + year + mpra + graftno + year + ethnicity)

## Summarize cox model to find HRs by age group
hr_summary <- summary(age_bin_cox)
hr_df <- data.frame(
  age_group = factor(labels, levels=labels),
  hazard_ratio = c(1, (hr_summary$coefficients[1:6, "exp(coef)"])),
  lower_ci = c(1, (hr_summary$conf.int[1:6, "lower .95"])),
  upper_ci = c(1, (hr_summary$conf.int[1:6, "upper .95"]))
)

hr_df <- hr_df[!hr_df$age_group == "(Intercept)", ]

age_plot <- ggplot(hr_df, aes(x = age_group, y = hazard_ratio)) + 
  geom_point() +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
  labs(title = "",
       x = "Age",
       y = "Hazard Ratio (95% CI)") +
  theme_minimal() +
scale_y_log10()

# Visual assessment implies one knot

## Make a binned pra variable
breaks <- seq(0, 100, by = 10)
labels <- paste0(breaks[-length(breaks)], "-", breaks[-1])

modelling_data_pra_bins <- na.omit(modelling_data) %>%
  mutate(pra_group = factor(cut(mpra, 
                                breaks = breaks, 
                                labels = labels, 
                                right = FALSE),
                            levels = labels)
  )

## Run cox model
pra_bin_cox <- coxph(data = modelling_data_pra_bins,
                     formula = Surv(time,
                                    offer) ~
                       pra_group + female + bloodgroup +
                       waiting_months  + age + ethnicity + kidneydisease +
                       comorbcount + year)

## Summarize cox model to find HRs by PRA group
hr_summary <- summary(pra_bin_cox)
hr_df <- data.frame(
  pra_group = factor(labels, levels=labels),
  hazard_ratio = c(1, (hr_summary$coefficients[1:9, "exp(coef)"])),
  lower_ci = c(1, (hr_summary$conf.int[1:9, "lower .95"])),
  upper_ci = c(1, (hr_summary$conf.int[1:9, "upper .95"]))
)

hr_df <- hr_df[!hr_df$pra_group == "(Intercept)", ]

pra_plot <- ggplot(hr_df, aes(x = pra_group, y = hazard_ratio)) + 
  geom_point() +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
  labs(title = "",
       x = "PRA",
       y = "Hazard Ratio (95% CI)") +
  theme_minimal() +
  scale_y_log10()

# One knot

### Years

year_bin_cox <- coxph(data = na.omit(modelling_data),
                     formula = Surv(time,
                                    offer) ~
                       factor(year) +female + bloodgroup +
                       waiting_months  + age + ethnicity + kidneydisease +
                       comorbcount)

## Summarize cox model to find HRs by age group
hr_summary <- summary(year_bin_cox)
hr_df <- data.frame(
  year = seq(min(modelling_data$year),max(modelling_data$year)),
  hazard_ratio = c(1, (hr_summary$coefficients[1:13, "exp(coef)"])),
  lower_ci = c(1, (hr_summary$conf.int[1:13, "lower .95"])),
  upper_ci = c(1, (hr_summary$conf.int[1:13, "upper .95"]))
)

hr_df <- hr_df[!hr_df$year == "(Intercept)", ]

year_plot <- ggplot(hr_df, aes(x = year, y = hazard_ratio)) + 
  geom_point() +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
  labs(title = "",
       x = "Year",
       y = "Hazard Ratio (95% CI)") +
  theme_minimal() +
  scale_y_log10() +
  scale_x_continuous(breaks = hr_df$year) 

# linear

## Make a binned waiting months variable
## Make a binned waiting months variable
breaks <- seq(0, 60, by = 6)  # 0 to 96 months (8 years) in 12-month intervals
labels <- paste0(breaks[-length(breaks)], "-", breaks[-1])  # Create labels for bins

modelling_data_wait_bins <- modelling_data %>%
  mutate(waiting_month_bin = factor(cut(waiting_months, 
                                         breaks = breaks, 
                                         labels = labels, 
                                         right = FALSE),
                                     levels = labels)) %>%
  filter(waiting_months <= 60) %>% 
  na.omit()

month_bin_wait <- coxph(data = modelling_data_wait_bins,
                        formula = Surv(time, offer) ~
                          waiting_month_bin + year + mpra + female +
                          kidneydisease + age + ethnicity + comorbcount)

hr_summary <- summary(month_bin_wait)

hr_df <- data.frame(
  wait_bin = factor(labels, levels = labels),  # Ensure factor levels match new bins
  hazard_ratio = c(1, hr_summary$coefficients[1:9, "exp(coef)"]),  # Adjust for 8 bins
  lower_ci = c(1, hr_summary$conf.int[1:9, "lower .95"]),  # Adjust for 8 bins
  upper_ci = c(1, hr_summary$conf.int[1:9, "upper .95"])  # Adjust for 8 bins
)

wait_plot <- ggplot(hr_df, aes(x = wait_bin, y = hazard_ratio)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
  labs(title = "",
       x = "Waiting months",
       y = "Hazard Ratio (95% CI)") +
  theme_minimal() +
  scale_y_log10()

## Merge plots

png("~/Documents/masters/wpp/hr_comparison_plots.png", width = 1200, height = 600, res = 100)

ggarrange(age_plot, pra_plot, wait_plot, year_plot)

dev.off()

##### Build different models using flexsurv, assess using survex #####

set.seed(2147)

# Split into training and test folds by seq id (ppn per graftno)

folds <- groupKFold(modelling_data$seq_id, k = 10)

## KM estimate (reference of uninformative model)

brier_scores_km <- c()

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
  
  model_timetonextoffer <- coxph(Surv(time, offer) ~ 1,
                                 data = train)
  
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
  brier_scores_km <- c(brier_scores_km, ibs)
}

## Parametric AFT distributions

distributions <- c("lnorm", "weibull", "genf", "gengamma", "llogis")

brier_mat <- c()

for (dist in distributions) {
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
    
    
    model_timetonextoffer <- flexsurvreg(Surv(time, offer) ~ ns(age, df = 2) +
                                           ns(mpra, df = 1)*bloodgroup + ns(waiting_months, df = 2)*bloodgroup +
                                           year*bloodgroup + female + kidneydisease*bloodgroup +
                                           graftno + bloodgroup*comorbcount +
                                           ethnicity,
                                         data = train,
                                         dist = dist,
                                         hess.control = list(tol.evalues = 1e-2),
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
  brier_mat[[paste("brier_scores", dist, sep = "_")]] <- brier_scores
}

## Spline AFT distributions

knots <- c(1, 2, 3, 4, 5)

brier_mat_spl <- c()

# Coefficients for inits in flexsurvspline
fl1 <- flexsurvspline(Surv(time,offer)~1,data=train,k=1) 

for (knot in knots) {
  brier_scores_spline <- c()
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
    
    model_timetonextoffer <- flexsurvspline(Surv(time, offer) ~ ns(age, df = 2) +
                                              ns(mpra, df = 1)*bloodgroup + ns(waiting_months, df = 2)*bloodgroup +
                                              year*bloodgroup + female + kidneydisease*bloodgroup +
                                              graftno + comorbcount*bloodgroup +
                                              ethnicity, 
                                            k = knot, 
                                            data = train,
                                            inits = c(coef(fl1), 0))
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
    brier_scores_spline <- c(brier_scores_spline, ibs)
  }
  brier_mat_spl[[paste("brier_scores", knot, "knot", sep = "_")]] <- brier_scores_spline
}

## Random forest

brier_scores_rf <- c()

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
  
  model_timetonextoffer <- rfsrc(Surv(time, offer) ~ age + mpra + 
                                   waiting_months + female +
                                   year + graftno +  kidneydisease +
                                   comorbcount*bloodgroup + bloodgroup +
                                   ethnicity,
                                 data = train)
  
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
  brier_scores_rf <- c(brier_scores_rf, ibs)
}

#### Merge into one dataset, save as a result ####

brier_scores_comparison <- cbind(as.data.frame(brier_mat), as.data.frame(brier_mat_spl)) %>% 
  as.data.frame() 

saveRDS(brier_scores_comparison, file = "~/Documents/masters/wpp/model_brier_scores.RDS")

brier_scores_plot_df <- brier_scores_comparison %>%
  t() %>% 
  as.data.frame() %>% 
  mutate(
    start = pmin(V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, na.rm = TRUE),
    end = pmax(V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, na.rm = TRUE),
    mean = rowMeans(across(V1:V10), na.rm = TRUE),
    model = c("Log-Normal", "Weibull", "Generalised F", 
              "Generalised Gamma", "Log-Logistic", "Spline 1 knot", "Spline 2 knot",
              "Spline 3 knot", "Spline 4 knot", "Spline 5 knot"))

model_levels = c("Spline 5 knot", "Spline 4 knot", 
                 "Spline 3 knot", "Spline 2 knot", "Spline 1 knot","Weibull", 
                 "Log-Normal", "Log-Logistic",
                 "Generalised Gamma", "Generalised F"
                 )

## Plot it!

png("~/Documents/masters/wpp/brier_plot.png", width = 800, height = 600, res = 100)


ggplot(data = brier_scores_plot_df, 
       aes(x = factor(model, levels = model_levels), y = mean)) +
  # Add lines instead of crossbars
  geom_segment(aes(xend = factor(model, levels = model_levels), yend = start), 
               size = 1, color = "black") +  # Line from mean to start
  geom_segment(aes(xend = factor(model, levels = model_levels), yend = end), 
               size = 1, color = "black") +  # Line from mean to end
  geom_point(size = 3, color = "blue") +  # Dot on the mean
  coord_flip() +
  xlab("") + 
  ylab("Integrated Brier Score") +
  scale_y_continuous(limits = c(0, 0.15), 
                     breaks = seq(0, 0.15, by = 0.02)) +  # Labels at every 0.02
  theme_minimal() +
  theme(
    panel.border = element_blank(),
    plot.background = element_blank(),
    axis.ticks.x = element_blank(),  # Remove x-axis ticks if needed
    panel.grid.major.y = element_blank(),  # Remove major y grid lines
    panel.grid.minor.y = element_blank()   # Remove minor y grid lines
  ) +
  # Add grid lines at every 0.01
  geom_hline(yintercept = seq(0, 0.15, by = 0.01), 
             linetype = "dotted", color = "grey") +
  # Add red dotted lines at specified y-values
  geom_hline(yintercept = mean(brier_scores_rf), linetype = "dotted", color = "red") +
  geom_hline(yintercept = mean(brier_scores_km), linetype = "dotted", color = "red") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
  geom_text(aes(x = 0, y = 0.01, label = "Perfect prediction"), 
            vjust = -3, color = "black") +
  geom_text(aes(x = 9, y = mean(brier_scores_rf), label = "Random forest prediction"), 
            vjust = -1, color = "black") +
  geom_text(aes(x = 0, y = mean(brier_scores_km), label = "Uninformative prediction"), 
            vjust = -1, color = "black") +
    # Add a solid horizontal line separating the labels from the graph
  geom_segment(aes(y = 0, yend = Inf, x = 0, xend = 0), 
               color = "black", size = 0.5)




dev.off()


 