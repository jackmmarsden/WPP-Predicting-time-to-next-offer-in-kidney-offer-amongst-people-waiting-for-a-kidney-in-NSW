### Sensitivity analysis testing predictions with complete cases data and data with missing PRA

## Utilises objects and libraries initiated in 02_wpp_modelling

# complete cases data is just what was used before
omitted_dat <- modelling_data

# repeat earlier steps without omitting all NAs to derive modelling data including people with missing PRAs
analysis_data_miss <- analysis_data_raw %>%
  filter(!is.na(ethnicity)) %>% 
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

# Create a vector of dates
cut_dates <- as.numeric(seq(from = as.Date("2006-01-01"), 
                            to = as.Date("2020-01-01"), 
                            by = "year"))

## Split into year categories (one row per year)

modelling_data_miss <- survSplit(Surv(endnum,
                                 offer) ~ .,
                            analysis_data_miss,
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

# Complete model from model building step
omit_model <- flexsurvreg(Surv(time, offer) ~ 
                            ns(age, df = 2) +
                            bloodgroup*ns(mpra, df = 1) + 
                            bloodgroup*ns(waiting_months, df = 2) +
                            bloodgroup*ns(year, df = 4) +
                            graftno + 
                            female +
                            bloodgroup*comorbcount +
                            bloodgroup*kidneydisease +
                            ethnicity,
                          data = omitted_dat %>% 
                            mutate(graftno = case_when(graftno == 1 ~ 1,
                                                       TRUE ~ 2)),
                          dist = 'llogis',
                          hess.control = list(tol.evalues = 1),
                          method = "Nelder-Mead")

# Identical model assuming PRA is 0 when PRA is missing
inclusive_model_pra_0 <- flexsurvreg(Surv(time, offer) ~ 
                                       ns(age, df = 2) +
                                       bloodgroup*ns(mpra, df = 1) + 
                                       bloodgroup*ns(waiting_months, df = 2) +
                                       bloodgroup*ns(year, df = 4) +
                                       graftno + 
                                       female +
                                       bloodgroup*comorbcount +
                                       bloodgroup*kidneydisease +
                                       ethnicity,
                          data = modelling_data_miss %>% 
                            mutate(graftno = case_when(graftno == 1 ~ 1,
                                                       TRUE ~ 2),
                                   mpra = case_when(is.na(mpra) ~ 0,
                                                    TRUE ~ mpra)),
                          dist = 'llogis',
                          hess.control = list(tol.evalues = 1),
                          method = "Nelder-Mead")

# Identical model assuming PRA is 100 when PRA is missing
inclusive_model_pra_100 <- flexsurvreg(Surv(time, offer) ~ 
                                         ns(age, df = 2) +
                                         bloodgroup*ns(mpra, df = 1) + 
                                         bloodgroup*ns(waiting_months, df = 2) +
                                         bloodgroup*ns(year, df = 4) +
                                         graftno + 
                                         female +
                                         bloodgroup*comorbcount +
                                         bloodgroup*kidneydisease +
                                         ethnicity,
                                     data = modelling_data_miss %>% 
                                       mutate(graftno = case_when(graftno == 1 ~ 1,
                                                                  TRUE ~ 2),
                                              mpra = case_when(is.na(mpra) ~ 100,
                                                               TRUE ~ mpra)),
                                     dist = 'llogis',
                                     hess.control = list(tol.evalues = 1),
                                     method = "Nelder-Mead")

# Make hypothetical patients to test predictions on

pred_set_baseline <- data.frame(
  age = 50,                            
  female = 0,                           
  graftno = 1,                         
  bloodgroup = factor("O", levels = levels(modelling_data$bloodgroup)),
  kidneydisease = factor("Glomerular disease", levels = levels(modelling_data$kidneydisease)),
  comorbcount = 0,
  ethnicity = factor("White", levels = levels(modelling_data$ethnicity)), 
  year = 2019,
  mpra = 0,
  waiting_months = 0  
)

pred_set_best_outcome <- data.frame(
  age = 20,                            
  female = 0,                           
  graftno = 1,                         
  bloodgroup = factor("AB", levels = levels(modelling_data$bloodgroup)),
  kidneydisease = factor("Hypertension", levels = levels(modelling_data$kidneydisease)),
  comorbcount = 0,
  ethnicity = factor("White", levels = levels(modelling_data$ethnicity)), 
  year = 2019,
  mpra = 0,
  waiting_months = 0  
)

pred_set_worst_outcome <- data.frame(
  age = 65,                            
  female = 1,                           
  graftno = 2,                         
  bloodgroup = factor("B", levels = levels(modelling_data$bloodgroup)),
  kidneydisease = factor("Reflux nephropathy", levels = levels(modelling_data$kidneydisease)),
  comorbcount = 1,
  ethnicity = factor("Indigenous", levels = levels(modelling_data$ethnicity)), 
  year = 2019,
  mpra = 100,
  waiting_months = 200  
)

# Predict time to event in each case

preds_omit_baseline<-  predict(omit_model,
                          newdata = pred_set_baseline,
                          type = "quantile",
                          p = c(0.25, 0.5, 0.75))$.pred

preds_omit_best<-  predict(omit_model,
                          newdata = pred_set_best_outcome,
                          type = "quantile",
                          p = c(0.25, 0.5, 0.75))$.pred

preds_omit_worst <-  predict(omit_model,
                          newdata = pred_set_worst_outcome,
                          type = "quantile",
                          p = c(0.25, 0.5, 0.75))$.pred

# Predictions for inclusive_model_pra_0
preds_0_baseline <- predict(inclusive_model_pra_0,
                            newdata = pred_set_baseline,
                            type = "quantile",
                            p = c(0.25, 0.5, 0.75))$.pred

preds_0_best <- predict(inclusive_model_pra_0,
                        newdata = pred_set_best_outcome,
                        type = "quantile",
                        p = c(0.25, 0.5, 0.75))$.pred

preds_0_worst <- predict(inclusive_model_pra_0,
                         newdata = pred_set_worst_outcome,
                         type = "quantile",
                         p = c(0.25, 0.5, 0.75))$.pred

# Predictions for inclusive_model_pra_100
preds_100_baseline <- predict(inclusive_model_pra_100,
                              newdata = pred_set_baseline,
                              type = "quantile",
                              p = c(0.25, 0.5, 0.75))$.pred

preds_100_best <- predict(inclusive_model_pra_100,
                          newdata = pred_set_best_outcome,
                          type = "quantile",
                          p = c(0.25, 0.5, 0.75))$.pred

preds_100_worst <- predict(inclusive_model_pra_100,
                           newdata = pred_set_worst_outcome,
                           type = "quantile",
                           p = c(0.25, 0.5, 0.75))$.pred

