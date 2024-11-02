# Utilises objects and libraries initiated in 02_wpp_modelling

dat <- modelling_data


## Build models ####

model_1 <- flexsurvreg(Surv(time, offer) ~ ns(age, df = 2) +
                         ns(mpra, df = 1)*bloodgroup + ns(waiting_months, df = 2)*bloodgroup +
                         year*bloodgroup + female + kidneydisease*bloodgroup +
                         graftno + comorbcount*bloodgroup +
                         ethnicity,
                       data = dat,
                       dist = 'genf',
                       hess.control = list(tol.evalues = 1e-2),
                       method = "Nelder-Mead")

model_2 <- flexsurvreg(Surv(time, offer) ~ ns(age, df = 2) +
                         ns(mpra, df = 1)*bloodgroup + ns(waiting_months, df = 2)*bloodgroup +
                         year*bloodgroup + female + kidneydisease*bloodgroup +
                         graftno + comorbcount*bloodgroup +
                         ethnicity,
                       data = dat,
                       dist = 'gengamma',
                       hess.control = list(tol.evalues = 1e-2),
                       method = "Nelder-Mead")

model_3 <- flexsurvreg(Surv(time, offer) ~ ns(age, df = 2) +
                         ns(mpra, df = 1)*bloodgroup + ns(waiting_months, df = 2)*bloodgroup +
                         year*bloodgroup + female + kidneydisease*bloodgroup +
                         graftno + comorbcount*bloodgroup +
                         ethnicity,
                       data = dat,
                       dist = 'lnorm',
                       hess.control = list(tol.evalues = 1e-2),
                       method = "Nelder-Mead")

model_4 <- flexsurvreg(Surv(time, offer) ~ ns(age, df = 2) +
                         ns(mpra, df = 1)*bloodgroup + ns(waiting_months, df = 2)*bloodgroup +
                         year*bloodgroup + female + kidneydisease*bloodgroup +
                         graftno + comorbcount*bloodgroup +
                         ethnicity,
                       data = dat,
                       dist = 'llogis',
                       hess.control = list(tol.evalues = 1e-2),
                       method = "Nelder-Mead")

model_5 <- flexsurvreg(Surv(time, offer) ~ ns(age, df = 2) +
                         ns(mpra, df = 1)*bloodgroup + ns(waiting_months, df = 2)*bloodgroup +
                         year*bloodgroup + female + kidneydisease*bloodgroup +
                         graftno + comorbcount*bloodgroup +
                         ethnicity,
                       data = dat,
                       dist = 'weibull',
                       hess.control = list(tol.evalues = 1e-2),
                       method = "Nelder-Mead")

fl1 <- flexsurvspline(Surv(time,offer)~1,data=dat,k=1) 

model_6 <- flexsurvspline(Surv(time, offer) ~ ns(age, df = 2) +
                            ns(mpra, df = 1)*bloodgroup + ns(waiting_months, df = 2)*bloodgroup +
                            year*bloodgroup + female + kidneydisease*bloodgroup +
                            graftno + comorbcount*bloodgroup +
                            ethnicity, 
                          k = 1, 
                          data = dat,
                          inits = c(coef(fl1), 0))

model_7 <- flexsurvspline(Surv(time, offer) ~ ns(age, df = 2) +
                            ns(mpra, df = 1)*bloodgroup + ns(waiting_months, df = 2)*bloodgroup +
                            year*bloodgroup + female + kidneydisease*bloodgroup +
                            graftno + comorbcount*bloodgroup +
                            ethnicity, 
                          k = 2, 
                          data = dat,
                          inits = c(coef(fl1), 0))

model_8 <- flexsurvspline(Surv(time, offer) ~ ns(age, df = 2) +
                            ns(mpra, df = 1)*bloodgroup + ns(waiting_months, df = 2)*bloodgroup +
                            year*bloodgroup + female + kidneydisease*bloodgroup +
                            graftno + comorbcount*bloodgroup +
                            ethnicity, 
                          k = 3, 
                          data = dat,
                          inits = c(coef(fl1), 0))

model_9 <- flexsurvspline(Surv(time, offer) ~ ns(age, df = 2) +
                            ns(mpra, df = 1)*bloodgroup + ns(waiting_months, df = 2)*bloodgroup +
                            year*bloodgroup + female + kidneydisease*bloodgroup +
                            graftno + comorbcount*bloodgroup +
                            ethnicity, 
                          k = 4, 
                          data = dat,
                          inits = c(coef(fl1), 0))

model_10 <- flexsurvspline(Surv(time, offer) ~ ns(age, df = 2) +
                             ns(mpra, df = 1)*bloodgroup + ns(waiting_months, df = 2)*bloodgroup +
                             year*bloodgroup + female + kidneydisease*bloodgroup +
                             graftno + comorbcount*bloodgroup +
                             ethnicity, 
                          k = 5, 
                          data = dat,
                          inits = c(coef(fl1), 0))

model_references <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

# Choose cut points to assess calibration at (chosen by trial and error)
cuts_7 <- seq(0.58, 0.82, by = 0.08)
cuts_14 <- seq(0.48, 0.72, by = 0.08)
cuts_30 <- seq(0.3, 0.54, by = 0.08)

# Initiate loop to gen calibration plot for each model
for(ref in model_references) {
  model <- get(paste0("model_", ref))
  
  ## One week calibration ####

  # Find predictions
  pred_7_days = predict(model,
                        type = "survival",
                        newdata = dat,
                        times = 7)

  # Bin predictions into appropriate cut point
  data_with_preds <- cbind(dat, pred_7_days) %>% 
    select(time, offer, .pred_survival) %>% 
    mutate(surv_bin = case_when(
      between(.pred_survival, 0.58, 0.66) ~ 0.58,
      between(.pred_survival, 0.66, 0.74) ~ 0.66,
      between(.pred_survival, 0.74, 0.82) ~ 0.74,
      between(.pred_survival, 0.82, 0.9) ~ 0.82
    ))
  
  actual_7 <- c()

  # Find KM survival probability at one week for all people in 
  # relevant bin
  for (cut in cuts_7) {
    data_surv_bin <- data_with_preds %>% 
      filter(near(surv_bin, cut)) 
  
      km_fit <- survfit(Surv(time, offer) ~ 1, data = data_surv_bin)
      surv_prob <- summary(km_fit, times = 7)$surv
      actual_7 <- c(actual_7, surv_prob)
    
  }
  
  ## Two weeks ####
  
  pred_14_days = predict(model,
                         type = "survival",
                         newdata = dat,
                         times = 14)
  
  data_with_preds <- cbind(dat, pred_14_days) %>% 
    select(time, offer, .pred_survival) %>% 
    mutate(surv_bin = case_when(
      between(.pred_survival, 0.4, 0.48) ~ 0.4,
      between(.pred_survival, 0.48, 0.56) ~ 0.48,
      between(.pred_survival, 0.56, 0.64) ~ 0.56,
      between(.pred_survival, 0.64, 0.72) ~ 0.64,
      between(.pred_survival, 0.72, 0.8) ~ 0.72
    ))
  
  actual_14 <- c()
  
  for (cut in cuts_14) {
    data_surv_bin <- data_with_preds %>% 
      filter(near(surv_bin, cut)) 
 
      km_fit <- survfit(Surv(time, offer) ~ 1, data = data_surv_bin)
      surv_prob <- summary(km_fit, times = 14)$surv
      actual_14 <- c(actual_14, surv_prob)
  
  }
  
  ## 1 month ####
  
  pred_30_days = predict(model,
                         type = "survival",
                         newdata = dat,
                         times = 30)
  
  data_with_preds <- cbind(dat, pred_30_days) %>% 
    mutate(surv_bin = case_when(
      between(.pred_survival, 0.3, 0.38) ~ 0.3,
      between(.pred_survival, 0.38, 0.46) ~ 0.38,
      between(.pred_survival, 0.46, 0.54) ~ 0.46,
      between(.pred_survival, 0.54, 0.62) ~ 0.54
    ))
  
  actual_30 <- c()
  
  for (cut in cuts_30) {
    data_surv_bin <- data_with_preds %>% 
      filter(near(surv_bin, cut)) 

      km_fit <- survfit(Surv(time, offer) ~ 1, data = data_surv_bin)
      surv_prob <- summary(km_fit, times = 30)$surv
      actual_30 <- c(actual_30, surv_prob)
    }

  #### Make a single calibration object
  model_calibration_name <- paste0("model_", ref, "_calibration")
  assign(model_calibration_name, cbind(cuts_7, actual_7, cuts_14, actual_14, cuts_30, actual_30), envir = .GlobalEnv)
}

## Arrange calibrations for plotting

model_calibrations <- list(
  model_1_calibration,
  model_2_calibration,
  model_3_calibration,
  model_4_calibration,
  model_5_calibration,
  model_6_calibration,
  model_7_calibration,
  model_8_calibration,
  model_9_calibration,
  model_10_calibration
)

#### IT'S TIME TO PLOT ####

for (i in seq_along(model_calibrations)) {
  # Get the current model data
  model_data <- model_calibrations[[i]]
  
  # Create the plot
  p <- ggplot(model_data, aes(x = 1-cuts_7, y = 1-actual_7)) +
    scale_x_continuous () +
    # Add the dashed line (x-y line for perfect calibration)
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
    # Add the red line-connected dot plot for 7 days
    geom_line(aes(color = '7 days')) +
    geom_point(aes(color = '7 days', shape = '7 days'), size = 3) +
    # Add the blue line-connected dot plot for 14 days
    geom_line(aes(x = 1-cuts_14, y = 1-actual_14, color = '14 days')) +
    geom_point(aes(x = 1-cuts_14, y = 1-actual_14, color = '14 days', shape = '14 days'), size = 3) +
    # Add the green line-connected dot plot for 30 days
    geom_line(aes(x = 1-cuts_30, y = 1-actual_30, color = '30 days')) +
    geom_point(aes(x = 1-cuts_30, y = 1-actual_30, color = '30 days', shape = '30 days'), size = 3) +
    labs(title = "",
         x = "",
         y = "") +
  # Different colours and shapes for different timepoints
    scale_color_manual(name='Calibration at',
                       breaks=c('7 days', '14 days', '30 days'),
                       values=c('7 days'='red', '14 days'='blue', '30 days'='forestgreen')) +
    scale_shape_manual(name = 'Calibration at',
                       breaks = c('7 days', '14 days', '30 days'),
                       values = c('7 days' = 16, '14 days' = 17, '30 days' = 18)) +  # Define distinct shapes
    # Set axis limits and labels
    xlim(0, 1) +
    ylim(0, 1) +
    theme_minimal() +
    theme(legend.position = "none")
  
  # Create a new object with a unique name for each plot
  plot_name <- paste0("model_", i, "_plot")
  assign(plot_name, p, envir = .GlobalEnv)
}

# Add names to plots and axis titles (where necessary)
model_1_plot <- model_1_plot + labs(title = "Generalised F") + theme(plot.title = element_text(hjust = 0.5))
model_2_plot <- model_2_plot + labs(title = "Generalised Gamma") + theme(plot.title = element_text(hjust = 0.5))
model_3_plot <- model_3_plot + labs(title = "Log-Logistic") + theme(plot.title = element_text(hjust = 0.5))
model_4_plot <- model_4_plot + labs(title = "Log-Normal") + theme(plot.title = element_text(hjust = 0.5))
model_5_plot <- model_5_plot + labs(title = "Weibull",
                                    y = "Observed probability") + theme(plot.title = element_text(hjust = 0.5))
model_6_plot <- model_6_plot + labs(title = "Spline 1 knot") + theme(plot.title = element_text(hjust = 0.5))

model_7_plot <- model_7_plot + labs(title = "Spline 2 knots") + theme(plot.title = element_text(hjust = 0.5))
model_8_plot <- model_8_plot + labs(title = "Spline 3 knots") + theme(plot.title = element_text(hjust = 0.5))
model_9_plot <- model_9_plot + labs(title = "Spline 4 knots",
                                    x = "Predicted probability") + theme(plot.title = element_text(hjust = 0.5))
model_10_plot <- model_10_plot + labs(title = "Spline 5 knots",
                                    x = "Predicted probability") + theme(plot.title = element_text(hjust = 0.5))

png("~/Documents/masters/wpp/calib_plots.png", width = 1000, height = 1000, res = 100)

# Arrange plots
ggarrange(model_1_plot, model_2_plot, model_3_plot, 
          model_4_plot, model_5_plot, model_6_plot, 
          model_7_plot, model_8_plot, model_9_plot, 
          model_10_plot,
          ncol = 2, nrow = 5,
          common.legend = TRUE,
          legend = "bottom")

dev.off()
