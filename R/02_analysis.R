#!/usr/bin/env Rscript

# 02_analysis.R
# Canonical ECON 475 forecasting script (course SUBMIT version), with
# repository-relative paths. Estimation and plotting logic is unchanged.

source("R/_project_root.R")
root <- find_project_root()
setwd(root)

# ECON 475 - Economic Forecasting Final Project
# One-time installation, if needed:
# install.packages(c("ggplot2", "dplyr", "forecast", "aTSA", "fGarch"))


required_packages <- c("ggplot2", "dplyr", "forecast", "aTSA", "fGarch")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop(
    paste0(
      "Install the following course packages before running the script: ",
      paste(missing_packages, collapse = ", ")
    )
  )
}

library(ggplot2)
library(dplyr)
library(forecast)

options(scipen = 999)

figures_dir <- file.path(root, "figures", "additional")
outputs_dir <- file.path(root, "outputs")
dir.create(figures_dir, showWarnings = FALSE)
dir.create(outputs_dir, showWarnings = FALSE)

save_text <- function(object, filename) {
  capture.output(object, file = file.path(outputs_dir, filename))
}

save_acf <- function(x, filename, title, lag_max = 20) {
  png(file.path(figures_dir, filename), width = 1800, height = 1200, res = 200)
  acf(x, lag.max = lag_max, main = title, na.action = na.pass)
  dev.off()
}

save_acf_pacf <- function(x, filename, series_label, lag_max = 20) {
  png(file.path(figures_dir, filename), width = 1800, height = 900, res = 180)
  par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
  acf(
    x,
    lag.max = lag_max,
    main = paste0("ACF: ", series_label),
    na.action = na.pass
  )
  pacf(
    x,
    lag.max = lag_max,
    main = paste0("PACF: ", series_label),
    na.action = na.pass
  )
  par(mfrow = c(1, 1))
  dev.off()
}

significant_acf_count <- function(x, lag_max) {
  acf_values <- as.numeric(
    acf(x, lag.max = lag_max, plot = FALSE, na.action = na.pass)$acf
  )[-1]
  threshold <- 1.96 / sqrt(sum(!is.na(x)))
  sum(abs(acf_values) > threshold, na.rm = TRUE)
}

acf_table <- function(x, lag_max) {
  values <- acf(
    x,
    lag.max = lag_max,
    plot = FALSE,
    na.action = na.pass
  )
  data.frame(
    Lag = as.integer(values$lag[-1]),
    ACF = as.numeric(values$acf[-1]),
    Significant_95pct = abs(as.numeric(values$acf[-1])) >
      1.96 / sqrt(sum(!is.na(x)))
  )
}

theme_course <- theme_bw() +
  theme(
    text = element_text(family = "Palatino"),
    panel.grid = element_blank(),
    legend.position = "bottom"
  )

###############################################################################
# Question 1 - Electricity retail sales
###############################################################################

elec <- read.csv(file.path(root, "data", "elec.csv"), header = TRUE, sep = ",")
stopifnot(nrow(elec) == 468)
stopifnot(!anyNA(elec$date), !anyNA(elec$elec))

elec$date_original <- elec$date
elec$date <- as.Date(
  paste0(substr(elec$date_original, 1, 4), "-", substr(elec$date_original, 6, 7), "-01")
)
stopifnot(!anyNA(elec$date), !anyDuplicated(elec$date))
elec$y <- log(elec$elec)
elec$time <- seq_len(nrow(elec))
elec$time2 <- elec$time^2
elec$month <- factor(as.numeric(format(elec$date, "%m")), levels = 1:12)

q1_training <- elec %>% filter(date <= as.Date("2010-12-01"))
q1_test <- elec %>% filter(date >= as.Date("2011-01-01"))
stopifnot(nrow(q1_training) == 456, nrow(q1_test) == 12)

# Q1(a): log electricity retail sales over time
q1_plot_a <- ggplot(elec, aes(x = date, y = y)) +
  geom_line(color = "forestgreen", linewidth = 0.5) +
  labs(
    title = "Log of U.S. Electricity Retail Sales",
    x = "Year",
    y = "Log electricity retail sales"
  ) +
  theme_course
ggsave(
  file.path(figures_dir, "q1_a_log_electricity.png"),
  q1_plot_a,
  width = 8.2,
  height = 4.8,
  dpi = 220
)

# Q1(b): linear and quadratic trends
q1_linear <- lm(y ~ time, data = q1_training)
q1_quadratic <- lm(y ~ time + time2, data = q1_training)

q1_trend_comparison <- data.frame(
  Model = c("Linear trend", "Quadratic trend"),
  AIC = c(AIC(q1_linear), AIC(q1_quadratic)),
  BIC = c(BIC(q1_linear), BIC(q1_quadratic)),
  Residual_SE = c(
    summary(q1_linear)$sigma,
    summary(q1_quadratic)$sigma
  )
)
write.csv(
  q1_trend_comparison,
  file.path(outputs_dir, "q1_trend_comparison.csv"),
  row.names = FALSE
)
save_text(summary(q1_linear), "q1_linear_summary.txt")
save_text(summary(q1_quadratic), "q1_quadratic_summary.txt")
q1_trend_coefficients <- bind_rows(
  lapply(
    list("Linear trend" = q1_linear, "Quadratic trend" = q1_quadratic),
    function(model) {
      table <- as.data.frame(summary(model)$coefficients)
      table$Parameter <- rownames(table)
      rownames(table) <- NULL
      table
    }
  ),
  .id = "Model"
)
write.csv(
  q1_trend_coefficients,
  file.path(outputs_dir, "q1_trend_coefficients.csv"),
  row.names = FALSE
)

q1_trend_choice <- if (
  BIC(q1_quadratic) < BIC(q1_linear)
) "Quadratic trend" else "Linear trend"
q1_trend_model <- if (
  q1_trend_choice == "Quadratic trend"
) q1_quadratic else q1_linear

write.csv(
  data.frame(Selected_trend = q1_trend_choice),
  file.path(outputs_dir, "q1_selected_trend.csv"),
  row.names = FALSE
)

# Q1(c): residual correlogram from the selected trend
save_acf(
  residuals(q1_trend_model),
  "q1_c_trend_residual_acf.png",
  "ACF of Selected Trend Residuals",
  lag_max = 25
)
write.csv(
  acf_table(residuals(q1_trend_model), 25),
  file.path(outputs_dir, "q1_trend_residual_acf.csv"),
  row.names = FALSE
)

# Q1(d): selected trend plus a full set of monthly dummy variables
if (q1_trend_choice == "Quadratic trend") {
  q1_deterministic <- lm(y ~ -1 + month + time + time2, data = q1_training)
} else {
  q1_deterministic <- lm(y ~ -1 + month + time, data = q1_training)
}
save_text(summary(q1_deterministic), "q1_deterministic_summary.txt")

q1_training$deterministic_fitted <- fitted(q1_deterministic)
q1_training$deterministic_residual <- residuals(q1_deterministic)

# Q1(e): deterministic residuals
q1_plot_e <- ggplot(q1_training, aes(x = date, y = deterministic_residual)) +
  geom_line(color = "forestgreen", linewidth = 0.5) +
  geom_hline(yintercept = 0, color = "gray50", linewidth = 0.3) +
  labs(
    title = "Residuals from the Trend and Seasonal Model",
    x = "Year",
    y = "Residual"
  ) +
  theme_course
ggsave(
  file.path(figures_dir, "q1_e_deterministic_residuals.png"),
  q1_plot_e,
  width = 8.2,
  height = 4.4,
  dpi = 220
)
save_acf_pacf(
  q1_training$deterministic_residual,
  "q1_e_deterministic_residual_acf_pacf.png",
  "trend and seasonal residuals",
  lag_max = 25
)
write.csv(
  acf_table(q1_training$deterministic_residual, 25),
  file.path(outputs_dir, "q1_deterministic_residual_acf.csv"),
  row.names = FALSE
)

# Q1(f): ARMA(p,q) model for the cyclical residual component
q1_bic_matrix <- matrix(NA, nrow = 4, ncol = 4)
rownames(q1_bic_matrix) <- paste0("AR(", 0:3, ")")
colnames(q1_bic_matrix) <- paste0("MA(", 0:3, ")")
q1_models <- vector("list", 16)

for (p in 0:3) {
  for (q in 0:3) {
    if (p == 0 && q == 0) next
    model_index <- p * 4 + q + 1
    q1_models[[model_index]] <- forecast::Arima(
      q1_training$deterministic_residual,
      order = c(p, 0, q),
      include.constant = FALSE
    )
    q1_bic_matrix[p + 1, q + 1] <- BIC(q1_models[[model_index]])
  }
}

write.csv(
  data.frame(AR = rownames(q1_bic_matrix), q1_bic_matrix, check.names = FALSE),
  file.path(outputs_dir, "q1_bic_matrix.csv"),
  row.names = FALSE
)

q1_minimum <- which(
  q1_bic_matrix == min(q1_bic_matrix, na.rm = TRUE),
  arr.ind = TRUE
)[1, ]
q1_selected_p <- q1_minimum[1] - 1
q1_selected_q <- q1_minimum[2] - 1
q1_cycle_model <- q1_models[[q1_selected_p * 4 + q1_selected_q + 1]]
save_text(summary(q1_cycle_model), "q1_selected_cycle_summary.txt")
q1_cycle_coefficients <- data.frame(
  Parameter = names(coef(q1_cycle_model)),
  Estimate = as.numeric(coef(q1_cycle_model)),
  Standard_error = sqrt(diag(q1_cycle_model$var.coef))
)
write.csv(
  q1_cycle_coefficients,
  file.path(outputs_dir, "q1_selected_cycle_coefficients.csv"),
  row.names = FALSE
)

q1_cycle_residual <- residuals(q1_cycle_model)
q1_cycle_diagnostics <- data.frame(
  Selected_p = q1_selected_p,
  Selected_q = q1_selected_q,
  BIC = BIC(q1_cycle_model),
  Significant_ACF_lags_1_to_25 = significant_acf_count(q1_cycle_residual, 25)
)
write.csv(
  q1_cycle_diagnostics,
  file.path(outputs_dir, "q1_cycle_diagnostics.csv"),
  row.names = FALSE
)

# Q1(g): final residual plot and correlogram
q1_final_residual_data <- data.frame(
  date = q1_training$date,
  residual = as.numeric(q1_cycle_residual)
)
q1_plot_g <- ggplot(q1_final_residual_data, aes(x = date, y = residual)) +
  geom_line(color = "forestgreen", linewidth = 0.5) +
  geom_hline(yintercept = 0, color = "gray50", linewidth = 0.3) +
  labs(
    title = paste0(
      "Final Residuals: ARMA(", q1_selected_p, ",", q1_selected_q, ") Cycle"
    ),
    x = "Year",
    y = "Residual"
  ) +
  theme_course
ggsave(
  file.path(figures_dir, "q1_g_final_residuals.png"),
  q1_plot_g,
  width = 8.2,
  height = 4.4,
  dpi = 220
)
save_acf_pacf(
  q1_cycle_residual,
  "q1_g_final_residual_acf_pacf.png",
  paste0("ARMA(", q1_selected_p, ",", q1_selected_q, ") residuals"),
  lag_max = 25
)
write.csv(
  acf_table(q1_cycle_residual, 25),
  file.path(outputs_dir, "q1_final_residual_acf.csv"),
  row.names = FALSE
)

# Q1(h): forecast log electricity retail sales for 2011
q1_deterministic_forecast <- as.numeric(
  predict(q1_deterministic, newdata = q1_test)
)
q1_cycle_forecast <- forecast::forecast(
  q1_cycle_model,
  h = nrow(q1_test),
  level = 95
)
q1_forecast_table <- data.frame(
  date = q1_test$date,
  actual_log_elec = q1_test$y,
  point_forecast = q1_deterministic_forecast + as.numeric(q1_cycle_forecast$mean),
  lower_95 = q1_deterministic_forecast + as.numeric(q1_cycle_forecast$lower[, 1]),
  upper_95 = q1_deterministic_forecast + as.numeric(q1_cycle_forecast$upper[, 1])
)
write.csv(
  q1_forecast_table,
  file.path(outputs_dir, "q1_forecasts_2011.csv"),
  row.names = FALSE
)

q1_plot_data <- elec %>%
  filter(date >= as.Date("2008-01-01")) %>%
  left_join(q1_forecast_table, by = "date")
q1_plot_h <- ggplot(q1_plot_data, aes(x = date)) +
  geom_line(aes(y = y, color = "Actual"), linewidth = 0.5) +
  geom_ribbon(
    aes(ymin = lower_95, ymax = upper_95),
    fill = "orange1",
    alpha = 0.30,
    na.rm = TRUE
  ) +
  geom_line(
    aes(y = point_forecast, color = "Forecast"),
    linewidth = 1,
    na.rm = TRUE
  ) +
  scale_color_manual(values = c("Actual" = "forestgreen", "Forecast" = "orange1")) +
  labs(
    title = "Forecast of Log Electricity Retail Sales for 2011",
    x = "Year",
    y = "Log electricity retail sales",
    color = NULL
  ) +
  theme_course
ggsave(
  file.path(figures_dir, "q1_h_forecast_2011.png"),
  q1_plot_h,
  width = 8.2,
  height = 4.8,
  dpi = 220
)

###############################################################################
# Question 2 - WTI oil price
###############################################################################

wti_raw <- read.csv(file.path(root, "data", "wti_oil_price.csv"), header = TRUE, sep = ",")
wti <- wti_raw[, c("date", "oil_price")]
stopifnot(nrow(wti) == 466)
stopifnot(!anyNA(wti$date), !anyNA(wti$oil_price))
wti$date <- as.Date(wti$date)
stopifnot(!anyNA(wti$date), !anyDuplicated(wti$date))
wti <- wti %>% arrange(date)
wti$time <- seq_len(nrow(wti))
wti$d_oil <- c(NA, diff(wti$oil_price))

q2_training <- wti %>% filter(date <= as.Date("2022-12-01"))
q2_test <- wti %>% filter(date >= as.Date("2023-01-01"))
stopifnot(nrow(q2_test) == 22)

# Q2(a): oil price level
q2_plot_a <- ggplot(wti, aes(x = date, y = oil_price)) +
  geom_line(color = "forestgreen", linewidth = 0.5) +
  labs(
    title = "Seasonally Adjusted WTI Oil Price",
    x = "Year",
    y = "Dollars per barrel"
  ) +
  theme_course
ggsave(
  file.path(figures_dir, "q2_a_oil_price_level.png"),
  q2_plot_a,
  width = 8.2,
  height = 4.8,
  dpi = 220
)

# Q2(b): ACF and PACF in levels
save_acf_pacf(
  q2_training$oil_price,
  "q2_b_level_acf_pacf.png",
  "WTI oil price in levels",
  lag_max = 20
)

# Q2(c): three manual Dickey-Fuller regressions
q2_df_data <- q2_training %>%
  mutate(
    lag_oil = c(NA, head(oil_price, -1)),
    d_oil = c(NA, diff(oil_price)),
    df_trend = seq_len(n())
  ) %>%
  filter(!is.na(d_oil), !is.na(lag_oil))

q2_df_baseline <- lm(d_oil ~ 0 + lag_oil, data = q2_df_data)
q2_df_drift <- lm(d_oil ~ lag_oil, data = q2_df_data)
q2_df_trend <- lm(d_oil ~ df_trend + lag_oil, data = q2_df_data)

df_row <- function(model, specification) {
  coefficient_table <- summary(model)$coefficients
  data.frame(
    Specification = specification,
    Phi_hat = coefficient_table["lag_oil", "Estimate"],
    Standard_error = coefficient_table["lag_oil", "Std. Error"],
    DF_statistic = coefficient_table["lag_oil", "t value"]
  )
}

q2_df_results <- bind_rows(
  df_row(q2_df_baseline, "Baseline"),
  df_row(q2_df_drift, "Drift"),
  df_row(q2_df_trend, "Drift and trend")
)
write.csv(
  q2_df_results,
  file.path(outputs_dir, "q2_manual_df_results.csv"),
  row.names = FALSE
)
save_text(summary(q2_df_baseline), "q2_df_baseline_summary.txt")
save_text(summary(q2_df_drift), "q2_df_drift_summary.txt")
save_text(summary(q2_df_trend), "q2_df_trend_summary.txt")

# Q2(d): aTSA ADF test in levels
q2_adf_level_output <- capture.output(
  q2_adf_level <- aTSA::adf.test(q2_training$oil_price, output = TRUE)
)
writeLines(q2_adf_level_output, file.path(outputs_dir, "q2_adf_level.txt"))
q2_adf_level_table <- bind_rows(lapply(
  names(q2_adf_level),
  function(type_name) {
    data.frame(
      Type = type_name,
      as.data.frame(q2_adf_level[[type_name]])
    )
  }
))
write.csv(
  q2_adf_level_table,
  file.path(outputs_dir, "q2_adf_level.csv"),
  row.names = FALSE
)

# Q2(e): differenced oil price
q2_diff_training <- q2_training %>% filter(!is.na(d_oil))
q2_plot_e <- ggplot(q2_diff_training, aes(x = date, y = d_oil)) +
  geom_line(color = "forestgreen", linewidth = 0.5) +
  geom_hline(yintercept = 0, color = "gray50", linewidth = 0.3) +
  labs(
    title = "First Difference of WTI Oil Price",
    x = "Year",
    y = "Change in dollars per barrel"
  ) +
  theme_course
ggsave(
  file.path(figures_dir, "q2_e_oil_price_difference.png"),
  q2_plot_e,
  width = 8.2,
  height = 4.5,
  dpi = 220
)
save_acf_pacf(
  q2_diff_training$d_oil,
  "q2_e_difference_acf_pacf.png",
  "first difference of WTI oil price",
  lag_max = 20
)
q2_adf_difference_output <- capture.output(
  q2_adf_difference <- aTSA::adf.test(q2_diff_training$d_oil, output = TRUE)
)
writeLines(
  q2_adf_difference_output,
  file.path(outputs_dir, "q2_adf_difference.txt")
)
q2_adf_difference_table <- bind_rows(lapply(
  names(q2_adf_difference),
  function(type_name) {
    data.frame(
      Type = type_name,
      as.data.frame(q2_adf_difference[[type_name]])
    )
  }
))
write.csv(
  q2_adf_difference_table,
  file.path(outputs_dir, "q2_adf_difference.csv"),
  row.names = FALSE
)

# Q2(f): ARMA(p,q) in differences, including ARMA(0,0)
q2_bic_matrix <- matrix(NA, nrow = 4, ncol = 4)
rownames(q2_bic_matrix) <- paste0("AR(", 0:3, ")")
colnames(q2_bic_matrix) <- paste0("MA(", 0:3, ")")
q2_models <- vector("list", 16)

for (p in 0:3) {
  for (q in 0:3) {
    model_index <- p * 4 + q + 1
    q2_models[[model_index]] <- forecast::Arima(
      q2_diff_training$d_oil,
      order = c(p, 0, q),
      include.constant = TRUE
    )
    q2_bic_matrix[p + 1, q + 1] <- BIC(q2_models[[model_index]])
  }
}

write.csv(
  data.frame(AR = rownames(q2_bic_matrix), q2_bic_matrix, check.names = FALSE),
  file.path(outputs_dir, "q2_bic_matrix.csv"),
  row.names = FALSE
)

q2_minimum <- which(
  q2_bic_matrix == min(q2_bic_matrix, na.rm = TRUE),
  arr.ind = TRUE
)[1, ]
q2_selected_p <- q2_minimum[1] - 1
q2_selected_q <- q2_minimum[2] - 1
q2_difference_model <- q2_models[[q2_selected_p * 4 + q2_selected_q + 1]]
save_text(summary(q2_difference_model), "q2_selected_difference_model_summary.txt")
q2_difference_coefficients <- data.frame(
  Parameter = names(coef(q2_difference_model)),
  Estimate = as.numeric(coef(q2_difference_model)),
  Standard_error = sqrt(diag(q2_difference_model$var.coef))
)
write.csv(
  q2_difference_coefficients,
  file.path(outputs_dir, "q2_selected_difference_model_coefficients.csv"),
  row.names = FALSE
)

q2_model_diagnostics <- data.frame(
  Selected_p = q2_selected_p,
  Selected_q = q2_selected_q,
  BIC = BIC(q2_difference_model),
  Significant_ACF_lags_1_to_20 = significant_acf_count(
    residuals(q2_difference_model), 20
  )
)
write.csv(
  q2_model_diagnostics,
  file.path(outputs_dir, "q2_model_diagnostics.csv"),
  row.names = FALSE
)

# Q2(g): forecast changes in oil prices
q2_change_forecast <- forecast::forecast(
  q2_difference_model,
  h = nrow(q2_test),
  level = 95
)
q2_change_forecast_table <- data.frame(
  date = q2_test$date,
  actual_change = q2_test$d_oil,
  point_forecast = as.numeric(q2_change_forecast$mean),
  lower_95 = as.numeric(q2_change_forecast$lower[, 1]),
  upper_95 = as.numeric(q2_change_forecast$upper[, 1])
)
write.csv(
  q2_change_forecast_table,
  file.path(outputs_dir, "q2_change_forecasts.csv"),
  row.names = FALSE
)

q2_change_plot_data <- wti %>%
  filter(date >= as.Date("2016-01-01")) %>%
  left_join(q2_change_forecast_table, by = "date")
q2_plot_g <- ggplot(q2_change_plot_data, aes(x = date)) +
  geom_line(aes(y = d_oil, color = "Actual change"), linewidth = 0.5) +
  geom_ribbon(
    aes(ymin = lower_95, ymax = upper_95),
    fill = "orange1",
    alpha = 0.30,
    na.rm = TRUE
  ) +
  geom_line(
    aes(y = point_forecast, color = "Forecast change"),
    linewidth = 1,
    na.rm = TRUE
  ) +
  scale_color_manual(
    values = c("Actual change" = "forestgreen", "Forecast change" = "orange1")
  ) +
  labs(
    title = "Forecast of Changes in WTI Oil Price",
    x = "Year",
    y = "Change in dollars per barrel",
    color = NULL
  ) +
  theme_course
ggsave(
  file.path(figures_dir, "q2_g_change_forecast.png"),
  q2_plot_g,
  width = 8.2,
  height = 4.8,
  dpi = 220
)

# Q2(h): equivalent ARIMA(p,1,q) model for level forecasts
q2_level_model <- forecast::Arima(
  q2_training$oil_price,
  order = c(q2_selected_p, 1, q2_selected_q),
  include.constant = TRUE
)
save_text(summary(q2_level_model), "q2_level_arima_summary.txt")
q2_level_forecast <- forecast::forecast(
  q2_level_model,
  h = nrow(q2_test),
  level = 95
)
q2_cumulative_change_point <- tail(q2_training$oil_price, 1) +
  cumsum(as.numeric(q2_change_forecast$mean))
q2_level_equivalence_check <- max(
  abs(as.numeric(q2_level_forecast$mean) - q2_cumulative_change_point)
)

q2_level_forecast_table <- data.frame(
  date = q2_test$date,
  actual_level = q2_test$oil_price,
  point_forecast = as.numeric(q2_level_forecast$mean),
  lower_95 = as.numeric(q2_level_forecast$lower[, 1]),
  upper_95 = as.numeric(q2_level_forecast$upper[, 1]),
  cumulative_change_check = q2_cumulative_change_point
)
write.csv(
  q2_level_forecast_table,
  file.path(outputs_dir, "q2_level_forecasts.csv"),
  row.names = FALSE
)
write.csv(
  data.frame(Max_absolute_point_forecast_difference = q2_level_equivalence_check),
  file.path(outputs_dir, "q2_level_equivalence_check.csv"),
  row.names = FALSE
)

q2_level_plot_data <- wti %>%
  filter(date >= as.Date("2016-01-01")) %>%
  left_join(q2_level_forecast_table, by = "date")
q2_plot_h <- ggplot(q2_level_plot_data, aes(x = date)) +
  geom_line(aes(y = oil_price, color = "Actual"), linewidth = 0.5) +
  geom_ribbon(
    aes(ymin = lower_95, ymax = upper_95),
    fill = "orange1",
    alpha = 0.30,
    na.rm = TRUE
  ) +
  geom_line(
    aes(y = point_forecast, color = "Forecast"),
    linewidth = 1,
    na.rm = TRUE
  ) +
  scale_color_manual(values = c("Actual" = "forestgreen", "Forecast" = "orange1")) +
  labs(
    title = "Forecast of the Level of WTI Oil Price",
    x = "Year",
    y = "Dollars per barrel",
    color = NULL
  ) +
  theme_course
ggsave(
  file.path(figures_dir, "q2_h_level_forecast.png"),
  q2_plot_h,
  width = 8.2,
  height = 4.8,
  dpi = 220
)

###############################################################################
# Question 3 - Disney stock price and volatility
###############################################################################

disney_raw <- read.csv(file.path(root, "data", "disney_stock_price.csv"), header = TRUE, sep = ",")
disney <- disney_raw[, c("date", "disney_stock")]
stopifnot(nrow(disney) == 4498)
stopifnot(!anyNA(disney$date), !anyNA(disney$disney_stock))
disney$date <- as.Date(disney$date)
stopifnot(!anyNA(disney$date), !anyDuplicated(disney$date))
disney <- disney %>% arrange(date)
disney$log_price <- log(disney$disney_stock)
disney$return <- c(NA, diff(disney$log_price))
disney$return2 <- disney$return^2

q3_training <- disney %>% filter(date <= as.Date("2024-09-30"))
q3_test <- disney %>% filter(date >= as.Date("2024-10-01"))
q3_return_training <- q3_training %>% filter(!is.na(return))
stopifnot(nrow(q3_test) > 0)

# Q3(a): log price and correlogram
q3_plot_a <- ggplot(disney, aes(x = date, y = log_price)) +
  geom_line(color = "forestgreen", linewidth = 0.5) +
  labs(
    title = "Log of Disney Stock Price",
    x = "Year",
    y = "Log closing price"
  ) +
  theme_course
ggsave(
  file.path(figures_dir, "q3_a_log_price.png"),
  q3_plot_a,
  width = 8.2,
  height = 4.8,
  dpi = 220
)
save_acf_pacf(
  q3_training$log_price,
  "q3_a_log_price_acf_pacf.png",
  "log Disney stock price",
  lag_max = 20
)

# Q3(b): log returns
q3_plot_b <- ggplot(q3_return_training, aes(x = date, y = return)) +
  geom_line(color = "forestgreen", linewidth = 0.5) +
  geom_hline(yintercept = 0, color = "gray50", linewidth = 0.3) +
  labs(
    title = "Disney Daily Log Returns",
    x = "Year",
    y = "Daily log return"
  ) +
  theme_course
ggsave(
  file.path(figures_dir, "q3_b_log_returns.png"),
  q3_plot_b,
  width = 8.2,
  height = 4.8,
  dpi = 220
)

# Q3(c): histogram and descriptive statistics
q3_returns <- q3_return_training$return
q3_return_mean <- mean(q3_returns)
q3_return_sd <- sd(q3_returns)
q3_skewness <- mean((q3_returns - q3_return_mean)^3) / q3_return_sd^3
q3_kurtosis <- mean((q3_returns - q3_return_mean)^4) / q3_return_sd^4
q3_descriptive_statistics <- data.frame(
  Statistic = c(
    "Mean", "Median", "Standard deviation", "Skewness",
    "Kurtosis", "Excess kurtosis"
  ),
  Value = c(
    q3_return_mean,
    median(q3_returns),
    q3_return_sd,
    q3_skewness,
    q3_kurtosis,
    q3_kurtosis - 3
  )
)
write.csv(
  q3_descriptive_statistics,
  file.path(outputs_dir, "q3_descriptive_statistics.csv"),
  row.names = FALSE
)

q3_plot_c <- ggplot(q3_return_training, aes(x = return)) +
  geom_histogram(
    bins = 60,
    fill = "forestgreen",
    color = "white",
    alpha = 0.85
  ) +
  labs(
    title = "Histogram of Disney Daily Log Returns",
    x = "Daily log return",
    y = "Frequency"
  ) +
  theme_course
ggsave(
  file.path(figures_dir, "q3_c_return_histogram.png"),
  q3_plot_c,
  width = 7.4,
  height = 4.8,
  dpi = 220
)

# Q3(d): correlogram of squared returns
save_acf(
  q3_return_training$return2,
  "q3_d_squared_return_acf.png",
  "ACF of Squared Disney Returns",
  lag_max = 20
)

# Q3(e): AR(1) model for squared returns
q3_squared_ar_data <- q3_return_training %>%
  mutate(lag_return2 = c(NA, head(return2, -1))) %>%
  filter(!is.na(lag_return2))
q3_squared_ar1 <- lm(return2 ~ lag_return2, data = q3_squared_ar_data)
save_text(summary(q3_squared_ar1), "q3_squared_return_ar1_summary.txt")
q3_ar1_coefficient <- summary(q3_squared_ar1)$coefficients["lag_return2", ]
write.csv(
  data.frame(
    Estimate = q3_ar1_coefficient["Estimate"],
    Standard_error = q3_ar1_coefficient["Std. Error"],
    t_value = q3_ar1_coefficient["t value"],
    p_value = q3_ar1_coefficient["Pr(>|t|)"]
  ),
  file.path(outputs_dir, "q3_squared_return_ar1_result.csv"),
  row.names = FALSE
)

# Q3(f)-(g): ARCH/GARCH models
q3_arch1 <- fGarch::garchFit(
  formula = ~garch(1, 0),
  data = q3_returns,
  include.mean = TRUE,
  cond.dist = "norm",
  trace = FALSE
)
q3_garch11 <- fGarch::garchFit(
  formula = ~garch(1, 1),
  data = q3_returns,
  include.mean = TRUE,
  cond.dist = "norm",
  trace = FALSE
)
q3_ar1_arch1 <- fGarch::garchFit(
  formula = ~arma(1, 0) + garch(1, 0),
  data = q3_returns,
  include.mean = TRUE,
  cond.dist = "norm",
  trace = FALSE
)
q3_ar1_garch11 <- fGarch::garchFit(
  formula = ~arma(1, 0) + garch(1, 1),
  data = q3_returns,
  include.mean = TRUE,
  cond.dist = "norm",
  trace = FALSE
)

q3_volatility_models <- list(
  "ARCH(1)" = q3_arch1,
  "GARCH(1,1)" = q3_garch11,
  "AR(1)-ARCH(1)" = q3_ar1_arch1,
  "AR(1)-GARCH(1,1)" = q3_ar1_garch11
)

q3_garch_comparison <- data.frame(
  Model = names(q3_volatility_models),
  BIC = vapply(
    q3_volatility_models,
    function(model) as.numeric(model@fit$ics["BIC"]),
    numeric(1)
  ),
  Convergence_code = vapply(
    q3_volatility_models,
    function(model) as.numeric(model@fit$convergence),
    numeric(1)
  ),
  Convergence_message = vapply(
    q3_volatility_models,
    function(model) as.character(model@fit$message),
    character(1)
  ),
  Finite_coefficients = vapply(
    q3_volatility_models,
    function(model) all(is.finite(model@fit$matcoef)),
    logical(1)
  ),
  Finite_volatility = vapply(
    q3_volatility_models,
    function(model) all(is.finite(model@sigma.t)),
    logical(1)
  )
)
write.csv(
  q3_garch_comparison,
  file.path(outputs_dir, "q3_garch_model_comparison.csv"),
  row.names = FALSE
)

q3_garch_coefficients <- bind_rows(lapply(
  names(q3_volatility_models),
  function(model_name) {
    model <- q3_volatility_models[[model_name]]
    coefficient_table <- as.data.frame(model@fit$matcoef)
    coefficient_table$Parameter <- rownames(coefficient_table)
    coefficient_table$Model <- model_name
    rownames(coefficient_table) <- NULL
    coefficient_table
  }
))
write.csv(
  q3_garch_coefficients,
  file.path(outputs_dir, "q3_garch_coefficients.csv"),
  row.names = FALSE
)

q3_garch_summary_output <- unlist(lapply(
  names(q3_volatility_models),
  function(model_name) {
    c(
      paste0("===== ", model_name, " ====="),
      capture.output(show(q3_volatility_models[[model_name]])),
      ""
    )
  }
))
writeLines(
  q3_garch_summary_output,
  file.path(outputs_dir, "q3_garch_model_summaries.txt")
)

if (any(!q3_garch_comparison$Finite_coefficients) ||
    any(!q3_garch_comparison$Finite_volatility)) {
  stop("At least one ARCH/GARCH model has non-finite estimates or volatility.")
}

# The installed fGarch version reports "singular convergence (7)" for these
# four course-specified default fits. The coefficient tables, standard errors,
# BIC values, and conditional volatilities are finite, so the returned course
# models are retained without changing the solver, distribution, or model.
# The exact diagnostic is written to the comparison table and disclosed in the
# answer document rather than being treated as ordinary convergence.

q3_best_model_name <- q3_garch_comparison$Model[
  which.min(q3_garch_comparison$BIC)
]
q3_best_model <- q3_volatility_models[[q3_best_model_name]]
write.csv(
  data.frame(Selected_model = q3_best_model_name),
  file.path(outputs_dir, "q3_selected_garch_model.csv"),
  row.names = FALSE
)

# Q3(h): estimated conditional volatility
q3_sigma_data <- data.frame(
  date = q3_return_training$date,
  conditional_volatility = as.numeric(q3_best_model@sigma.t)
)
q3_plot_h <- ggplot(q3_sigma_data, aes(x = date, y = conditional_volatility)) +
  geom_line(color = "forestgreen", linewidth = 0.5) +
  labs(
    title = paste0("Estimated Conditional Volatility: ", q3_best_model_name),
    x = "Year",
    y = "Conditional volatility"
  ) +
  theme_course
ggsave(
  file.path(figures_dir, "q3_h_conditional_volatility.png"),
  q3_plot_h,
  width = 8.2,
  height = 4.8,
  dpi = 220
)

# Q3(i): forecast conditional volatility for October-November 2024
q3_fGARCH_predict <- methods::getMethod("predict", "fGARCH")
q3_volatility_forecast <- q3_fGARCH_predict(
  q3_best_model,
  n.ahead = nrow(q3_test)
)
q3_volatility_forecast_table <- data.frame(
  date = q3_test$date,
  volatility_forecast = q3_volatility_forecast$standardDeviation
)
write.csv(
  q3_volatility_forecast_table,
  file.path(outputs_dir, "q3_volatility_forecasts.csv"),
  row.names = FALSE
)

q3_plot_i_history <- q3_sigma_data %>%
  filter(date >= as.Date("2020-01-01"))
q3_plot_i <- ggplot() +
  geom_line(
    data = q3_plot_i_history,
    aes(x = date, y = conditional_volatility, color = "Estimated volatility"),
    linewidth = 0.5
  ) +
  geom_line(
    data = q3_volatility_forecast_table,
    aes(x = date, y = volatility_forecast, color = "Forecast volatility"),
    linewidth = 1
  ) +
  scale_color_manual(
    values = c(
      "Estimated volatility" = "forestgreen",
      "Forecast volatility" = "orange1"
    )
  ) +
  labs(
    title = "Disney Conditional Volatility and Forecast",
    x = "Year",
    y = "Conditional volatility",
    color = NULL
  ) +
  theme_course
ggsave(
  file.path(figures_dir, "q3_i_volatility_forecast.png"),
  q3_plot_i,
  width = 8.2,
  height = 4.8,
  dpi = 220
)

###############################################################################
# Output checks
###############################################################################

project_checks <- data.frame(
  Check = c(
    "Electricity observations",
    "WTI observations",
    "Disney observations",
    "Q1 candidate BIC values",
    "Q2 candidate BIC values",
    "Q1 forecast observations",
    "Q2 forecast observations",
    "Q3 volatility forecast observations",
    "Q2 level-equivalence maximum difference"
  ),
  Value = c(
    nrow(elec),
    nrow(wti),
    nrow(disney),
    sum(!is.na(q1_bic_matrix)),
    sum(!is.na(q2_bic_matrix)),
    nrow(q1_forecast_table),
    nrow(q2_level_forecast_table),
    nrow(q3_volatility_forecast_table),
    q2_level_equivalence_check
  )
)
write.csv(
  project_checks,
  file.path(outputs_dir, "project_checks.csv"),
  row.names = FALSE
)

capture.output(sessionInfo(), file = file.path(outputs_dir, "session_info.txt"))

cat("Final Project analysis completed successfully.\n")
cat("Q1 selected trend:", q1_trend_choice, "\n")
cat("Q1 selected cycle: ARMA(", q1_selected_p, ",", q1_selected_q, ")\n", sep = "")
cat("Q2 selected difference model: ARMA(", q2_selected_p, ",", q2_selected_q, ")\n", sep = "")
cat("Q2 maximum level-forecast equivalence difference:", q2_level_equivalence_check, "\n")
cat("Q3 selected volatility model:", q3_best_model_name, "\n")
