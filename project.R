# ==============================================================================
# PROJECT: Intesa Sanpaolo (ISP.MI) Stock Forecasting
# Models: ARIMA, Prophet, ETS, GAM
# ==============================================================================

rm(list = ls())

#libraries
libs <- c("quantmod", "fpp2", "zoo", "ggplot2", "forecast", "gam", "prophet", "gridExtra", "urca")
for (lib in libs) {
  if (!require(lib, character.only = TRUE)) install.packages(lib)
  library(lib, character.only = TRUE)
}

### 2. CUSTOM FUNCTIONS
arima.aic.manual <- function(mydata, intord = 0, seasord = 0){
  aicm <- matrix(0,5,5)
  
  LowestIndex <- 0.0
  MAIndex <- 0
  ARIndex <- 0
  
  SecondLowestIndex <- 0.0  
  MAIndexSecond <- 0
  ARIndexSecond <- 0
  
  for (i in 0:4) for (j in 0:4) {
    fit<-arima(mydata, order = c(i,intord,j), 
               seasonal = list(order = c(0,seasord,0), period = 12), method = "ML")
    aicm[i+1,j+1] <- fit$aic
    
    if (i==0 & j==0) {
      LowestIndex <- fit$aic
      SecondLowestIndex <- fit$aic
    } else if (LowestIndex > aicm[i+1,j+1])  {
      LowestIndex <- fit$aic
      ARIndex <- i
      MAIndex <- j
      
    }else if (SecondLowestIndex > aicm[i+1,j+1]) {
      SecondLowestIndex <- fit$aic
      ARIndexSecond <- i
      MAIndexSecond <- j      
    }
  }
  cat("Lowest Index =",LowestIndex, "\n")
  cat("AR =",ARIndex, "\n")
  cat("MA =",MAIndex, "\n")
  cat("Second Lowest Index =",SecondLowestIndex, "\n")
  cat("AR 2° =",ARIndexSecond, "\n")
  cat("MA 2° =",MAIndexSecond, "\n")
  rownames(aicm) <- c(0,1,2,3,4)
  colnames(aicm) <- c(0,1,2,3,4)
  print(aicm)
}

# ==============================================================================
# PHASE 1: Data acquisition and cleaning
# ==============================================================================

ticker <- "ISP.MI"
end_date <- as.Date("2026-01-01")
start_date <- end_date - (365 * 5)

# Download data
data_raw <- getSymbols(ticker, src = "yahoo", auto.assign = FALSE, from = start_date, to = end_date)
prices <- na.omit(data_raw[, 6])

# Train/Test Split (80/20)
n_total <- length(prices)
split_idx <- floor(0.8 * n_total)

train_vec <- prices[1:split_idx]
test_vec  <- prices[(split_idx + 1):n_total]
test_actual <- as.numeric(test_vec)

#Frequency 252 (giorni di trading)
train_ts <- ts(as.numeric(train_vec), frequency = 252)
test_ts  <- ts(as.numeric(test_vec), frequency = 252)


df_plot <- data.frame(
  Date = index(prices),
  Price = as.numeric(prices),
  Set = c(rep("Train", length(train_vec)), rep("Test", length(test_vec))))

ggplot(df_plot, aes(x = Date, y = Price, color = Set)) +
  geom_line(linewidth = 0.4) +
  scale_color_manual(values = c("Train" = "#0072B2", "Test" = "#D55E00")) +
  labs(title = paste(ticker, "- Historical Price Data (Train vs Test)"), 
       subtitle = "5-Year Daily Adjusted Close Prices",
       y = "Price (€)", x = "Date") +
  theme_minimal() +
  theme(legend.position = "bottom")

# ==============================================================================
# PHASE 2: ACF  e PACF (differenziazione)
# ==============================================================================

# Original
p1 <- autoplot(Acf(train_ts, plot = FALSE, lag.max = 60)) + ggtitle("ACF - Original Series") + theme_minimal()
p2 <- autoplot(Pacf(train_ts, plot = FALSE, lag.max = 60)) + ggtitle("PACF - Original Series") + theme_minimal()
grid.arrange(p1, p2, ncol = 1)

# Differenced
diff_ts <- diff(train_ts)
p3 <- autoplot(Acf(diff_ts, plot = FALSE, lag.max = 60)) + ggtitle("ACF - Differenced Series") + theme_minimal()
p4 <- autoplot(Pacf(diff_ts, plot = FALSE, lag.max = 60)) + ggtitle("PACF - Differenced Series") + theme_minimal()
grid.arrange(p3, p4, ncol = 1)

# ==============================================================================
# PHASE 3: ARIMA
# ==============================================================================

arima.aic.manual(train_ts, intord = 1, seasord = 0)

# random walk (0,1,0)
model_arima <- arima(train_ts, order = c(0, 1, 0), method = "ML")
summary(model_arima)
checkresiduals(model_arima)

# Forecasting
fcst_arima <- forecast(model_arima, h = length(test_ts))
pred_arima <- as.numeric(fcst_arima$mean)

mse_arima <- mean((test_actual - pred_arima)^2)
cat("MSE: ", mse_arima, "AIC:", model_arima$aic, "\n")

# ==============================================================================
# PHASE 4: ETS model
# ==============================================================================

model_ets <- ets(train_ts, model = "AAN", damped = FALSE)

#forecast
fcst_ets <- forecast(model_ets, h = length(test_ts))

pred_ets <- as.numeric(fcst_ets$mean)
fit_ets <- as.numeric(fitted(model_ets))

mse_ets <- mean((test_actual - pred_ets)^2)
cat("MSE:", mse_ets, "AIC:", model_ets$aic, "\n")
checkresiduals(model_ets)


# ==============================================================================
# PHASE 5: GAM (GAM + ARIMA on residuals)
# ==============================================================================

#Setup
t_idx <- 1:length(train_vec)
df_gam_train <- data.frame(y = as.numeric(train_vec), t = t_idx)
df_gam_test  <- data.frame(t = (length(train_vec) + 1):n_total)

# Select best model
g1 <- gam(y ~ t, data = df_gam_train)
g2 <- gam(y ~ s(t, df=4), data = df_gam_train)
g3 <- gam(y ~ s(t, df=10), data = df_gam_train)
model_gam <- list(g1, g2, g3)[[which.min(c(AIC(g1), AIC(g2), AIC(g3)))]]

#Residuals 
resid_gam <- residuals(model_gam)

par(mfrow=c(2,1))
plot(resid_gam, type="l", main="GAM Residuals", ylab="Residual", xlab="Time Index")
abline(h=0, col="red")
Acf(resid_gam, main="ACF of GAM Residuals")
par(mfrow=c(1,1))
print(Box.test(resid_gam, lag = 20, type = "Ljung-Box"))

# arima fit on residual
model_resid_gam <- auto.arima(resid_gam)
summary(model_resid_gam)

# re-check residual
checkresiduals(model_resid_gam)

#Forecast
pred_base_gam <- predict(model_gam, newdata = df_gam_test)
fcst_resid_gam <- forecast(model_resid_gam, h = length(test_ts))

pred_gam_hybrid <- as.numeric(pred_base_gam) + as.numeric(fcst_resid_gam$mean)
fit_gam_hybrid <- as.numeric(fitted(model_gam)) + fitted(model_resid_gam)

mse_gam <- mean((test_actual - pred_gam_hybrid)^2)
cat("AIC:",model_resid_gam$aic,"MSE:", mse_gam)



# ==============================================================================
# PHASE 6: PROPHET (Prophet + ARIMA on residuals)
# ==============================================================================
# Setup
df_prophet <- data.frame(ds = index(train_vec), y = as.numeric(train_vec))

model_prophet <- prophet(df_prophet, 
                         daily.seasonality = FALSE, 
                         yearly.seasonality = FALSE,
                         weekly.seasonality = TRUE)

future_prophet <- make_future_dataframe(model_prophet, periods = length(test_vec), freq = "day")
fcst_prophet_obj <- predict(model_prophet, future_prophet)

# Prophet residuals
fitted_prophet <- fcst_prophet_obj$yhat[1:nrow(df_prophet)]
resid_prophet <- df_prophet$y - fitted_prophet

# residual check
par(mfrow=c(2,1))
plot(df_prophet$ds, resid_prophet, type="l", main="Prophet Residuals", ylab="Residual", xlab="Date")
abline(h=0, col="red")
Acf(resid_prophet, main="ACF of Prophet Residuals")
par(mfrow=c(1,1))
print(Box.test(resid_prophet, lag = 20, type = "Ljung-Box"))

# residuals with ARIMA
model_resid_prophet <- auto.arima(resid_prophet)
summary(model_resid_prophet)
checkresiduals(model_resid_prophet)

#Forecast
fcst_resid_prophet <- forecast(model_resid_prophet, h = length(test_ts))

# trend/seasonality
pred_base_prophet <- fcst_prophet_obj$yhat[(n_total - length(test_vec) + 1):n_total]

# residuals
pred_corr_prophet <- as.numeric(fcst_resid_prophet$mean)
pred_prophet_hybrid <- pred_base_prophet + pred_corr_prophet
fit_prophet_hybrid <- fitted_prophet + fitted(model_resid_prophet)

# mse and aic
mse_prophet <- mean((test_actual - pred_prophet_hybrid)^2)
aic_prophet_hybrid <- model_resid_prophet$aic

cat("MSE:", mse_prophet, "AIC:",aic_prophet_hybrid )


# ==============================================================================
# PHASE 7: result and comparison
# ==============================================================================

# table
results_table <- data.frame(
  Model = c("ARIMA (0,1,0)", "Prophet", "ETS (Trend)", "GAM"),
  AIC = round(c(model_arima$aic, model_resid_prophet$aic, model_ets$aic, model_resid_gam$aic), 2),
  MSE = round(c(mse_arima, mse_prophet, mse_ets, mse_gam), 5)
)

print(results_table)

# vector for plot
full_arima   <- c(as.numeric(fitted(model_arima)), pred_arima)
full_ets     <- c(fit_ets, pred_ets)
full_prophet <- c(as.numeric(fit_prophet_hybrid), pred_prophet_hybrid)
full_gam     <- c(as.numeric(fit_gam_hybrid), pred_gam_hybrid)

# alignment
len_target <- length(prices)
full_arima   <- full_arima[1:len_target]
full_ets     <- full_ets[1:len_target]
full_prophet <- full_prophet[1:len_target]
full_gam     <- full_gam[1:len_target]

# Plot
plot(index(prices), as.numeric(prices), type="l", col="lightgray", lwd=2,
     main="Model Comparison: In-Sample Fit & Out-of-Sample Forecast",
     ylab="Price (€)", xlab="Date")

lines(index(prices), full_arima, col="red", lwd=1)
lines(index(prices), full_prophet, col="blue", lwd=1)
lines(index(prices), full_ets, col="darkgreen", lwd=1)
lines(index(prices), full_gam, col="purple", lwd=1)

abline(v=index(prices)[split_idx], col="black", lty=2, lwd=1.5)
text(index(prices)[split_idx], min(prices), " Test Set Start", pos=4, cex=0.8)

legend("topleft", 
       legend=c("Actual Data", "ARIMA", "Prophet", "ETS", "GAM"),
       col=c("lightgray", "red", "blue", "darkgreen", "purple"), 
       lwd=2, bty="n", cex=0.8)

# zoom on Test Set
plot(index(test_vec), test_actual, type="l", col="black", lwd=2,
     main="Zoom: Test Set Forecasting Accuracy",
     ylab="Price (€)", xlab="Date", 
     ylim=c(min(test_actual)*0.9, max(test_actual)*1.1))

lines(index(test_vec), pred_arima, col="red", lwd=2)
lines(index(test_vec), pred_prophet_hybrid, col="blue", lwd=2)
lines(index(test_vec), pred_ets, col="darkgreen", lwd=2)
lines(index(test_vec), pred_gam_hybrid, col="purple", lwd=2)

legend("topleft", 
       legend=c("Actual", "ARIMA", "Prophet", "ETS", "GAM"),
       col=c("black", "red", "blue", "darkgreen", "purple"), 
       lwd=2, bty="n", cex=0.8)


# ==============================================================================
# PHASE 8: forecast 30 day next
# Refitting prophet that win before
# ==============================================================================

#full dataset
df_full <- data.frame(ds = index(prices), y = as.numeric(prices))

#Fit Full Prophet Model
final_prophet <- prophet(df_full, 
                         daily.seasonality = FALSE, 
                         yearly.seasonality = FALSE,
                         weekly.seasonality = TRUE)

# ARIMA on full residuals
in_sample_fcst <- predict(final_prophet, df_full)
full_residuals <- df_full$y - in_sample_fcst$yhat

final_resid_arima <- auto.arima(full_residuals)
summary(final_resid_arima)
checkresiduals(final_resid_arima)

days_ahead <- 30

# trend/seasonality forecast
future_dates_final <- make_future_dataframe(final_prophet, periods = days_ahead, freq = "day")
fcst_prophet_final <- predict(final_prophet, future_dates_final)
future_base <- tail(fcst_prophet_final, days_ahead)

# ARIMA forecast
fcst_resid_final <- forecast(final_resid_arima, h = days_ahead, level = 95)
final_mean <- future_base$yhat + as.numeric(fcst_resid_final$mean)

# IC sigma trand/seasonality + sigma ARIMA
sigma_p <- (future_base$yhat_upper - future_base$yhat_lower) / (2 * 1.96)
sigma_a <- (fcst_resid_final$upper[,1] - fcst_resid_final$lower[,1]) / (2 * 1.96)
sigma_tot <- sqrt(sigma_p^2 + sigma_a^2)

final_lower <- final_mean - (1.96 * sigma_tot)
final_upper <- final_mean + (1.96 * sigma_tot)

# df for plot
df_future <- data.frame(
  Date = as.Date(future_base$ds),
  Price = final_mean,
  Lower = final_lower,
  Upper = final_upper,
  Type = "Forecast")

# history for plot
df_history <- data.frame(
  Date = as.Date(tail(df_full$ds, 120)),
  Price = tail(df_full$y, 120),
  Lower = NA, Upper = NA,
  Type = "History"
)

plot_data <- rbind(df_history, df_future)

# final plot
y_lims <- c(min(plot_data$Price, plot_data$Lower, na.rm=T)*0.98, 
            max(plot_data$Price, plot_data$Upper, na.rm=T)*1.02)

plot(plot_data$Date, plot_data$Price, type="n", ylim=y_lims,
     main=paste("Intesa Sanpaolo: Forecast (Next", days_ahead, "Days)"),
     xlab="Date", ylab="Price (€)")

# ic area
idx_fut <- which(plot_data$Type == "Forecast")
polygon(c(plot_data$Date[idx_fut], rev(plot_data$Date[idx_fut])),
        c(plot_data$Lower[idx_fut], rev(plot_data$Upper[idx_fut])),
        col = rgb(0, 0, 1, 0.2), border = NA)


idx_hist <- which(plot_data$Type == "History")
lines(plot_data$Date[idx_hist], plot_data$Price[idx_hist], col="black", lwd=2)
lines(plot_data$Date[idx_fut], plot_data$Price[idx_fut], col="blue", lwd=3)

# points and labels
last_hist_val <- tail(df_history$Price, 1)
last_hist_date <- tail(df_history$Date, 1)
points(last_hist_date, last_hist_val, pch=19, col="black")
text(last_hist_date, last_hist_val, labels=round(last_hist_val, 3), pos=3, cex=0.8)

last_pred_val <- tail(df_future$Price, 1)
last_pred_date <- tail(df_future$Date, 1)
points(last_pred_date, last_pred_val, pch=19, col="blue")
text(last_pred_date, last_pred_val, labels=round(last_pred_val, 3), pos=4, cex=0.8, col="blue")

grid()
legend("topleft", legend=c("Historical Data", "Hybrid Forecast", "95% Confidence Interval"),
       col=c("black", "blue", rgb(0, 0, 1, 0.2)), lty=1, lwd=c(2,3,10), bty="n")

