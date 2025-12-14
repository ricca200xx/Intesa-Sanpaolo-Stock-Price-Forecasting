#library requirement
if (!require("quantmod")) install.packages("quantmod")
if (!require("fpp2")) install.packages("fpp2")
if (!require("zoo")) install.packages("zoo")
if (!require("ggplot2")) install.packages("ggplot2")

library(quantmod)
library(zoo)
library(fpp2)   
library(gridExtra)
library(urca)

################################################################################
#Phase 0 revenue data
################################################################################
library(readxl)
revenue_apple_qurter <- read_excel("C:/Users/ricky/OneDrive/Desktop/BEFD project/BEFD-project/revenue_apple_qurter.xlsx")

#################################################################################
#Phase 1 Add the dataset and data cleaning
#################################################################################

symbol <- "AAPL"
data_fine <- Sys.Date()
data_inizio <- data_fine - (365 * 5)

#caricamento e preparazione dati
data_raw <- getSymbols(symbol, src = "yahoo", auto.assign = FALSE,
                       from = data_inizio, to = data_fine)

adj_price <- data_raw[, 6]

# creazione sequenza completa 365 giorni
all_date <- seq.Date(from = start(adj_price), 
                     to = end(adj_price), 
                     by = "day")

# riempimento dei buchi (Weekends/Festivi)
emp <- xts(order.by = all_date)
full_ts <- merge(emp, adj_price)
all_price <- na.locf(full_ts) 

# Divisione train e test
n_obs <- length(all_price)
split_point <- floor(0.8 * n_obs)

train <- all_price[1:split_point]
test  <- all_price[(split_point + 1):n_obs]

# creazione ts frequency = 365
train_set <- ts(as.numeric(train), frequency = 365)
test_set  <- ts(as.numeric(test), frequency = 365)

# plot
df_plot <- data.frame(
  Data = index(all_price),
  price = as.numeric(all_price),
  type = c(rep("Train", length(train)), rep("Test", length(test))))

ggplot(df_plot, aes(x = Data, y = price, color = type)) +
  geom_line(linewidth = 0.5) +
  scale_color_manual(values = c("Train" = "#0072B2", "Test" = "#D55E00")) +
  labs(title = "Apple Stock (AAPL)",
       x = "Data", y = "price adjusted ($)") +
  theme_minimal() +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 year") +
  theme(legend.position = "bottom")

#################################################################################
#Phasev2 (Ace e Pacf normal/differentation)
#################################################################################

p1 <- autoplot(Acf(train_set, plot = FALSE, lag.max = 60)) + 
  ggtitle("ACF- Original") + theme_minimal()

p2 <- autoplot(Pacf(train_set, plot = FALSE, lag.max = 60)) + 
  ggtitle("PACF - Original") + theme_minimal()

grid.arrange(p1, p2)

#differentation of the series
diff_train <- diff(train_set)

p3 <- autoplot(Acf(diff_train, plot = FALSE, lag.max = 60)) + 
  ggtitle("ACF - Differentition") + theme_minimal()

p4 <- autoplot(Pacf(diff_train, plot = FALSE, lag.max = 60)) + 
  ggtitle("PACF - Differentition") + theme_minimal()

grid.arrange(p3, p4)

################################################################################
#Phase 3 #auto arima function(Stefano)
###############################################################################


################################################################################
# Phase 4: Prophet Model
################################################################################
library(prophet)

df_prophet <- data.frame(ds = index(train), y  = as.numeric(train))

# 2. Setup e Fit del Modello
# Disabilitiamo la stagionalità giornaliera/settimanale se non rilevante per dati daily stock
# Apple ha forti componenti di trend, lasciamo growth linear (o logistic se avessimo un cap)
m_prophet <- prophet(df_prophet, 
                     daily.seasonality = TRUE, 
                     yearly.seasonality = TRUE,
                     weekly.seasonality = TRUE)

# 3. Creazione finestra futura (lunghezza del test set)
future <- make_future_dataframe(m_prophet, periods = length(test), freq = "day")

# 4. Previsione
forecast_prophet <- predict(m_prophet, future)

# 5. Plotting (stile Lab 5)
plot(m_prophet, forecast_prophet) + ggtitle("Prophet Model Forecast - AAPL")

# 6. Calcolo AIC per selezione modello
# Prophet non restituisce l'AIC standard. Lo calcoliamo dai residui del training.
# AIC = n * log(RSS/n) + 2 * k
# k = numero di parametri (approssimato con numero di changepoints + stagionalità)

y_hat_train <- forecast_prophet$yhat[1:nrow(df_prophet)]
residuals_prophet <- df_prophet$y - y_hat_train
rss_prophet <- sum(residuals_prophet^2)
n <- nrow(df_prophet)
# k approssimato: num changepoints stimate (>0) + seasonality parameters
k_prophet <- sum(m_prophet$params$delta != 0) + 5 # 5 for basic seasonality terms approximation

aic_prophet <- n * log(rss_prophet/n) + 2 * k_prophet
aic_prophet
# AIC 5285.663


################################################################################
# Phase 5: Diffusion Models (DIMORA)
################################################################################

library(DIMORA)

train_values <- as.numeric(train)
diff_price_train <- diff(train_values)
diff_price_train <- na.omit(diff_price_train)

# STEP 1: Bass Model (BM) - Serve come base solida
bm_model <- BM(diff_price_train, display = FALSE)
summary(bm_model)

m_base <-5.815944e+03 
p_base <- 6.497097e-06
q_base <- 7.724523e-04

# STEP 2: Generalized Bass Model (GBM) con SHOCK RETTANGOLARE
start_shock <- 500  
end_shock   <- 750
c_shock     <- -0.2 # Intensità negativa (freno al prezzo)

# Vettore stime iniziali: c(m, p, q, a1, b1, c1)
prelim_gbm <- c(m_base, p_base, q_base, start_shock, end_shock, c_shock)

# Usiamo tryCatch per evitare il crash se l'ottimizzazione è difficile
gbm_model <- tryCatch({
  GBM(diff_train, shock = "rett", nshock = 1, 
      prelimestimates = prelim_gbm, display = FALSE)
}, error = function(e) {
  message("GBM non converge nemmeno con aiuto. Uso BM.")
  return(bm_model)
})

if(length(gbm_model$pars) > 3) cat("GBM ottimizzato con successo con shock.\n")

# STEP 3: Guseo-Guidolin Model (GGM)
prelim_ggm <- c(m_base, 0.001, 0.1, p_base, q_base)
ggm_model <- GGM(diff_train, prelimestimates = prelim_ggm, display = FALSE)


# ------------------------------------------------------------------------------
# CONFRONTO E SELEZIONE (AIC)
# ------------------------------------------------------------------------------
n <- length(diff_train)

# AIC BM
rss_bm <- sum(residuals(bm_model)^2)
aic_bm <- n * log(rss_bm/n) + 2 * 3

# AIC GBM
rss_gbm <- sum(residuals(gbm_model)^2)
k_gbm <- length(gbm_model$pars) # 6 parametri con shock
aic_gbm <- n * log(rss_gbm/n) + 2 * k_gbm

# AIC GGM
rss_ggm <- sum(residuals(ggm_model)^2)
k_ggm <- 5
aic_ggm <- n * log(rss_ggm/n) + 2 * k_ggm

cat("\n---------------- CONFRONTO MODELLI ----------------\n")
cat("AIC BM  (Base):       ", aic_bm, "\n")
cat("AIC GBM (con Shock):  ", aic_gbm, "\n")
cat("AIC GGM (Dinamico):   ", aic_ggm, "\n")

# Selezione Migliore
best_aic <- min(aic_bm, aic_gbm, aic_ggm)
if (best_aic == aic_gbm) {
  best_mod <- gbm_model; best_name <- "GBM (Shock)"
} else if (best_aic == aic_ggm) {
  best_mod <- ggm_model; best_name <- "GGM"
} else {
  best_mod <- bm_model; best_name <- "BM"
}
cat(">>> WINNER:", best_name, "\n")

total_len <- length(all_price)

# 2. Generazione della curva (Predict)
# predict() restituisce il cumulato delle differenze stimate.
# Usiamo 1:(total_len - 1) perché l'operazione diff() iniziale ha accorciato la serie di 1.
pred_cum_diff <- predict(best_mod, newx = 1:(total_len - 1))

# 3. Ricostruzione del Prezzo (Livello)
# Prezzo_t = Prezzo_Iniziale + Cumulato_Differenze_t
# Prendiamo il primo prezzo dal tuo oggetto 'train'
p0 <- as.numeric(train[1])

# Aggiungiamo 0 all'inizio per allineare le lunghezze (al tempo t=1 il cumulato è 0)
pred_price_full <- p0 + c(0, pred_cum_diff)

# 4. Estrazione del vettore per il Test Set
# Indici: dalla fine del train (+1) fino alla fine totale
idx_test_start <- length(train) + 1
pred_diffusion_test <- pred_price_full[idx_test_start:total_len]

# 5. Plot
# Usiamo index(all_price) per avere le date corrette sull'asse X
plot(index(all_price), as.numeric(all_price), type="l", col="gray", lwd=2,
     main = paste("Diffusion Model:", best_name), 
     ylab = "Price ($)", xlab = "Date")

# Aggiungiamo la linea rossa della previsione (Fit Train + Forecast Test)
lines(index(all_price), pred_price_full, col="red", lwd=2)

legend("topleft", legend=c("Real Price", paste("Forecast", best_name)),
       col=c("gray", "red"), lwd=2)

################################################################################
# Phase 6: GAM MODEL
################################################################################
library(gam)

# 1. Preparazione Dati
# Creiamo un regressore temporale 't'
t_train <- 1:length(train)
gam_data_train <- data.frame(y = as.numeric(train), t = t_train)

# Per il test set
t_test <- (length(train) + 1):(length(train) + length(test))
gam_data_test <- data.frame(t = t_test)

# 2. Definizione e Selezione Modelli
# Proviamo diverse complessità per la spline s() sul tempo t
# Modello 1: Lineare (baseline)
gam_1 <- gam(y ~ t, data = gam_data_train)

# Modello 2: Spline con 4 gradi di libertà (flessibilità media)
gam_2 <- gam(y ~ s(t, df=4), data = gam_data_train)

# Modello 3: Spline con 10 gradi di libertà (alta flessibilità per catturare oscillazioni)
gam_3 <- gam(y ~ s(t, df=10), data = gam_data_train)

# 3. Selezione tramite AIC
aic_gam1 <- AIC(gam_1)
aic_gam2 <- AIC(gam_2)
aic_gam3 <- AIC(gam_3)

cat("------------------------------------------------\n")
cat("Confronto AIC GAM:\n")
cat("GAM Lineare AIC:", aic_gam1, "\n")
cat("GAM Spline (df=4) AIC:", aic_gam2, "\n")
cat("GAM Spline (df=10) AIC:", aic_gam3, "\n")

# Trova il minimo
aic_vals <- c(aic_gam1, aic_gam2, aic_gam3)
models_gam <- list(gam_1, gam_2, gam_3)
best_gam_idx <- which.min(aic_vals)
best_gam_model <- models_gam[[best_gam_idx]]

cat("Modello GAM selezionato: Modello", best_gam_idx, "con AIC:", min(aic_vals), "\n")
cat("------------------------------------------------\n")

# 4. Previsione
pred_gam_test <- predict(best_gam_model, newdata = gam_data_test)

# 5. Visualizzazione Risultato GAM con DATE
# Estraiamo le date reali dagli oggetti originali
date_train <- index(train)
date_test  <- index(test)

# Calcoliamo i limiti del grafico per assicurarci che si vedano sia il Train che il Test
# Limiti Prezzo (Y)
y_min <- min(c(gam_data_train$y, pred_gam_test))
y_max <- max(c(gam_data_train$y, pred_gam_test))
# Limiti Tempo (X)
x_range <- c(min(date_train), max(date_test))

# Creiamo il grafico vuoto con le dimensioni giuste (type="n")
plot(x_range, c(y_min, y_max), type="n", 
     main="GAM Best Fit vs Data", xlab="Date", ylab="Price", ylim=c(y_min, y_max))

# 1. Disegniamo i dati Reali del Train (Grigio)
lines(date_train, gam_data_train$y, col="gray", lwd=2)

# 2. Disegniamo il Fitted Model (Blu) - Come il modello ha imparato
lines(date_train, fitted(best_gam_model), col="blue", lwd=2)

# 3. Disegniamo la Previsione sul Test (Verde)
lines(date_test, pred_gam_test, col="green", lwd=2)

# Legenda
legend("topleft", legend=c("Train Real", "Fitted", "Forecast"), 
       col=c("gray", "blue", "green"), lty=1, lwd=2)


