# 📈 Intesa Sanpaolo Stock Price Forecasting

![R](https://img.shields.io/badge/Language-R-blue.svg)
![License](https://img.shields.io/badge/License-CC%20BY--NC--SA%203.0-lightgrey.svg)
![Status](https://img.shields.io/badge/Status-Completed-success.svg)

> **University of Padua** > Department of Mathematics "Tullio Levi-Civita"  
> Course: *Business Economic and Financial Data (AA. 25-26)*

## 📄 Project Overview
This project aims to analyze and forecast the stock price evolution of **Intesa Sanpaolo S.p.A. (ISP.MI)** using advanced statistical and econometric methods. 

We compared four distinct forecasting approaches—ranging from traditional benchmarks to modern regression-based algorithms—to identify the model that best captures the non-linear dynamics and stochastic nature of the banking sector. A key feature of this analysis is the implementation of **Hybrid Models**, where deterministic trends are modeled via GAM or Prophet, and stochastic residuals are corrected using ARIMA processes.

**Final Goal:** Produce a reliable 30-day forecast for January 2026.

## 📊 Dataset
* **Source:** Yahoo! [cite_start]Finance (`quantmod`).
* **Ticker:** `ISP.MI`.
* **Timeframe:** 5 Years (ending Jan 1, 2026)].
* **Target Variable:** Adjusted Close Price.
* **Frequency:** Daily (Trading days only, approx. 252/year).

## 🛠️ Models Implemented

We employed a rigorous "Train-Test" split strategy (80% Train, 20% Test) to evaluate the following models:

### 1. ARIMA (Benchmark)
* **Approach:** Box-Jenkins methodology.
* **Result:** The grid search identified an **ARIMA(0,1,0)** (Random Walk).
* **Insight:** The best predictor for tomorrow is simply today's price, serving as a baseline for complexity comparison.

### 2. Holt's Exponential Smoothing (ETS)
* **Specification:** `ETS(A,A,N)` (Additive Error, Additive Trend, No Seasonality).
* **Insight:** Captures local trend changes by weighting recent observations more heavily, but proved too stiff for rapid market rallies.

### 3. Hybrid GAM (Generalized Additive Model)
* **Structure:** Semi-parametric model combining smoothing splines with ARIMA errors.
* **Formula:** $y_t = s(t) + \eta_t$ where $\eta_t$ is an AR(1) process.
* **Insight:** Uses splines ($df=10$) to model non-linear trends flexibly.

### 4. Hybrid Prophet (Winner 🏆)
* **Structure:** Decomposable time series model developed by Meta, enhanced with residual correction.
* **Components:** Piecewise linear trend + Weekly seasonality + ARIMA(1,0,2) for residuals.
* **Performance:** Achieved the lowest error by capturing both the deterministic trend and short-term shocks.

## 🏆 Results & Comparison

The models were evaluated based on **Mean Squared Error (MSE)** on the unseen Test Set.

| Model | AIC | MSE (Test Set) | Performance |
| :--- | :--- | :--- | :--- |
| **Hybrid Prophet** | *N/A (Bayesian)* | **0.6723** | ⭐ **Best Fit** |
| **Hybrid GAM** | -4092.30 | 0.8336 | 🥈 Runner-up |
| **ETS (Trend)** | 84.38 | 1.1803 | 🥉 Conservative |
| **ARIMA (0,1,0)** | -4065.80 | 1.8487 | ❌ Baseline |

>**Key Finding:** Traditional models (ARIMA/ETS) failed to capture the strong bullish rally of late 2025. The **Prophet model**, specifically when corrected with ARIMA residuals to handle autocorrelation, provided the most accurate trajectory.

## 🔮 Final Forecast (Next 30 Days)
Using the fully calibrated **Hybrid Prophet** model on the entire dataset:
* **Outlook:** Bullish (Positive Trend) 📈.
* **Prediction:** The price is expected to break the **€6.00** threshold.
* **Reliability:** Ljung-Box test on final residuals ($p > 0.05$) confirms White Noise behavior, ensuring statistical validity.

## 💻 Installation & Usage

### Prerequisites
Ensure you have R installed. The script requires the following libraries:

```r
install.packages(c("quantmod", "fpp2", "zoo", "ggplot2", 
                   "forecast", "gam", "prophet", "gridExtra", "urca"))
