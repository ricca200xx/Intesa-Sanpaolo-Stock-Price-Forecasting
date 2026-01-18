# 📈 Intesa Sanpaolo Stock Price Forecasting

![R](https://img.shields.io/badge/Language-R-blue.svg)
![License](https://img.shields.io/badge/License-CC%20BY--NC--SA%203.0-lightgrey.svg)
![Status](https://img.shields.io/badge/Status-Completed-success.svg)

> **University of Padua** > Department of Mathematics "Tullio Levi-Civita"  
> Course: *Business Economic and Financial Data (AA. 25-26)*

## 📄 Project Overview
[cite_start]This project aims to analyze and forecast the stock price evolution of **Intesa Sanpaolo S.p.A. (ISP.MI)** using advanced statistical and econometric methods[cite: 5, 23]. 

[cite_start]We compared four distinct forecasting approaches—ranging from traditional benchmarks to modern regression-based algorithms—to identify the model that best captures the non-linear dynamics and stochastic nature of the banking sector[cite: 88, 96]. [cite_start]A key feature of this analysis is the implementation of **Hybrid Models**, where deterministic trends are modeled via GAM or Prophet, and stochastic residuals are corrected using ARIMA processes[cite: 117, 373].

[cite_start]**Final Goal:** Produce a reliable 30-day forecast for January 2026[cite: 30, 443].

## 📊 Dataset
* **Source:** Yahoo! [cite_start]Finance (`quantmod`)[cite: 29].
* [cite_start]**Ticker:** `ISP.MI`[cite: 5].
* [cite_start]**Timeframe:** 5 Years (ending Jan 1, 2026)[cite: 30].
* [cite_start]**Target Variable:** Adjusted Close Price[cite: 31].
* [cite_start]**Frequency:** Daily (Trading days only, approx. 252/year)[cite: 33].

## 🛠️ Models Implemented

[cite_start]We employed a rigorous "Train-Test" split strategy (80% Train, 20% Test) to evaluate the following models[cite: 34]:

### 1. ARIMA (Benchmark)
* **Approach:** Box-Jenkins methodology.
* [cite_start]**Result:** The grid search identified an **ARIMA(0,1,0)** (Random Walk)[cite: 109, 147].
* [cite_start]**Insight:** The best predictor for tomorrow is simply today's price, serving as a baseline for complexity comparison[cite: 110].

### 2. Holt's Exponential Smoothing (ETS)
* [cite_start]**Specification:** `ETS(A,A,N)` (Additive Error, Additive Trend, No Seasonality)[cite: 172].
* [cite_start]**Insight:** Captures local trend changes by weighting recent observations more heavily, but proved too stiff for rapid market rallies[cite: 103, 426].

### 3. Hybrid GAM (Generalized Additive Model)
* [cite_start]**Structure:** Semi-parametric model combining smoothing splines with ARIMA errors[cite: 214, 274].
* [cite_start]**Formula:** $y_t = s(t) + \eta_t$ where $\eta_t$ is an AR(1) process[cite: 295].
* [cite_start]**Insight:** Uses splines ($df=10$) to model non-linear trends flexibly[cite: 220].

### 4. Hybrid Prophet (Winner 🏆)
* [cite_start]**Structure:** Decomposable time series model developed by Meta, enhanced with residual correction[cite: 304, 337].
* [cite_start]**Components:** Piecewise linear trend + Weekly seasonality + ARIMA(1,0,2) for residuals[cite: 308, 436].
* [cite_start]**Performance:** Achieved the lowest error by capturing both the deterministic trend and short-term shocks[cite: 395].

## 🏆 Results & Comparison

[cite_start]The models were evaluated based on **Mean Squared Error (MSE)** on the unseen Test Set[cite: 391].

| Model | AIC | MSE (Test Set) | Performance |
| :--- | :--- | :--- | :--- |
| **Hybrid Prophet** | *N/A (Bayesian)* | **0.6723** | ⭐ **Best Fit** |
| **Hybrid GAM** | -4092.30 | 0.8336 | 🥈 Runner-up |
| **ETS (Trend)** | 84.38 | 1.1803 | 🥉 Conservative |
| **ARIMA (0,1,0)** | -4065.80 | 1.8487 | ❌ Baseline |

> [cite_start]**Key Finding:** Traditional models (ARIMA/ETS) failed to capture the strong bullish rally of late 2025[cite: 420, 426]. [cite_start]The **Prophet model**, specifically when corrected with ARIMA residuals to handle autocorrelation, provided the most accurate trajectory[cite: 427, 483].

## 🔮 Final Forecast (Next 30 Days)
[cite_start]Using the fully calibrated **Hybrid Prophet** model on the entire dataset[cite: 431]:
* [cite_start]**Outlook:** Bullish (Positive Trend) 📈[cite: 484].
* [cite_start]**Prediction:** The price is expected to break the **€6.00** threshold[cite: 464].
* [cite_start]**Reliability:** Ljung-Box test on final residuals ($p > 0.05$) confirms White Noise behavior, ensuring statistical validity[cite: 440].

## 💻 Installation & Usage

### Prerequisites
[cite_start]Ensure you have R installed[cite: 497]. The script requires the following libraries:

```r
install.packages(c("quantmod", "fpp2", "zoo", "ggplot2", 
                   "forecast", "gam", "prophet", "gridExtra", "urca"))
