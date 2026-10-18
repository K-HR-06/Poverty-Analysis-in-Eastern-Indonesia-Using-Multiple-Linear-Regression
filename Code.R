library(ggplot2)
library(nortest)
library(lmtest)
library(car)

# Data (2021socio_economic_indonesia)

data <- read.csv(file.choose(), header = TRUE, sep=";", row.names = NULL)
head(data)
nrow(data)
colnames(data)

data_indonesia_timur <- na.omit(data)
nrow(data_indonesia_timur)

# Cek Korelasi

library(Hmisc)

predictors <- as.matrix(data_indonesia_timur[, c("life_exp", "reg_gdp", "avg_schooltime","exp_percap", "tingkat_pengangguran_terbuka", "rasio_gini")])
rcorr_results <- rcorr(predictors, type = "pearson")
rcorr_results # Tidak ada variabel prediktor yang mempunyai hubungan linear sempurna dengan variabel prediktor yang lain (paling tinggi 0.67)

cor.test(data_indonesia_timur$reg_gdp, data_indonesia_timur$poorpeople_percentage) # 0.1179859
cor.test(data_indonesia_timur$life_exp, data_indonesia_timur$poorpeople_percentage) # -0.6325085 
cor.test(data_indonesia_timur$avg_schooltime, data_indonesia_timur$poorpeople_percentage) # -0.5811735 
cor.test(data_indonesia_timur$exp_percap, data_indonesia_timur$poorpeople_percentage) # -0.6960772

cor.test(log(data_indonesia_timur$reg_gdp), data_indonesia_timur$poorpeople_percentage) # -0.381757 

cor.test(data_indonesia_timur$tingkat_pengangguran_terbuka, data_indonesia_timur$poorpeople_percentage) # -0.1920938
cor.test(data_indonesia_timur$rasio_gini, data_indonesia_timur$poorpeople_percentage) # 0.5542061

library(purrr)
library(dplyr)
library(broom)

target_var <- "poorpeople_percentage"
predictors <- c("reg_gdp", "life_exp", "avg_schooltime", 
                "exp_percap", "tingkat_pengangguran_terbuka", "rasio_gini")

correlation_summary <- data_indonesia_timur %>%
  select(all_of(predictors)) %>%
  map_dfr(~ tidy(cor.test(.x, data_indonesia_timur[[target_var]])), .id = "Predictor") %>%
  select(Predictor, Correlation = estimate, P_Value = p.value) %>%
  arrange(desc(abs(Correlation))) # Sort by strength of correlation

print(correlation_summary)

# Tidak hanya sekedar naik R^2 dan AIC serta BIC turun, tetapi model juga harus sejujur mungkin

demo_partial_model <- lm(poorpeople_percentage ~ reg_gdp + life_exp + avg_schooltime + exp_percap + tingkat_pengangguran_terbuka + rasio_gini, data = data_indonesia_timur)
summary(demo_partial_model) # R^2 = 0.7113

first_partial_model <- lm(poorpeople_percentage ~ reg_gdp + life_exp + avg_schooltime + exp_percap + rasio_gini, data = data_indonesia_timur)
summary(first_partial_model) # R^2 = 0.7112

AIC(first_partial_model) # 1478.814
BIC(first_partial_model) # 1503.207

vif(first_partial_model)

# Cek Outlier 

std_resid <- rstandard(first_partial_model)
outlier_3sd <- which(abs(std_resid) > 3) # If |residual| > 3, then it is an outlier 
outlier_3sd # tidak ada

# Cek High Leverage

hatvalues(first_partial_model)
which(hatvalues(first_partial_model) > 2*(5+1)/241) # ada 16

# Cek influential observation menggunakan Cook's distance
cook <- cooks.distance(first_partial_model)
which(cook > 1) # Tidak ada influential point jadi tidak ada baris data yang dibuang

anova(first_partial_model)

e <- resid(first_partial_model)
lillie.test(e) # Errornya tidak berdistribusi normal

bptest(first_partial_model) # Errornya Heteroskedastisitas

dwtest(first_partial_model) # Errornya berauto-korelasi

vif(first_partial_model) # tidak ada multikolinearitas

# Mengatasi error tidak berdistribusi normal

# 1) Regresi Robust -> memberikan bobot yang lebih kecil terhadap outlier

library(MASS)

model_robust <- rlm(poorpeople_percentage ~ reg_gdp + life_exp + avg_schooltime + exp_percap + rasio_gini, data = data_indonesia_timur, psi = psi.huber) # psi.huber merupakan metode pembobotan standar
summary(model_robust)

prediksi_robust <- predict(model_robust)
aktual <- data_indonesia_timur$poorpeople_percentage
r_squared_robust <- cor(aktual, prediksi_robust)^2
print(paste("R-squared Robust:", r_squared_robust)) # R^2 = 0.710562643782257

e_robust <- resid(model_robust)
lillie.test(e_robust) # Errornya tidak berdistribusi normal

bptest(model_robust) # Errornya Heteroskedastisitas

dwtest(model_robust) # Errornya berauto-korelasi

vif(model_robust)

# 2) Regresi Kuantil -> memprediksi median karena jauh lebih kebal terhadap outlier dibandingkan OLS yang memprediksi mean

library(quantreg)

model_kuantil <- rq(poorpeople_percentage ~ reg_gdp + life_exp + avg_schooltime + exp_percap + rasio_gini, data = data_indonesia_timur, tau = 0.5) # tau 0.5 artinya memprediksi median, kalau misalnya 0.9 berarti memprediksi batas atas, dan sebaliknya
summary(model_kuantil, se = "boot")

prediksi_kuantil <- predict(model_kuantil)
aktual <- data_indonesia_timur$poorpeople_percentage
r_squared_kuantil <- cor(aktual, prediksi_kuantil)^2
print(paste("R-squared Kuanti:", r_squared_kuantil)) # R^2 = 0.706378285703981

e_kuantil <- resid(model_kuantil)
lillie.test(e_kuantil) # Errornya tidak berdistribusi normal

bptest(model_kuantil) # Errornya Heteroskedastisitas

dwtest(model_kuantil) # Errornya berauto-korelasi

vif(first_partial_model) # tetap pakai model awal untuk acuan VIF pada Regresi Kuantil

# Mengatasi Auto-korelasi

# 1) Regresi Cochrane - Orcutt

library(orcutt)

model_co <- cochrane.orcutt(first_partial_model)
summary(model_co) # R^2 = 0.5802  

e_co <- resid(model_co)
lillie.test(e_co) # Errornya tidak berdistribusi normal

bptest(model_co) # Errornya Heteroskedastisitas

dwtest(model_co) # Errornya tidak berauto-korelasi (independen)

vif(first_partial_model) # jika di model OLS tidak ada multikolinearitas, maka 99% kemungkinan di model Cochrane-Orcutt juga tidak ada multikolinearitas

# 2) Regresi Generalized Least Square

library(nlme)

model_gls <- gls(poorpeople_percentage ~ reg_gdp + life_exp + avg_schooltime + exp_percap + rasio_gini, data = data_indonesia_timur, correlation = corAR1(form = ~ 1)) # form = ~1 mengasumsikan urutan data adalah urutan waktu/urutan observasi
summary(model_gls)

model_gls <- gls(poorpeople_percentage ~ life_exp + avg_schooltime + exp_percap + rasio_gini, data = data_indonesia_timur, correlation = corAR1(form = ~ 1)) # form = ~1 mengasumsikan urutan data adalah urutan waktu/urutan observasi
summary(model_gls)

prediksi_gls <- predict(model_gls)
aktual <- data_indonesia_timur$poorpeople_percentage
r_squared_gls <- cor(aktual, prediksi_gls)^2
print(paste("R-squared GLS:", r_squared_gls)) # R^2 = 0.707800673403728

e_gls <- resid(model_gls)
lillie.test(e_gls) # Errornya tidak berdistribusi normal

res_gls <- residuals(model_gls, type = "normalized")
res_gls_sq <- res_gls^2
bp_manual <- lm(res_gls_sq ~ reg_gdp + life_exp + avg_schooltime + exp_percap + rasio_gini, data = data_indonesia_timur)
summary(bp_manual) # Heteroskedastisitas (p-value < 0.05, tolak H0)

resid_gls_norm <- residuals(model_gls, type = "normalized")
dwtest(resid_gls_norm ~ 1) # tidak berauto-korelasi

vif(model_gls)ss

AIC(model_gls) # 1475.542
BIC(model_gls) # 1503.218

# Mengatasi Heteroskedastisitas

# 1) Regresi Weighted Least Square

library (lmtest)

abs_resid <- abs(first_partial_model$residuals) # Ambil nilai residual (error) mutlak
model_var <- lm(abs_resid ~ first_partial_model$fitted.values) # Regresikan residual tersebut terhadap nilai prediksi (fitted values)
varians_est <- model_var$fitted.values # Hitung nilai varians estimasi

# pred_var <- predict(model_indonesia_timur2)
# weights <- 1 / (pred_var^2)
# weights[weights <= 0] <- min(weights[weights > 0])

bobot_wls <- 1 / (varians_est^2)

wt <- 1 / lm(abs(first_partial_model$residuals) ~ first_partial_model$fitted.values)$fitted.values^2

model_wls <- lm(poorpeople_percentage ~ reg_gdp + life_exp + avg_schooltime + exp_percap + rasio_gini, data = data_indonesia_timur, weights = wt)
summary(model_wls) # R^2 = 0.6697

model_wls_2 <- lm(poorpeople_percentage ~ reg_gdp + life_exp + exp_percap + rasio_gini, data = data_indonesia_timur, weights = wt)
summary(model_wls_2)

e_wls2 <- resid(model_wls_2)
lillie.test(e_wls2) # Errornya tidak berdistribusi normal

bptest(model_wls_2) # Errornya Homoskedastisitas

Box.test(e_wls2, type = "Ljung-Box") # Pengganti dwtest untuk model WLS dan Errornya berauto-korelasi

vif(model_wls_2)

AIC(model_wls) # 1453.783
BIC(model_wls) # 1478.177

## GA DIPAKE!

# Mengatasi Multikolinearitasi

library(glmnet)
library(dplyr)

Y <- data_indonesia_timur$poorpeople_percentage

X_vars <- data_indonesia_timur %>%
  select(life_exp, avg_schooltime, exp_percap, rasio_kepemilikan_tabungan, rasio_beban_tanggungan_anak_sekolah)

X <- as.matrix(X_vars)

## Pakai Ridge

cv_ridge <- cv.glmnet(
  X, Y,
  alpha = 0,             # Alpha = 0 untuk Ridge Regression
  nfolds = 10,           # 10-fold cross-validation
  type.measure = "mse"
)

lambda_optimal_ridge <- cv_ridge$lambda.min
print(paste("Lambda optimal (Ridge):", lambda_optimal_ridge))

model_ridge_final <- glmnet(
  X, Y,
  alpha = 0,
  lambda = lambda_optimal_ridge
)

summary(model_ridge_final)

koefisien_ridge <- coef(model_ridge_final)
print(koefisien_ridge)

prediksi_ridge <- predict(
  model_ridge_final, 
  newx = X,                 # Matriks X yang sama yang digunakan untuk pelatihan
  s = model_ridge_final$lambda, # Menggunakan lambda optimal dari model akhir
  type = "response"         # Output sebagai nilai prediksi (Y-hat)
)

prediksi_ridge_vektor <- as.vector(prediksi_ridge)
r_squared_ridge <- cor(Y, prediksi_ridge_vektor)^2
print(paste("Pseudo R-Squared (Ridge):", r_squared_ridge)) # R^2 = 0.68830173568242

# Cek Homoskedastisitas

resid_ridge <- Y - prediksi_ridge_vektor
plot(prediksi_ridge_vektor, resid_ridge, main="Residuals vs. Fitted Values (Ridge)", xlab="Fitted Values", ylab="Residuals")
abline(h = 0, col = "red") # Heteroskedastisitas -> tersebar jauh dari garis merah di 0

# Cek Normalitas
qqnorm(resid_ridge)
qqline(resid_ridge, col = "red") # Berdistribusi normal -> konsisten di garis merah

## Pakai Lasso
cv_lasso <- cv.glmnet(
  X, Y,
  alpha = 1,             # Alpha = 1 untuk Lasso Regression
  nfolds = 10,           # 10-fold cross-validation
  type.measure = "mse"
)

lambda_optimal_lasso <- cv_lasso$lambda.min
print(paste("Lambda optimal (Lasso):", lambda_optimal_lasso))

lambda_1se_lasso <- cv_lasso$lambda.1se
print(paste("Lambda 1se (Lasso):", lambda_1se_lasso))

model_lasso_final <- glmnet(
  X, Y,
  alpha = 1,
  lambda = lambda_optimal_lasso
)

model_lasso_final2 <- glmnet(
  X, Y,
  alpha = 1,
  lambda = lambda_1se_lasso
)

koefisien_lasso <- coef(model_lasso_final)
print(koefisien_lasso)

koefisien_lasso2 <- coef(model_lasso_final2)
print(koefisien_lasso2)

prediksi_lasso <- predict(
  model_lasso_final, 
  newx = X,                 # Matriks X yang sama yang digunakan untuk pelatihan
  s = model_lasso_final$lambda, # Menggunakan lambda optimal dari model akhir
  type = "response"         # Output sebagai nilai prediksi (Y-hat)
)

prediksi_lasso2 <- predict(
  model_lasso_final2, 
  newx = X,                 # Matriks X yang sama yang digunakan untuk pelatihan
  s = model_lasso_final2$lambda, # Menggunakan lambda optimal dari model akhir
  type = "response"         # Output sebagai nilai prediksi (Y-hat)
)

prediksi_lasso_vektor <- as.vector(prediksi_lasso)
prediksi_lasso_vektor2 <- as.vector(prediksi_lasso2)

r_squared_lasso <- cor(Y, prediksi_lasso_vektor)^2
print(paste("Pseudo R-Squared (Lasso):", r_squared_lasso)) # R^2 = 0.702023708288913

r_squared_lasso2 <- cor(Y, prediksi_lasso_vektor2)^2
print(paste("Pseudo R-Squared (Lasso2):", r_squared_lasso2)) # R^2 = 0.686983324886125 -> lebih akurat dan membutuhkan data yang lebih sedikit -> FINAL

# Cek Homoskedastisitas

resid_lasso <- Y - prediksi_lasso2
plot(prediksi_lasso_vektor2, resid_lasso, main="Residuals vs. Fitted Values (Ridge)", xlab="Fitted Values", ylab="Residuals")
abline(h = 0, col = "red") # Heteroskedastisitas -> tersebar jauh dari garis merah di 0

# Cek Normalitas
qqnorm(resid_lasso)
qqline(resid_lasso, col = "red") # Berdistribusi normal -> konsisten di garis merah
