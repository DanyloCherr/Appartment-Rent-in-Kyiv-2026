# ----- 20 СПОСТЕРЕЖЕНЬ -----
model_20q_df_date <- model_df_20q[, !names(model_df_20q) %in% c("Ч", "РівДолар")]
df_long_dateq <- model_20q_df_date %>%
  pivot_longer(cols = c(К1, К2, К3), 
               names_to = "Кімнатність", 
               values_to = "Ціна") %>%
  mutate(Кімнатність = factor(Кімнатність, 
                              levels = c("К1", "К2", "К3"),
                              labels = c("1", "2", "3")))

model_20q_df <- model_20q_df_date[, -1]
df_longq <- df_long_dateq[, -1]


# ==== 1) ПОВНА МОДЕЛЬ ====
model_full_20q <- lm(Ціна ~ ., data = df_longq)
summary(model_full_20q)


# ==== 2) АНАЛІЗ ЗАЛИШКІВ І ВИЯВЛЕННЯ ВИКИДІВ ====
residuals_plot(model_full_20q)

residuals_regressors_plot(model_full_20q, c(2, 4), type = "rstudent")

cooks_dist(model_full_20q)

df_fits(model_full_20q)

df_betas(model_full_20q)


# ==== 3) ФУНКЦІОНАЛЬНІ ПЕРЕТВОРЕННЯ ====
# ЛІНІЙНІСТЬ
plot(model_full_20q, which = 1)
# Зі збільшенням прогнозованих значень крива сильніше відхиляється від нуля.

av_plots(model_full_20q)

crPlots(model_full_20q) # ІГР - ?
par(mfrow = c(1, 1))

resettest(model_full_20q, power = 2) # p-value = 0.01387


# ГОМОСКЕДАСТИЧНІСТЬ 
# Загальна варіабельність
plot(model_full_20q, which = 3)
# Зі збільшенням прогнозованих значень дисперсія трохи зростає.

residuals_regressors_plot(model_full_20q, c(2, 4), type = "rstudent")
# ІГР - ?

bptest(model_full_20q) # p-value = 0.00277


# Міжгрупова варіабельність
bartlett.test(residuals(model_full_20q) ~ df_longq$Кімнатність) # p-value = 0.02926

leveneTest(residuals(model_full_20q) ~ df_longq$Кімнатність) # p-value = 0.09165   

# Гетероскедастичність!


bc <- boxcox(model_full_20q, lambda = seq(0, 1, by = 0.1))
lambda_opt <- bc$x[which.max(bc$y)]
cat("Оптимальне λ:", round(lambda_opt, 2), "\n") # 0.32 
# ДІ містить 0.5 - візьмемо його.

model_Sqrt_20q <- lm(sqrt(Ціна) ~ ., data = df_longq)
summary(model_Sqrt_20q)



# ======== НОВА МОДЕЛЬ 1 ========
# ЛІНІЙНІСТЬ
plot(model_Sqrt_20q, which = 1)
residuals_plot(model_Sqrt_20q) # Трохи рівномірніше.

residuals_regressors_plot(model_Sqrt_20q, c(2, 4), type = "rstudent")

av_plots(model_Sqrt_20q)

crPlots(model_Sqrt_20q) 
par(mfrow = c(1, 1))

resettest(model_Sqrt_20q, power = 2) # p-value = 0.827


# ГОМОСКЕДАСТИЧНІСТЬ 
# Загальна варіабельність
plot(model_Sqrt_20q, which = 3) 

bptest(model_Sqrt_20q) # p-value = 0.02547

# Міжгрупова варіабельність
bartlett.test(residuals(model_Sqrt_20q) ~ df_longq$Кімнатність) # p-value = 0.2218

leveneTest(residuals(model_Sqrt_20q) ~ df_longq$Кімнатність) # p-value = 0.2109  
# БП-тест міг провалитися через сильну гетероскедастичність між групами.


# Над категоріальними змінними ФП не застосовуємо.
all_preds <- names(coef(model_Sqrt_20q))[-1]
numeric_preds <- all_preds[!grepl("Кімнатність", all_preds)]
observe_transforms_x(model_Sqrt_20q, regressors = numeric_preds)

model_Sqrt_20q_Lg <- update(model_Sqrt_20q, . ~ . + log(Долар))
# AIC_diff = 7.6211114
summary(model_Sqrt_20q_Lg)


# ======== НОВА МОДЕЛЬ 2 ========
# ЛІНІЙНІСТЬ
plot(model_Sqrt_20q_Lg, which = 1) 
residuals_plot(model_Sqrt_20q_Lg)

residuals_regressors_plot(model_Sqrt_20q_Lg, c(2, 4), type = "rstudent")

av_plots(model_Sqrt_20q_Lg)

crPlots(model_Sqrt_20q_Lg) # ІФС - дуга.
par(mfrow = c(1, 1))

resettest(model_Sqrt_20q_Lg, power = 2) # p-value = 0.6533


# ГОМОСКЕДАСТИЧНІСТЬ 
# Загальна варіабельність
plot(model_Sqrt_20q_Lg, which = 3)

bptest(model_Sqrt_20q_Lg) # p-value = 0.1501

# Міжгрупова варіабельність
bartlett.test(residuals(model_Sqrt_20q_Lg) ~ df_longq$Кімнатність) # p-value = 0.2131

leveneTest(residuals(model_Sqrt_20q_Lg) ~ df_longq$Кімнатність) # p-value = 0.3817   

observe_transforms_x(model_Sqrt_20q_Lg, regressors = numeric_preds)

model_Sqrt_20q_Lg2 <- update(model_Sqrt_20q_Lg, . ~ . + log(Євро))
# AIC_diff = 13.73534
summary(model_Sqrt_20q_Lg2)



# ======== НОВА МОДЕЛЬ 3 ========
# ЛІНІЙНІСТЬ
plot(model_Sqrt_20q_Lg2, which = 1) 
residuals_plot(model_Sqrt_20q_Lg2)

residuals_regressors_plot(model_Sqrt_20q_Lg2, c(3, 4), type = "rstudent")
# Для рівнів Кімнатності зник зсув середнього від нуля.
# ІЦЖВ - структурний зсув? 

av_plots(model_Sqrt_20q_Lg2)

crPlots(model_Sqrt_20q_Lg2)
par(mfrow = c(1, 1))

resettest(model_Sqrt_20q_Lg2, power = 2) # p-value = 0.8151


# ГОМОСКЕДАСТИЧНІСТЬ 
# Загальна варіабельність
plot(model_Sqrt_20q_Lg2, which = 3)

bptest(model_Sqrt_20q_Lg2) # p-value = 0.004985

# Міжгрупова варіабельність
bartlett.test(residuals(model_Sqrt_20q_Lg2) ~ df_longq$Кімнатність) # p-value = 0.1142

leveneTest(residuals(model_Sqrt_20q_Lg2) ~ df_longq$Кімнатність) # p-value = 0.03724      

observe_transforms_x(model_Sqrt_20q_Lg2, regressors = numeric_preds)

model_Sqrt_20q_Lg2Sq <- update(model_Sqrt_20q_Lg2, . ~ . + I(ІФС^2))
# AIC_diff = 4.47136749       
summary(model_Sqrt_20q_Lg2Sq)

waldtest(model_Sqrt_20q_Lg2, model_Sqrt_20q_Lg2Sq, 
         vcov = function(x) vcovCL(x, cluster = df_longq$Кімнатність)) # +


# ======== НОВА МОДЕЛЬ 4 ========
# ЛІНІЙНІСТЬ
plot(model_Sqrt_20q_Lg2Sq, which = 1) # Гірше?
residuals_plot(model_Sqrt_20q_Lg2Sq) 

residuals_regressors_plot(model_Sqrt_20q_Lg2Sq, c(2, 4), type = "rstudent")
# З'явився зсув медіани для К3.

av_plots(model_Sqrt_20q_Lg2Sq) 

crPlots(model_Sqrt_20q_Lg2Sq)
# Виправили дугу для ІФС.
par(mfrow = c(1, 1))

resettest(model_Sqrt_20q_Lg2Sq, power = 2) # p-value = 0.9331


# ГОМОСКЕДАСТИЧНІСТЬ 
# Загальна варіабельність
plot(model_Sqrt_20q_Lg2Sq, which = 3)

bptest(model_Sqrt_20q_Lg2Sq) # p-value = 0.001666

# Міжгрупова варіабельність
bartlett.test(residuals(model_Sqrt_20q_Lg2Sq) ~ df_longq$Кімнатність) # p-value = 0.2123

leveneTest(residuals(model_Sqrt_20q_Lg2Sq) ~ df_longq$Кімнатність) # p-value = 0.09857      

observe_transforms_x(model_Sqrt_20q_Lg2Sq, regressors = numeric_preds)

# Проблеми з гомоскедастичністю? Графіки виглядають нормально.
# БП-тест міг провалити тест через залежність даних.
# Модель систематично недооцінює ціну 3-кімнатних квартир.



# ==== 4) НОРМАЛЬНІСТЬ ====
qq_plot(model_Sqrt_20q_Lg2Sq)
shapiro.test(residuals(model_Sqrt_20q_Lg2Sq)) # p-value = 0.312
ad.test(residuals(model_Sqrt_20q_Lg2Sq)) # p-value = 0.3029



# ==== 5) АВТОКОРЕЛЯЦІЯ ====
time_series_residuals(model_Sqrt_20q_Lg2Sq)
resplot_matrix_cr(model_Sqrt_20q_Lg2Sq) # e1 та е2 корелюють, е3 з іншими - навряд.
cor_residuals_cr(model_Sqrt_20q_Lg2Sq) # Підверджено думку вище.

acf_cr(model_Sqrt_20q_Lg2Sq) # Часової автокореляції немає.

# Отже, використовуємо SE DK.
summary(model_Sqrt_20q_Lg2Sq)
model_feolsq <- feols(formula(model_Sqrt_20q_Lg2Sq), data = df_long_dateq, 
                     panel.id = ~ Кімнатність + Дата)

robustSE_diff(model_feolsq, vcov = "DK") # SE збільшується не більше ніж у 1.1 разів.



# ==== 6) ВІДБІР ЗМІННИХ ====
all_models <- regsubsets(formula(model_Sqrt_20q_Lg2Sq), 
                         data = df_longq, nbest = 3, nvmax = 15)
best_models <- best_models_summary(all_models, 20)


# 1. Євро + Долар + ІФС + ІГР + ІЦБ + ЧисНасел + ІЦЖВ + ІЦЖП + Кімнатність2 + Кімнатність3 + log(Долар) + log(Євро) + I(ІФС^2)  
# | Adj R² = 0.9479 (p = 14)
summary(model_feolsq, vcov = "DK")

lm1 <- update(model_feolsq, . ~ . - ЧисНасел)
summary(lm1, vcov = "DK")

lm1 <- update(lm1, . ~ . - ІГР)
summary(lm1, vcov = "DK")
# Це модель 4.3.


# 5. Євро + Долар + ІФС + ІЦБ + ЧисНасел + ІЦЖВ + Кімнатність2 + Кімнатність3 + log(Долар) + log(Євро) + I(ІФС^2)  | Adj R² = 0.9440 (p = 12)
lm2 <- update(model_feolsq, . ~ . - ІГР - ІЦЖП)
summary(lm2, vcov = "DK")

lm2 <- update(lm2, . ~ . - ЧисНасел)
summary(lm2, vcov = "DK")
# Це модель 6.1.


# 7. Євро + Долар + ІФС + ІГР + ІЦБ + ЧисНасел + ІЦЖВ + ІЦЖП + Кімнатність2 + Кімнатність3 + log(Долар) + log(Євро)  | Adj R² = 0.9432 (p = 13)
lm3 <- update(model_feolsq, . ~ . - I(I(ІФС^2)))
summary(lm3, vcov = "DK")

lm3 <- update(lm3, . ~ . - ІЦЖВ)
summary(lm3, vcov = "DK")


# -8. Євро + Долар + ІФС + ІГР + ІЦБ + ІЦЖВ + Кімнатність2 + Кімнатність3 + log(Долар) + log(Євро) + I(ІФС^2)  | Adj R² = 0.9422 (p = 12)
lm4 <- update(model_feolsq, . ~ . - ІЦЖП - ЧисНасел)
summary(lm3, vcov = "DK")

lm4 <- update(lm4, . ~ . - ІГР)
summary(lm4, vcov = "DK")
# Це модель 6.1 (було вже).


# 9. Євро + Долар + ІЦБ + ЧисНасел + ІЦЖВ + ІЦЖП + Кімнатність2 + Кімнатність3 + log(Долар) + log(Євро)  | Adj R² = 0.9387 (p = 11)
lm4 <- update(model_feolsq, . ~ . - ІФС - I(I(ІФС^2)) - ІГР)
summary(lm4, vcov = "DK")

lm4 <- update(lm4, . ~ . - ІЦЖП)
summary(lm4, vcov = "DK")
# Це модель 10.5.


# -11. Євро + Долар + ІЦБ + ЧисНасел + ІЦЖВ + Кімнатність2 + Кімнатність3 + log(Долар) + log(Євро) + I(ІФС^2)  | Adj R² = 0.9373 (p = 11)
lm5 <- update(lm4, . ~ . + I(ІФС^2))
summary(lm5, vcov = "DK")


# 14.11. Євро + Долар + ІЦБ + ІЦЖВ + Кімнатність2 + Кімнатність3 + log(Долар) + log(Євро)  | Adj R² = 0.9311 (p = 9)
lm5 <- feols(sqrt(Ціна) ~ Євро + Долар + ІЦБ + ІЦЖВ + Кімнатність + log(Долар) + log(Євро),
             data = df_long_dateq, panel.id = ~ Кімнатність + Дата)
summary(lm5, vcov = "DK")

# Вилучимо з розгляду lm3, оскільки вона містить ІГР ("погана" змінна).
final_modelsq <- list(lm1, lm2, lm4, lm5)
final_modelsq <- lapply(final_modelsq, function(m) feols(clean_formula(m), 
                                                         data = df_long_dateq, 
                                                         panel.id = ~ Кімнатність + Дата))
# ======== 7) ЕФЕКТИ ВЗАЄМОДІЇ ========
# Працюємо із чотирма моделями.
lm1 <- final_modelsq[[1]]
lm2 <- final_modelsq[[2]]
lm3 <- final_modelsq[[3]]
lm4 <- final_modelsq[[4]]


#### lm1 ####
lm1 <- update(lm1, . ~ . - I(I(ІФС^2)) + ІФС^2)
summary(lm1, vcov = "DK")

aic_base <- AIC(lm1)
bic_base <- BIC(lm1)

lm1_lmcl <- lm(formula(lm1), data = df_longq)
interactions_result <- observe_best_interactions(lm1_lmcl, df_long_dateq, top_n = 30)


# 1. Євро / Долар | ΔAIC = 11.25 | p = 0.0013 
inter_lm <- update(lm1, . ~ . + I(Євро / Долар))
summary(inter_lm, vcov = "DK")

inter_lm <- update(inter_lm, . ~ . - Євро)
summary(inter_lm, vcov = "DK")

aic_base - AIC(inter_lm) # 12.47407
bic_base - BIC(inter_lm) # 12.47407
inter_lm1 <- inter_lm


# 2. Долар / Євро | ΔAIC = 10.55 | p = 0.0018 
inter_lm <- update(lm1, . ~ . + I(Долар / Євро))
summary(inter_lm, vcov = "DK")

inter_lm <- update(inter_lm, . ~ . - Євро)
summary(inter_lm, vcov = "DK")

inter_lm <- update(inter_lm, . ~ . - log(Долар))
summary(inter_lm, vcov = "DK")

aic_base - AIC(inter_lm) # 13.02558
bic_base - BIC(inter_lm) # 15.11992
inter_lm2 <- inter_lm


#### lm2 ####
lm2 <- update(lm2, . ~ . - I(I(ІФС^2)) + ІФС^2)
summary(lm2, vcov = "DK")

aic_base <- AIC(lm2)
bic_base <- BIC(lm2)

lm2_lmcl <- lm(formula(lm2), data = df_longq)
interactions_result <- observe_best_interactions(lm2_lmcl, df_long_dateq, top_n = 30)


# 1. Євро / Долар | ΔAIC = 8.07 | p = 0.0047 
inter_lm <- update(lm2, . ~ . + I(Євро / Долар))
summary(inter_lm, vcov = "DK")

inter_lm <- update(inter_lm, . ~ . - Євро)
summary(inter_lm, vcov = "DK")

aic_base - AIC(inter_lm) # 9.350324
bic_base - BIC(inter_lm) # 9.350324
inter_lm3 <- inter_lm


# 2. Долар / Євро | ΔAIC = 7.07 | p = 0.0073 
inter_lm <- update(lm2, . ~ . + I(Долар / Євро))
summary(inter_lm, vcov = "DK")

inter_lm <- update(inter_lm, . ~ . - Євро)
summary(inter_lm, vcov = "DK")

inter_lm <- update(inter_lm, . ~ . - log(Долар))
summary(inter_lm, vcov = "DK")

aic_base - AIC(inter_lm) # 9.350324
bic_base - BIC(inter_lm) # 9.350324
inter_lm4 <- inter_lm



#### lm3 ####
summary(lm3, vcov = "DK")

aic_base <- AIC(lm3)
bic_base <- BIC(lm3)

lm3_lmcl <- lm(formula(lm3), data = df_longq)
interactions_result <- observe_best_interactions(lm3_lmcl, df_long_dateq, top_n = 30)


# 1. Долар × ІЦЖВ | ΔAIC = 11.01 | p = 0.0012 
inter_lm <- update(lm3, . ~ . + Долар:ІЦЖВ)
summary(inter_lm, vcov = "DK")

aic_base - AIC(inter_lm) # 11.00509
bic_base - BIC(inter_lm) # 8.910743
inter_lm5 <- inter_lm

# Можна поки що залишити, але є нюанс. Перевага цієї моделі була в тому, що вона
# є дещо простішою за інші. Кандидат на вилучення.



#### lm4 ####
summary(lm4, vcov = "DK")

aic_base <- AIC(lm4)
bic_base <- BIC(lm4)

lm4_lmcl <- lm(formula(lm4), data = df_longq)
interactions_result <- observe_best_interactions(lm4_lmcl, df_long_dateq, top_n = 30)

# 1. Долар × ІЦЖВ | ΔAIC = 12.54 | p = 0.0005 
inter_lm <- update(lm4, . ~ . + Долар:ІЦЖВ)
summary(inter_lm, vcov = "DK")

aic_base - AIC(inter_lm) # 12.53815
bic_base - BIC(inter_lm) # 10.4438
inter_lm6 <- inter_lm
# Ця модель не містить ЧисНасел, тому якщо ЧисНасел не робить вамого внеску у 
# виконання регресійних припущень, вона є кращою за попередню.

inter_modelsq <- list(inter_lm1, inter_lm2, inter_lm3, 
                      inter_lm4, inter_lm5, inter_lm6)

final_modelsq <- append(final_modelsq, inter_modelsq)



# ======== 8) ВАЛІДАЦІЯ МОДЕЛІ ========
final_models_lmq <- lapply(final_modelsq, function(m) lm(clean_formula(m), data = df_longq))


suppressWarnings(
  models_validation(final_models_lmq, data = df_longq, kfold_number = 5,
                    horizon = 1, init_window = 12)
)



# ======== 9) ВИБІР НАЙКРАЩОЇ МОДЕЛІ ========
# Вибір найкращої моделі буде оснований на тому, яка модель має найменше "порушень".
# Найбільшою проблемою для повної моделі було зміщення середнього для рівнів
# категоріальної змінної.
length(final_models_lmq)

residuals_plot(final_models_lmq[[1]]) # Задовільно.
residuals_regressors_plot(final_models_lmq[[1]], c(3, 4)) # Значна недооцінка всіх К.

residuals_plot(final_models_lmq[[2]]) 
residuals_regressors_plot(final_models_lmq[[2]], c(3, 4)) # Приблизно те саме.

residuals_plot(final_models_lmq[[3]]) # Краще.
residuals_regressors_plot(final_models_lmq[[3]], c(3, 4)) # Незначна? недооцінка для К2.
# +

residuals_plot(final_models_lmq[[4]]) # Майже те саме, що і для 3.
residuals_regressors_plot(final_models_lmq[[4]], c(3, 4)) # Незначна недооцінка всіх К.

residuals_plot(final_models_lmq[[5]]) 
residuals_regressors_plot(final_models_lmq[[5]], c(3, 4)) # Величезна недооцінка К3.

residuals_plot(final_models_lmq[[6]]) 
residuals_regressors_plot(final_models_lmq[[6]], c(3, 4)) # Величезна недооцінка K3.

residuals_plot(final_models_lmq[[7]])
residuals_regressors_plot(final_models_lmq[[7]], c(3, 3)) # Величезна недооцінка K3.

residuals_plot(final_models_lmq[[8]]) 
residuals_regressors_plot(final_models_lmq[[8]], c(3, 3)) # Значна недооцінка К2, K3.

residuals_plot(final_models_lmq[[9]]) 
residuals_regressors_plot(final_models_lmq[[9]], c(3, 3)) # Значна K3.

residuals_plot(final_models_lmq[[10]])
residuals_regressors_plot(final_models_lmq[[10]], c(3, 3)) # Значна недооцінка К1; незначна - К3.


best_modelsq <- list(final_models_lmq[[3]]) # Найкраща модель - одна.


lm_to_feols_summary(best_modelsq[[1]], df_long_dateq)
models_validation(best_modelsq, df_longq, horizon = 1, kfold_number = 5,
                  init_window = 12)


# Методологічне питання.
# Чи варто розглядати модель із ІндМатСтан (26 щомісячних спостережень)?
# Скоріш за все, модель на її основі буде гіршою за модель на поквартальних даних,
# а усереднення даних за 2 роки по кварталах потягне за собою роботу з надзвичайно
# малою вибіркою.