# ----- 20 СПОСТЕРЕЖЕНЬ -----
# ==== 1) МУЛЬТИКОЛІНЕАРНІСТЬ-1. КОРЕЛЯЦІЙНА МАТРИЦЯ ====
model2_20q_df <- upd_model_df(model_df_20q, remove = "Ч", room_num = 2)
colnames(model2_20q_df)[colnames(model2_20q_df) == "К2"] <- "Ціна"
model2_20q_df <- model2_20q_df[, !names(model2_20q_df) %in% "РівДолар"]
head(model2_20q_df, 3)

model2_20q_df_new <- model2_20q_df
model2_20q_df_new$`ІЦЖП_ІЦЖВ` <- model2_20q_df$ІЦЖП / model2_20q_df$ІЦЖВ
model2_20q_df_new <- model2_20q_df_new[, !names(model2_20q_df) %in% c("ІЦЖП", "ІЦЖВ")]
head(model2_20q_df_new)

# 7 предикторів.



# ==== 2) ПОВНА МОДЕЛЬ ====
model_full2_20q <- lm(Ціна ~ ., data = model2_20q_df_new)
summary(model_full2_20q) 



# ==== 3) МУЛЬТИКОЛІНЕАРНІСТЬ-2. VIF, ЧИСЛО ОБУМОВЛЕНОСТІ ====
model2_20q_df_norm <- as.data.frame(lapply(model2_20q_df_new, unit_length_scale))
model_full2_20q_norm <- lm(Ціна ~ ., data = model2_20q_df_norm)
vif(model_full2_20q_norm)
# Євро     Долар       ІФС       ІГР       ІЦБ  ЧисНасел ІЦЖП_ІЦЖВ 
# 6.309188  4.204932  1.621489  1.153071  3.192897  3.812054  3.290519 

condition_number(model2_20q_df_norm, 0) # 12.32629



# ==== 4) АНАЛІЗ ЗАЛИШКІВ І ВИЯВЛЕННЯ ВИКИДІВ ====
residuals_plot(model_full2_20q)

residuals_regressors_plot(model_full2_20q, c(2, 4), type = "rstudent")

cooks_dist(model_full2_20q)

df_fits(model_full2_20q)

df_betas(model_full2_20q)



# ==== 5) ФУНКЦІОНАЛЬНІ ПЕРЕТВОРЕННЯ ====
# ЛІНІЙНІСТЬ
plot(model_full2_20q, which = 1)
# ?

residuals_regressors_plot(model_full2_20q, c(2, 4), type = "rstudent")
# Тенденцій не видно.

av_plots(model_full2_20q) # ІГР - під питанням.

crPlots(model_full2_20q) # ІГР - ? ІФС - U-патерн.

resettest(model_full2_20q, power = 2) # p-value = 0.6804


# ГОМОСКЕДАСТИЧНІСТЬ
plot(model_full2_20q, which = 3) # Дзвін?

bptest(model_full2_20q) # p-value = 0.2329

bc <- boxcox(model_full2_20q, lambda = seq(-2, 2, by = 0.1))
lambda_opt <- bc$x[which.max(bc$y)]
cat("Оптимальне λ:", round(lambda_opt, 2), "\n") # -0.18 
# ДІ містить 1. Не перетворюємо.

observe_transforms_y(model_full2_20q)

observe_transforms_x(model_full2_20q, regressors = names(coef(model_full2_20q))[-1])


# ==== 6) НОРМАЛЬНІСТЬ ====
qq_plot(model_full2_20q)
shapiro.test(residuals(model_full2_20q)) # p-value = 0.3937

ad.test(residuals(model_full2_20q)) # p-value = 0.5366



# ==== 7) АВТОКОРЕЛЯЦІЯ ====
time_series_residuals(model_full2_20q)
dwtest(model_full2_20q) # DW = 2.131, p-value = 0.07656
summary(model_full2_20q)
coeftest(model_full2_20q, vcov = vcovHAC(model_full2_20q))



# ==== 8) ВІДБІР ЗМІННИХ ====
all_models <- regsubsets(formula(model_full2_20q), 
                         data = model2_20q_df_new, nbest = 3, nvmax = 11)
best_models <- best_models_summary(all_models, 12)


# 1. ІФС + ІГР + ІЦБ + ЧисНасел + ІЦЖП_ІЦЖВ  | Adj R² = 0.7301 (p = 6)
lm1 <- lm(Ціна ~ ІФС + ІГР + ІЦБ + ЧисНасел + ІЦЖП_ІЦЖВ,
          data = model2_20q_df_new)
summary(lm1)
coeftest(lm1, vcov = vcovHAC(lm1))

lm1 <- update(lm1, . ~ . - ІФС)
summary(lm1)
coeftest(lm1, vcov = vcovHAC(lm1))

lm1 <- update(lm1, . ~ . - ІГР)
summary(lm1)
coeftest(lm1, vcov = vcovHAC(lm1))
# Це модель 9.1.2.


# -2. Долар + ІФС + ІГР + ІЦБ + ЧисНасел + ІЦЖП_ІЦЖВ  | Adj R² = 0.7094 (p = 7)
lm2 <- lm(Ціна ~ Долар + ІФС + ІГР + ІЦБ + ЧисНасел + ІЦЖП_ІЦЖВ,
          data = model2_20q_df_new)
summary(lm2)
coeftest(lm2, vcov = vcovHAC(lm2)) # Долар - незначуща. Після її вилучення 
# отримаємо попередню модель.


# -3. Євро + ІФС + ІГР + ІЦБ + ЧисНасел + ІЦЖП_ІЦЖВ  | Adj R² = 0.7094 (p = 7)
lm2 <- lm(Ціна ~ Євро + ІФС + ІГР + ІЦБ + ЧисНасел + ІЦЖП_ІЦЖВ,
          data = model2_20q_df_new)
summary(lm2)
coeftest(lm2, vcov = vcovHAC(lm2)) # Те саме.



# ======== 9) ЕФЕКТИ ВЗАЄМОДІЇ ========
aic_base <- AIC(lm1)
bic_base <- BIC(lm1)

interactions_result <- observe_best_interactions(lm1, model_df_20q, top_n = 10)

# Фінальна модель - одна!
summary(lm1)


#### ФІНАЛЬНА МОДЕЛЬ ####
final_modelq_df_norm <- as.data.frame(lapply(lm1$model, unit_length_scale))
final_modelq_norm <- lm(formula(lm1), data = model2_20q_df_norm)
vif(final_modelq_norm)
# ІЦБ  ЧисНасел ІЦЖП_ІЦЖВ 
# 2.294060  2.197417  2.561132 

condition_number(final_modelq_df_norm, 0) # 8.829588

plot(lm1, which = 1)

residuals_regressors_plot(lm1, c(2, 2), type = "rstudent")

av_plots(lm1) 

crPlots(lm1)

resettest(lm1, power = 2) # p-value = 0.3482

plot(lm1, which = 3)

bptest(lm1) # p-value = 0.01427
# Гетероскедастичність!

qq_plot(lm1)
shapiro.test(residuals(lm1)) # p-value = 0.9343

ad.test(residuals(lm1)) # p-value = 0.8772

time_series_residuals(lm1)
dwtest(lm1) # DW = 1.4678, p-value = 0.02491
summary(lm1)
coeftest(lm1, vcov = vcovHAC(lm1))

final_models2q <- list()
final_models2q$`3varq` <- lm1


# ======== 10) ВАЛІДАЦІЯ МОДЕЛІ ========
models_summary(final_models2q)
suppressWarnings(
  models_validation(final_models2q, data = model2_20q_df_new, kfold_number = 5,
                    horizon = 1, init_window = 12)
) # Pred. R²: 0.5877 низький.
# МОДЕЛЬ СУТТЄВО ГІРША ЗА МОДЕЛЬ НА ШИРШІЙ ВИБІРЦІ.



