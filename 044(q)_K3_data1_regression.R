# ----- 20 СПОСТЕРЕЖЕНЬ -----
# ==== 1) МУЛЬТИКОЛІНЕАРНІСТЬ-1. КОРЕЛЯЦІЙНА МАТРИЦЯ ====
model3_20q_df <- upd_model_df(model_df_20q, remove = "Ч", room_num = 3)
colnames(model3_20q_df)[colnames(model3_20q_df) == "К3"] <- "Ціна"
model3_20q_df <- model3_20q_df[, !names(model3_20q_df) %in% "РівДолар"]
head(model3_20q_df, 3)

model3_20q_df_new <- model3_20q_df
model3_20q_df_new$`ІЦЖП_ІЦЖВ` <- model3_20q_df$ІЦЖП / model3_20q_df$ІЦЖВ
model3_20q_df_new <- model3_20q_df_new[, !names(model3_20q_df) %in% c("ІЦЖП", "ІЦЖВ")]
head(model3_20q_df_new)
# 7 предикторів.



# ==== 2) ПОВНА МОДЕЛЬ ====
model_full3_20q <- lm(Ціна ~ ., data = model3_20q_df_new)
summary(model_full3_20q) 



# ==== 3) АНАЛІЗ ЗАЛИШКІВ І ВИЯВЛЕННЯ ВИКИДІВ ====
residuals_plot(model_full3_20q)
# Якщо не звертати уваги на крайню точку, то цього разу графік виглядає краще.

residuals_regressors_plot(model_full3_20q, c(2, 4), type = "rstudent")

cooks_dist(model_full3_20q)

df_fits(model_full3_20q)

df_betas(model_full3_20q)



# ==== 4) ФУНКЦІОНАЛЬНІ ПЕРЕТВОРЕННЯ ====
# ЛІНІЙНІСТЬ
plot(model_full3_20q, which = 1)

residuals_regressors_plot(model_full3_20q, c(2, 4), type = "rstudent")
# Тенденцій не видно.

av_plots(model_full3_20q) 

crPlots(model_full3_20q) # ІГР - ? 

resettest(model_full3_20q, power = 2) # p-value = 0.1106


# ГОМОСКЕДАСТИЧНІСТЬ
plot(model_full3_20q, which = 3)
# Гомоскедастиність псується тим самим крайнім спостереженням.

bptest(model_full3_20q) # p-value = 0.5194

bc <- boxcox(model_full3_20q, lambda = seq(-2, 2, by = 0.1))
lambda_opt <- bc$x[which.max(bc$y)]
cat("Оптимальне λ:", round(lambda_opt, 2), "\n") # -2
# ДІ містить 1. Не перетворюємо.

observe_transforms_y(model_full3_20q)

observe_transforms_x(model_full3_20q, regressors = names(coef(model_full3_20q))[-1])



# ==== 5) НОРМАЛЬНІСТЬ ====
qq_plot(model_full3_20q)
shapiro.test(residuals(model_full3_20q)) # p-value = 0.4567

ad.test(residuals(model_full3_20q)) # p-value = 0.5039



# ==== 6) АВТОКОРЕЛЯЦІЯ ====
time_series_residuals(model_full3_20q)
dwtest(model_full3_20q) # DW = 2.1053, p-value = 0.0669
summary(model_full3_20q)
coeftest(model_full3_20q, vcov = vcovHAC(model_full3_20q))



# ==== 7) ВІДБІР ЗМІННИХ ====
all_models <- regsubsets(formula(model_full3_20q), 
                         data = model3_20q_df_new, nbest = 3, nvmax = 11)
best_models <- best_models_summary(all_models, 12)


# 1. ІФС + ІГР + ІЦБ + ЧисНасел + ІЦЖП_ІЦЖВ  | Adj R² = 0.5926 (p = 6)
lm1 <- lm(Ціна ~ ІФС + ІГР + ІЦБ + ЧисНасел + ІЦЖП_ІЦЖВ,
          data = model3_20q_df_new)
summary(lm1)
coeftest(lm1, vcov = vcovHAC(lm1))

lm1 <- update(lm1, . ~ . - ІФС)
summary(lm1)
coeftest(lm1, vcov = vcovHAC(lm1))

# Це модель 4.2.4.


# -2. Долар + ІФС + ІГР + ІЦБ + ЧисНасел + ІЦЖП_ІЦЖВ  | Adj R² = 0.5644 (p = 7)
lm2 <- lm(Ціна ~ Долар + ІФС + ІГР + ІЦБ + ЧисНасел + ІЦЖП_ІЦЖВ,
          data = model3_20q_df_new)
summary(lm2)
coeftest(lm2, vcov = vcovHAC(lm2)) # Долар - незначуща. Після її вилучення 
# отримаємо попередню модель.


# -3. Євро + ІФС + ІГР + ІЦБ + ЧисНасел + ІЦЖП_ІЦЖВ  | Adj R² = 0.7094 (p = 7)
lm2 <- lm(Ціна ~ Євро + ІФС + ІГР + ІЦБ + ЧисНасел + ІЦЖП_ІЦЖВ,
          data = model3_20q_df_new)
summary(lm2)
coeftest(lm2, vcov = vcovHAC(lm2)) # Те саме.



# ======== 8) ЕФЕКТИ ВЗАЄМОДІЇ ========
aic_base <- AIC(lm1)
bic_base <- BIC(lm1)

interactions_result <- observe_best_interactions(lm1, model_df_20q, top_n = 10)

# Фінальна модель - одна!
summary(lm1)


#### ФІНАЛЬНА МОДЕЛЬ ####
final_modelq_df_norm <- as.data.frame(lapply(lm1$model, unit_length_scale))
final_modelq_norm <- lm(formula(lm1), data = final_modelq_df_norm)
vif(final_modelq_norm)
# ІГР       ІЦБ  ЧисНасел ІЦЖП_ІЦЖВ 
# 1.070126  2.392409  2.221767  2.563064 

condition_number(final_modelq_df_norm, 0) # 9.039134

plot(lm1, which = 1)

residuals_regressors_plot(lm1, c(2, 2), type = "rstudent")

av_plots(lm1) 

crPlots(lm1)

resettest(lm1, power = 2) # p-value = 0.1131

plot(lm1, which = 3)

bptest(lm1) # p-value = 0.6646

qq_plot(lm1)
shapiro.test(residuals(lm1)) # p-value = 0.07592

ad.test(residuals(lm1)) # p-value = 0.1556

time_series_residuals(lm1)
dwtest(lm1) # DW = 2.0814, p-value = 0.2531
summary(lm1)
coeftest(lm1, vcov = vcovHAC(lm1))

final_models3q <- list()
final_models3q$`4varq` <- lm1


# ======== 10) ВАЛІДАЦІЯ МОДЕЛІ ========
models_summary(final_models3q)
suppressWarnings(
  models_validation(final_models3q, data = model3_20q_df_new, kfold_number = 5,
                    horizon = 1, init_window = 12)
)
# Погано!