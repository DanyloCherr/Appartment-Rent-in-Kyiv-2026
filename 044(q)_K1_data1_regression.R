# ----- 20 СПОСТЕРЕЖЕНЬ -----
# ==== 1) МУЛЬТИКОЛІНЕАРНІСТЬ-1. КОРЕЛЯЦІЙНА МАТРИЦЯ ====
model_cor_matrix(model_df_20q[, -1], "")

model1_20q_df <- upd_model_df(model_df_20q, remove = "Ч", room_num = 1)
colnames(model1_20q_df)[colnames(model1_20q_df) == "К1"] <- "Ціна"
head(model1_20q_df, 3)

model_cor_matrix(model1_20q_df, "")
model1_20q_df <- model1_20q_df[, !names(model1_20q_df) %in% "РівДолар"]
head(model1_20q_df, 3)
# 8 предикторів


# ==== 2) ПОВНА МОДЕЛЬ ====
model_full1_20q <- lm(Ціна ~ ., data = model1_20q_df)
summary(model_full1_20q) # Багато незначущих.


# ==== 3) МУЛЬТИКОЛІНЕАРНІСТЬ-2. VIF, ЧИСЛО ОБУМОВЛЕНОСТІ ====
model1_20q_df_norm <- as.data.frame(lapply(model1_20q_df, unit_length_scale))
model_full1_20q_norm <- lm(Ціна ~ ., data = model1_20q_df_norm)
vif(model_full1_20q_norm)
#      Євро     Долар       ІФС       ІГР       ІЦБ  ЧисНасел      ІЦЖВ      ІЦЖП 
# 12.776018  5.849003  2.455021  3.038516  3.200196  4.055058 13.393851 17.607613 
# Погано. Розглянемо тоді відношення ІЦЖВ/ІЦЖП.

model1_20q_df_new <- model1_20q_df
model1_20q_df_new$`ІЦЖП_ІЦЖВ` <- model1_20q_df$ІЦЖП / model1_20q_df$ІЦЖВ
model1_20q_df_new <- model1_20q_df_new[, !names(model1_20q_df) %in% c("ІЦЖП", "ІЦЖВ")]
head(model1_20q_df_new)

model_full1_20q <- lm(Ціна ~ ., data = model1_20q_df_new)
summary(model_full1_20q) 

model1_20q_df_norm <- as.data.frame(lapply(model1_20q_df_new, unit_length_scale))
model_full1_20q_norm <- lm(formula(model_full1_20q), data = model1_20q_df_norm)
vif(model_full1_20q_norm)
# Євро     Долар       ІФС       ІГР       ІЦБ  ЧисНасел ІЦЖП_ІЦЖВ 
# 6.309188  4.204932  1.621489  1.153071  3.192897  3.812054  3.290519 

condition_number(model1_20q_df_norm, 0) # 12.32629


# ==== 4) АНАЛІЗ ЗАЛИШКІВ І ВИЯВЛЕННЯ ВИКИДІВ ====
residuals_plot(model_full1_20q)

residuals_regressors_plot(model_full1_20q, c(2, 4), type = "rstudent")

cooks_dist(model_full1_20q)

df_fits(model_full1_20q)

df_betas(model_full1_20q)


# ==== 5) ФУНКЦІОНАЛЬНІ ПЕРЕТВОРЕННЯ ====
# ЛІНІЙНІСТЬ
plot(model_full1_20q, which = 1)
# ?

residuals_regressors_plot(model_full1_20q, c(2, 4), type = "rstudent")
# Тенденцій не видно.

av_plots(model_full1_20q) # ІФС, ІГР, ІЦЖВ, ІЦЖП - під питанням.

crPlots(model_full1_20q) # ІГР - ? ІФС - U-патерн.

resettest(model_full1_20q, power = 2) # p-value = 0.6964


# ГОМОСКЕДАСТИЧНІСТЬ
plot(model_full1_20q, which = 3) # Дзвін?.
# Скоріш за все проблема не у формі зв'язку, а в особливості даних (пам'ятаймо,
# що у нас всього 20 спостережень). Така форма зумовлена кількома спостереженнями,
# які тягнуть криву вгору.

residuals_regressors_plot(model_full1_20q, c(2, 4), type = "rstudent")
# ІГР - не гарно, але без патернів.

bptest(model_full1_20q) # p-value = 0.388

bc <- boxcox(model_full1_20q, lambda = seq(-2, 2, by = 0.1))
lambda_opt <- bc$x[which.max(bc$y)]
cat("Оптимальне λ:", round(lambda_opt, 2), "\n") # 0.18
# ДІ містить 1. Не перетворюємо.

observe_transforms_y(model_full1_20q)

observe_transforms_x(model_full1_20q, regressors = names(coef(model_full1_20q))[-1])


# ==== 6) НОРМАЛЬНІСТЬ ====
qq_plot(model_full1_20q)
shapiro.test(residuals(model_full1_20q)) # p-value = 0.3475

ad.test(residuals(model_full1_20q)) # p-value = 0.548


# ==== 7) АВТОКОРЕЛЯЦІЯ ====
time_series_residuals(model_full1_20q)
dwtest(model_full1_20q) # DW = 2.1884, p-value = 0.102
summary(model_full1_20q)
coeftest(model_full1_20q, vcov = vcovHAC(model_full1_20q))


# ==== 8) ВІДБІР ЗМІННИХ ====
all_models <- regsubsets(formula(model_full1_20q), 
                         data = model1_20q_df_new, nbest = 3, nvmax = 11)
best_models <- best_models_summary(all_models, 12)


# 1. ІФС + ІГР + ІЦБ + ЧисНасел + ІЦЖП_ІЦЖВ  | Adj R² = 0.8155 (p = 6)
lm1 <- lm(Ціна ~ ІФС + ІГР + ІЦБ + ЧисНасел + ІЦЖП_ІЦЖВ,
          data = model1_20q_df_new)
summary(lm1)
coeftest(lm1, vcov = vcovHAC(lm1))

lm1 <- update(lm1, . ~ . - ІФС)
summary(lm1)
coeftest(lm1, vcov = vcovHAC(lm1))

lm1 <- update(lm1, . ~ . - ІГР)
summary(lm1)
coeftest(lm1, vcov = vcovHAC(lm1))
# Це модель 5.1.8.


# -4. Євро + ІГР + ІЦБ + ЧисНасел + ІЦЖП_ІЦЖВ  | Adj R² = 0.8026 (p = 6)
lm2 <- lm(Ціна ~ Євро + ІГР + ІЦБ + ЧисНасел + ІЦЖП_ІЦЖВ,
          data = model1_20q_df_new)
summary(lm2)
coeftest(lm2, vcov = vcovHAC(lm1)) # Євро - незначуща. Після її вилучення 
# отримаємо попередню модель.


# -8. Долар + ІГР + ІЦБ + ЧисНасел + ІЦЖП_ІЦЖВ  | Adj R² = 0.8010 (p = 6)
lm2 <- lm(Ціна ~ Долар + ІГР + ІЦБ + ЧисНасел + ІЦЖП_ІЦЖВ,
          data = model1_20q_df_new)
summary(lm2)
coeftest(lm2, vcov = vcovHAC(lm2)) # Те саме.


# -9. Євро + ІЦБ + ЧисНасел + ІЦЖП_ІЦЖВ  | Adj R² = 0.7974 (p = 5)
lm2 <- lm(Ціна ~ Євро + ІЦБ + ЧисНасел + ІЦЖП_ІЦЖВ,
          data = model1_20q_df_new)
summary(lm2)
coeftest(lm2, vcov = vcovHAC(lm2))


final_models1q <- list()
final_models1q$`3varq` <- lm1



# ======== 9) ЕФЕКТИ ВЗАЄМОДІЇ ========
aic_base <- AIC(lm1)
bic_base <- BIC(lm1)

interactions_result <- observe_best_interactions(lm1, model_df_20q, top_n = 10)

# Фінальна модель - одна!
summary(lm1)


#### ФІНАЛЬНА МОДЕЛЬ ####
final_modelq_df_norm <- as.data.frame(lapply(lm1$model, unit_length_scale))
final_modelq_norm <- lm(formula(lm1), data = model1_20q_df_norm)
vif(final_modelq_norm)
# ІЦБ  ЧисНасел ІЦЖП_ІЦЖВ 
# 2.294060  2.197417  2.561132 

condition_number(final_modelq_df_norm, 0) # 8.829588

plot(lm1, which = 1)

residuals_regressors_plot(lm1, c(2, 2), type = "rstudent")

av_plots(lm1) 

crPlots(lm1)

resettest(lm1, power = 2) # p-value = 0.3545

plot(lm1, which = 3) # Дзвін?

bptest(lm1) # p-value = 0.03562
# Гетероскедастичність!

qq_plot(lm1)
shapiro.test(residuals(lm1)) # p-value = 0.8961

ad.test(residuals(lm1)) # p-value = 0.8207

time_series_residuals(lm1)
dwtest(lm1) # DW = 1.5515, p-value = 0.03907
summary(lm1)
coeftest(lm1, vcov = vcovHAC(lm1))



# ======== 10) ВАЛІДАЦІЯ МОДЕЛІ ========
suppressWarnings(
  models_validation(final_models1q, data = model1_20q_df_new, kfold_number = 5,
                    horizon = 1, init_window = 12)
)

# Тут одна модель найкраща. АЛЕ ВОНА ГІРША ЗА ПОПЕРЕДНІ!
summary(final_models1q[[1]])
best_models1 <- append(best_models1, final_models1q)
