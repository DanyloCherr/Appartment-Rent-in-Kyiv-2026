model_df_date <- model_df_2[, !names(model_df_2) %in% c("Ч", "ІЦЖВ")]
model_df_date$`Євро_Долар` <- model_df_date$Євро / model_df_date$Долар
df_long_date <- model_df_date %>%
  pivot_longer(cols = c(К1, К2, К3), 
               names_to = "Кімнатність", 
               values_to = "Ціна") %>%
  mutate(Кімнатність = factor(Кімнатність, 
                              levels = c("К1", "К2", "К3"),
                              labels = c("1", "2", "3")))

model_df <- model_df_date[, -1]
df_long <- df_long_date[, -1]

# ==== 1) ПОВНА МОДЕЛЬ ====
model_full <- lm(Ціна ~ . - Євро - Долар, data = df_long)
summary(model_full)


# ==== 2) АНАЛІЗ ЗАЛИШКІВ І ВИЯВЛЕННЯ ВИКИДІВ ====
residuals_plot(model_full) # Кілька екстремальних значень праворуч.

residuals_regressors_plot(model_full, c(2, 4), type = "rstudent")

cooks_dist(model_full)

df_fits(model_full)

df_betas(model_full)
# 18 105 108 111 116 


# ==== 3) ФУНКЦІОНАЛЬНІ ПЕРЕТВОРЕННЯ ====
# ЛІНІЙНІСТЬ І ГОМОСКЕДАСТИЧНІСТЬ 
residuals_plot(model_full)

plot(model_full, which = 1) # U-патерн.

plot(model_full, which = 3) # Дисперсія зростає.

residuals_regressors_plot(model_full, c(3, 3), type = "rstudent")
# Сильна крос-секційна гетероскедастичність.

av_plots(model_full)

crPlots(model_full)

resettest(model_full, power = 2) # p-value = 5.718e-12

bptest(model_full) # p-value = 0.0001277

bartlett.test(residuals(model_full) ~ df_long$Кімнатність) # p-value = 9.773e-05
# Тест Бартлетта чутливий до ненормальності.

leveneTest(residuals(model_full) ~ df_long$Кімнатність) # p-value = 0.004388 

# Отже, у моделі наявні проблеми з лінійністю (?) та гомоскедастичністю.


bc <- boxcox(model_full, lambda = seq(-1, 1, by = 0.1))
lambda_opt <- bc$x[which.max(bc$y)]
cat("Оптимальне λ:", round(lambda_opt, 2), "\n") # -0.13

model_log <- lm(log(Ціна) ~ . - Євро - Долар, data = df_long)
summary(model_log)



# ======== НОВА МОДЕЛЬ 1 ========
# ЛІНІЙНІСТЬ І ГОМОСКЕДАСТИЧНІСТЬ 
residuals_plot(model_log) # Позбавилися трикунтника.

plot(model_log, which = 1) # Майже на прямій.

plot(model_log, which = 3) # Дисперсія майже стала.

residuals_regressors_plot(model_log, c(3, 3), type = "rstudent")
# Крос-секційна гетероскедастичність слабка. Зміщення в Кімнатність менше.
# ЧистГрнКред - патерн змійки (як і було).

av_plots(model_log)

crPlots(model_log)
par(mfrow = c(1, 1))
# Ці два графіки показують, що проблем з лінійністю ЧистГрнКред немає.

resettest(model_log, power = 2) # p-value = 0.7115

bptest(model_log) # p-value = 0.02406

bartlett.test(residuals(model_log) ~ df_long$Кімнатність) # p-value = 0.4385
# Тест Бартлетта чутливий до ненормальності.

leveneTest(residuals(model_log) ~ df_long$Кімнатність) # p-value = 0.5004
# Отже, у моделі наявні проблеми з лінійністю (?) та гомоскедастичністю.

all_preds <- names(coef(model_log))[-1]
numeric_preds <- all_preds[!grepl("Кімнатність", all_preds)]
observe_transforms_x(model_log, regressors = numeric_preds)

model_log_Lg <- update(model_log, . ~ . + log(ЧистГрнКред) - ЧистГрнКред)
# AIC_diff = 9.761999  
summary(model_log_Lg)



# ======== НОВА МОДЕЛЬ 2 ========
# ЛІНІЙНІСТЬ І ГОМОСКЕДАСТИЧНІСТЬ 
residuals_plot(model_log_Lg)

plot(model_log_Lg, which = 1)

plot(model_log_Lg, which = 3)

residuals_regressors_plot(model_log_Lg, c(3, 3), type = "rstudent")
# ЧистГрнКред - патерн змійки (як і було).
# ІЦБ, ІЦЖП - воронки (як і було).

av_plots(model_log_Lg)

crPlots(model_log_Lg)
par(mfrow = c(1, 1))

resettest(model_log_Lg, power = 2) # p-value = 0.5566

bptest(model_log_Lg) # p-value = 0.005693

bartlett.test(residuals(model_log_Lg) ~ df_long$Кімнатність) # p-value = 0.3037
# Тест Бартлетта чутливий до ненормальності.

leveneTest(residuals(model_log_Lg) ~ df_long$Кімнатність) # p-value = 0.2209
# Отже, у моделі наявні проблеми з лінійністю (?) та гомоскедастичністю.

observe_transforms_x(model_log_Lg, regressors = numeric_preds[numeric_preds != "ЧистГрнКред"])
# Спробуймо виправити воронку для ІЦБ та ІЦЖП.
# UPD: 
# 1) Функціональними перетвореннями (log, poly(x, 4)) змійку в ЧистГрнКред виправити не вдалося.
# Якщо не допоміг поліном такого високого степеня, то скоріш за все проблема не в нелінійності, а
# в особливості панельних дани: на кожне значення ЧистГрнКред маємо три залишки, які можуть 
# створювати хибний патерн змійки. Зауважимо, що в моделі без "Кімнатності" такої проблеми не було.
# 2) ФП також не допомогли усунити воронки для ІЦБ та ІЦЖП. Можливо, тут причина теж у панельних
# даних: через те, що фактор має три рівні, ці воронки тепер більш явні.

# Отже, у моделі наявна локальна загальна гетероскедастичність: ІФС, ІГР (?), ІЦБ, ІЦЖП.
# (все одно ми використовуємо HAC оцінки)

model_feols <- feols(formula(model_log_Lg), data = df_long_date, 
                     panel.id = ~ Кімнатність + Дата)


# ==== 4) НОРМАЛЬНІСТЬ ====
qq_plot(model_log_Lg)
shapiro.test(residuals(model_log_Lg)) # p-value = 0.6402
ad.test(residuals(model_log_Lg)) # p-value = 0.3001


# ==== 5) АВТОКОРЕЛЯЦІЯ ====
time_series_residuals(model_log_Lg)
resplot_matrix_cr(model_log_Lg)
cor_residuals_cr(model_log_Lg)
# Помітна крос-секційна кореляція (> 0.4)

acf_cr(model_log_Lg)
# Всюди спостерігається додатна автокореляція першого порядку.



# ==== 6) ВІДБІР ЗМІННИХ ====
all_models <- regsubsets(formula(model_log_Lg), 
                         data = df_long, nbest = 3, nvmax = 15)
best_models <- best_models_summary(all_models, 20)


# 1. ІФС + ІГР + ІндМатСтан + РівДолар + Євро_Долар + Кімнатність2 + Кімнатність3 + log(ЧистГрнКред)  | Adj R² = 0.9421 (p = 9)
lm1 <- update(model_feols, . ~ . - ІФС - ІЦЖП)
summary(lm1, vcov = "DK")

lm1 <- update(lm1, . ~ . - ІЦБ)
summary(lm1, vcov = "DK")

lm1 <- update(lm1, . ~ . - Євро_Долар)
summary(lm1, vcov = "DK")

lm1 <- update(lm1, . ~ . - ІГР)
summary(lm1, vcov = "DK")

lm1 <- update(lm1, . ~ . - РівДолар)
summary(lm1, vcov = "DK")

# Модель 17.17.

# 11. ІФС + ІндМатСтан + РівДолар + Кімнатність2 + Кімнатність3 + log(ЧистГрнКред)  | Adj R² = 0.9369 (p = 7)
lm2 <- feols(log(Ціна) ~ ІФС + ІндМатСтан + РівДолар + Кімнатність + log(ЧистГрнКред),
             data = df_long_date, panel.id = ~ Кімнатність + Дата)
summary(lm2, vcov = "DK")


# 12. ІФС + ІндМатСтан + Євро_Долар + Кімнатність2 + Кімнатність3 + log(ЧистГрнКред)  | Adj R² = 0.9360 (p = 7)
lm3 <- update(lm2, . ~ . - РівДолар + Євро_Долар)
summary(lm3, vcov = "DK") # pval(Євро_Долар) = 9.4690e-02
# Поки що залишимо.


final_models <- list(lm1, lm2, lm3)



# ======== 7) ЕФЕКТИ ВЗАЄМОДІЇ ========
# Працюємо із трьома моделями.
lm1 <- final_models[[1]]
lm2 <- final_models[[2]]
lm3 <- final_models[[3]]
# UPD 1. Жодних ефектів взаємодії ми не додали. Це сталося не в останню чергу через недостатню кількість
# дійсно впливових числових змінних. Тому надалі розгялядатимемо ще й взаємодії із ЧистГрнКред.
# UPD 2. Взаємодії із ЧистГрнКред за умови наявності log(ЧистГрнКред) виявилися незначущими. Спробуймо
# ввести взаємодії із log(ЧистГрнКред).


#### lm1 ####
summary(lm1)
aic_base <- AIC(lm1)
bic_base <- BIC(lm1)

lm1_lmcl <- lm(formula(lm1), data = df_long)
interactions_result <- observe_best_interactions(lm1_lmcl, df_long_date, top_n = 30, complex_interactions = TRUE)

residuals_regressors_plot(lm1_lmcl, c(2, 2))

# 2. ІндМатСтан × log(ЧистГрнКред) | ΔAIC = 12.81 | p = 0.0002 
inter_lm <- update(lm1, . ~ . + ІндМатСтан:log(ЧистГрнКред))
summary(inter_lm, vcov = "DK")

residuals_regressors_plot(update(lm1_lmcl, . ~ . + ІндМатСтан:log(ЧистГрнКред)), c(2, 2))
# Зміщення трохи виправилось.

aic_base - AIC(inter_lm) # 12.81314
bic_base - BIC(inter_lm) # 9.885891

inter_lm1 <- inter_lm



#### lm2 ####
summary(lm2)
aic_base <- AIC(lm2)
bic_base <- BIC(lm2)

lm2_lmcl <- lm(formula(lm2), data = df_long)
residuals_regressors_plot(lm2_lmcl, c(3, 2)) # Зміщення К1, К3.

interactions_result <- observe_best_interactions(lm2_lmcl, df_long_date, top_n = 30, complex_interactions = TRUE)


# 2. ІндМатСтан × log(ЧистГрнКред) | ΔAIC = 13.25 | p = 0.0002 
inter_lm <- update(lm2, . ~ . + ІндМатСтан:log(ЧистГрнКред))
summary(inter_lm, vcov = "DK")

residuals_regressors_plot(update(lm2_lmcl, . ~ . + ІндМатСтан:log(ЧистГрнКред)), c(3, 2))
# Зміщення майже немає!

aic_base - AIC(inter_lm) # 13.25228
bic_base - BIC(inter_lm) # 10.32503

inter_lm2 <- inter_lm


# -3. РівДолар × log(ЧистГрнКред) | ΔAIC = 8.78 | p = 0.0015
inter_lm <- update(lm2, . ~ . + РівДолар:log(ЧистГрнКред))
summary(inter_lm, vcov = "DK") 

inter_lm <- update(inter_lm, . ~ . - log(ЧистГрнКред))
summary(inter_lm, vcov = "DK") 

residuals_regressors_plot(update(lm2_lmcl, . ~ . + РівДолар:log(ЧистГрнКред) - log(ЧистГрнКред)), c(3, 2))

aic_base - AIC(inter_lm) # 5.953467
bic_base - BIC(inter_lm) # 5.953467

# Ця модель гірша за lm3, оскільки вона має сильніше зміщення для категорій.



#### lm3 ####
summary(lm3)
aic_base <- AIC(lm3)
bic_base <- BIC(lm3)

lm3_lmcl <- lm(formula(lm3), data = df_long)
residuals_regressors_plot(lm3_lmcl, c(3, 2)) # Зміщення К1, К3 (але незначне).

interactions_result <- observe_best_interactions(lm3_lmcl, df_long_date, top_n = 30, complex_interactions = TRUE)

# -3. ІндМатСтан × log(ЧистГрнКред) | ΔAIC = 5.95 | p = 0.0063
inter_lm <- update(lm3, . ~ . + ІндМатСтан:log(ЧистГрнКред))
summary(inter_lm, vcov = "DK") # Погано.

# Серед моделей без взаємодій найкращою моделлю є lm3, оскільки вона має найменше зміщення для категорій.


final_models <- list(lm3, inter_lm1, inter_lm2)



# ======== 8) ВАЛІДАЦІЯ МОДЕЛІ ========
final_models_lm <- lapply(final_models, function(m) lm(clean_formula(m), data = df_long))

suppressWarnings(
  models_validation(final_models_lm, data = df_long, kfold_number = 5,
                    horizon = 1, init_window = 30)
)



# ======== 9) ВИБІР НАЙКРАЩОЇ МОДЕЛІ ========
length(final_models_lm)

residuals_plot(final_models_lm[[1]])
residuals_regressors_plot(final_models_lm[[1]], c(3, 2)) # Незначна недооцінка.

residuals_plot(final_models_lm[[2]]) # Трохи краще.
residuals_regressors_plot(final_models_lm[[2]], c(3, 2)) # Загалом, така ж недооцінка.

residuals_plot(final_models_lm[[3]]) # Як 2.
residuals_regressors_plot(final_models_lm[[3]], c(3, 2)) # Майже немає зміщення.


lm_to_feols_summary(final_models_lm[[1]], df_long_date) #- Протилежні знаки.
lm_to_feols_summary(final_models_lm[[2]], df_long_date) # Логічніше, вищий R2.
lm_to_feols_summary(final_models_lm[[3]], df_long_date) # Те саме, але містить Євро замість ЧисНасел.


# Найкращою є модель, що має найменше зміщення для рівнів категорії - 3.

best_models <- list(final_models_lm[[3]])

assumptions_check(final_models_lm[[3]])

models_summary(best_models)
suppressWarnings(
  models_validation(best_models, data = df_long, kfold_number = 5,
                    horizon = 1, init_window = 30)
)


