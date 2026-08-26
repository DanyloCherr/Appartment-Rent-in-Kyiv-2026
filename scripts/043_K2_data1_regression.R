# ----- 26 спостережень -----
# ==== 1) МУЛЬТИКОЛІНЕАРНІСТЬ-1. КОРЕЛЯЦІЙНА МАТРИЦЯ ====
model_cor_matrix(model_df_26[, -1], "") # О ГОСПОДИ!

model2_26_df <- upd_model_df(model_df_26, "ЧистГрнКред", room_num = 2)
model_cor_matrix(model2_26_df, "")

model2_26_df <- upd_model_df(model2_26_df, "Ч", room_num = 2)
model_cor_matrix(model2_26_df, "")

model2_26_df <- upd_model_df(model2_26_df, "ІЦБ", room_num = 2)
model_cor_matrix(model2_26_df, "")
# Сильно корельованими залишаються Євро і долар. Замінимо їх ефектом взаємодії.
colnames(model2_26_df)[colnames(model2_26_df) == "К2"] <- "Ціна"

model2_26_df_inter <- model2_26_df
model2_26_df_inter$Євро_Долар <- model2_26_df$Євро / model2_26_df$Долар
model2_26_df_inter <- upd_model_df(model2_26_df_inter, c("Євро", "Долар"))
model_cor_matrix(model2_26_df_inter, "")
# Але тепер курс валют не корелює з ціною. Якщо виявиться, що він не є значущим 
# предиктором у моделі, розглянемо модель із однією з валют.
model2_26_df <- upd_model_df(model2_26_df, "Євро")

head(model2_26_df)
head(model2_26_df_inter)


# ==== 2) ПОВНА МОДЕЛЬ ====
model_full2_26 <- lm(Ціна ~ ., data = model2_26_df)
summary(model_full2_26)

model_full2_26i <- lm(Ціна ~ ., data = model2_26_df_inter)
summary(model_full2_26i)
# Багато статистично незначущих змінних.
# Друга трохи краща.


# ==== 3) МУЛЬТИКОЛІНЕАРНІСТЬ-2. VIF, ЧИСЛО ОБУМОВЛЕНОСТІ ====
compute_vifs(model_full2_26)
compute_vifs(model_full2_26i)

model2_26_df_norm <- as.data.frame(lapply(model2_26_df, unit_length_scale))
condition_number(model2_26_df_norm, 0) # 20.95837

model2_26_dfi_norm <- as.data.frame(lapply(model2_26_df_inter, unit_length_scale))
condition_number(model2_26_dfi_norm, 0) # 22.84647


# ==== 4) АНАЛІЗ ЗАЛИШКІВ І ВИЯВЛЕННЯ ВИКИДІВ ====
residuals_plot(model_full2_26)
residuals_plot(model_full2_26i)

residuals_regressors_plot(model_full2_26, c(2, 3), type = "rstudent")
residuals_regressors_plot(model_full2_26i, c(2, 3), type = "rstudent")

cooks_dist(model_full2_26)
df_fits(model_full2_26)
df_betas(model_full2_26)
# 25

cooks_dist(model_full2_26i)
df_fits(model_full2_26i)
df_betas(model_full2_26i)
# 25 - це останнє спостереження з вибірки.


# ==== 5) ФУНКЦІОНАЛЬНІ ПЕРЕТВОРЕННЯ ====
# ЛІНІЙНІСТЬ І ГОМОСКЕДАСТИЧНІСТЬ
plot(model_full2_26, which = 1)
plot(model_full2_26i, which = 1)
# U-подібні патерни.

residuals_regressors_plot(model_full2_26, c(2, 3), type = "rstudent")
residuals_regressors_plot(model_full2_26i, c(2, 3), type = "rstudent")
# Трендів не видно.
# Долар, ІндМатСтан - воронки. ІФС, ІГР - дивно.

av_plots(model_full2_26)
av_plots(model_full2_26i) # ІндМатСтан - не дуже.


resettest(model_full2_26, power = 2) # p-value = 1.847e-05
resettest(model_full2_26i, power = 2) # p-value = 0.0003365


plot(model_full2_26, which = 3) # Краще, але проблеми є.
plot(model_full2_26i, which = 3) # Таке.

bptest(model_full2_26) # p-value = 0.2349
bptest(model_full2_26i) # p-value = 0.1809


# ФУНКЦІОНАЛЬНІ ПЕРЕТВОРЕННЯ
observe_transforms_y(model_full2_26)

bc <- boxcox(model_full2_26, lambda = seq(-2, 2, by = 0.1))
lambda_opt <- bc$x[which.max(bc$y)]
cat("Оптимальне λ:", round(lambda_opt, 2), "\n")
# 0 потрапляє в ДІ, можна спробувати логарифм.

# Залишки мають U-подібний патерн, тому використаємо логарифм від відгуку.
plot(model_full2_26, which = 1)
model_full2_26 <- lm(log(Ціна) ~ ., data = model2_26_df)
plot(model_full2_26, which = 1)


observe_transforms_x(model_full2_26, regressors = names(coef(model_full2_26))[-1])
# +sqrt(ІФС) -> 25.1829579  
model_full2_26 <- update(model_full2_26, . ~ . + sqrt(ІФС))


observe_transforms_y(model_full2_26i)

bc <- boxcox(model_full2_26i, lambda = seq(-2, 2, by = 0.1))
lambda_opt <- bc$x[which.max(bc$y)]
cat("Оптимальне λ:", round(lambda_opt, 2), "\n")
# Застосусуємо обернене перетворення до відгуку.

model_full2_26i <- lm(I(1/Ціна) ~ ., data = model2_26_df_inter)
observe_transforms_x(model_full2_26i, regressors = names(coef(model_full2_26i))[-1])
# +sqrt(ІФС) -> 18.2459061   
model_full2_26i <- update(model_full2_26i, . ~ . + sqrt(ІФС))


# ======== НОВА МОДЕЛЬ 1 ========
summary(model_full2_26)
summary(model_full2_26i)

# ЛІНІЙНІСТЬ
plot(model_full2_26, which = 1)
plot(model_full2_26i, which = 1)

residuals_regressors_plot(model_full2_26, c(2, 4), type = "rstudent")
residuals_regressors_plot(model_full2_26i, c(2, 4), type = "rstudent")

av_plots(model_full2_26)
av_plots(model_full2_26i)


resettest(model_full2_26, power = 2) # p-value = 0.004996
resettest(model_full2_26i, power = 2) # p-value = 0.0001465

plot(model_full2_26, which = 3)
plot(model_full2_26i, which = 3)

bptest(model_full2_26) # p-value = 0.4218
bptest(model_full2_26i) # p-value = 0.2063


# ФУНКЦІОНАЛЬНІ ПЕРЕТВОРЕННЯ
observe_transforms_x(model_full2_26, regressors = names(coef(model_full2_26))[-1])
# +log(ІГР) -> 14.4143063  
model_full2_26 <- update(model_full2_26, . ~ . + log(ІГР))


observe_transforms_x(model_full2_26i, regressors = names(coef(model_full2_26i))[-1])
# Другу модель не змінюємо.


# ======== НОВА МОДЕЛЬ 2 ========
summary(model_full2_26)

plot(model_full2_26, which = 1) # Непогано.

residuals_regressors_plot(model_full2_26, c(2, 4), type = "rstudent")

av_plots(model_full2_26)

resettest(model_full2_26, power = 2) # p-value = 0.3296

plot(model_full2_26, which = 3)

bptest(model_full2_26) # p-value = 0.04953

# Перша модель - невеликі проблеми з лінійністю та біда з гомоскедастичністю.
# Друга модель - проблема з обома.


observe_best_interactions(model_full2_26, model2_26_df, top_n = 10)
# Значного покращення немає.

observe_best_interactions(model_full2_26i, model2_26_df_inter, top_n = 10)
# 1. РівДолар × Євро_Долар | ΔAIC = 19.14 | p = 0.0003 
model_full2_26i <- update(model_full2_26i, . ~ . + РівДолар:Євро_Долар)
resettest(model_full2_26i, power = 2) # 0.2914 - не допомогло.
summary(model_full2_26i)
plot(model_full2_26i, which = 1)



# ==== 6) НОРМАЛЬНІСТЬ ====
qq_plot(model_full2_26)
shapiro.test(residuals(model_full2_26)) # p-value = 0.7146
qq_plot(model_full2_26i)
shapiro.test(residuals(model_full2_26i)) # p-value = 0.5987
# Гіпотезу про нормальність не відхиляємо. На графіку спостерігається права асиметрія,
# проте оскільки спостережень мало, можемо стверджувати, що вона асиметрія зумовлена
# кількома окремим спостереженнями.
# При чому, будемо використовувати HC3-SE, оскільки модель потерпає від 
# гетероскедастичності, тому при перевірці значимості та побудові ДІ будемо
# відштовхуватися від них.



# ==== 7) АВТОКОРЕЛЯЦІЯ ====
time_series_residuals(model_full2_26) # Не очевидно.
dwtest(model_full2_26) # DW = 2.0875, p-value = 0.1274
acf(residuals(model_full2_26)) # Можлива автокореляція 2 порядку.

time_series_residuals(model_full2_26i) # Не очевидно.
dwtest(model_full2_26i) # DW = 2.0832, p-value = 0.1289
acf(residuals(model_full2_26i))
# Графік acf показав, що автокореляція є незначною.

summary(model_full2_26)
coeftest(model_full2_26, vcov = vcovHC(model_full2_26, type = "HC3"))

summary(model_full2_26i)
coeftest(model_full2_26i, vcov = vcovHC(model_full2_26i, type = "HC3"))

# ЗАСТЕРЕЖЕННЯ ЩОДО МОДЕЛІ
# Нелінійність, гетероскдестичність.
# Використовуємо робастні похибки та ставимося до висновків із моделі з обережністю.



# ==== 8) ВІДБІР ЗМІННИХ ====
#### Перша модель (містить Долар) ####
all_models <- regsubsets(Ціна ~ Долар + ІФС + ІГР + ЧисНасел + 
                           РівДолар + ІндМатСтан + 
                           sqrt(ІФС) + log(ІГР), 
                         data = model2_26_df, nbest = 3)
best_models <- best_models_summary(all_models, 10)

# 1.1.1. Це повна модель.
summary(model_full2_26)
coeftest(model_full2_26, vcov = vcovHC(model_full2_26, type = "HC3"))

lm1 <- update(model_full1_26, . ~ . - ІГР)
summary(lm1)
coeftest(lm1, vcov = vcovHC(lm1, type = "HC3"))

lm1 <- update(lm1, . ~ . - sqrt(ІГР)) 
summary(lm1)
coeftest(lm1, vcov = vcovHC(lm1, type = "HC3"))

lm1 <- update(lm1, . ~ . - ЧисНасел)
summary(lm1)
coeftest(lm1, vcov = vcovHC(lm1, type = "HC3"))
# Тут ІндМатСтан не є значущою. Модель не має сенсу.


# 9. Долар + ІФС + РівДолар + ІндМатСтан + sqrt(ІФС)  | Adj R² = 0.8187 (p = 6) 
# Після вилучення незначущих змінних це попередня модель.


#### Друга модель (містить Євро/Долар) ####
all_models <- regsubsets(I(1/Ціна) ~ ІФС + ІГР + ЧисНасел + 
                           РівДолар + ІндМатСтан + Євро_Долар + 
                           sqrt(ІФС) + РівДолар:Євро_Долар, data = model2_26_df_inter, nbest = 3)
best_models <- best_models_summary(all_models, 10)

# 1.2.1 Повна модель.
lm1 <- model_full2_26i
summary(lm1)
coeftest(lm1, vcov = vcovHC(lm1, type = "HC3"))

lm1 <- update(lm1, . ~ . - ІГР)
summary(lm1)
coeftest(lm1, vcov = vcovHC(lm1, type = "HC3")) # ІндМатСтан - не значуща.

# 5. Повна модель без ІГР та ефекту взаємодії.
lm1 <- update(lm1, . ~ . - РівДолар:Євро_Долар)
summary(lm1)
coeftest(lm1, vcov = vcovHC(lm1, type = "HC3")) # ІндМатСтан - не значуща.

# 8. ІФС + ІГР + ЧисНасел + ІндМатСтан + sqrt(ІФС) + РівДолар:Євро_Долар  | Adj R² = 0.8751 (p = 7)
lm1 <- lm(formula = I(1/Ціна) ~ ІФС + ІГР + ЧисНасел + ІндМатСтан + sqrt(ІФС) 
          + РівДолар:Євро_Долар, data = model2_26_df_inter) 
summary(lm1)
coeftest(lm1, vcov = vcovHC(lm1, type = "HC3")) # ІндМатСтан - не значуща.

# ВИСНОВОК.
# Змінна ІндМатСтан не є хорошим предиктором у поясненні Ціни.