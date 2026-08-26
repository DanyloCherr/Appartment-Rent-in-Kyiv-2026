# ----- 26 спостережень -----
# ==== 1) МУЛЬТИКОЛІНЕАРНІСТЬ-1. КОРЕЛЯЦІЙНА МАТРИЦЯ ====
model_cor_matrix(model_df_26[, -1], "") # О ГОСПОДИ!
colnames(model_df_26)
model1_26_df <- upd_model_df(model_df_26, "ЧистГрнКред")
model_cor_matrix(model1_26_df, "")

model1_26_df <- upd_model_df(model1_26_df, "Ч")
model_cor_matrix(model1_26_df, "")

model1_26_df <- upd_model_df(model1_26_df, "ІЦБ")
model_cor_matrix(model1_26_df, "")
# Сильно корельованими залишаються Євро і долар. Замінимо їх ефектом взаємодії.
(model_df_26)
colnames(model1_26_df)[colnames(model1_26_df) == "К1"] <- "Ціна"

model1_26_df_inter <- model1_26_df
model1_26_df_inter$Євро_Долар <- model1_26_df$Євро / model1_26_df$Долар
model1_26_df_inter <- upd_model_df(model1_26_df_inter, c("Євро", "Долар"))
model_cor_matrix(model1_26_df_inter, "")
# Але тепер курс валют не корелює з ціною. Якщо виявиться, що він не є значущим 
# предиктором у моделі, розглянемо модель із однією з валют.
model1_26_df <- upd_model_df(model1_26_df, "Євро")

head(model1_26_df)
head(model1_26_df_inter)


# ==== 2) ПОВНА МОДЕЛЬ ====
model_full1_26 <- lm(Ціна ~ ., data = model1_26_df)
summary(model_full1_26)

model_full1_26i <- lm(Ціна ~ ., data = model1_26_df_inter)
summary(model_full1_26i)
# Багато статистично незначущих змінних.
# Друга трохи краща.


# ==== 3) МУЛЬТИКОЛІНЕАРНІСТЬ-2. VIF, ЧИСЛО ОБУМОВЛЕНОСТІ ====
compute_vifs(model_full1_26)
compute_vifs(model_full1_26i)

model1_26_df_norm <- as.data.frame(lapply(model1_26_df, unit_length_scale))
condition_number(model1_26_df_norm, 0) # 20.95837

model1_26_dfi_norm <- as.data.frame(lapply(model1_26_df_inter, unit_length_scale))
condition_number(model1_26_dfi_norm, 0) # 22.84647


# ==== 4) АНАЛІЗ ЗАЛИШКІВ І ВИЯВЛЕННЯ ВИКИДІВ ====
residuals_plot(model_full1_26)
residuals_plot(model_full1_26i)

residuals_regressors_plot(model_full1_26, c(2, 3), type = "rstudent")
residuals_regressors_plot(model_full1_26i, c(2, 3), type = "rstudent")

cooks_dist(model_full1_26)
df_fits(model_full1_26)
df_betas(model_full1_26)
# 25

cooks_dist(model_full1_26i)
df_fits(model_full1_26i)
df_betas(model_full1_26i)
# 25 - це останнє спостереження з вибірки.


# ==== 5) ФУНКЦІОНАЛЬНІ ПЕРЕТВОРЕННЯ ====
# ЛІНІЙНІСТЬ І ГОМОСКЕДАСТИЧНІСТЬ
plot(model_full1_26, which = 1)
plot(model_full1_26i, which = 1)
# U-подібні патерни.

residuals_regressors_plot(model_full1_26, c(2, 3), type = "rstudent")
residuals_regressors_plot(model_full1_26i, c(2, 3), type = "rstudent")
# Трендів не видно.
# Долар, ІндМатСтан - воронки. ІФС, ІГР - дивно.

av_plots(model_full1_26)
av_plots(model_full1_26i) # ІндМатСтан - не дуже.


resettest(model_full1_26, power = 2) # p-value = 1.332e-05
resettest(model_full1_26i, power = 2) # p-value = 0.0002675


plot(model_full1_26, which = 3) # Краще, але проблеми є.
plot(model_full1_26i, which = 3) # Таке.

bptest(model_full1_26) # p-value = 0.09052
bptest(model_full1_26i) # p-value = 0.1158


# ФУНКЦІОНАЛЬНІ ПЕРЕТВОРЕННЯ
observe_transforms_y(model_full1_26)

bc <- boxcox(model_full1_26, lambda = seq(-2, 2, by = 0.1))
lambda_opt <- bc$x[which.max(bc$y)]
cat("Оптимальне λ:", round(lambda_opt, 2), "\n")
# 1 потрапляє в ДІ, тому перетворення не застосовуємо.

# Залишки мають U-подібний патерн, тому використаємо логарифм від відгуку.
plot(model_full1_26, which = 1)
model_sq_26 <- lm(I(Ціна^2) ~ ., data = model1_26_df)
plot(model_sq_26, which = 1)

model_log_26 <- lm(log(Ціна^2) ~ ., data = model1_26_df)
plot(model_log_26, which = 1)
# Перетворення відгуку не допомагає. Спробуймо перетворити предиктори.

model_full1_26_Lg <- update(model_full1_26, . ~ . + I(Долар^2))
residuals_regressors_plot(model_full1_26_Lg, c(2, 4), type = "rstudent")

# Не допомагає. Залишимо структуру моделі як є.
# Проаналізуємо робастні стандартні похибки.

summary(model_full1_26)
coeftest(model_full1_26, vcov = vcovHC(model_full1_26, type = "HC3"))

summary(model_full1_26i)
coeftest(model_full1_26i, vcov = vcovHC(model_full1_26i, type = "HC3"))
# Чи є різниця суттєвою?
# Повернімося до функціональних перетворень.

observe_transforms_x(model_full1_26, regressors = names(coef(model_full1_26))[-1])
# +sqrt(ІФС) -> 14.548018
model_full1_26 <- update(model_full1_26, . ~ . + sqrt(ІФС))


observe_transforms_y(model_full1_26i)

bc <- boxcox(model_full1_26i, lambda = seq(-2, 2, by = 0.1))
lambda_opt <- bc$x[which.max(bc$y)]
cat("Оптимальне λ:", round(lambda_opt, 2), "\n")

observe_transforms_x(model_full1_26i, regressors = names(coef(model_full1_26i))[-1])
# +sqrt(ІФС) -> 11.524110 
model_full1_26i <- update(model_full1_26i, . ~ . + sqrt(ІФС))



# ======== НОВА МОДЕЛЬ 1 ========
# ==== 5.1.1) МУЛЬТИКОЛІНЕАРНІСТЬ-2. VIF, ЧИСЛО ОБУМОВЛЕНОСТІ ====
summary(model_full1_26)
summary(model_full1_26i)

compute_vifs(model_full1_26)
compute_vifs(model_full1_26i)


# ==== 5.1.2) АНАЛІЗ ЗАЛИШКІВ І ВИЯВЛЕННЯ ВИКИДІВ ====
# Розберемося в кінці.
# 25 - це останнє спостереження з вибірки.


# ==== 5.1.3) ФУНКЦІОНАЛЬНІ ПЕРЕТВОРЕННЯ ====
# ЛІНІЙНІСТЬ І ГОМОСКЕДАСТИЧНІСТЬ
plot(model_full1_26, which = 1)
plot(model_full1_26i, which = 1)
# U-подібні патерни.

residuals_regressors_plot(model_full1_26, c(2, 4), type = "rstudent")
residuals_regressors_plot(model_full1_26i, c(2, 4), type = "rstudent")
# Трендів не видно.

av_plots(model_full1_26)
av_plots(model_full1_26i) # ІндМатСтан - не дуже.


resettest(model_full1_26, power = 2) # p-value = 0.001651
resettest(model_full1_26i, power = 2) # p-value = 0.0001642

plot(model_full1_26, which = 3)
plot(model_full1_26i, which = 3)

bptest(model_full1_26) # p-value = 0.3207
bptest(model_full1_26i) # p-value = 0.3957


# ФУНКЦІОНАЛЬНІ ПЕРЕТВОРЕННЯ
observe_transforms_y(model_full1_26)

bc <- boxcox(model_full1_26, lambda = seq(-6, 2, by = 0.1))
lambda_opt <- bc$x[which.max(bc$y)]
cat("Оптимальне λ:", round(lambda_opt, 2), "\n") # -5.52 - overkill

observe_transforms_x(model_full1_26, regressors = names(coef(model_full1_26))[-1])
# +sqrt(ІГР) -> 22.2617200  
model_full1_26 <- update(model_full1_26, . ~ . + sqrt(ІГР))


observe_transforms_y(model_full1_26i)

bc <- boxcox(model_full1_26i, lambda = seq(-6, 2, by = 0.1))
lambda_opt <- bc$x[which.max(bc$y)]
cat("Оптимальне λ:", round(lambda_opt, 2), "\n") 

observe_transforms_x(model_full1_26i, regressors = names(coef(model_full1_26i))[-1])
# +sqrt(ІФС) -> 11.524110 
model_full1_26i <- update(model_full1_26i, . ~ . + sqrt(ІГР))



# ======== НОВА МОДЕЛЬ 2 ========
# ==== 5.2.1) МУЛЬТИКОЛІНЕАРНІСТЬ-2. VIF, ЧИСЛО ОБУМОВЛЕНОСТІ ====
summary(model_full1_26)
summary(model_full1_26i)

compute_vifs(model_full1_26)
compute_vifs(model_full1_26i)


# ==== 5.2.2) АНАЛІЗ ЗАЛИШКІВ І ВИЯВЛЕННЯ ВИКИДІВ ====
# Розберемося в кінці.
# 25 - це останнє спостереження з вибірки.


# ==== 5.2.3) ФУНКЦІОНАЛЬНІ ПЕРЕТВОРЕННЯ ====
# ЛІНІЙНІСТЬ І ГОМОСКЕДАСТИЧНІСТЬ
plot(model_full1_26, which = 1) # Непогано.
plot(model_full1_26i, which = 1)
# U-подібні патерни.

residuals_regressors_plot(model_full1_26, c(2, 4), type = "rstudent")
residuals_regressors_plot(model_full1_26i, c(2, 4), type = "rstudent")
# Трендів не видно.

av_plots(model_full1_26)
av_plots(model_full1_26i) # ІндМатСтан - не дуже.


resettest(model_full1_26, power = 2) # p-value = 0.01882
resettest(model_full1_26i, power = 2) # p-value = 0.0007177

plot(model_full1_26, which = 3)
plot(model_full1_26i, which = 3)

bptest(model_full1_26) # p-value = 0.3207
bptest(model_full1_26i) # p-value = 0.3957


# ФУНКЦІОНАЛЬНІ ПЕРЕТВОРЕННЯ
observe_transforms_y(model_full1_26)

bc <- boxcox(model_full1_26, lambda = seq(-6, 2, by = 0.1))
lambda_opt <- bc$x[which.max(bc$y)]
cat("Оптимальне λ:", round(lambda_opt, 2), "\n") # -5.52 - overkill

observe_transforms_x(model_full1_26, regressors = names(coef(model_full1_26))[-1])


observe_transforms_y(model_full1_26i)

bc <- boxcox(model_full1_26i, lambda = seq(-6, 2, by = 0.1))
lambda_opt <- bc$x[which.max(bc$y)]
cat("Оптимальне λ:", round(lambda_opt, 2), "\n") 

observe_transforms_x(model_full1_26i, regressors = names(coef(model_full1_26i))[-1])


# Діагноз. Для моделі порушується лінійність. При чому для окремих змінних avPlots
# виглядають нормально, а для відгуку спостерігається U-подібний вигин.
# RESET тест дає мале p-value. Застосуємо ортогональні поліноми до найбільш 
# підозрілих змінних: ЧисНасел, ІндМатСтан та РівДолар (як одна з ключових).

model_poly <- update(model_full1_26, . ~ . + poly(РівДолар, 2))
resettest(model_poly, power = 2)
summary(model_poly)
plot(model_poly, which = 1)

model_polyi <- update(model_full1_26i, . ~ . + poly(ЧисНасел, 2))
resettest(model_polyi, power = 2)
summary(model_polyi)
plot(model_polyi, which = 1)


# Для трьох ключових змінних поліноми не спрацювали. RESET test все одно виявляє 
# ознаки нелінійності. Остання спроба - додати ефекти взаємодії.
observe_best_interactions(model_full1_26, model1_26_df, top_n = 10)
# Найкраще:
# 1. ІФС / ЧисНасел | ΔAIC = 4.40 | p = 0.0538 
# але все ще незначуще...
# ВІДКИДАЄМО МОДЕЛЬ

observe_best_interactions(model_full1_26i, model1_26_df_inter, top_n = 10)
# 1. РівДолар × Євро_Долар | ΔAIC = 8.73 | p = 0.0125 
model_full1_26i <- update(model_full1_26i, . ~ . + РівДолар:Євро_Долар)
resettest(model_full1_26i, power = 2)
summary(model_full1_26i)
plot(model_full1_26i, which = 1)

observe_best_interactions(model_full1_26i, model1_26_df_inter, top_n = 10)
# Більше додавати не будемо, хоча і можемо.
# RESET каже про нелінійність, хоча вигин став менш серйозним.
# Отже, схоже, що нелійнійсть моделі ховається у параметрах бета, а не в самих
# регресорах.


# ==== 6) НОРМАЛЬНІСТЬ ====
qq_plot(model_full1_26i)
shapiro.test(residuals(model_full1_26i)) # p-value = 0.2068
# Гіпотезу про нормальність не відхиляємо. На графіку спостерігається права асиметрія,
# проте оскільки спостережень мало, можемо стверджувати, що вона асиметрія зумовлена
# кількома окремим спостереженнями.
# При чому, будемо використовувати HC3-SE, оскільки модель потерпає від 
# гетероскедастичності, тому при перевірці значимості та побудові ДІ будемо
# відштовхуватися від них.



# ==== 7) АВТОКОРЕЛЯЦІЯ ====
time_series_residuals(model_full1_26i) # Не очевидно.
dwtest(model_full1_26i) # DW = 1.9464, p-value = 0.05952
acf(residuals(model_full1_26i))
# Графік acf показав, що автокореляція є незначною.

summary(model_full1_26i)
coeftest(model_full1_26i, vcov = vcovHC(model_full1_26i, type = "HC3"))

# ЗАСТЕРЕЖЕННЯ ЩОДО МОДЕЛІ
# Нелінійність, гетероскдестичність.
# Використовуємо робастні похибки та ставимося до висновків із моделі з обережністю.



# ==== 8) ВІДБІР ЗМІННИХ ====
all_models <- regsubsets(Ціна ~ ІФС + ІГР + ЧисНасел + 
                           РівДолар + ІндМатСтан + Євро_Долар + 
                           sqrt(ІФС) + sqrt(ІГР) + РівДолар:Євро_Долар, 
                         data = model1_26_df_inter, nbest = 3)
best_models <- best_models_summary(all_models, 10)

# 1.3.1. ІФС + ЧисНасел + РівДолар + ІндМатСтан + Євро_Долар + sqrt(ІФС) + sqrt(ІГР) + РівДолар:Євро_Долар  | Adj R² = 0.8219 (p = 9)
# 3.2.3. ІФС + ЧисНасел + РівДолар + ІндМатСтан + Євро_Долар + sqrt(ІФС) + РівДолар:Євро_Долар  | Adj R² = 0.8098 (p = 8)
# 5.8.4. ІФС + ІГР + ЧисНасел + РівДолар + ІндМатСтан + Євро_Долар + sqrt(ІФС) + РівДолар:Євро_Долар  | Adj R² = 0.8062 (p = 9)
# 10.10.10. ІФС + РівДолар + ІндМатСтан + Євро_Долар + sqrt(ІФС) + РівДолар:Євро_Долар  | Adj R² = 0.7592 (p = 7)


lm1 <- lm(Ціна ~ ІФС + ЧисНасел + РівДолар + ІндМатСтан + Євро_Долар + sqrt(ІФС) 
          + sqrt(ІГР) + РівДолар:Євро_Долар, data = model1_26_df_inter)
summary(lm1)
coeftest(lm1, vcov = vcovHC(lm1, type = "HC3"))

lm1 <- update(lm1, . ~ . - sqrt(ІГР)) # Тепер це модель 3.2.3.
summary(lm1)
coeftest(lm1, vcov = vcovHC(lm1, type = "HC3"))

lm1 <- update(lm1, . ~ . - ЧисНасел)
summary(lm1)
coeftest(lm1, vcov = vcovHC(lm1, type = "HC3"))
# Тут ІндМатСтан не є значущою. Модель не має сенсу.


lm2 <- lm(Ціна ~  ІФС + ІГР + ЧисНасел + РівДолар + ІндМатСтан + Євро_Долар 
          + sqrt(ІФС) + РівДолар:Євро_Долар, data = model1_26_df_inter)
summary(lm2)
coeftest(lm2, vcov = vcovHC(lm2, type = "HC3"))

lm2 <- update(lm2, . ~ . - ІГР, data = model1_26_df_inter) # тепер це попередня модель.
summary(lm2)
coeftest(lm2, vcov = vcovHC(lm2, type = "HC3"))


lm3 <- lm(Ціна ~ ІФС + РівДолар + ІндМатСтан + Євро_Долар + sqrt(ІФС) + РівДолар:Євро_Долар,
          data = model1_26_df_inter)
summary(lm3)
coeftest(lm3, vcov = vcovHC(lm3, type = "HC3"))

lm3 <- update(lm3, . ~ . - РівДолар:Євро_Долар, data = model1_26_df_inter) # тепер це попередня модель.
summary(lm3)
coeftest(lm3, vcov = vcovHC(lm3, type = "HC3"))

# ВИСНОВОК.
# ІндМатСтан - не значуща!