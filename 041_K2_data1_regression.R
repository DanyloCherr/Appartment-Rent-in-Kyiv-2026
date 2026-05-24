# Чого варто очікувати від моделі на основі цін для двокімнатних квартир?
# Кореляція між К1 і К2 = 0.98, тому якщо ціни матимуть однакових розподіл і 
# одну структуру дисперсії, то отримана модель має бути такою ж з точністю
# до оцінених параметрів.

breaks_1k <- seq(min(model_df_75$К1), max(model_df_75$К1), length.out = 16)
breaks_2k <- seq(min(model_df_75$К2), max(model_df_75$К2), length.out = 16)

par(mfrow = c(1, 2))
hist(model_df_75$К1, breaks = breaks_1k, main = "1-кімнатні", col = "steelblue")
hist(model_df_75$К2, breaks = breaks_2k, main = "2-кімнатні", col = "tomato")
par(mfrow = c(1, 1))

ks.test(model_df_75$К1, model_df_75$К2) # p-value = 4.899e-10
# Отже, розподіли цін подібні.
# Форму дисперсії перевіримо згодом: через залишки.


# ----- 75 СПОСТЕРЕЖЕНЬ -----
# ==== 1) МУЛЬТИКОЛІНЕАРНІСТЬ-1. КОРЕЛЯЦІЙНА МАТРИЦЯ ====
model_cor_matrix(model_df_75[, -1], "")

model2_75_df <- upd_model_df(model_df_75, remove = "Ч", room_num = 2)
colnames(model2_75_df)[colnames(model2_75_df) == "К2"] <- "Ціна"
head(model2_75_df, 3)
# 7 предикторів


# ==== 2) ПОВНА МОДЕЛЬ ====
model_full2_75 <- lm(Ціна ~ ., data = model2_75_df)
summary(model_full2_75)



# ==== 3) МУЛЬТИКОЛІНЕАРНІСТЬ-2. VIF, ЧИСЛО ОБУМОВЛЕНОСТІ ====
# ТЕ САМЕ, ЩО І БУЛО! 
model2_75_df_norm <- as.data.frame(lapply(model2_75_df, unit_length_scale))
model_full2_75_norm <- lm(Ціна ~ ., data = model2_75_df_norm)
vif(model_full2_75_norm)

condition_number(model2_75_df_norm, 0)


# ==== 4) АНАЛІЗ ЗАЛИШКІВ І ВИЯВЛЕННЯ ВИКИДІВ ====
residuals_plot(model_full2_75)
# Автокореляція? U-паттерн?

residuals_regressors_plot(model_full2_75, c(2, 4), type = "rstudent")

cooks_dist(model_full2_75)

df_fits(model_full2_75)

df_betas(model_full2_75)

# Спостереження 33, 34, 72 не пройшли жодного з тестів.
# Повернімося до дослідження їхнього впливу в кінці.



# ==== 5) ФУНКЦІОНАЛЬНІ ПЕРЕТВОРЕННЯ ====
# ЛІНІЙНІСТЬ
plot(model_full2_75, which = 1)
# Лінія не зовсім пряма, але пряміша, ніж для К1!

residuals_regressors_plot(model_full2_75, c(2, 4), type = "rstudent")
# Тенденцій не видно.

av_plots(model_full2_75)

crPlots(model_full2_75)

resettest(model_full2_75, power = 2) # p-value = 0.06202


# ГОМОСКЕДАСТИЧНІСТЬ
plot(model_full2_75, which = 3)
# У цьому випадку точки скупчуються трохи сильніше, ніж у попередньому.

residuals_regressors_plot(model_full2_75, c(2, 4), type = "rstudent")
# ІФС - під питанням. ІГР та ЧисНасел псують картину через екстремальні 
# значення.

bptest(model_full2_75) # p-value = 0.03931
# Можливо, гетероскедастичність виправиться після ФП над відгуком.


bc <- boxcox(model_full2_75, lambda = seq(-2, 2, by = 0.1))
lambda_opt <- bc$x[which.max(bc$y)]
cat("Оптимальне λ:", round(lambda_opt, 2), "\n") # -0.63 
# Але оберемо log, оскільки ДІ містить 0 та для К1 був 0.

model_log2_75 <- lm(log(Ціна) ~ ., data = model2_75_df)
summary(model_log2_75)



# ======== НОВА МОДЕЛЬ 1 ========
# ЛІНІЙНІСТЬ
plot(model_log2_75, which = 1) # Те саме?
residuals_plot(model_log2_75) # Трохи рівномірніше.

residuals_regressors_plot(model_log2_75, c(2, 4), type = "rstudent")
# Можливо, трохи краще.

av_plots(model_log2_75)
# Трошечки краще, але основні проблеми з ІФС, ІГР залишаються.

resettest(model_log2_75, power = 2) # p-value = 0.168

crPlots(model_log2_75)


# ГОМОСКЕДАСТИЧНІСТЬ
plot(model_log2_75, which = 3)
bptest(model_log2_75) # p-value = 0.01647


observe_transforms_x(model_log2_75, regressors = names(coef(model_log2_75))[-1])

model_log2_75_Sq <- lm(log(Ціна) ~ . + I(Долар^2), data = model2_75_df) 
# AIC_diff = 3.7182077



# ======== НОВА МОДЕЛЬ 2 ========
summary(model_log2_75_Sq)

# ЛІНІЙНІСТЬ
plot(model_log2_75_Sq, which = 1) # Те саме?
residuals_plot(model_log2_75_Sq) # Трохи рівномірніше?

residuals_regressors_plot(model_log2_75_Sq, c(2, 4), type = "rstudent")
# Здається, Долар і ІЦБ виглядають рівномірніше.

av_plots(model_log2_75_Sq)

resettest(model_log2_75_Sq, power = 2) # p-value = 0.1099

crPlots(model_log2_75_Sq)
# Для деяких змінних виглядає ідеально.


# ГОМОСКЕДАСТИЧНІСТЬ
plot(model_log2_75_Sq, which = 3) # Майже виправили.
bptest(model_log2_75_Sq) # p-value = 0.05032 - приймаємо.


# AIC = -121.1533
# Для моделі з 1-кімнатними квартирами на цьому етапі було AIC = -128.7928.
# Не дивно, що знайдеться таке перетворення, яке суттєво покращить AIC.

observe_transforms_x(model_log2_75_Sq, regressors = names(coef(model_log2_75_Sq))[-1])
model_log2_75_SqSq <- lm(log(Ціна) ~ . + I(Долар^2) + I(Євро^2), data = model2_75_df) 
# AIC_diff = 4.045678



# ======== НОВА МОДЕЛЬ 3 ========
summary(model_log2_75_SqSq)

# ЛІНІЙНІСТЬ
plot(model_log2_75_SqSq, which = 1) # Те саме?
residuals_plot(model_log2_75_SqSq) # Менш однорідно?

residuals_regressors_plot(model_log2_75_SqSq, c(3, 3), type = "rstudent")
# Не значно.

av_plots(model_log2_75_SqSq) # Гірше?

resettest(model_log2_75_SqSq, power = 2) # p-value = 0.214

crPlots(model_log2_75_SqSq)
# Тепер і євро виглядає ідеально. ІЦБ - ще трохи краще.


# ГОМОСКЕДАСТИЧНІСТЬ
plot(model_log2_75_SqSq, which = 3) # Не скажеш, що краще.
bptest(model_log2_75_SqSq) # p-value = 0.1267 - краще.

# Отже, спробуємо поки цю модель, що містить Євро^2, адже ніколи не пізно 
# цю змінну вилучити.

observe_transforms_x(model_log2_75_SqSq, regressors = names(coef(model_log2_75_SqSq))[-1])
model_log2_75_SqSq <- lm(log(Ціна) ~ . + I(Долар^2) + I(Євро^2), data = model2_75_df) 
# AIC_diff = 4.045678e



# ==== 6) НОРМАЛЬНІСТЬ ====
qq_plot(model_log2_75_Sq)
shapiro.test(residuals(model_log2_75_Sq)) # p-value = 0.8839
qq_plot(model_log2_75_SqSq)
shapiro.test(residuals(model_log2_75_SqSq)) # p-value = 0.9475




# ==== 7) АВТОКОРЕЛЯЦІЯ ====
time_series_residuals(model_log2_75_SqSq)
dwtest(model_log2_75_SqSq) # DW = 0.81302, p-value = 2.245e-11
summary(model_log2_75_SqSq)
coeftest(model_log2_75_SqSq, vcov = vcovHAC(model_log2_75_SqSq))


# ==== 8) ВІДБІР ЗМІННИХ ====
e2 <- residuals(model_log2_75_SqSq)
rho2 <- cor(e2[-1], e2[-length(e2)])

all_models <- regsubsets(log(Ціна) ~ . + I(Долар^2) + I(Євро^2), data = model2_75_df, nbest = 3)
best_models <- best_models_summary(all_models, 10)

# 1.4.2. Євро + Долар + ІГР + ІЦБ + ЧисНасел + РівДолар + I(Долар^2) + I(Євро^2)  | Adj R² = 0.8315 (p = 9)
lm1 <- lm(log(Ціна) ~ Євро + Долар + ІГР + ІЦБ + ЧисНасел + РівДолар + I(Долар^2) + I(Євро^2),
          data = model2_75_df)
summary(lm1)
coeftest(lm1, vcov = vcovHAC(lm1))


# 2.1.1. Євро + Долар + ІЦБ + ЧисНасел + РівДолар + I(Долар^2) + I(Євро^2)  | Adj R² = 0.8283 (p = 8)
# Це та сама модель, але без ІГР.
lm2 <- lm(log(Ціна) ~ Євро + Долар + ІЦБ + ЧисНасел + РівДолар + I(Долар^2) + I(Євро^2),
          data = model2_75_df)
summary(lm2)
coeftest(lm2, vcov = vcovHAC(lm2)) # pval(Долар) = 0.0512557 


# -3.10.3. Євро + Долар + ІФС + ІЦБ + ЧисНасел + РівДолар + I(Долар^2) + I(Євро^2)  | Adj R² = 0.8263 (p = 9)
lm3 <- lm(log(Ціна) ~ Євро + Долар + ІФС + ІЦБ + ЧисНасел + РівДолар + I(Долар^2) + I(Євро^2),
          data = model2_75_df)
summary(lm3)
coeftest(lm3, vcov = vcovHAC(lm3)) # pval(ІФС) = 0.4799013 

lm3 <- update(lm3, . ~ . - ІФС, data = model2_75_df)
summary(lm3)
coeftest(lm3, vcov = vcovHAC(lm3)) # pval(Долар) = 0.0512557  
# Тепер це модель 2.1.1.

lm3_gls <- gls(log(Ціна) ~ Євро + Долар + ІФС + ІЦБ + ЧисНасел + РівДолар + I(Долар^2) + I(Євро^2), 
               correlation = corAR1(form = ~1, value = rho2, fixed = TRUE),
               data = model2_75_df)
summary(lm3_gls) # Багато незначущих.


# - 4.7.4. Долар + ІГР + ІЦБ + ЧисНасел + РівДолар + I(Долар^2) + I(Євро^2)  | Adj R² = 0.8208 (p = 8)
lm3 <- lm(log(Ціна) ~ Долар + ІГР + ІЦБ + ЧисНасел + РівДолар + I(Долар^2) + I(Євро^2),
          data = model2_75_df)
summary(lm3) 
coeftest(lm3, vcov = vcovHAC(lm3)) # pval(Долар^2) = 0.1096203

lm3 <- update(lm3, . ~ . - I(Долар^2), data = model2_75_df)
summary(lm3)
coeftest(lm3, vcov = vcovHAC(lm3)) # pval(Долар) = 0.233710    

lm3 <- update(lm3, . ~ . - Долар, data = model2_75_df)
summary(lm3)
coeftest(lm3, vcov = vcovHAC(lm3))

lm3 <- update(lm3, . ~ . - ІГР, data = model2_75_df)
summary(lm3)
coeftest(lm3, vcov = vcovHAC(lm3))

lm3 <- update(lm3, . ~ . - ІЦБ, data = model2_75_df)
summary(lm3)
coeftest(lm3, vcov = vcovHAC(lm3)) 
BIC(lm3) # -97.9662 - непогано, але порівнюючи з іншими моделями, ця є досить слабкою.


lm3_gls <- gls(log(Ціна) ~ Долар + ІГР + ІЦБ + ЧисНасел + РівДолар + I(Долар^2) + I(Євро^2),
               correlation = corAR1(form = ~1, value = rho2, fixed = TRUE),
               data = model2_75_df)
summary(lm3_gls)

lm3_gls <- update(lm3_gls, . ~ . - ІГР)
summary(lm3_gls) # Знову не значущі змінні. Забудемо про цю модель.


# -7.2.7. Долар + ІЦБ + ЧисНасел + РівДолар + I(Долар^2) + I(Євро^2)  | Adj R² = 0.8178 (p = 7)
lm3 <- lm(log(Ціна) ~ Долар + ІЦБ + ЧисНасел + РівДолар + I(Долар^2) + I(Євро^2),
          data = model2_75_df)
summary(lm3)
coeftest(lm3, vcov = vcovHAC(lm3))

lm3 <- update(lm3, . ~ . - I(Долар^2), data = model2_75_df)
summary(lm3)
coeftest(lm3, vcov = vcovHAC(lm3))

lm3 <- update(lm3, . ~ . - Долар, data = model2_75_df)
summary(lm3)
coeftest(lm3, vcov = vcovHAC(lm3))

lm3 <- update(lm3, . ~ . - ІЦБ, data = model2_75_df)
summary(lm3)
coeftest(lm3, vcov = vcovHAC(lm3))
# Це та ж модель з попереднього кроку.

final_models2 <- list("8vars_Rsq1" = lm1, "7vars_Rsq2" = lm2)



# Тепер за повну модель візьмемо таку ж, як і для випадку однокімнатних квартир.
e2 <- residuals(model_log2_75_Sq)
rho2 <- cor(e2[-1], e2[-length(e2)])

all_models <- regsubsets(log(Ціна) ~ . + I(Долар^2), data = model2_75_df, nbest = 3)
best_models <- best_models_summary(all_models, 10)

# -1.4.3. Євро + Долар + ІГР + ІЦБ + ЧисНасел + РівДолар + I(Долар^2)  | Adj R² = 0.8185 (p = 8)
lm1 <- lm(log(Ціна) ~ Євро + Долар + ІГР + ІЦБ + ЧисНасел + РівДолар + I(Долар^2),
          data = model2_75_df)
summary(lm1)
coeftest(lm1, vcov = vcovHAC(lm1))

lm1 <- update(lm1, . ~ . - I(Долар^2))
summary(lm1)
coeftest(lm1, vcov = vcovHAC(lm1)) # :(

lm1_gls <- gls(log(Ціна) ~ Євро + Долар + ІГР + ІЦБ + ЧисНасел + РівДолар + I(Долар^2),
            correlation = corAR1(form = ~1, value = rho2, fixed = TRUE),
            data = model2_75_df)
summary(lm1_gls) # Погано.


# -3.1.2. Євро + Долар + ІЦБ + ЧисНасел + РівДолар + I(Долар^2)  | Adj R² = 0.8158 (p = 7)
lm1 <- lm(log(Ціна) ~ Євро + Долар + ІЦБ + ЧисНасел + РівДолар + I(Долар^2),
          data = model2_75_df)
summary(lm1)
coeftest(lm1, vcov = vcovHAC(lm1))

lm1 <- update(lm1, . ~ . - I(Долар^2))
summary(lm1)
coeftest(lm1, vcov = vcovHAC(lm1)) # :(

lm1_gls <- gls(log(Ціна) ~ Євро + Долар + ІЦБ + ЧисНасел + РівДолар + I(Долар^2),
               correlation = corAR1(form = ~1, value = rho2, fixed = TRUE),
               data = model2_75_df)
summary(lm1_gls) # Погано.


# 4.6.4 Євро + Долар + ІФС + ІЦБ + ЧисНасел + РівДолар + I(Долар^2)  | Adj R² = 0.8151 (p = 8)
lm1 <- lm(log(Ціна) ~ Євро + Долар + ІФС + ІЦБ + ЧисНасел + РівДолар + I(Долар^2),
          data = model2_75_df)
summary(lm1)
coeftest(lm1, vcov = vcovHAC(lm1))

lm1 <- update(lm1, . ~ . - ІФС)
summary(lm1)
coeftest(lm1, vcov = vcovHAC(lm1)) # Знову видаляти долар?.. 

lm1_gls <- gls(log(Ціна) ~ Євро + Долар + ІФС + ІЦБ + ЧисНасел + РівДолар + I(Долар^2),
               correlation = corAR1(form = ~1, value = rho2, fixed = TRUE),
               data = model2_75_df)
summary(lm1_gls) # Погано.
# Досить.

# Спробуємо відразу внести ефект взаємодії, як у моделі для 1-кімнатних.
lm2K_inter <- lm(log(Ціна) ~ Євро + Долар + ІГР + ІЦБ + ЧисНасел + РівДолар + I(Долар^2) + Долар:ІЦБ,
                 data = model2_75_df)
summary(lm2K_inter)
coeftest(lm2K_inter, vcov = vcovHAC(lm2K_inter))

lm2K_inter <- update(lm2K_inter, . ~ . - I(Долар^2))
summary(lm2K_inter)
coeftest(lm2K_inter, vcov = vcovHAC(lm2K_inter))
# Це вже краще, хоча ЧисНасел не є значущою.
# ВИСНОВОК.
# Якщо дуже захочемо мати абсолютно таку ж саму модель, як і для 1-кімнатних квартир
# (для порівняння), то візьмемо відразу фінальну модель, і просто замінимо К1 на К2.
# Але варто зауважити, що потужність такої моделі буде меншою, ніж тієї, що містить 
# доданок Євро^2.



# ======== 9) ЕФЕКТИ ВЗАЄМОДІЇ ========
# Працюємо із двома моделями.
lm1 <- final_models2[[1]]
lm2 <- final_models2[[2]]

#### lm1 ####
summary(lm1)
aic_base <- AIC(lm1)
bic_base <- BIC(lm1)

interactions_result <- observe_best_interactions(lm1, model2_75_df, top_n = 10)

# 1. Євро / Долар | ΔAIC = 20.55 | p = 0.0000 
inter_lm <- update(lm1, . ~ . + I(Євро / Долар))
summary(inter_lm)
coeftest(inter_lm, vcov = vcovHAC(inter_lm)) 

inter_lm <- update(inter_lm, . ~ . - ІГР)
summary(inter_lm)
coeftest(inter_lm, vcov = vcovHAC(inter_lm))

aic_base - AIC(inter_lm) # 21.98788
bic_base - BIC(inter_lm) # 21.98788
inter_lm1 <- inter_lm


# 2. Євро × Долар | ΔAIC = 19.40 | p = 0.0000 
inter_lm <- update(lm1, . ~ . + Євро:Долар)
summary(inter_lm)
coeftest(inter_lm, vcov = vcovHAC(inter_lm)) 

inter_lm <- update(inter_lm, . ~ . - Долар)
summary(inter_lm)
coeftest(inter_lm, vcov = vcovHAC(inter_lm))

inter_lm <- update(inter_lm, . ~ . - ІГР)
summary(inter_lm)
coeftest(inter_lm, vcov = vcovHAC(inter_lm))

aic_base - AIC(inter_lm) # 19.8573
bic_base - BIC(inter_lm) # 22.13396
inter_lm2 <- inter_lm


# 3. Долар / Євро | ΔAIC = 12.79 | p = 0.0004 
inter_lm <- update(lm1, . ~ . + I(Долар / Євро))
summary(inter_lm)
coeftest(inter_lm, vcov = vcovHAC(inter_lm)) 

inter_lm <- update(inter_lm, . ~ . - I(Долар^2))
summary(inter_lm)
coeftest(inter_lm, vcov = vcovHAC(inter_lm))

aic_base - AIC(inter_lm) # 10.56671
bic_base - BIC(inter_lm) # 10.56671
inter_lm3 <- inter_lm


# 4. Долар × ІЦБ | ΔAIC = 12.31 | p = 0.0005 
inter_lm <- update(lm1, . ~ . + Долар:ІЦБ)
summary(inter_lm)
coeftest(inter_lm, vcov = vcovHAC(inter_lm)) 

inter_lm <- update(inter_lm, . ~ . - ЧисНасел)
summary(inter_lm)
coeftest(inter_lm, vcov = vcovHAC(inter_lm))

aic_base - AIC(inter_lm) # 12.28047
bic_base - BIC(inter_lm) # 12.28047
inter_lm4 <- inter_lm


# 5. ІЦБ / Долар | ΔAIC = 11.93 | p = 0.0006 
inter_lm <- update(lm1, . ~ . + I(ІЦБ / Долар))
summary(inter_lm)
coeftest(inter_lm, vcov = vcovHAC(inter_lm)) 

inter_lm <- update(inter_lm, . ~ . - ЧисНасел)
summary(inter_lm)
coeftest(inter_lm, vcov = vcovHAC(inter_lm))

aic_base - AIC(inter_lm) # 12.11424
bic_base - BIC(inter_lm) # 12.11424
inter_lm5 <- inter_lm


# ВИСНОВОК.
# НАЙКРАЩІ МОДЕЛІ - 1 ТА 2.
summary(inter_lm1)
summary(inter_lm2)
coeftest(inter_lm1, vcov = vcovHAC(inter_lm1)) 
coeftest(inter_lm2, vcov = vcovHAC(inter_lm2)) 
# Поки що залишимо обидві, але основну увагу приділимо другій, оскільки вона
# містить на одну змінну менше.

assumptions_check(lm1) # DW = 0.81964
assumptions_check(inter_lm1) # DW = 0.92085
assumptions_check(inter_lm2) # DW = 0.91798
# Виконуються всі припущення, крім некорельованості.

final_models2[["Євро / Долар"]] <- inter_lm1
final_models2[["Євро × Долар"]] <- inter_lm2



#### lm2 ####
summary(lm2)
aic_base <- AIC(lm2)
bic_base <- BIC(lm2)

interactions_result <- observe_best_interactions(lm2, model2_75_df, top_n = 10)
# 1. Євро / Долар | ΔAIC = 22.49 | p = 0.0000 
# 2. Євро × Долар | ΔAIC = 21.22 | p = 0.0000 
# Перші два перетворення не мають сенсу, оскільки отримаємо ті ж моделі, що і при lm1.

# 3. Долар / Євро | ΔAIC = 13.97 | p = 0.0002 
inter_lm <- update(lm2, . ~ . + I(Долар / Євро))
summary(inter_lm)
coeftest(inter_lm, vcov = vcovHAC(inter_lm)) 

inter_lm <- update(inter_lm, . ~ . - I(Долар^2))
summary(inter_lm)
coeftest(inter_lm, vcov = vcovHAC(inter_lm)) 

aic_base - AIC(inter_lm) # 10.99282
bic_base - BIC(inter_lm) # 10.99282
inter_lm6 <- inter_lm
# Ця модель слабша за попередні: кількість параметрів та ж, але внесок в AIC/BIC
# удвічі менший.

names(final_models2)
