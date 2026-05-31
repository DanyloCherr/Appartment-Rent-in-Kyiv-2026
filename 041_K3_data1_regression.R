library(nortest)

# ----- 75 СПОСТЕРЕЖЕНЬ -----
# ==== 1) МУЛЬТИКОЛІНЕАРНІСТЬ-1. КОРЕЛЯЦІЙНА МАТРИЦЯ ====
model_cor_matrix(model_df_75[, -1], "")

model3_75_df <- upd_model_df(model_df_75, remove = "Ч", room_num = 3)
colnames(model3_75_df)[colnames(model3_75_df) == "К3"] <- "Ціна"
head(model3_75_df, 3)
# 7 предикторів


# ==== 2) ПОВНА МОДЕЛЬ ====
model_full3_75 <- lm(Ціна ~ ., data = model3_75_df)
summary(model_full3_75)


# ==== 3) АНАЛІЗ ЗАЛИШКІВ І ВИЯВЛЕННЯ ВИКИДІВ ====
residuals_plot(model_full3_75)
# Спостереження з великим значенням ціни відокремлені від основної хмари даних.

residuals_regressors_plot(model_full3_75, c(2, 4), type = "rstudent")

cooks_dist(model_full3_75)

df_fits(model_full3_75)

df_betas(model_full3_75)

# Спостереження 1 29 64 69 71 72 не пройшли жодного з тестів.
# Повернімося до дослідження їхнього впливу в кінці.



# ==== 4) ФУНКЦІОНАЛЬНІ ПЕРЕТВОРЕННЯ ====
# ЛІНІЙНІСТЬ
plot(model_full3_75, which = 1)
# U-патерн?

residuals_regressors_plot(model_full3_75, c(2, 4), type = "rstudent")
# Тенденцій не видно.

av_plots(model_full3_75) # ІФС, ІГР - під питанням.

crPlots(model_full3_75) # ІГР - ?

resettest(model_full3_75, power = 2) # p-value = 1.253e-07


# ГОМОСКЕДАСТИЧНІСТЬ
plot(model_full3_75, which = 3)
# Проблема з екстремальними спостереженнями.

residuals_regressors_plot(model_full3_75, c(2, 4), type = "rstudent")
# ІФС - під питанням. ІГР та ЧисНасел псують картину через екстремальні 
# значення.

bptest(model_full3_75) # p-value = 0.003783
# Можливо, гетероскедастичність виправиться після ФП над відгуком.


bc <- boxcox(model_full3_75, lambda = seq(-2, 2, by = 0.1))
lambda_opt <- bc$x[which.max(bc$y)]
cat("Оптимальне λ:", round(lambda_opt, 2), "\n") # -1.84
# ДІ містить -1, тому візьмемо її. 

model_log3_75 <- lm(I(1/Ціна) ~ ., data = model3_75_df)
summary(model_log3_75)
# Чому модель називається "...log..."? :D



# ======== НОВА МОДЕЛЬ 1 ========
# ЛІНІЙНІСТЬ
plot(model_log3_75, which = 1) # Трохи краще?
residuals_plot(model_log3_75) # Трохи рівномірніше.

residuals_regressors_plot(model_log3_75, c(2, 4), type = "rstudent")
# Можливо, трохи краще.

av_plots(model_log3_75)
# ІГР, ІЦБ, ЧисНасел - трохи краще.

resettest(model_log3_75, power = 2) # p-value = 0.000426

crPlots(model_log3_75) # ІФС - майже ідеально.


# ГОМОСКЕДАСТИЧНІСТЬ
plot(model_log3_75, which = 3)
bptest(model_log3_75) # p-value = 0.03911


observe_transforms_x(model_log3_75, regressors = names(coef(model_log3_75))[-1])

model_log3_75_Lg <- lm(I(1/Ціна) ~ . + log(РівДолар), data = model3_75_df) 
# AIC_diff = 7.2918002



# ======== НОВА МОДЕЛЬ 2 ========
summary(model_log3_75_Lg)

# ЛІНІЙНІСТЬ
plot(model_log3_75_Lg, which = 1) # Дуга випрямляється.
residuals_plot(model_log3_75_Lg) # Трохи рівномірніше?

residuals_regressors_plot(model_log3_75_Lg, c(2, 4), type = "rstudent")
# РівДолар, ІФС - краще. Долар - трохи менш однорідно?

av_plots(model_log3_75_Lg)
# ІФС, ІЦБ - краще; ЧисНасел, Рівдолар - гірше?

resettest(model_log3_75_Lg, power = 2) # p-value = 0.02012

crPlots(model_log3_75_Lg)
# Для багатьох - покращення. 


# ГОМОСКЕДАСТИЧНІСТЬ
plot(model_log3_75_Lg, which = 3) # Майже виправили.
bptest(model_log3_75_Lg) # p-value = 0.0348


observe_transforms_x(model_log3_75_Lg, regressors = names(coef(model_log3_75_Lg))[-1])
model_log3_75_LgSq <- update(model_log3_75_Lg, . ~ . + I(Долар^2)) 
# AIC_diff = 6.6335676 



# ======== НОВА МОДЕЛЬ 3 ========
summary(model_log3_75_LgSq)

# ЛІНІЙНІСТЬ
plot(model_log3_75_LgSq, which = 1) # Те саме?
residuals_plot(model_log3_75_LgSq) # Однорідніше.

residuals_regressors_plot(model_log3_75_LgSq, c(3, 3), type = "rstudent")
# Трохи однорідніше.

av_plots(model_log3_75_LgSq) # Гірше?

resettest(model_log3_75_LgSq, power = 2) # p-value = 0.02044

crPlots(model_log3_75_LgSq) # Краще.


# ГОМОСКЕДАСТИЧНІСТЬ
plot(model_log3_75_LgSq, which = 3) # Трохи краще.
bptest(model_log3_75_LgSq) # p-value = 0.3011 - виправили непостійність!


observe_transforms_x(model_log3_75_LgSq, regressors = names(coef(model_log3_75_LgSq))[-1])
model_log3_75_LgSq2 <- update(model_log3_75_LgSq, . ~ . + I(Євро^2)) 
# AIC_diff = 6.89778430



# ======== НОВА МОДЕЛЬ 4 ========
summary(model_log3_75_LgSq2)

# ЛІНІЙНІСТЬ
plot(model_log3_75_LgSq2, which = 1) # Пряміше.
residuals_plot(model_log3_75_LgSq2) # Однорідніше.

residuals_regressors_plot(model_log3_75_LgSq2, c(2, 5), type = "rstudent")
# Добре. Проблеми залишаються із ІФС та ІГР.

av_plots(model_log3_75_LgSq2)

resettest(model_log3_75_LgSq2, power = 2) # p-value = 0.09297

crPlots(model_log3_75_LgSq2) # Краще.

# ГОМОСКЕДАСТИЧНІСТЬ
plot(model_log3_75_LgSq2, which = 3) # Трохи краще.
bptest(model_log3_75_LgSq2) # p-value = 0.508


observe_transforms_x(model_log3_75_LgSq2, regressors = names(coef(model_log3_75_LgSq2))[-1])
# Значних покращень немає.


# ==== 5) НОРМАЛЬНІСТЬ ====
qq_plot(model_log3_75_LgSq2)
shapiro.test(residuals(model_log3_75_LgSq2)) # p-value = 0.04189
# qq plot показує, що розподіл залишків вцілому є нормальним, проте на 
# правому кінці наявні два екстремальні спостереження, які не належать ДІ.
# Скоріш за все через ці кілька екстремальних значень тест Шапіро-Вілка
# видає мале p-value. Використаймо інший тест.

ad.test(residuals(model_log3_75_LgSq2)) # p-value = 0.259



# ==== 6) АВТОКОРЕЛЯЦІЯ ====
time_series_residuals(model_log3_75_LgSq2)
dwtest(model_log3_75_LgSq2) # DW = 0.94261, p-value = 1.473e-09
summary(model_log3_75_LgSq2)
coeftest(model_log3_75_LgSq2, vcov = vcovHAC(model_log3_75_LgSq2))


# ==== 7) ВІДБІР ЗМІННИХ ====
e3 <- residuals(model_log3_75_LgSq2)
rho3 <- cor(e3[-1], e3[-length(e3)])

all_models <- regsubsets(I(1/Ціна) ~ . + log(РівДолар) + I(Долар^2) + I(Євро^2), 
                         data = model3_75_df, nbest = 3, nvmax = 11)
best_models <- best_models_summary(all_models, 12)

# -1.2.4. Повна модель без ІФС | Adj R² = 0.7673 (p = 10)
lm1 <- update(model_log3_75_LgSq2, . ~ . - ІФС)
summary(lm1)
coeftest(lm1, vcov = vcovHAC(lm1))


# 3.1.2. Євро + Долар + ІЦБ + ЧисНасел + РівДолар + log(РівДолар) + I(Долар^2) + I(Євро^2)  | Adj R² = 0.7630 (p = 9)
# Це та сама модель, але без ІГР.
lm2 <- update(model_log3_75_LgSq2, . ~ . - ІФС - ІГР)
summary(lm2)
coeftest(lm2, vcov = vcovHAC(lm2))
# Ця модель краща за попередню.


# 5. Євро + Долар + ІГР + ІЦБ + РівДолар + log(РівДолар) + I(Долар^2) + I(Євро^2)  | Adj R² = 0.7441 (p = 9)
# Попередня модель, але із ІГР замість ЧИсНасел.
lm3 <- update(model_log3_75_LgSq2, . ~ . - ІФС - ЧисНасел)
summary(lm3)
coeftest(lm3, vcov = vcovHAC(lm3))

lm3 <- update(lm3, . ~ . - ІЦБ)
summary(lm3)
coeftest(lm3, vcov = vcovHAC(lm3)) 

lm3 <- update(lm3, . ~ . - ІГР)
summary(lm3)
coeftest(lm3, vcov = vcovHAC(lm3)) 
# Це модель 11.3.11. Можна залишити.


# -6. Долар + ІГР + ІЦБ + ЧисНасел + РівДолар + log(РівДолар) + I(Долар^2) + I(Євро^2)  | Adj R² = 0.7426 (p = 9)
lm4 <- update(model_log3_75_LgSq2, . ~ . - ІФС - Євро)
summary(lm4)
coeftest(lm4, vcov = vcovHAC(lm4))
# Ця модель є менш практичною за 3.1.2., оскільки хоч вони обидві містять однакову кількість
# параметрів, ця модель замість Євро містить іншу змінну (хоча все ще містить 
# Євро^2), що ускладнює процедуру збору даних для моделі.

# -7. Євро + Долар + ІЦБ + РівДолар + log(РівДолар) + I(Долар^2) + I(Євро^2)  | Adj R² = 0.7421 (p = 8)
lm4 <- lm(I(1/Ціна) ~ Євро + Долар + ІЦБ + РівДолар + log(РівДолар) + I(Долар^2) + I(Євро^2), 
          data = model3_75_df)
summary(lm4)
coeftest(lm4, vcov = vcovHAC(lm4))

lm4 <- update(lm4, . ~ . - ІЦБ)
summary(lm4)
coeftest(lm4, vcov = vcovHAC(lm4))
# Це модель 11.3.11.

final_models3 = list("8_varRsq3" = lm2, "6_varRsq11" = lm3)


# ======== 8) ЕФЕКТИ ВЗАЄМОДІЇ ========
# Працюємо із двома моделями.
lm1 <- final_models3[[1]]
lm2 <- final_models3[[2]]

#### lm1 ####
summary(lm1)
aic_base <- AIC(lm1)
bic_base <- BIC(lm1)

interactions_result <- observe_best_interactions(lm1, model3_75_df, top_n = 10)


# 1. Євро / Долар | ΔAIC = 18.35 | p = 0.0000 
inter_lm <- update(lm1, . ~ . + I(Євро / Долар))
summary(inter_lm)
coeftest(inter_lm, vcov = vcovHAC(inter_lm)) 

aic_base - AIC(inter_lm) # 18.34887
bic_base - BIC(inter_lm) # 16.0722
inter_lm1 <- inter_lm


# 2. Євро × Долар | ΔAIC = 17.50  | p = 0.0000 
inter_lm <- update(lm1, . ~ . + Євро:Долар)
summary(inter_lm)
coeftest(inter_lm, vcov = vcovHAC(inter_lm)) 

inter_lm <- update(inter_lm, . ~ . - Долар)
summary(inter_lm)
coeftest(inter_lm, vcov = vcovHAC(inter_lm))

aic_base - AIC(inter_lm) # 18.93835
bic_base - BIC(inter_lm) # 18.93835
inter_lm2 <- inter_lm


# 3. Долар / Євро | ΔAIC = 12.04  | p = 0.0004 
inter_lm <- update(lm1, . ~ . + I(Долар / Євро))
summary(inter_lm)
coeftest(inter_lm, vcov = vcovHAC(inter_lm)) 

inter_lm <- update(inter_lm, . ~ . - I(Долар^2))
summary(inter_lm)
coeftest(inter_lm, vcov = vcovHAC(inter_lm))

aic_base - AIC(inter_lm) # 11.45379
bic_base - BIC(inter_lm) # 11.45379
inter_lm3 <- inter_lm
# Ця модель мало чим відрізняється від іншої, де ефект взаємодії є оберненим.
# Проте залишимо її, і, можливо, потім оберемо на основі схожості з іншою
# моделлю (для одно- чи двокімнатних квартир).

summary(inter_lm1)
summary(inter_lm2)
summary(inter_lm3)
coeftest(inter_lm1, vcov = vcovHAC(inter_lm1)) 
coeftest(inter_lm2, vcov = vcovHAC(inter_lm2)) 
coeftest(inter_lm3, vcov = vcovHAC(inter_lm3)) 

assumptions_check(lm1) # DW = 0.88554
assumptions_check(inter_lm1) # DW = 1.0876
assumptions_check(inter_lm2) # DW = 1.0742
assumptions_check(inter_lm3) # DW = 1.0305
# Виконуються всі припущення, крім некорельованості.

final_models3[["8Євро / Долар"]] <- inter_lm1
final_models3[["8Євро × Долар"]] <- inter_lm2
final_models3[["8Долар / Євро"]] <- inter_lm3



#### lm2 ####
summary(lm2)
aic_base <- AIC(lm2)
bic_base <- BIC(lm2)

interactions_result <- observe_best_interactions(lm2, model3_75_df, top_n = 10)


# -1. Євро × РівДолар | ΔAIC = 20.48 | p = 0.0000 
inter_lm <- update(lm2, . ~ . + Євро:Долар)
summary(inter_lm)
coeftest(inter_lm, vcov = vcovHAC(inter_lm)) 

inter_lm <- update(inter_lm, . ~ . - I(Долар^2))
summary(inter_lm)
coeftest(inter_lm, vcov = vcovHAC(inter_lm))

inter_lm <- update(inter_lm, . ~ . - Євро)
summary(inter_lm)
coeftest(inter_lm, vcov = vcovHAC(inter_lm))

aic_base - AIC(inter_lm) # 0.323478
bic_base - BIC(inter_lm) # 2.600144
# Adjusted R-squared:  0.7324.
# Модель значно слабша за попередні.


# 2. Євро / РівДолар | ΔAIC = 19.16 | p = 0.0000 
inter_lm <- update(lm2, . ~ . + I(Євро/РівДолар))
summary(inter_lm)
coeftest(inter_lm, vcov = vcovHAC(inter_lm)) 

aic_base - AIC(inter_lm) # 19.1647
bic_base - BIC(inter_lm) # 16.88803
inter_lm1 <- inter_lm


# 3. РівДолар / Євро | ΔAIC = 18.10 | p = 0.0000
inter_lm <- update(lm2, . ~ . + I(РівДолар/Євро))
summary(inter_lm)
coeftest(inter_lm, vcov = vcovHAC(inter_lm)) 

inter_lm <- update(inter_lm, . ~ . - I(Євро^2))
summary(inter_lm)
coeftest(inter_lm, vcov = vcovHAC(inter_lm))

aic_base - AIC(inter_lm) # 20.09904
bic_base - BIC(inter_lm) # 20.09904
inter_lm2 <- inter_lm
# Друга модель є простішою, а КД має такий же.


summary(inter_lm1)
summary(inter_lm2)
coeftest(inter_lm1, vcov = vcovHAC(inter_lm1)) 
coeftest(inter_lm2, vcov = vcovHAC(inter_lm2)) 

assumptions_check(lm2) # DW = 0.90442
assumptions_check(inter_lm1) # DW = 1.1132
assumptions_check(inter_lm2) # DW = 1.1042
# Виконуються всі припущення, крім некорельованості.

final_models3[["6Євро / РівДолар"]] <- inter_lm1
final_models3[["6РівДолар / Євро"]] <- inter_lm2


suppressWarnings(
  models_validation(final_models3, data = model3_75_df, kfold_number = 5,
                    horizon = 1, init_window = 48)
)


# ======== 11) ВИБІР НАЙКРАЩОЇ МОДЕЛІ ========
length(final_models3)

coeftest(final_models3[[1]], vcov = vcovHAC(final_models3[[1]])) # - Сповільнення Долара.
coeftest(final_models3[[2]], vcov = vcovHAC(final_models3[[2]])) # - Сповільнення Долара.
coeftest(final_models3[[3]], vcov = vcovHAC(final_models3[[3]])) # - Сповільнення Долара.
coeftest(final_models3[[4]], vcov = vcovHAC(final_models3[[4]])) # +
coeftest(final_models3[[5]], vcov = vcovHAC(final_models3[[5]])) # - Сповільнення Євро.
coeftest(final_models3[[6]], vcov = vcovHAC(final_models3[[6]])) # - Сповільнення Долара.
coeftest(final_models3[[7]], vcov = vcovHAC(final_models3[[7]])) # - Сповільнення Долара.

residuals_plot(final_models3[[4]]) 
residuals_regressors_plot(final_models3[[4]], c(3, 3)) 

best_models3 <- final_models3[4]

models_validation(best_models3, model3_75_df, horizon = 1, kfold_number = 5,
                  init_window = 48)
