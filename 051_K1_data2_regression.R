# ----- 46 СПОСТЕРЕЖЕНЬ -----
# ==== 1) МУЛЬТИКОЛІНЕАРНІСТЬ-1. КОРЕЛЯЦІЙНА МАТРИЦЯ ====
model_cor_matrix(model_df_2[, -1], "")

model1_df <- upd_model_df(model_df_2, remove = "Ч", room_num = 1)
colnames(model1_df)[colnames(model1_df) == "К1"] <- "Ціна"
model_cor_matrix(model1_df, "")

model1_df$`Євро_Долар` <- model1_df$Євро / model1_df$Долар
model1_df <- model1_df[!names(model1_df) %in% c("ІЦЖВ")]
model1_df <- model1_df[!names(model1_df) %in% c("Євро", "Долар")]
model_cor_matrix(model1_df, "")

head(model1_df, 3)



# ==== 2) ПОВНА МОДЕЛЬ ====
model_full1_2 <- lm(Ціна ~ ., data = model1_df)
summary(model_full1_2)


# ==== 3) МУЛЬТИКОЛІНЕАРНІСТЬ-2. VIF, ЧИСЛО ОБУМОВЛЕНОСТІ ====
model1_df_norm <- as.data.frame(lapply(model1_df, unit_length_scale))

model_full1_2_norm <- lm(Ціна ~ ., data = model1_df_norm)
vif(model_full1_2_norm)
# ІФС         ІГР         ІЦБ        ІЦЖП  ІндМатСтан    РівДолар ЧистГрнКред  Євро_Долар 
# 4.372233    8.758243    9.578793    5.982114    4.752739    3.015318    3.668954    3.188518 

condition_number(model1_df_norm, 0) # 48.08488



# ==== 3) АНАЛІЗ ЗАЛИШКІВ І ВИЯВЛЕННЯ ВИКИДІВ ====
residuals_plot(model_full1_2)

residuals_regressors_plot(model_full1_2, c(2, 4), type = "rstudent")

cooks_dist(model_full1_2)

df_fits(model_full1_2)

df_betas(model_full1_2)

# Повернімося до дослідження їхнього впливу в кінці.



# ==== 4) ФУНКЦІОНАЛЬНІ ПЕРЕТВОРЕННЯ ====
# ЛІНІЙНІСТЬ І ГОМОСКЕДАСТИЧНІСТЬ
plot(model_full1_2, which = 1) # Хвиля.

residuals_regressors_plot(model_full1_2, c(2, 4), type = "rstudent")
# Тенденцій не видно.
# ІФС, ІГР, ІЦЖП - воронки.

av_plots(model_full1_2) # ІЦЖП - під питанням.

crPlots(model_full1_2)

resettest(model_full1_2, power = 2) # p-value = 0.04072

plot(model_full1_2, which = 3)

bptest(model_full1_2) # p-value = 0.4956

bc <- boxcox(model_full1_2, lambda = seq(-2, 2, by = 0.1))
lambda_opt <- bc$x[which.max(bc$y)]
cat("Оптимальне λ:", round(lambda_opt, 2), "\n") # 0.4956
# ДІ містить 1.

observe_transforms_y(model_full1_2)

observe_transforms_x(model_full1_2, regressors = names(coef(model_full1_2))[-1])

model_log1_2 <- lm(log(Ціна) ~ ., data = model1_df)
summary(model_log1_2)



# ======== НОВА МОДЕЛЬ 1 ========
# ЛІНІЙНІСТЬ І ГОМОСКЕДАСТИЧНІСТЬ
plot(model_log1_2, which = 1)
residuals_plot(model_log1_2) # Не краще?

residuals_regressors_plot(model_log1_2, c(2, 4), type = "rstudent")

av_plots(model_log1_2)

resettest(model_log1_2, power = 2) # p-value = 0.02278

crPlots(model_log1_2)

plot(model_log1_2, which = 3)
bptest(model_log1_2) # p-value = 0.1011

observe_transforms_x(model_log1_2, regressors = names(coef(model_log1_2))[-1])

# Проблеми з нелінійсністю: ІЦЖП має U-патерн.
model_log1_2_Sq <- lm(log(Ціна) ~ . + I(ІЦЖП^2), data = model1_df)
# AIC_diff = 13.9753350 



# ======== НОВА МОДЕЛЬ 2 ========
# ЛІНІЙНІСТЬ І ГОМОСКЕДАСТИЧНІСТЬ
plot(model_log1_2_Sq, which = 1) # Менша дуга, але тепер їх дві.
residuals_plot(model_log1_2_Sq) # Рівномірніше.
plot(model_log1_2_Sq, which = 3) # Не дуже.

residuals_regressors_plot(model_log1_2_Sq, c(2, 4), type = "rstudent")
# Все стало трохи краще.

av_plots(model_log1_2_Sq)
# Краще: ІЦБ, ІЦЖП (тепер лінійна!), Євро_Долар.
# Слабко клеяться до прямої: ІФС, ІГР, РівДолар(?)

crPlots(model_log1_2_Sq) # Висновки як і для avPlots.
# РівДолар - найбільш проблемна.

resettest(model_log1_2_Sq, power = 2) # p-value = 0.8665
# Виправили нелінійність!

bptest(model_log1_2_Sq) # p-value = 0.5235

observe_transforms_x(model_log1_2_Sq, regressors = names(coef(model_log1_2_Sq))[-1])

# Спробуймо виправити вигин для РівДолар.
# UPD: майже нічого не змінилося. Не додаємо.
model_log1_2_SqLg<- update(model_log1_2_Sq, . ~ . + log(ІЦБ))
# AIC diff = 13.3207279


# ======== НОВА МОДЕЛЬ 3 ========
# ЛІНІЙНІСТЬ І ГОМОСКЕДАСТИЧНІСТЬ
summary(model_log1_2_SqLg)
# Тепер ІЦЖП та I(ІЦЖП^2) мають величезні p-value (і на графіках виглядають гірше)
# Це могло статися через те, що + log(ІЦБ) просто перетягнув на себе значущість інших
# предикторів.

plot(model_log1_2_SqLg, which = 1) # Одна маленька дуга.
residuals_plot(model_log1_2_SqLg)
plot(model_log1_2_SqLg, which = 3) # Дисперсія не стала за рахунок окремої
# хмари даних.

residuals_regressors_plot(model_log1_2_SqLg, c(2, 4), type = "rstudent")

av_plots(model_log1_2_SqLg)

crPlots(model_log1_2_SqLg) # Висновки як і для avPlots.
# Загалом, краще.

resettest(model_log1_2_SqLg, power = 2) # p-value = 0.006915
# Тест тепер провалюється!

bptest(model_log1_2_SqLg) # p-value = 0.1939

observe_transforms_x(model_log1_2_SqLg, regressors = names(coef(model_log1_2_SqLg))[-1])
# log(ІЦБ) вносить багато проблем. Вилучимо його.
# Працюємо з моделлю model_log1_2_Sq.
# Не будемо гнатися за R2 та AIC. Можливо, вони ще покращаться при додаванні 
# ефектів взаємодії.
par(mfrow = c(1, 1))



# ==== 5) НОРМАЛЬНІСТЬ ====
qq_plot(model_log1_2_Sq)
shapiro.test(residuals(model_log1_2_Sq)) # p-value = 0.1782
ad.test(residuals(model_log1_2_Sq)) # p-value = 0.3099



# ==== 6) АВТОКОРЕЛЯЦІЯ ====
time_series_residuals(model_log1_2_Sq)
dwtest(model_log1_2_Sq) # DW = 1.0135, p-value = 4.2e-06
summary(model_log1_2_Sq)
coeftest(model_log1_2_Sq, vcov = vcovHAC(model_log1_2_Sq))



# ==== 7) ВІДБІР ЗМІННИХ ====
all_models <- regsubsets(log(Ціна) ~ . + I(ІЦЖП^2), 
                         data = model1_df, nbest = 3, nvmax = 11)
best_models <- best_models_summary(all_models, 12)


# 1.1.6. ІЦБ + ІЦЖП + ІндМатСтан + РівДолар + Євро_Долар + I(ІЦЖП^2)  | Adj R² = 0.8622 (p = 7)
lm1 <- update(model_log1_2_Sq, . ~ . - ІФС - ІГР)
summary(lm1)
coeftest(lm1, vcov = vcovHAC(lm1)) # pval(РівДолар) = 0.1114745

lm1 <- update(lm1, . ~ . - РівДолар)
summary(lm1)
coeftest(lm1, vcov = vcovHAC(lm1)) # pval(Євро_Долар) = 0.0667301 
# Це модель 5.2.4.
# Можливо, виправиться при додавання взаємодій.


# 9. ІЦБ + ІЦЖП + ІндМатСтан + РівДолар + I(ІЦЖП^2)  | Adj R² = 0.8408 (p = 6)
lm2 <- lm(log(Ціна) ~ ІЦБ + ІЦЖП + ІндМатСтан + РівДолар + I(ІЦЖП^2), data = model1_df)
summary(lm2)
coeftest(lm2, vcov = vcovHAC(lm2)) # pval(РівДолар) = 0.2643936

lm2 <- update(lm2, . ~ . - РівДолар)
summary(lm2)
coeftest(lm2, vcov = vcovHAC(lm2))
# Це модель 10.3.10.


# -11. ІФС + ІЦБ + ІЦЖП + ІндМатСтан + I(ІЦЖП^2)  | Adj R² = 0.8316 (p = 6)
lm3 <- lm(log(Ціна) ~ ІФС + ІЦБ + ІЦЖП + ІндМатСтан + I(ІЦЖП^2), data = model1_df)
summary(lm3)
coeftest(lm3, vcov = vcovHAC(lm3))


# -12. ІЦБ + ІЦЖП + РівДолар + I(ІЦЖП^2)  | Adj R² = 0.8129 (p = 5)
lm3 <- lm(log(Ціна) ~ ІЦБ + ІЦЖП + РівДолар + I(ІЦЖП^2), data = model1_df)
summary(lm3)
coeftest(lm3, vcov = vcovHAC(lm3))


final_models1_2 <- list(lm1, lm2)


# ======== 8) ЕФЕКТИ ВЗАЄМОДІЇ ========
# Працюємо із двома моделями.
lm1 <- final_models1_2[[1]]
lm2 <- final_models1_2[[2]]
models_summary(list(lm1, lm2), model_full = model_log1_2_Sq)


#### lm1 ####
summary(lm1)
aic_base <- AIC(lm1)
bic_base <- BIC(lm1)

interactions_result <- observe_best_interactions(lm1, model1_df, top_n = 10)

# 1. ІЦБ / ІЦЖП | ΔAIC = 11.89 | p = 0.0007 
inter_lm <- update(lm1, . ~ . + I(ІЦБ / ІЦЖП))
summary(inter_lm)
coeftest(inter_lm, vcov = vcovHAC(inter_lm)) 

aic_base - AIC(inter_lm) # 11.88557
bic_base - BIC(inter_lm) # 10.05693
inter_lm1 <- inter_lm

assumptions_check(inter_lm1) # Погано((


plot(inter_lm1, which = 1) # Одна маленька дуга.
residuals_plot(inter_lm1) # Добре?
plot(inter_lm1, which = 3)

residuals_regressors_plot(inter_lm1, c(2, 4), type = "rstudent")
# Гетероскедастичність під питанням.

av_plots(inter_lm1)

crPlots(inter_lm1) 

resettest(inter_lm1, power = 2) # p-value = 0.007903
# Тест тепер провалюється! Проте графіки показують, інше.

bptest(inter_lm1) # p-value = 0.03767



#### lm2 ####
summary(lm2)
aic_base <- AIC(lm2)
bic_base <- BIC(lm2)

interactions_result <- observe_best_interactions(lm2, model1_df, top_n = 10)

# 1. ІЦБ / ІЦЖП | ΔAIC = 9.14 | p = 0.0020 
inter_lm <- update(lm2, . ~ . + I(ІЦБ / ІЦЖП))
summary(inter_lm)
coeftest(inter_lm, vcov = vcovHAC(inter_lm)) 

aic_base - AIC(inter_lm) # 9.135509
bic_base - BIC(inter_lm) # 7.306868
inter_lm2 <- inter_lm

assumptions_check(inter_lm2) # Погано((


plot(inter_lm2, which = 1) # Одна маленька дуга.
residuals_plot(inter_lm2) # Добре?
plot(inter_lm2, which = 3)

residuals_regressors_plot(inter_lm2, c(2, 4), type = "rstudent")
# Гетероскедастичність під питанням.

av_plots(inter_lm2)

crPlots(inter_lm2) 

resettest(inter_lm2, power = 2) # p-value = 0.03671
# Тест тепер провалюється! Проте графіки показують інше.

bptest(inter_lm2) # p-value = 0.02627

anova(inter_lm1, inter_lm2) # p-value = 0.005711
models_summary(list(inter_lm1, inter_lm2))
# Отже, кращою є модель із Євро_Долар.

final_models1_2 <- append(final_models1_2, list(inter_lm1, inter_lm2))



# ======== 9) ВАЛІДАЦІЯ МОДЕЛІ ========
suppressWarnings(
  models_validation(final_models1_2, data = model1_df, kfold_number = 5,
                    horizon = 1, init_window = 30)
)
# Показники валідації є прийнятними для всіх моделей, тому за найкращу візьмемо
# модель із Євро_Долар.


# ======== 10) ВИБІР НАЙКРАЩОЇ МОДЕЛІ ========
best_models1_2 <- list(inter_lm1)



# ----- МОДЕЛЬ ІЗ ЧИСТГРНКРЕД ----
# ==== 1) МУЛЬТИКОЛІНЕАРНІСТЬ-1. КОРЕЛЯЦІЙНА МАТРИЦЯ ====
model1_df_temp <- upd_model_df(model_df_2, remove = "Ч", room_num = 1)
colnames(model1_df_temp)[colnames(model1_df_temp) == "К1"] <- "Ціна"
model_cor_matrix(model1_df_temp, "")

# Перевіримо значущість ЧистГрнКред і євро.
model_LOANS <- lm(Ціна ~ ЧистГрнКред, data = model1_df_temp)
model_EUR <- lm(Ціна ~ Євро, data = model1_df_temp)
models_summary(list(model_LOANS, model_EUR))

model1_df_temp$`Євро_Долар` <- model1_df_temp$Євро / model1_df_temp$Долар
model1_df_temp <- model1_df_temp[!names(model1_df_temp) %in% c("ІЦЖВ")]
model_cor_matrix(model1_df_temp, "")



# ==== 2) ПОВНА МОДЕЛЬ ====
model_full1_2_temp <- lm(Ціна ~ . - Євро - Долар, data = model1_df_temp)
summary(model_full1_2_temp)


# ==== 3) МУЛЬТИКОЛІНЕАРНІСТЬ-2. VIF, ЧИСЛО ОБУМОВЛЕНОСТІ ====
model1_df_norm_temp <- as.data.frame(lapply(model1_df_temp, unit_length_scale))

model_full1_2_norm_temp <- lm(Ціна ~ . - Євро - Долар, data = model1_df_norm_temp)
vif(model_full1_2_norm_temp)
# ІФС        ІГР        ІЦБ       ІЦЖВ ІндМатСтан   РівДолар Євро_Долар 
# 4.183288   6.967531  11.190711   5.493593   3.932199   2.415863   2.257848 
# Погано. Повернімося назад і вилучимо ІЦЖВ замість ІЦЖП.

# ІФС        ІГР        ІЦБ       ІЦЖП ІндМатСтан   РівДолар Євро_Долар 
# 4.357019   7.103669   9.394029   4.431241   4.214926   2.422075   2.246537 

condition_number(model1_df_norm_temp, 0) # 51.56485


# ==== 3) АНАЛІЗ ЗАЛИШКІВ І ВИЯВЛЕННЯ ВИКИДІВ ====
residuals_plot(model_full1_2_temp)

residuals_regressors_plot(model_full1_2_temp, c(2, 4), type = "rstudent")

cooks_dist(model_full1_2_temp)

df_fits(model_full1_2_temp)

df_betas(model_full1_2_temp)

# Спостереження 3 43 не пройшли жодного з тестів.
# Повернімося до дослідження їхнього впливу в кінці.



# ==== 4) ФУНКЦІОНАЛЬНІ ПЕРЕТВОРЕННЯ ====
# ЛІНІЙНІСТЬ І ГОМОСКЕДАСТИЧНІСТЬ
residuals_plot(model_full1_2_temp)
plot(model_full1_2_temp, which = 1) # U-shape
plot(model_full1_2_temp, which = 3)

residuals_regressors_plot(model_full1_2_temp, c(2, 4), type = "rstudent")

av_plots(model_full1_2_temp) # ІЦЖП - під питанням.

crPlots(model_full1_2_temp) # Євро_Долар - U-shape.

resettest(model_full1_2_temp, power = 2) # p-value = 0.1894

bptest(model_full1_2_temp) # p-value = 0.1181

bc <- boxcox(model_full1_2_temp, lambda = seq(-2, 2, by = 0.1))
lambda_opt <- bc$x[which.max(bc$y)]
cat("Оптимальне λ:", round(lambda_opt, 2), "\n") # 0.42 
# ДІ містить 1.

observe_transforms_x(model_full1_2_temp, regressors = names(coef(model_full1_2_temp))[-1])


# ==== 5) НОРМАЛЬНІСТЬ ====
qq_plot(model_full1_2_temp)
shapiro.test(residuals(model_full1_2_temp)) # p-value = 0.6098
ad.test(residuals(model_full1_2_temp)) # p-value = 0.5049



# ==== 6) АВТОКОРЕЛЯЦІЯ ====
time_series_residuals(model_full1_2_temp)
dwtest(model_full1_2_temp) # DW = 0.96307, p-value = 1.061e-06
summary(model_full1_2_temp)
coeftest(model_full1_2_temp, vcov = vcovHAC(model_full1_2_temp))



# ==== 7) ВІДБІР ЗМІННИХ ====
all_models <- regsubsets(Ціна ~ . - Євро - Долар, data = model1_df_temp, nbest = 3, nvmax = 11)
best_models <- best_models_summary(all_models, 12)


# 1.1.11 ІФС + ІндМатСтан + ЧистГрнКред + Євро_Долар  | Adj R² = 0.9112 (p = 5)
lm1 <- lm(Ціна ~ ІФС + ІндМатСтан + ЧистГрнКред + Євро_Долар, data = model1_df_temp)
summary(lm1)
coeftest(lm1, vcov = vcovHAC(lm1))



final_models1_2[[length(final_models1_2) + 1]] <- lm1



# ======== 8) ЕФЕКТИ ВЗАЄМОДІЇ ========
# Працюємо із однією моделлю.
#### lm1 ####
summary(lm1)
aic_base <- AIC(lm1)
bic_base <- BIC(lm1)

interactions_result <- observe_best_interactions(lm1, model1_df_temp, top_n = 10)
# Не додаємо.



# ======== 9) ВАЛІДАЦІЯ МОДЕЛІ ========
suppressWarnings(
  models_validation(list(lm1), data = model1_df_temp, kfold_number = 5,
                    horizon = 1, init_window = 30)
)



# ======== 10) ВИБІР НАЙКРАЩОЇ МОДЕЛІ ========
best_models1_2[[length(best_models1_2) + 1]] <- lm1
models_summary(best_models1_2)




# ----- МОДЕЛЬ ІЗ ЄВРО, але без ІЦБ, ІЦЖП ----
# ==== 1) МУЛЬТИКОЛІНЕАРНІСТЬ-1. КОРЕЛЯЦІЙНА МАТРИЦЯ ====
model1_df_temp2 <- model1_df_temp[, !names(model1_df_temp) %in% c("ІЦБ", "ІГР", "Євро_Долар", "Долар")]
model_cor_matrix(model1_df_temp2, "")


# ==== 2) ПОВНА МОДЕЛЬ ====
model_full1_2_temp2 <- lm(Ціна ~ ., data = model1_df_temp2)
summary(model_full1_2_temp2)


# ==== 3) МУЛЬТИКОЛІНЕАРНІСТЬ-2. VIF, ЧИСЛО ОБУМОВЛЕНОСТІ ====
model1_df_norm_temp2 <- as.data.frame(lapply(model1_df_temp2, unit_length_scale))

model_full1_2_norm_temp2 <- lm(Ціна ~ ., data = model1_df_norm_temp2)
vif(model_full1_2_norm_temp2)

condition_number(model1_df_norm_temp2, 0) # 51.56485


# ==== 3) АНАЛІЗ ЗАЛИШКІВ І ВИЯВЛЕННЯ ВИКИДІВ ====
residuals_plot(model_full1_2_temp2)

residuals_regressors_plot(model_full1_2_temp2, c(2, 4), type = "rstudent")

cooks_dist(model_full1_2_temp2)

df_fits(model_full1_2_temp2)

df_betas(model_full1_2_temp2)

# Спостереження 3 46 не пройшли жодного з тестів.
# Повернімося до дослідження їхнього впливу в кінці.



# ==== 4) ФУНКЦІОНАЛЬНІ ПЕРЕТВОРЕННЯ ====
# ЛІНІЙНІСТЬ І ГОМОСКЕДАСТИЧНІСТЬ
residuals_plot(model_full1_2_temp2)
plot(model_full1_2_temp2, which = 1) # U-shape
plot(model_full1_2_temp2, which = 3)

residuals_regressors_plot(model_full1_2_temp2, c(2, 4), type = "rstudent")

av_plots(model_full1_2_temp2) # ІЦЖП - під питанням.

crPlots(model_full1_2_temp2) # Євро_Долар - U-shape.

resettest(model_full1_2_temp2, power = 2) # p-value = 0.03366

bptest(model_full1_2_temp2) # p-value = 0.4801

bc <- boxcox(model_full1_2_temp2, lambda = seq(-2, 2, by = 0.1))
lambda_opt <- bc$x[which.max(bc$y)]
cat("Оптимальне λ:", round(lambda_opt, 2), "\n") # 0.79  
# ДІ містить 1.

observe_transforms_y(model_full1_2_temp2)
observe_transforms_x(model_full1_2_temp2, regressors = names(coef(model_full1_2_temp2))[-1])

model_1_2_temp2_Sq <- update(model_full1_2_temp2, . ~ . + log(Євро))
# AIC diff: 8.4329161


# ======== НОВА МОДЕЛЬ 1 ========
# ЛІНІЙНІСТЬ І ГОМОСКЕДАСТИЧНІСТЬ
plot(model_1_2_temp2_Sq, which = 1) # +
residuals_plot(model_1_2_temp2_Sq)
plot(model_1_2_temp2_Sq, which = 3)

residuals_regressors_plot(model_1_2_temp2_Sq, c(2, 4), type = "rstudent")

av_plots(model_1_2_temp2_Sq)
crPlots(model_1_2_temp2_Sq)

resettest(model_1_2_temp2_Sq, power = 2) # p-value = 0.1385

bptest(model_1_2_temp2_Sq) # p-value = 0.1285

observe_transforms_x(model_1_2_temp2_Sq, regressors = names(coef(model_1_2_temp2_Sq))[-1])



# ==== 5) НОРМАЛЬНІСТЬ ====
qq_plot(model_1_2_temp2_Sq)
shapiro.test(residuals(model_1_2_temp2_Sq)) # p-value = 0.325
ad.test(residuals(model_1_2_temp2_Sq)) # p-value = 0.1558



# ==== 6) АВТОКОРЕЛЯЦІЯ ====
time_series_residuals(model_1_2_temp2_Sq)
dwtest(model_1_2_temp2_Sq) # DW = 0.92559, p-value = 6.627e-07
summary(model_1_2_temp2_Sq)
coeftest(model_1_2_temp2_Sq, vcov = vcovHAC(model_1_2_temp2_Sq))



# ==== 7) ВІДБІР ЗМІННИХ ====
all_models <- regsubsets(Ціна ~ . + log(Євро), data = model1_df_temp2, nbest = 3, nvmax = 11)
best_models <- best_models_summary(all_models, 12)


# 1. Євро + ІФС + ІндМатСтан + ЧистГрнКред + log(Євро)  | Adj R² = 0.9150 (p = 6)
lm1 <- lm(Ціна ~ Євро + ІФС + ІндМатСтан + ЧистГрнКред + log(Євро), data = model1_df_temp2)
summary(lm1)
coeftest(lm1, vcov = vcovHAC(lm1))
# Зупинимось тут, поки не дізнаємося, чи буде така модель кращою за ту, що не містить жодних 
# ФП.


