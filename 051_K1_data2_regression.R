# ----- 46 СПОСТЕРЕЖЕНЬ -----
# ==== 1) МУЛЬТИКОЛІНЕАРНІСТЬ-1. КОРЕЛЯЦІЙНА МАТРИЦЯ ====
model_cor_matrix(model_df_2[, -1], "")

model1_df <- upd_model_df(model_df_2, remove = "Ч", room_num = 1)
colnames(model1_df)[colnames(model1_df) == "К1"] <- "Ціна"
model_cor_matrix(model1_df, "")

model1_df$`Євро_Долар` <- model1_df$Євро / model1_df$Долар
model1_df <- model1_df[!names(model1_df) %in% c("ІЦЖВ", "ЧистГрнКред")]
model1_df <- model1_df[!names(model1_df) %in% c("Євро", "Долар")]
model_cor_matrix(model1_df, "")

head(model1_df, 3)
# 7 предикторів



# ==== 2) ПОВНА МОДЕЛЬ ====
model_full1_2 <- lm(Ціна ~ ., data = model1_df)
summary(model_full1_2)


# ==== 3) МУЛЬТИКОЛІНЕАРНІСТЬ-2. VIF, ЧИСЛО ОБУМОВЛЕНОСТІ ====
model1_df_norm <- as.data.frame(lapply(model1_df, unit_length_scale))

model_full1_2_norm <- lm(Ціна ~ ., data = model1_df_norm)
vif(model_full1_2_norm)
# ІФС        ІГР        ІЦБ       ІЦЖВ ІндМатСтан   РівДолар Євро_Долар 
# 4.183288   6.967531  11.190711   5.493593   3.932199   2.415863   2.257848 
# Погано. Повернімося назад і вилучимо ІЦЖВ замість ІЦЖП.

# ІФС        ІГР        ІЦБ       ІЦЖП ІндМатСтан   РівДолар Євро_Долар 
# 4.357019   7.103669   9.394029   4.431241   4.214926   2.422075   2.246537 

condition_number(model1_df_norm, 0) # 39.79285



# ==== 3) АНАЛІЗ ЗАЛИШКІВ І ВИЯВЛЕННЯ ВИКИДІВ ====
residuals_plot(model_full1_2)

residuals_regressors_plot(model_full1_2, c(2, 4), type = "rstudent")

cooks_dist(model_full1_2)

df_fits(model_full1_2)

df_betas(model_full1_2)

# Спостереження 1 8 13 не пройшли жодного з тестів.
# Повернімося до дослідження їхнього впливу в кінці.



# ==== 4) ФУНКЦІОНАЛЬНІ ПЕРЕТВОРЕННЯ ====
# ЛІНІЙНІСТЬ І ГОМОСКЕДАСТИЧНІСТЬ
plot(model_full1_2, which = 1) # U-shape

residuals_regressors_plot(model_full1_2, c(2, 4), type = "rstudent")
# Тенденцій не видно.
# ІФС, ІГР, ІЦЖП - воронки.

av_plots(model_full1_2) # ІЦЖП - під питанням.

crPlots(model_full1_2) # Євро_Долар - U-shape.

resettest(model_full1_2, power = 2) # p-value = 0.002734

plot(model_full1_2, which = 3)

bptest(model_full1_2) # p-value = 0.1469

bc <- boxcox(model_full1_2, lambda = seq(-2, 2, by = 0.1))
lambda_opt <- bc$x[which.max(bc$y)]
cat("Оптимальне λ:", round(lambda_opt, 2), "\n") # -0.1

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
models_summary(list(lm1, lm2), model_full = model_log1_2_Lg)


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
residuals_plot(inter_lm1)
plot(inter_lm1, which = 3) # Дисперсія не стала за рахунок окремої
# хмари даних.

residuals_regressors_plot(inter_lm1, c(2, 4), type = "rstudent")

av_plots(inter_lm1)

crPlots(inter_lm1) # Висновки як і для avPlots.
# Загалом, краще.

resettest(inter_lm1, power = 2) # p-value = 0.006915
# Тест тепер провалюється!

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

