model_df_75_m <- merge(model_df_75, KYIV_INFLOW, by = "Дата")
model_cor_matrix(model_df_75_m[, -1], "")

model_df_date <- model_df_75_m[, !names(model_df_75_m) %in% c("Ч")]
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
model_full <- lm(Ціна ~ ., data = df_long)
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

resettest(model_full, power = 2) # p-value = 0.0138

bptest(model_full) # p-value = 3.216e-09

bartlett.test(residuals(model_full) ~ df_long$Кімнатність) # p-value = 9.881e-11
# Тест Бартлетта чутливий до ненормальності.

leveneTest(residuals(model_full) ~ df_long$Кімнатність) # p-value = 3.519e-06 

# Отже, у моделі наявні проблеми з лінійністю (?) та гомоскедастичністю.


bc <- boxcox(model_full, lambda = seq(-1, 1, by = 0.1))
lambda_opt <- bc$x[which.max(bc$y)]
cat("Оптимальне λ:", round(lambda_opt, 2), "\n") # 0.35

model_Sqrt <- lm(sqrt(Ціна) ~ ., data = df_long)
summary(model_Sqrt)



# ======== НОВА МОДЕЛЬ 1 ========
# ЛІНІЙНІСТЬ І ГОМОСКЕДАСТИЧНІСТЬ 
residuals_plot(model_Sqrt) # Позбавилися трикунтника.

plot(model_Sqrt, which = 1) # Майже на прямій.

plot(model_Sqrt, which = 3) # Дисперсія майже стала.

residuals_regressors_plot(model_Sqrt, c(3, 3), type = "rstudent")
# Крос-секційна гетероскедастичність слабка. Зміщення в Кімнатність менше.
# ЧистГрнКред - патерн змійки (як і було).

av_plots(model_Sqrt)

crPlots(model_Sqrt)
par(mfrow = c(1, 1))
# Ці два графіки показують, що проблем з лінійністю ЧистГрнКред немає.

resettest(model_Sqrt, power = 2) # p-value = 0.0241

bptest(model_Sqrt) # p-value = 1.903e-07

bartlett.test(residuals(model_Sqrt) ~ df_long$Кімнатність) # p-value = 9.095e-06
# Тест Бартлетта чутливий до ненормальності.

leveneTest(residuals(model_Sqrt) ~ df_long$Кімнатність) # p-value = 1.658e-06
# Отже, у моделі наявні проблеми з лінійністю (?) та гомоскедастичністю.

all_preds <- names(coef(model_Sqrt))[-1]
numeric_preds <- all_preds[!grepl("Кімнатність", all_preds)]
observe_transforms_x(model_Sqrt, regressors = numeric_preds)

model_Sqrt_Lg <- update(model_Sqrt, . ~ . + log(РівДолар))
# AIC_diff = 48.695916  
summary(model_Sqrt_Lg)



# ======== НОВА МОДЕЛЬ 2 ========
# ЛІНІЙНІСТЬ І ГОМОСКЕДАСТИЧНІСТЬ 
residuals_plot(model_Sqrt_Lg)

plot(model_Sqrt_Lg, which = 1)

plot(model_Sqrt_Lg, which = 3)

residuals_regressors_plot(model_Sqrt_Lg, c(3, 3), type = "rstudent")
# ЧистГрнКред - патерн змійки (як і було).
# ІЦБ, ІЦЖП - воронки (як і було).

av_plots(model_Sqrt_Lg)

crPlots(model_Sqrt_Lg)
par(mfrow = c(1, 1))

resettest(model_Sqrt_Lg, power = 2) # p-value = 0.009674

bptest(model_Sqrt_Lg) # p-value = 1.858e-05

bartlett.test(residuals(model_Sqrt_Lg) ~ df_long$Кімнатність) # p-value = 0.0004721
# Тест Бартлетта чутливий до ненормальності.

leveneTest(residuals(model_Sqrt_Lg) ~ df_long$Кімнатність) # p-value = 0.0001402 
# Отже, у моделі наявні проблеми з лінійністю (?) та гомоскедастичністю.

observe_transforms_x(model_Sqrt_Lg, regressors = numeric_preds[numeric_preds != "ЧистГрнКред"])

model_Sqrt_Lg_Lg <- update(model_Sqrt_Lg, .~. + log(Долар))
#AIC_diff = 1.321468e+01
summary(model_Sqrt_Lg_Lg)



# ======== НОВА МОДЕЛЬ 3 ========
# ЛІНІЙНІСТЬ І ГОМОСКЕДАСТИЧНІСТЬ 
residuals_plot(model_Sqrt_Lg_Lg)

plot(model_Sqrt_Lg_Lg, which = 1)

plot(model_Sqrt_Lg_Lg, which = 3)

residuals_regressors_plot(model_Sqrt_Lg_Lg, c(3, 3), type = "rstudent")

av_plots(model_Sqrt_Lg_Lg)

crPlots(model_Sqrt_Lg_Lg)
par(mfrow = c(1, 1))

resettest(model_Sqrt_Lg_Lg, power = 2) # p-value = 0.0157

bptest(model_Sqrt_Lg_Lg) # p-value = 0.0001151

bartlett.test(residuals(model_Sqrt_Lg_Lg) ~ df_long$Кімнатність) # p-value = 0.0003071

leveneTest(residuals(model_Sqrt_Lg_Lg) ~ df_long$Кімнатність) # p-value = 0.0001235  

observe_transforms_x(model_Sqrt_Lg_Lg, regressors = numeric_preds[numeric_preds != "ЧистГрнКред"])

model_Sqrt_Lg_Lg_Sq <- update(model_Sqrt_Lg_Lg, .~. + I(РівДолар^2))
#AIC_diff = 22.35713323 
summary(model_Sqrt_Lg_Lg_Sq)



# ======== НОВА МОДЕЛЬ 4 ========
# ЛІНІЙНІСТЬ І ГОМОСКЕДАСТИЧНІСТЬ 
residuals_plot(model_Sqrt_Lg_Lg_Sq)

plot(model_Sqrt_Lg_Lg_Sq, which = 1)

plot(model_Sqrt_Lg_Lg_Sq, which = 3)

residuals_regressors_plot(model_Sqrt_Lg_Lg_Sq, c(3, 3), type = "rstudent")

av_plots(model_Sqrt_Lg_Lg_Sq)

crPlots(model_Sqrt_Lg_Lg_Sq)
par(mfrow = c(1, 1))

resettest(model_Sqrt_Lg_Lg_Sq, power = 2) # p-value = 0.007078

bptest(model_Sqrt_Lg_Lg_Sq) # p-value = 0.0001715

bartlett.test(residuals(model_Sqrt_Lg_Lg_Sq) ~ df_long$Кімнатність) # p-value = 0.0001715

leveneTest(residuals(model_Sqrt_Lg_Lg_Sq) ~ df_long$Кімнатність) # p-value = 0.0001715  

observe_transforms_x(model_Sqrt_Lg_Lg_Sq, regressors = numeric_preds[numeric_preds != "ЧистГрнКред"])


model_feols <- feols(formula(model_Sqrt_Lg_Lg_Sq), data = df_long_date, 
                     panel.id = ~ Кімнатність + Дата)


# ==== 4) НОРМАЛЬНІСТЬ ====
qq_plot(model_Sqrt_Lg_Lg_Sq)
shapiro.test(residuals(model_Sqrt_Lg_Lg_Sq)) # p-value = 0.7379
ad.test(residuals(model_Sqrt_Lg_Lg_Sq)) # p-value = 0.4937


# ==== 5) АВТОКОРЕЛЯЦІЯ ====
time_series_residuals(model_Sqrt_Lg_Lg_Sq)
resplot_matrix_cr(model_Sqrt_Lg_Lg_Sq)
cor_residuals_cr(model_Sqrt_Lg_Lg_Sq)
# Крос-секційна кореляція тільки між К1 та К2.

acf_cr(model_Sqrt_Lg_Lg_Sq)
# Всюди спостерігається додатна автокореляція першого порядку.



# ==== 6) ВІДБІР ЗМІННИХ ====
all_models <- regsubsets(formula(model_Sqrt_Lg_Lg_Sq), 
                         data = df_long, nbest = 3, nvmax = 15)
best_models <- best_models_summary(all_models, 30)


# -1. Євро + Долар + ІФС + ІГР + ІЦБ + РівДолар + Притік + Кімнатність2 + Кімнатність3 + log(РівДолар) + log(Долар) + I(РівДолар^2)  | Adj R² = 0.9449 (p = 13)
lm1 <- update(model_feols, . ~ . - ЧисНасел)
summary(lm1, vcov = "DK")

lm1 <- update(lm1, . ~ . - ІЦБ)
summary(lm1, vcov = "DK")

lm1 <- update(lm1, . ~ . - ІГР)
summary(lm1, vcov = "DK")

# 9.4.11?



# -12.4 Євро + Долар + РівДолар + Притік + Кімнатність2 + Кімнатність3 + log(РівДолар) + log(Долар) + I(РівДолар^2)  | Adj R² = 0.9428 (p = 10)
lm2 <- update(model_feols, . ~ . - ІФС - ЧисНасел - ІГР - ІЦБ)
summary(lm2, vcov = "DK")
# Ця модель краща за 1. Тому 1 виключаємо.



# -13.11 Долар + ЧисНасел + РівДолар + Притік + Кімнатність2 + Кімнатність3 + log(РівДолар) + log(Долар) + I(РівДолар^2)  | Adj R² = 0.9418 (p = 10)
lm3 <- update(model_feols, . ~ . - ІФС - Євро - ІГР - ІЦБ)
summary(lm3, vcov = "DK")



# 14.6 Долар + ІЦБ + РівДолар + Притік + Кімнатність2 + Кімнатність3 + log(РівДолар) + I(РівДолар^2)  | Adj R² = 0.9415 (p = 9)
lm4 <- feols(sqrt(Ціна) ~ Долар + ІЦБ + РівДолар + Притік + Кімнатність + log(РівДолар) + I(РівДолар^2),
             data = df_long_date, panel.id = ~ Кімнатність + Дата)
summary(lm4, vcov = "DK")
# Ця модель є кращою за всі попередні, оскільки містить на одну змінну змінну менше,
# а R2 має майже такий самий.



# 17. ЧисНасел + РівДолар + Притік + Кімнатність2 + Кімнатність3 + log(РівДолар) + I(РівДолар^2)  | Adj R² = 0.9387 (p = 8)
lm5 <- feols(sqrt(Ціна) ~ ЧисНасел + РівДолар + Притік + Кімнатність + log(РівДолар) + I(РівДолар^2),
             data = df_long_date, panel.id = ~ Кімнатність + Дата)
summary(lm5, vcov = "DK")


# 19. Долар + РівДолар + Притік + Кімнатність2 + Кімнатність3 + log(РівДолар) + I(РівДолар^2)  | Adj R² = 0.9354 (p = 8)
lm6 <- feols(sqrt(Ціна) ~ Долар + РівДолар + Притік + Кімнатність + log(РівДолар) + I(РівДолар^2),
             data = df_long_date, panel.id = ~ Кімнатність + Дата)
summary(lm6, vcov = "DK")

lm6 <- update(lm6, .~. - Долар)
summary(lm6, vcov = "DK")


# 23. РівДолар + Притік + Кімнатність2 + Кімнатність3 + log(РівДолар)  | Adj R² = 0.9279 (p = 6)
lm7 <- feols(sqrt(Ціна) ~ РівДолар + Притік + Кімнатність + log(РівДолар),
             data = df_long_date, panel.id = ~ Кімнатність + Дата)
summary(lm7, vcov = "DK")


final_models <- list(lm4, lm5, lm6, lm7)


# ======== 7) ЕФЕКТИ ВЗАЄМОДІЇ ========
# Працюємо із чотирма моделями.
lm1 <- final_models[[1]]
lm2 <- final_models[[2]]
lm3 <- final_models[[3]]
lm4 <- final_models[[4]]

models_summary(final_models)

#### lm1 ####
summary(lm1)
aic_base <- AIC(lm1)
bic_base <- BIC(lm1)

lm1_lmcl <- lm(formula(lm1), data = df_long)
interactions_result <- observe_best_interactions(lm1_lmcl, df_long_date, top_n = 30, complex_interactions = TRUE)

residuals_regressors_plot(lm1_lmcl, c(2, 2))

# -1. Притік × Кімнатність | ΔAIC = 33.11 | p = 0.0000 
inter_lm <- update(lm1, . ~ . + Притік:Кімнатність)
summary(inter_lm, vcov = "DK")
# Проблема зі значущістю. Проте залишимо.

inter_lm1 <- inter_lm



#### lm2 ####
summary(lm2)
aic_base <- AIC(lm2)
bic_base <- BIC(lm2)

lm2_lmcl <- lm(formula(lm2), data = df_long)
residuals_regressors_plot(lm2_lmcl, c(3, 2)) # Зміщення К1, К3.

interactions_result <- observe_best_interactions(lm2_lmcl, df_long_date, top_n = 30, complex_interactions = TRUE)


# 1. ЧисНасел × Кімнатність | ΔAIC = 34.83 | p = 0.0000 
inter_lm <- update(lm2, . ~ . + ЧисНасел:Кімнатність)
summary(inter_lm, vcov = "DK")

inter_lm2 <- inter_lm


# 2. Притік × Кімнатність | ΔAIC = 31.08 | p = 0.0000
inter_lm <- update(lm2, . ~ . + Притік:Кімнатність)
summary(inter_lm, vcov = "DK") 
models_summary(list(inter_lm, inter_lm1))
# Ця модель краща за першу, оскільки має менші АІС, ВІС.

inter_lm1 <- inter_lm


#### lm3 ####
summary(lm3)
aic_base <- AIC(lm3)
bic_base <- BIC(lm3)

lm3_lmcl <- lm(formula(lm3), data = df_long)
residuals_regressors_plot(lm3_lmcl, c(3, 2)) # Зміщення К1, К3 (але незначне).

interactions_result <- observe_best_interactions(lm3_lmcl, df_long_date, top_n = 30, complex_interactions = TRUE)

# 1. Притік × Кімнатність | ΔAIC = 27.96 | p = 0.0000 
inter_lm <- update(lm3, . ~ . + Притік:Кімнатність)
summary(inter_lm, vcov = "DK") 
# Гірше, але залишимо для порівняння з іншими.

inter_lm3 <- inter_lm


#### lm4 ####
summary(lm4)

lm4_lmcl <- lm(formula(lm4), data = df_long)

interactions_result <- observe_best_interactions(lm4_lmcl, df_long_date, top_n = 10, complex_interactions = TRUE)

# 1. Притік × Кімнатність | ΔAIC = 25.14 | p = 0.0000 
inter_lm <- update(lm4, . ~ . + Притік:Кімнатність)
summary(inter_lm, vcov = "DK") 
# Гірше, але залишимо для порівняння з іншими.

inter_lm4 <- inter_lm



final_models <- list(inter_lm1, inter_lm2, inter_lm3, inter_lm4)



# ======== 8) ВАЛІДАЦІЯ МОДЕЛІ ========
final_models_lm <- lapply(final_models, function(m) lm(clean_formula(m), data = df_long))

suppressWarnings(
  models_validation(final_models_lm, data = df_long, kfold_number = 5,
                    horizon = 1, init_window = 30)
)



# ======== 9) ВИБІР НАЙКРАЩОЇ МОДЕЛІ ========
length(final_models_lm)

residuals_plot(final_models_lm[[1]])
residuals_regressors_plot(final_models_lm[[1]], c(3, 3)) # Незначна недооцінка.

residuals_plot(final_models_lm[[2]])
residuals_regressors_plot(final_models_lm[[2]], c(3, 3)) # Трохи більше відхилення від нуля.

residuals_plot(final_models_lm[[3]])
residuals_regressors_plot(final_models_lm[[3]], c(3, 3)) # Щось посередині між 1 та 2.

residuals_plot(final_models_lm[[4]])
residuals_regressors_plot(final_models_lm[[4]], c(3, 3)) # Мабуть, найгірше.

lm_to_feols_summary(final_models_lm[[1]], df_long_date)
lm_to_feols_summary(final_models_lm[[2]], df_long_date)
lm_to_feols_summary(final_models_lm[[3]], df_long_date)
lm_to_feols_summary(final_models_lm[[4]], df_long_date)


# Модель 2 не має проблем зі значущістю. Візьмемо її.

best_models <- list(final_models_lm[[2]])

assumptions_check(final_models_lm[[2]])

models_summary(best_models)
suppressWarnings(
  models_validation(best_models, data = df_long, kfold_number = 5,
                    horizon = 1, init_window = 30)
)
