library(tidyr)
library(plm)
library(fixest)

lm_to_feols_summary <- function(model, data){
  modelf <- feols(formula(model), data = data, panel.id = ~ Кімнатність + Дата)
  summary(modelf, vcov = "DK")
}


reset_panel <- function(model, data, power = 2:3) {
  y_name <- names(data)[1]
  y_hat <- fitted(model)
  
  for (k in power) {
    col_name <- paste0("y_hat_", k)
    data[[col_name]] <- y_hat^k
  }
  
  all_vars <- names(data)
  base_vars <- setdiff(all_vars, paste0("y_hat_", power))
  formula_base <- as.formula(paste(y_name, "~", paste(base_vars[-1], collapse = " + ")))
  model_base <- lm(formula_base, data = data)
  
  # 2. Розширена модель
  extra_terms <- paste(paste0("y_hat_", power), collapse = " + ")
  formula_reset <- as.formula(paste(y_name, "~ . +", extra_terms))
  model_reset <- lm(formula_reset, data = data)
  
  # Кластер із data (df_long)
  cluster_vec <- data[["Кімнатність"]]
  
  wt <- waldtest(model_base, model_reset, 
                 vcov = function(x) vcovCL(x, cluster = cluster_vec))

  cat("=== RESET-тест для панельних даних ===\n")
  cat("Додано:", paste0("ŷ^", power, collapse = ", "), "\n")
  cat(sprintf("Статистика: %.4f\n", wt$F[2]))
  cat(sprintf("p-value:    %.4f\n", wt$`Pr(>F)`[2]))

  invisible(list(waldtest = wt, model_reset = model_reset))
}


robustSE_diff <- function(model, vcov){
  # Виводить матрицю із різницями стандартних помилок початкової моделі
  # та моделі на основі робастних SE.
  if(!inherits(model, "fixest")){
    sm_coefs <- summary(model)$coefficients
    ordinary_SE <- sm_coefs[, 2]
    
    sm_coefs_robust <- coeftest(model, vcov = vcov)
    robust_SE <- sm_coefs_robust[, 2]
  }else{
    len <- length(summary(model)$se)
    sm_coefs <- (summary(model)$se)[c(1:len)]
    ordinary_SE <- sm_coefs
    
    sm_coefs_robust <- (summary(model, vcov = vcov)$se)[c(1:length(sm_coefs))]
    robust_SE <- sm_coefs_robust
  }
  cat("SE ordinary", "\n")
  print(ordinary_SE)
  
  cat("\nSE robust", "\n")
  print(robust_SE)
  
  cat("\nSE diff", "\n")
  print(robust_SE - ordinary_SE)
  cat("\n")
  
  cat("\nSE ratio", "\n")
  print(robust_SE /  ordinary_SE)
}


acf_cr <- function(model){
  data <- model$model
  par(mfrow = c(1, 3))
  
  for(room in c("1", "2", "3")){
    e <- residuals(model)[data[["Кімнатність"]] == room]
    acf(e, main = paste("ACF -", room, "кімнатні"))
  }
  par(mfrow = c(1, 1))
}


resplot_matrix_cr <- function(model){
  data <- model$model
  e_1k <- residuals(model)[data$Кімнатність == "1"]
  e_2k <- residuals(model)[data$Кімнатність == "2"]
  e_3k <- residuals(model)[data$Кімнатність == "3"]
  
  pairs(~ e_1k + e_2k + e_3k, 
        main = "Крос-секційна кореляція залишків")
}


cor_residuals_cr <- function(model){
  data <- model$model
  e_1k <- residuals(model)[data$Кімнатність == "1"]
  e_2k <- residuals(model)[data$Кімнатність == "2"]
  e_3k <- residuals(model)[data$Кімнатність == "3"]
  
  return(cor(cbind(e_1k, e_2k, e_3k)))
}


# Стовпець із Дата нам ще знадобиться.
model_75_df_date <- model_df_75[, !names(model_df_75) %in% c("Ч")]
df_long_date <- model_75_df_date %>%
  pivot_longer(cols = c(К1, К2, К3), 
               names_to = "Кімнатність", 
               values_to = "Ціна") %>%
  mutate(Кімнатність = factor(Кімнатність, 
                              levels = c("К1", "К2", "К3"),
                              labels = c("1", "2", "3")))

model_75_df <- model_75_df_date[, -1]
df_long <- df_long_date[, -1]

# N.B.1. Таблиця стала майже утричі довшою, але незалежних даних у нас не побільшало,
# оскільки для кожної дати спостереження повторюється тричі. Через таке значне 
# збільшення кількості ступенів свободи стандартні помилки можуть бути заниженими,
# тому потрібно використовувати кластерні SE для аналізу.
# 
# Також через однакові спостереження посилюється автокореляція.
# Для панельних даних автокореляцію можна розділити на два типи:
# 1. часова (всередині одного типу квартир): та сама автокореляція, яку ми вже бачили
# для кожної з моделей;
# 2. крос-секційна (між типами квартир в один місяць).
# Для панельних даних ДВ-тест використовувати некоректно, оскільки він не розрізняє
# рівні (типи) автокореляції.
#
# Отже, при дослідженні автокореляції використовуємо інший тест, який враховує
# панельність даних, та інші робастні стандартні помилки (кластеризовані SE або
# HAC для панельних даних).

# N.B.2. R2 також є завищеним для моделі з панельними даними, оскільки тепер 
# варіабельність складається із двох частин: мінливість даних всередині
# кожного типу квартир і мінливість між типами.
# Варто подумати над використанням R2 within, що показує R2 всередині групи.

# N.B.3. RESET тест потребує незалежних залишків (оскільки він працює на основі
# F-статистики, яка ґрунтується на цьому припущенні). При додатній автокореляції -
# а також при при кластеризованих даних - p-value може бути заниженим, а при 
# від'ємній - завищеним.



# ==== 1) ПОВНА МОДЕЛЬ ====
model_full_75 <- lm(Ціна ~ ., data = df_long)
summary(model_full_75)


# ==== 2) АНАЛІЗ ЗАЛИШКІВ І ВИЯВЛЕННЯ ВИКИДІВ ====
residuals_plot(model_full_75) # Майже ідеально!

residuals_regressors_plot(model_full_75, c(2, 4), type = "rstudent")
# Вертикальний розкид при одному X показує варіаціацію між типами квартир.
# Традиційно, проблеми із ІГР та ІФС.
# Боксплоти для К1 та К2 виглядають гарно. Для К3 - IQR більший за інші 
# рівні (гетероскедастичність між рівнями); медіана не нуль.

cooks_dist(model_full_75)

df_fits(model_full_75)

df_betas(model_full_75)
# Повернімося до дослідження їхнього впливу в кінці.


# ==== 3) ФУНКЦІОНАЛЬНІ ПЕРЕТВОРЕННЯ ====
# ЛІНІЙНІСТЬ
plot(model_full_75, which = 1)
# Зі збільшенням прогнозованих значень крива сильніше відхиляється від нуля.

residuals_regressors_plot(model_full_75, c(2, 4), type = "rstudent")
# Тенденцій не видно.

av_plots(model_full_75) # ІГР - під питанням.

crPlots(model_full_75) # ІГР - ?

resettest(model_full_75, power = 2) # p-value = 0.00409


# ГОМОСКЕДАСТИЧНІСТЬ 
# Загальна варіабельність
plot(model_full_75, which = 3)
# Зі збільшенням прогнозованих значень дисперсія трохи зростає.

residuals_regressors_plot(model_full_75, c(2, 4), type = "rstudent")
# ІФС, ІГР - ?

bptest(model_full_75) # p-value = 1.163e-07


# Міжгрупова варіабельність
# Перевіримо, чи однакові дисперсії між групами.

bartlett.test(residuals(model_full_75) ~ df_long$Кімнатність) # p-value = 4.468e-07
# Тест Бартлетта чутливий до ненормальності.

leveneTest(residuals(model_full_75) ~ df_long$Кімнатність) # p-value = 0.0004201 

# Отже, у моделі наявні проблеми з лінійністю та гомоскедастичністю.


bc <- boxcox(model_full_75, lambda = seq(0, 1, by = 0.1))
lambda_opt <- bc$x[which.max(bc$y)]
cat("Оптимальне λ:", round(lambda_opt, 2), "\n") # 0.42 
# ДІ містить 0.5 - візьмемо його.

model_Sqrt_75 <- lm(sqrt(Ціна) ~ ., data = df_long)
summary(model_Sqrt_75)



# ======== НОВА МОДЕЛЬ 1 ========
# ЛІНІЙНІСТЬ
plot(model_Sqrt_75, which = 1) # Трохи краще?
residuals_plot(model_Sqrt_75) # Трохи рівномірніше.

residuals_regressors_plot(model_Sqrt_75, c(2, 4), type = "rstudent")
# Здається, все трохи краще. Проте для К3 різниці не видно.

av_plots(model_Sqrt_75)

crPlots(model_Sqrt_75) # Загалом, +.

resettest(model_Sqrt_75, power = 2) # p-value = 0.1485

# Тепер моделі не бракує степеневих перетворень.
# Можемо вважати, що нелінійність усунуто.


# ГОМОСКЕДАСТИЧНІСТЬ 
# Загальна варіабельність
plot(model_Sqrt_75, which = 3) # +

residuals_regressors_plot(model_Sqrt_75, c(2, 4), type = "rstudent")

bptest(model_Sqrt_75) # p-value = 0.0005144

# Міжгрупова варіабельність
bartlett.test(residuals(model_Sqrt_75) ~ df_long$Кімнатність) # p-value = 0.01035

leveneTest(residuals(model_Sqrt_75) ~ df_long$Кімнатність) # p-value = 0.002868  
# БП-тест міг провалитися через сильну гетероскедастичність між групами.


# Над категоріальними змінними ФП не застосовуємо.
all_preds <- names(coef(model_Sqrt_75))[-1]
numeric_preds <- all_preds[!grepl("Кімнатність", all_preds)]
observe_transforms_x(model_Sqrt_75, regressors = numeric_preds)

model_Sqrt_75_Lg <- update(model_Sqrt_75, . ~ . + log(Долар))
# AIC_diff = 8.5399162
summary(model_Sqrt_75)



# ======== НОВА МОДЕЛЬ 2 ========
# ЛІНІЙНІСТЬ
plot(model_Sqrt_75_Lg, which = 1) # Те саме?
residuals_plot(model_Sqrt_75_Lg) # Можливо, на правому кінці стало краще.

residuals_regressors_plot(model_Sqrt_75_Lg, c(2, 4), type = "rstudent")
# Для К3 відбувся зсув середнього (в гіршу сторону).
# Але це нічого. Міжгрупову варіабельність будемо виправляти через GLS
# або просто використаємо робастні SE.

av_plots(model_Sqrt_75_Lg)

crPlots(model_Sqrt_75_Lg) # Загалом, +.

resettest(model_Sqrt_75_Lg, power = 2) # p-value = 0.1758


# ГОМОСКЕДАСТИЧНІСТЬ 
# Загальна варіабельність
plot(model_Sqrt_75_Lg, which = 3) # Те саме?

bptest(model_Sqrt_75_Lg) # p-value = 0.003058

# Міжгрупова варіабельність
bartlett.test(residuals(model_Sqrt_75_Lg) ~ df_long$Кімнатність) # p-value = 0.003984

leveneTest(residuals(model_Sqrt_75_Lg) ~ df_long$Кімнатність) # p-value = 0.006931   
# БП-тест міг провалитися через сильну гетероскедастичність між групами.

# Над категоріальними змінними ФП не застосовуємо.
observe_transforms_x(model_Sqrt_75_Lg, regressors = numeric_preds)

model_Sqrt_75_Lg2 <- update(model_Sqrt_75_Lg, . ~ . + log(РівДолар))
# AIC_diff = 12.95123
summary(model_Sqrt_75_Lg2)



# ======== НОВА МОДЕЛЬ 3 ========
# ЛІНІЙНІСТЬ
plot(model_Sqrt_75_Lg2, which = 1) # Краще/Гірше?
residuals_plot(model_Sqrt_75_Lg2) # Краще/Гірше?

residuals_regressors_plot(model_Sqrt_75_Lg2, c(2, 4), type = "rstudent")
# Для К3 - зсув середнього ближче до 0, К1 - далі.
# ІФС - трохи рівномірніше.

av_plots(model_Sqrt_75_Lg2) # ІЦБ +, РівДолар -?

crPlots(model_Sqrt_75_Lg2)
par(mfrow = c(1, 1))

resettest(model_Sqrt_75_Lg2, power = 2) # p-value = 0.05044


# ГОМОСКЕДАСТИЧНІСТЬ 
# Загальна варіабельність
plot(model_Sqrt_75_Lg2, which = 3) # Те саме?

bptest(model_Sqrt_75_Lg2) # p-value = 0.003982

# Міжгрупова варіабельність
bartlett.test(residuals(model_Sqrt_75_Lg2) ~ df_long$Кімнатність) # p-value = 0.06521

leveneTest(residuals(model_Sqrt_75_Lg2) ~ df_long$Кімнатність) # p-value = 0.08483    

# Над категоріальними змінними ФП не застосовуємо.
observe_transforms_x(model_Sqrt_75_Lg2, regressors = numeric_preds)
# Додамо ще log(Євро), але якщо він не виправить проблему з непостійністю - вилучимо.
# UPD 1. log(Євро) трохи зіпсував RESET тест, хоч і покращив тести на гомоскедастиність
# (БП все одно < 0.05). Тому спробуємо додати не логарифм, а квадрат.
# UPD 1. RESET не став кращим.
# Але пам'ятаймо про ненадійсніть RESET для кластеризованих даних!

model_Sqrt_75_Lg3 <- update(model_Sqrt_75_Lg2, . ~ . + log(Євро))
# AIC_diff = 12.3060858 
summary(model_Sqrt_75_Lg3)


# ======== НОВА МОДЕЛЬ 4 ========
# ЛІНІЙНІСТЬ
plot(model_Sqrt_75_Lg3, which = 1) 
residuals_plot(model_Sqrt_75_Lg3) 

residuals_regressors_plot(model_Sqrt_75_Lg3, c(2, 4), type = "rstudent")
# Значний зсув медіани наявний тільки для К1.

#av_plots(model_Sqrt_75_Lg3) 

#crPlots(model_Sqrt_75_Lg2)
par(mfrow = c(1, 1))

resettest(model_Sqrt_75_Lg3, power = 2) # p-value = 0.04153


# ГОМОСКЕДАСТИЧНІСТЬ 
# Загальна варіабельність
plot(model_Sqrt_75_Lg3, which = 3) # Те саме?

bptest(model_Sqrt_75_Lg3) # p-value = 0.01874

# Міжгрупова варіабельність
bartlett.test(residuals(model_Sqrt_75_Lg3) ~ df_long$Кімнатність) # p-value = 0.1062

leveneTest(residuals(model_Sqrt_75_Lg3) ~ df_long$Кімнатність) # p-value = 0.07672     

observe_transforms_x(model_Sqrt_75_Lg3, regressors = numeric_preds)

model_Sqrt_75_Lg3Sq <- update(model_Sqrt_75_Lg3, . ~ . + I(Долар^2))
# AIC_diff = 10.96835547 
# Додаємо нову змінну з обережністю (перевіримо значимість) для уникнення перенавчання.
summary(model_Sqrt_75_Lg3Sq)

waldtest(model_Sqrt_75_Lg3, model_Sqrt_75_Lg3Sq, 
         vcov = function(x) vcovCL(x, cluster = df_long$Кімнатність)) # +



# ======== НОВА МОДЕЛЬ 5 ========
# ЛІНІЙНІСТЬ
plot(model_Sqrt_75_Lg3Sq, which = 1) 
residuals_plot(model_Sqrt_75_Lg3Sq) 

residuals_regressors_plot(model_Sqrt_75_Lg3Sq, c(2, 4), type = "rstudent")
# Значний зсув медіани наявний тільки для К1.

av_plots(model_Sqrt_75_Lg3Sq) 

crPlots(model_Sqrt_75_Lg3Sq)
par(mfrow = c(1, 1))

resettest(model_Sqrt_75_Lg3Sq, power = 2) # p-value = 0.0487


# ГОМОСКЕДАСТИЧНІСТЬ 
# Загальна варіабельність
plot(model_Sqrt_75_Lg3Sq, which = 3) # Те саме?

bptest(model_Sqrt_75_Lg3Sq) # p-value = 0.02226

# Міжгрупова варіабельність
bartlett.test(residuals(model_Sqrt_75_Lg3Sq) ~ df_long$Кімнатність) # p-value = 0.07846

leveneTest(residuals(model_Sqrt_75_Lg3Sq) ~ df_long$Кімнатність) # p-value = 0.1556     

observe_transforms_x(model_Sqrt_75_Lg3Sq, regressors = numeric_preds)
# +square(Євро): AIC_diff = 10.31685, але задля ергономічності моделі зупинимося із
# додаванням змінних.

# ВИСНОВОК.
# RESET та БП тести провалились. Це могло статися через панельність даних (сильна залежність
# всередині). Проте графіки показують, що з припущеннями лінійності та гетероскедастичності
# все гаразд.

# Будуємо модель із кластеризованими + HAC SE
model_feols <- feols(formula(model_Sqrt_75_Lg3Sq), data = df_long_date, 
                     panel.id = ~ Кімнатність + Дата)
# Driscoll-Kraay (SCC) — стійкі до гетероскедастичності + автокореляції + крос-секційної 
# залежності (SCC = Spatial Correlation Consistent).

robustSE_diff(model_feols, vcov = "DK") # Для багатьох змінних SE збільшилися не сильно.
# Значущість втратив тільки ІГР.


# ==== 4) НОРМАЛЬНІСТЬ ====
qq_plot(model_Sqrt_75_Lg3Sq)
shapiro.test(residuals(model_Sqrt_75_Lg3Sq)) # p-value = 0.8922
ad.test(residuals(model_Sqrt_75_Lg3Sq)) # p-value = 0.5927


# ==== 5) АВТОКОРЕЛЯЦІЯ ====
time_series_residuals(model_Sqrt_75_Lg3Sq)
resplot_matrix_cr(model_Sqrt_75_Lg3Sq)
cor_residuals_cr(model_Sqrt_75_Lg3Sq)

acf_cr(model_Sqrt_75_Lg3Sq)
# Всюди спостерігається додатна автокореляція першого порядку.


# ==== 6) ВІДБІР ЗМІННИХ ====
e <- residuals(model_Sqrt_75_Lg3Sq)
models_summary(list(model_Sqrt_75_Lg3Sq))
rho <- cor(e[-1], e[-length(e)])

all_models <- regsubsets(formula(model_Sqrt_75_Lg3Sq), 
                         data = df_long, nbest = 3, nvmax = 15)
best_models <- best_models_summary(all_models, 20)
# Для панельних даних дисперсія сигма є заниженою, а тому Cp є завищеним, 
# проте не обов'язково ближчим до p. Отже, Cp не є інформативним показником
# якості моделі для панельних даних.


# 1.1. Євро + Долар + ІГР + ІЦБ + ЧисНасел + РівДолар + Кімнатність2 + Кімнатність3 + log(Долар) + log(РівДолар) 
# + log(Євро) + I(Долар^2)  | Adj R² = 0.9226 (p = 13)
lm1 <- update(model_feols, . ~ . - ІФС)
summary(lm1, vcov = "DK")

lm1 <- update(lm1, . ~ . - ІГР)
summary(lm1, vcov = "DK")

lm1 <- update(lm1, . ~ . - log(Долар))
summary(lm1, vcov = "DK")

lm1 <- update(lm1, . ~ . - РівДолар)
summary(lm1, vcov = "DK")
# Це модель 11.11.


# -4.7. Євро + Долар + ІФС + ІЦБ + ЧисНасел + РівДолар + Кімнатність2 + Кімнатність3 + log(Долар) + log(РівДолар) 
# + log(Євро) + I(Долар^2)  | Adj R² = 0.9204 (p = 13)
lm2 <- update(model_feols, . ~ . - ІГР)
summary(lm2, vcov = "DK")

lm2 <- update(lm2, . ~ . - ІФС)
summary(lm2, vcov = "DK")

lm2 <- update(lm2, . ~ . - log(Долар))
summary(lm2, vcov = "DK")

lm2 <- update(lm2, . ~ . - РівДолар)
summary(lm2, vcov = "DK")
# Та сама модель.


# -8. Євро + Долар + ІЦБ + ЧисНасел + Кімнатність2 + Кімнатність3 + log(Долар) + log(РівДолар) + log(Євро) + I(Долар^2)  | Adj R² = 0.9174 (p = 11)
lm2 <- update(model_feols, . ~ . - ІГР - РівДолар - ІФС)
summary(lm2, vcov = "DK")

lm2 <- update(lm2, . ~ . - log(Долар))
summary(lm2, vcov = "DK")

lm2 <- update(lm2, . ~ . - ІЦБ)
summary(lm2, vcov = "DK")

lm2 <- update(lm2, . ~ . - ЧисНасел)
summary(lm2, vcov = "DK") # Виглядає дуже ергономічно. Залишимо.


# -9. Євро + Долар + ІЦБ + ЧисНасел + РівДолар + Кімнатність2 + Кімнатність3 + log(РівДолар) + log(Євро) + I(Долар^2)  | Adj R² = 0.9172 (p = 11)
lm3 <- update(model_feols, . ~ . - ІГР - log(Долар) - ІФС)
summary(lm3, vcov = "DK")

lm3 <- update(lm3, . ~ . - РівДолар)
summary(lm3, vcov = "DK")

lm3 <- update(lm3, . ~ . - ІЦБ)
summary(lm3, vcov = "DK")

lm3 <- update(lm3, . ~ . - ЧисНасел)
summary(lm3, vcov = "DK") # Та сама модель.


# -10. Євро + ІЦБ + ЧисНасел + РівДолар + Кімнатність2 + Кімнатність3 + log(Долар) + log(РівДолар) + log(Євро) + I(Долар^2)  | Adj R² = 0.9168 (p = 11)
lm3 <- update(model_feols, . ~ . - ІГР - Долар - ІФС)
summary(lm3, vcov = "DK")

lm3 <- update(lm3, . ~ . - РівДолар)
summary(lm3, vcov = "DK")

lm3 <- update(lm3, . ~ . - ІЦБ)
summary(lm3, vcov = "DK")

lm3 <- update(lm3, . ~ . - ЧисНасел)
summary(lm3, vcov = "DK") # Та ж модель, але з log(Долар) замість Долар.


# 16. Євро + Долар + ЧисНасел + Кімнатність2 + Кімнатність3 + log(Долар) + log(РівДолар) + log(Євро)  | Adj R² = 0.9090 (p = 9)
lm3 <- feols(sqrt(Ціна) ~ Євро + Долар + ЧисНасел + Кімнатність + log(Долар) + log(РівДолар) + log(Євро),
             data = df_long_date, panel.id = ~ Кімнатність + Дата)
summary(lm3, vcov = "DK")

lm3 <- update(lm3, . ~ . - ЧисНасел)
summary(lm3, vcov = "DK") # Це модель №8, але з логарифмом замість квадрата (виберемо її замість квадрата).


# 20. Євро + ІЦБ + ЧисНасел + Кімнатність2 + Кімнатність3 + log(РівДолар)  | Adj R² = 0.8945 (p = 7)
lm4 <- feols(sqrt(Ціна) ~ Євро + ІЦБ + ЧисНасел + Кімнатність + log(РівДолар),
             data = df_long_date, panel.id = ~ Кімнатність + Дата)
summary(lm4, vcov = "DK")

lm4 <- update(lm4, . ~ . - ІЦБ)
summary(lm4, vcov = "DK")

lm4 <- update(lm4, . ~ . - Євро)
summary(lm4, vcov = "DK")

lm4 <- update(lm4, . ~ . + РівДолар - log(РівДолар))
summary(lm4, vcov = "DK") # Це одна з вибраних моделей для 1-кімнатних квартир. Залишимо.

final_models = list("9_var" = lm1, "7_var" = lm3, "4_var" = lm4)


# ======== 7) ЕФЕКТИ ВЗАЄМОДІЇ ========
# Працюємо із трьома моделями.
lm1 <- final_models[[1]]
lm2 <- final_models[[2]]
lm3 <- final_models[[3]]

summary(lm1, vcov = "DK")
AIC(lm1)
BIC(lm1)

summary(lm2, vcov = "DK")
AIC(lm2)
BIC(lm2)

summary(lm3, vcov = "DK")
AIC(lm3)
BIC(lm3)




#### lm1 ####
lm1 <- update(lm1, . ~ . - I(I(I(I(I(I(Долар^2))))))  + Долар^2)
summary(lm1)

aic_base <- AIC(lm1)
bic_base <- BIC(lm1)

lm1_lmcl <- lm(formula(lm1), data = df_long)
interactions_result <- observe_best_interactions(lm1_lmcl, df_long_date, top_n = 30)

# 1. Євро / Долар | ΔAIC = 27.28 | p = 0.0000 
inter_lm <- update(lm1, . ~ . + I(Євро / Долар))
summary(inter_lm, vcov = "DK")

inter_lm <- update(inter_lm, . ~ . - Євро)
summary(inter_lm, vcov = "DK")

aic_base - AIC(inter_lm) # 24.66552
bic_base - BIC(inter_lm) # 24.66552
inter_lm1 <- inter_lm


# 2. Долар × ІЦБ | ΔAIC = 26.16 | p = 0.0000 
inter_lm <- update(lm1, . ~ . + Долар:ІЦБ)
summary(inter_lm, vcov = "DK")

inter_lm <- update(inter_lm, . ~ . - ЧисНасел)
summary(inter_lm, vcov = "DK")

aic_base - AIC(inter_lm) # 25.80666
bic_base - BIC(inter_lm) # 25.80666
inter_lm2 <- inter_lm


# 3. Євро × Долар | ΔAIC = 25.54 | p = 0.0000 
inter_lm <- update(lm1, . ~ . + Долар:Євро)
summary(inter_lm, vcov = "DK")

inter_lm <- update(inter_lm, . ~ . - Долар)
summary(inter_lm, vcov = "DK")

aic_base - AIC(inter_lm) # 27.28265
bic_base - BIC(inter_lm) # 27.28265
inter_lm3 <- inter_lm


# 4. Долар / Євро | ΔAIC = 25.25 | p = 0.0000 
inter_lm <- update(lm1, . ~ . + I(Долар / Євро))
summary(inter_lm, vcov = "DK")

inter_lm <- update(inter_lm, . ~ . - Євро)
summary(inter_lm, vcov = "DK")

aic_base - AIC(inter_lm) # 22.93532
bic_base - BIC(inter_lm) # 22.93532

summary(inter_lm1, vcov = "DK")
# Яка модель краща (в плані знаків перед коефіцієнтами): 4 чи 1?

inter_lm4 <- inter_lm


# 5. ІЦБ / Долар | ΔAIC = 25.22 | p = 0.0000 
inter_lm <- update(lm1, . ~ . + I(ІЦБ / Долар))
summary(inter_lm, vcov = "DK")

inter_lm <- update(inter_lm, . ~ . - ЧисНасел)
summary(inter_lm, vcov = "DK")

aic_base - AIC(inter_lm) # 25.04295
bic_base - BIC(inter_lm) # 25.04295

inter_lm5 <- inter_lm


# 6. ЧисНасел × Кімнатність | ΔAIC = 23.45 | p = 0.0000 
inter_lm <- update(lm1, . ~ . + ЧисНасел:Кімнатність)
summary(inter_lm, vcov = "DK")

inter_lm <- update(inter_lm, . ~ . - ІЦБ)
summary(inter_lm, vcov = "DK")

aic_base - AIC(inter_lm) # 10.20238
bic_base - BIC(inter_lm) # 6.827105

inter_lm6 <- inter_lm

# Яка з моделей краща?
summary(inter_lm1, vcov = "DK")
summary(inter_lm2, vcov = "DK") # Не містить ЧисНасел. Може бути корисним для data2.
summary(inter_lm3, vcov = "DK")
summary(inter_lm4, vcov = "DK") # lm1 має більш логічні знаки перед коефіцієнтами.
summary(inter_lm5, vcov = "DK") # ІЦБ/Долар легше інтерпретувати, ніж Долар:ІЦБ (lm2)?
summary(inter_lm6, vcov = "DK") # Показує різницю у впливі ЧисНасел на К1, К2, К3.

final_models[["9Євро / Долар"]] <- inter_lm1
final_models[["9Долар × ІЦБ"]] <- inter_lm2
final_models[["9Євро × Долар"]] <- inter_lm3
final_models[["9Долар / Євро"]] <- inter_lm4
final_models[["9ІЦБ / Долар"]] <- inter_lm5
final_models[["9ЧисНасел × Кімнатність"]] <- inter_lm6


#### lm2 ####
summary(lm2, vcov = "DK")
aic_base <- AIC(lm2)
bic_base <- BIC(lm2)

lm2_lmcl <- lm(formula(lm2), data = df_long)
interactions_result <- observe_best_interactions(lm2_lmcl, df_long_date, top_n = 30)

# -1. Долар / Євро | ΔAIC = 4.00 | p = 0.0166 
inter_lm <- update(lm2, . ~ . + I(Долар / Євро))
summary(inter_lm, vcov = "DK")

inter_lm <- update(inter_lm, . ~ . - log(Долар))
summary(inter_lm, vcov = "DK")

inter_lm <- update(inter_lm, . ~ . - Євро)
summary(inter_lm, vcov = "DK")

aic_base - AIC(inter_lm) # -3.708628
bic_base - BIC(inter_lm) # -0.3333497
# Слабко.


#### lm3 ####
summary(lm3, vcov = "DK")
aic_base <- AIC(lm3)
bic_base <- BIC(lm3)

lm3_lmcl <- lm(formula(lm3), data = df_long)
interactions_result <- observe_best_interactions(lm3_lmcl, df_long_date, top_n = 30)


# 1. ЧисНасел × Кімнатність | ΔAIC = 14.95 | p = 0.0001 
inter_lm <- update(lm3, . ~ . + ЧисНасел:Кімнатність)
summary(inter_lm, vcov = "DK")

aic_base - AIC(inter_lm) # 14.95298
bic_base - BIC(inter_lm) # 8.202421

inter_lm7 <- inter_lm


# 2. ЧисНасел / РівДолар | ΔAIC = 7.21 | p = 0.0028 
inter_lm <- update(lm3, . ~ . + I(ЧисНасел / РівДолар))
summary(inter_lm, vcov = "DK")

inter_lm <- update(inter_lm, . ~ . - РівДолар)
summary(inter_lm, vcov = "DK")

aic_base - AIC(inter_lm) # 6.706733
bic_base - BIC(inter_lm) # 6.706733
# Трохи краще за початкову модель, але і складніше -> відкинемо.

final_models[["4ЧисНасел × Кімнатність"]] <- inter_lm7



# ======== 8) ВАЛІДАЦІЯ МОДЕЛІ ========
final_models_lm <- lapply(final_models, function(m) lm(clean_formula(m), data = df_long))

suppressWarnings(
  models_validation(final_models_lm, data = df_long, kfold_number = 5,
                    horizon = 1, init_window = 48)
)



# ======== 9) ВИБІР НАЙКРАЩОЇ МОДЕЛІ ========
length(final_models_lm)

residuals_plot(final_models_lm[[1]]) # Задовільно.
residuals_regressors_plot(final_models_lm[[1]], c(3, 4)) # К3

residuals_plot(final_models_lm[[2]]) 
residuals_regressors_plot(final_models_lm[[2]], c(3, 4)) # +
# +

residuals_plot(final_models_lm[[3]]) # Краще.
residuals_regressors_plot(final_models_lm[[3]], c(3, 4)) # Незначна недооцінка для К2.
# Хоч тут все непогано, але навіщо нам ця модель, як є така ж із взаємодією (краща)?

residuals_plot(final_models_lm[[4]]) # Майже те саме, що і для 3.
residuals_regressors_plot(final_models_lm[[4]], c(3, 4)) # Малесенька переоцінка К1.
# +

residuals_plot(final_models_lm[[5]]) 
residuals_regressors_plot(final_models_lm[[5]], c(3, 3)) # Незначна К3.
# +

residuals_plot(final_models_lm[[6]]) 
residuals_regressors_plot(final_models_lm[[6]], c(3, 4)) # +
# +

residuals_plot(final_models_lm[[7]])
residuals_regressors_plot(final_models_lm[[7]], c(3, 3)) # +
# +

residuals_plot(final_models_lm[[8]]) 
residuals_regressors_plot(final_models_lm[[8]], c(3, 3)) # +
# +

residuals_plot(final_models_lm[[9]]) 
residuals_regressors_plot(final_models_lm[[9]], c(3, 3)) # К1, К3.

residuals_plot(final_models_lm[[10]])
residuals_regressors_plot(final_models_lm[[10]], c(2, 2)) # +
# +


lm_to_feols_summary(final_models_lm[[2]], df_long_date) #- Протилежні знаки.
lm_to_feols_summary(final_models_lm[[4]], df_long_date) # Логічніше, вищий R2.
lm_to_feols_summary(final_models_lm[[5]], df_long_date) # Те саме, але містить Євро замість ЧисНасел.
summary(final_models_lm[[10]])
AIC(final_models_lm[[5]])
BIC(final_models_lm[[5]])
lm_to_feols_summary(final_models_lm[[6]], df_long_date) #- Менш логічно.
lm_to_feols_summary(final_models_lm[[7]], df_long_date) #-
lm_to_feols_summary(final_models_lm[[8]], df_long_date) #- Частка індекса та зв. змінної?!
lm_to_feols_summary(final_models_lm[[10]], df_long_date) # Нижчий R2, але модель цікава.
AIC(final_models_lm[[10]])
BIC(final_models_lm[[10]])

best_models <- final_models_lm[c(5, 10)]
# Із трьох (була ще 4) обраних моделей у плані діаграм розкиду залишків найкращою є 5.

models_summary(best_models)
models_validation(best_models, df_long, horizon = 1, kfold_number = 5,
                  init_window = 48)

