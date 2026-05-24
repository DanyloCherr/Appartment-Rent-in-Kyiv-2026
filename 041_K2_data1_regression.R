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
# ==== 5.1.1) ЛІНІЙНІСТЬ, ГОМОСКЕДАСТИЧНІСТЬ ====
# ЛІНІЙНІСТЬ
plot(model_log2_75, which = 1) # Те саме?
residuals_plot(model_log2_75) # Трохи рівномірніше.

residuals_regressors_plot(model_log2_75, c(2, 4), type = "rstudent")
# Можливо, трохи краще.

av_plots(model_log2_75)

# Трошечки краще, але основні проблеми з ІФС, ІГР залишаються.



# ГОМОСКЕДАСТИЧНІСТЬ
plot(model_log1_75, which = 3)
# Те саме?


bptest(model_log2_75)
# Тест Бройша-Паґана: p-value = 0.07515 - гіпотезу про гомоскедастичність не відхиляємо.


# ЛІНІЙНІСТЬ.
resettest(model_log1_75, power = 2) # p-value = 0.214
resettest(model_log1_75, power = 3) # p-value = 5.08e-05
# Нормально. Кубічний член все одно поки не додаватимемо.


crPlots(model_log1_75)
# Трошки краще (але не значно)

# Модифікуємо функцію так, щоб вона не лише заміняла змінні на трансформовані, 
# а і був варіант, де до моделі додається нова змінна, а стара при цьому залишається.
observe_transforms_x(model_log1_75, regressors = names(coef(model_log1_75))[-1])
# Можна було б застосувати BoxTidwell, якби ми вручну не прописали функцію, яка 
# підбирає перетворення.
# До долара варто застосувати одне з перетворень: +sqrt, +square, +inverse, +log
# Кожне з них однаково покращує модель, тому поки що залишимо дві моделі - з квадратом і логарифмом
model_log1_75_Sq <- lm(log(Ціна) ~ . + I(Долар^2), data = model1_75_df) 
model_log1_75_Lg <- lm(log(Ціна) ~ . + log(Долар), data = model1_75_df) 



# ======== НОВА МОДЕЛЬ 2 ========

# ==== 5.2.1) ПОВНА МОДЕЛЬ ====
summary(model_log1_75_Sq)
summary(model_log1_75_Lg)
# Всі значення майже однакові. 
# pval(ІФС) = 0.81139. Вільний член тепер додатний.


# ==== 5.2.2) МУЛЬТИКОЛІНЕАРНІСТЬ-2. VIF, ЧИСЛО ОБУМОВЛЕНОСТІ ====
model_data1 <- model_log1_75_Sq$model
colnames(model_data1)[1] <- "Log_Ціна"
model_log1_75_Sq_df_norm <- as.data.frame(lapply(model_data1, unit_length_scale))
model_log1_75_Sq_norm <- lm(Log_Ціна ~ ., data = model_data1)

model_data2 <- model_log1_75_Lg$model
colnames(model_data2)[1] <- "Log_Ціна"
model_log1_75_Lg_df_norm <- as.data.frame(lapply(model_data2, unit_length_scale))
model_log1_75_Lg_norm <- lm(Log_Ціна ~ ., data = model_data2)

vif(model_log1_75_Sq_norm)
vif(model_log1_75_Lg_norm)
# Євро: 6.1 -> 6.4. Решта, крім Долара - майже те саме.

condition_number(model_log1_75_Sq_df_norm, 0)
condition_number(model_log1_75_Lg_df_norm, 0)
# Ну, мабуть, це нормально :0


# ==== 5.2.3) АНАЛІЗ ЗАЛИШКІВ І ВИЯВЛЕННЯ ВИКИДІВ ====
residuals_plot(model_log1_75_Sq)
residuals_plot(model_log1_75_Lg)
# Трохи рівномірніше.

residuals_regressors_plot(model_log1_75_Sq, c(2, 4), type = "rstudent")
residuals_regressors_plot(model_log1_75_Lg, c(2, 4), type = "rstudent")
# Долар^2 виглядає трохи краще, ніж log(Долар)
# Трохи рівномірніше.

# Відстань Кука.
cooks_dist(model_log1_75_Sq)
cooks_dist(model_log1_75_Lg)

df_fits(model_log1_75_Sq)
df_fits(model_log1_75_Lg)

df_betas(model_log1_75_Sq)
df_betas(model_log1_75_Lg)
# Впливових точок тепер на одну менше.

influential_points <- model_log1_75_Sq$model[c(13, 33, 48, 72), ]
influential_points


# ==== 5.2.4) ЛІНІЙНІСТЬ, ГОМОСКЕДАСТИЧНІСТЬ, НОРМАЛЬНІСТЬ ====

# ЛІНІЙНІСТЬ
plot(model_log1_75_Sq, which = 1)
plot(model_log1_75_Lg, which = 1)
# Те саме?


av_plots(model_log1_75_Sq)
av_plots(model_log1_75_Lg)
# ІФС, ЧисНасел виглядають трохи краще. Проблема з ІГР та ж.


# ГОМОСКЕДАСТИЧНІСТЬ
plot(model_log1_75_Sq, which = 3)
plot(model_log1_75_Lg, which = 3)
# Краще!


bptest(model_log1_75_Sq) # p-value = 0.2087
bptest(model_log1_75_Lg) # p-value = 0.2024
# Було p-value = 0.07515.


# ЛІНІЙНІСТЬ.
resettest(model_log1_75_Sq, power = 2) # p-value = 0.3665
resettest(model_log1_75_Sq, power = 3) # p-value = 7.923e-06
resettest(model_log1_75_Lg, power = 2) # p-value = 0.3707
resettest(model_log1_75_Lg, power = 3) # p-value = 8.167e-06


crPlots(model_log1_75_Sq)
crPlots(model_log1_75_Lg)
# Євро, Долар - трохи краще. Долар, нова змінна виглядають ідеально.


observe_transforms_x(model_log1_75_Sq, regressors = names(coef(model_log1_75_Sq))[-1])
# +square(ІГР) = 2.238090e+00 - найкращий показник. Проте для збереження простоти моделі,
# залишимо як є.
observe_transforms_x(model_log1_75_Lg, regressors = names(coef(model_log1_75_Lg))[-1])
# Аналогічно.


# НОРМАЛЬНІСТЬ
qq_plot(model_full1_75) # p-value = 0.4237
shapiro.test(residuals(model_full1_75))

qq_plot(model_log1_75) # З логарифмом виглядає трохи краще.
shapiro.test(residuals(model_log1_75)) # p-value = 0.894

qq_plot(model_log1_75_Sq) # Трохи гірше, ніж було.
shapiro.test(residuals(model_log1_75_Sq)) # p-value = 0.6961
qq_plot(model_log1_75_Lg) # Log(Долар) на крапельку кращий за Долар^2.
shapiro.test(residuals(model_log1_75_Lg)) # p-value = 0.7184

# Отже, між логарифмом і квадратом значимої різниці немає, тому візьмемо квадрат.


# ==== 6) АВТОКОРЕЛЯЦІЯ ====
time_series_residuals(model_full1_75)
dwtest(model_full1_75) # DW = 0.67066, p-value = 1.14e-13. Погано.

time_series_residuals(model_log1_75)
dwtest(model_log1_75) # DW = 0.72937, p-value = 1.921e-12. Все ще погано.

time_series_residuals(model_log1_75_Sq)
dwtest(model_log1_75_Sq) # DW = 0.80641, p-value = 3.258e-11

time_series_residuals(model_log1_75_Lg)
dwtest(model_log1_75_Lg) # DW = 0.79976, p-value = 2.511e-11

# Як можна виправити автокореляцію: 
# - додати упущену змінну;
# - зважені або узагальнені найменші квадрати;
# - МЕТОДИ