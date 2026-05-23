# ----- 38 спостережень -----
# ==== 1) МУЛЬТИКОЛІНЕАРНІСТЬ-1. КОРЕЛЯЦІЙНА МАТРИЦЯ ====
model_cor_matrix(model_df_38[, -1], "")
# cor(Долар, Євро) = 0.94. Вилучимо Долар.

model1_38_df <- upd_model_df(model_df_38, "Долар")

model_cor_matrix(model1_38_df, "")
colnames(model1_38_df)[colnames(model1_38_df) == "К1"] <- "Ціна"
head(model1_38_df)
# 8 предикторів


# ==== 2) ПОВНА МОДЕЛЬ ====
model_full1_38 <- lm(Ціна ~ ., data = model1_38_df)
summary(model_full1_38)
# Багато статистично незначущих змінних.


# ==== 3) МУЛЬТИКОЛІНЕАРНІСТЬ-2. VIF, ЧИСЛО ОБУМОВЛЕНОСТІ ====
compute_vifs(model_full1_38)
# Ч, ЧистГрнКред - критично. Приберемо Ч.

model_full1_38 <- update(model_full1_38, . ~ . - Ч)
summary(model_full1_38) # Великі pval.
compute_vifs(model_full1_38)
# Краще. Але показники для ІЦБ, РівДолар і ЧистГрнКред > 6.5.

# Була спроба повернути Долар у модель. Але тоді VIF Долара та Євро > 15.

model1_38_df_norm <- as.data.frame(lapply(model1_38_df, unit_length_scale))
condition_number(model1_38_df_norm, 0) # 113.0342
# Поки що залишимо як є.



# ==== 4) АНАЛІЗ ЗАЛИШКІВ І ВИЯВЛЕННЯ ВИКИДІВ ====
residuals_plot(model_full1_38)

residuals_regressors_plot(model_full1_38, c(2, 4), type = "rstudent")

# Відстань Кука.
cooks_dist(model_full1_38)

df_fits(model_full1_38)

df_betas(model_full1_38)

# Спостереження 34, 36, 37 не пройшли жодного з тестів.
model1_38_df_date <- model_df_38[, !names(model_df_38) %in% c("К2", "К3", "Ч")]
influential_points <- model1_38_df_date[c(34, 36, 37), ]
influential_points
plot_model_df(data1, model1_38_df_date, influential_points)
# 37 - через ІГР.
# 34, 36, 37 - РівДолар впав.
# 36, 37 - ЧистГрнКред стрімко зростають під кінець 2022 р.
# Дослідимо вплив цих точок пізніше - для кінцевої моделі.



# ==== 5) ФУНКЦІОНАЛЬНІ ПЕРЕТВОРЕННЯ ====
# ЛІНІЙНІСТЬ
plot(model_full1_38, which = 1) # Не дуже добре.

residuals_regressors_plot(model_full1_38, c(2, 4), type = "rstudent")
# Трендів не видно.

av_plots(model_full1_38)
# Непогано, проте для ІЦБ пряма має малий нахил.

resettest(model_full1_38, power = 2) # p-value = 0.07549
# Межа. Можливо, квадратичний член додати варто.


crPlots(model_full1_38)
# LOESS є чутливим до малого розміру вибірки, тому її хвилястість є природною.


# ГОМОСКЕДАСТИЧНІСТЬ
plot(model_full1_38, which = 3)
residuals_regressors_plot(model_full1_38, c(2, 4), type = "rstudent")
# Дисперсія залишків збільшується. 
# Це характерно для ІФС?, ІГР?, ІЦБ, ЧистГрнКред

bptest(model_full1_38) # p-value = 0.3173
# Але графіки показують інше. 


# ФУНКЦІОНАЛЬНІ ПЕРЕТВОРЕННЯ
observe_transforms_y(model_full1_38)

bc <- boxcox(model_full1_38, lambda = seq(-6, 2, by = 0.1))

lambda_opt <- bc$x[which.max(bc$y)]
cat("Оптимальне λ:", round(lambda_opt, 2), "\n")
# 1 потрапляє в ДІ, тому перетворення не застосовуємо.

observe_transforms_x(model_full1_38, regressors = names(coef(model_full1_38))[-1])
# Найбільша різниця - при перетворення ІФС.

model_full1_38_Lg <- update(model_full1_38, . ~ . +log(ІФС))


# ======== НОВА МОДЕЛЬ 1 ========
# ==== 5.1.1) МУЛЬТИКОЛІНЕАРНІСТЬ-2. VIF, ЧИСЛО ОБУМОВЛЕНОСТІ ====
summary(model_full1_38_Lg)
# Покращилась статистична значущість і значно зріс R2.

compute_vifs(model_full1_38_Lg)
# Краще. Але показники для ІЦБ, РівДолар і ЧистГрнКред > 7.5.
# Можливо, з мультиколінеарністю тепер трохи гірше.


# ==== 5.1.2) АНАЛІЗ ЗАЛИШКІВ І ВИЯВЛЕННЯ ВИКИДІВ ====
# Зробимо для фінальної моделі.


# ==== 5.1.3) ФУНКЦІОНАЛЬНІ ПЕРЕТВОРЕННЯ ====
# ЛІНІЙНІСТЬ
plot(model_full1_38_Lg, which = 1) # Не дуже добре.

residuals_regressors_plot(model_full1_38_Lg, c(2, 4), type = "rstudent")

av_plots(model_full1_38_Lg)
# Тепер для ІЦБ нахил менш горизонтальний.

resettest(model_full1_38_Lg, power = 2) # p-value = 0.0006964
# Потрібен квадратичний член! Повернімося до цього у відповідному підрозділі.


# ГОМОСКЕДАСТИЧНІСТЬ
plot(model_full1_38_Lg, which = 3)
residuals_regressors_plot(model_full1_38_Lg, c(2, 4), type = "rstudent")
# Дисперсія залишків збільшується. 
# Це характерно для ІФС?, ІГР?, ІЦБ, ЧистГрнКред і log(ІФС)

bptest(model_full1_38_Lg) # p-value = 0.07392


# ЦЯ МОДЕЛЬ Є ГІРШОЮ ЗА ПОПЕРЕДНЮ. log(ІФС) лише вносить більшу гетероскедастичність.
# Повернімося до попередньої моделі та оберімо ФП на основі графіків.


# ФУНКЦІОНАЛЬНІ ПЕРЕТВОРЕННЯ
residuals_regressors_plot(model_full1_38, c(2, 4), type = "rstudent")
# Найсильніші порушення спостерігаються для ІЦБ і ЧисГрнКред. Оберемо для них ФП,
# яке найбільше покращує AIC.

observe_transforms_x(model_full1_38, regressors = names(coef(model_full1_38))[-1])
# square(ЧистГрнКред) -> 1.0413972439 

model_full1_38_Sq <- update(model_full1_38, . ~ . - ЧистГрнКред + I(ЧистГрнКред^2))


# ======== НОВА МОДЕЛЬ 2 ========
# ==== 5.2.1) МУЛЬТИКОЛІНЕАРНІСТЬ-2. VIF, ЧИСЛО ОБУМОВЛЕНОСТІ ====
summary(model_full1_38_Sq)

compute_vifs(model_full1_38_Sq)
# Показники для ІЦБ, РівДолар і ЧистГрнКред > 6.


# ==== 5.2.2) АНАЛІЗ ЗАЛИШКІВ І ВИЯВЛЕННЯ ВИКИДІВ ====
# Зробимо для фінальної моделі.


# ==== 5.2.3) ФУНКЦІОНАЛЬНІ ПЕРЕТВОРЕННЯ ====
# ЛІНІЙНІСТЬ
plot(model_full1_38_Sq, which = 1) # Не дуже добре, але на крапельку краще.

residuals_regressors_plot(model_full1_38_Sq, c(2, 4), type = "rstudent")

av_plots(model_full1_38_Sq)
# Тепер для ІЦБ нахил менш горизонтальний.
# РівДолар виглядає не дуже.

resettest(model_full1_38_Sq, power = 2) # p-value = 0.1237

# ГОМОСКЕДАСТИЧНІСТЬ
plot(model_full1_38_Sq, which = 3) # На крапельку краще.
residuals_regressors_plot(model_full1_38_Sq, c(2, 4), type = "rstudent")

bptest(model_full1_38_Sq) # p-value = 0.3965

# Не значно краще. Спробуємо ФП для відгука.


# ФУНКЦІОНАЛЬНІ ПЕРЕТВОРЕННЯ
observe_transforms_y(model_full1_38)
residuals_plot(model_full1_38)
residuals_regressors_plot(model_full1_38, c(2, 4), type = "rstudent")

model_log_38 <- lm(log(Ціна) ~ . - Ч, data = model1_38_df)
residuals_plot(model_log_38)
residuals_regressors_plot(model_log_38, c(2, 4), type = "rstudent")

model_Sq_38 <- lm(I(Ціна^2) ~ . - Ч, data = model1_38_df)
residuals_plot(model_Sq_38)
residuals_regressors_plot(model_Sq_38, c(2, 4), type = "rstudent")
# Значно краще не стало.

# square(ЧистГрнКред) -> 1.0413972439 

model_cor_matrix(model_df_38[, -1], "")
# ---- ВИСНОВОК ----
# Нова є змінна сильно корелює одночасно із РівДолар і Ч, тому забудемо за неї.