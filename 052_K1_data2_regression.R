# ------ 22 спостереження ------
# ==== 1) МУЛЬТИКОЛІНЕАРНІСТЬ-1. КОРЕЛЯЦІЙНА МАТРИЦЯ ====
model1_df_22 <- upd_model_df(model_df_2_22, remove = "Ч", room_num = 1)
colnames(model1_df_22)[colnames(model1_df_22) == "К1"] <- "Ціна"
model_cor_matrix(model1_df_22, "")

model1_df_22$`Євро_Долар` <- model1_df_22$Євро / model1_df_22$Долар
model1_df_22 <- model1_df_22[!names(model1_df_22) %in% c("ІЦЖВ")]
model1_df_22 <- model1_df_22[!names(model1_df_22) %in% c("Євро", "Долар")]
model_cor_matrix(model1_df_22, "")
# Вилучимо також ІФС через сильну кореляцію з двома предикторами (UPD: вилучаємо ІГР замість ІФС).

model1_df_22 <- model1_df_22[!names(model1_df_22) %in% c("ІГР")]

model1_df_22$`ІЦБ_ІЦЖП` <- model1_df_22$ІЦБ / model1_df_22$ІЦЖП
model1_df_22 <- model1_df_22[!names(model1_df_22) %in% c("ІЦБ", "ІЦЖП")]
model_cor_matrix(model1_df_22, "")
head(model1_df_22, 3)



# ==== 2) ПОВНА МОДЕЛЬ ====
# Дослідимо вплив нової змінної на ціну.
model_ANGROWTH <- lm(Ціна ~ ТемпЗмінКошт, data = model1_df_22)
summary(model_ANGROWTH) # не слабко.


model_full1_2_22_22 <- lm(Ціна ~ ., data = model1_df_22)
summary(model_full1_2_22_22)



# ==== 3) МУЛЬТИКОЛІНЕАРНІСТЬ-2. VIF, ЧИСЛО ОБУМОВЛЕНОСТІ ====
model1_df_norm_22 <- as.data.frame(lapply(model1_df_22, unit_length_scale))

model_full1_2_22_norm_22 <- lm(Ціна ~ ., data = model1_df_norm_22)
vif(model_full1_2_22_norm_22)
# ІГР          ІЦБ         ІЦЖП   ІндМатСтан     РівДолар  ЧистГрнКред ТемпЗмінКошт   Євро_Долар 
# 16.041892    44.885223    14.937555     4.232386     9.600000    23.824670     4.229245     2.467792 
# Погано. Повернімося назад і розглянемо відношення ІЦБ\ІЦЖП.
# Тоді замість ІФС вилучимо ІГР.

# UPD:
# ІФС   ІндМатСтан     РівДолар  ЧистГрнКред ТемпЗмінКошт   Євро_Долар     ІЦБ_ІЦЖП 
# 4.158201     2.789256    10.434731     8.014460     3.943294     2.373254     4.798544 

condition_number(model1_df_norm_22, 0) # 68.14573



# ==== 3) АНАЛІЗ ЗАЛИШКІВ І ВИЯВЛЕННЯ ВИКИДІВ ====
residuals_plot(model_full1_2_22_22)

residuals_regressors_plot(model_full1_2_22_22, c(2, 4), type = "rstudent")

cooks_dist(model_full1_2_22_22)

df_fits(model_full1_2_22_22)

df_betas(model_full1_2_22_22)

# Спостереження 1 7 не пройшли жодного з тестів.



# ==== 4) ФУНКЦІОНАЛЬНІ ПЕРЕТВОРЕННЯ ====
# ЛІНІЙНІСТЬ І ГОМОСКЕДАСТИЧНІСТЬ
plot(model_full1_2_22, which = 1) # Непогано.
residuals_plot(model_full1_2_22)
plot(model_full1_2_22, which = 3)

residuals_regressors_plot(model_full1_2_22, c(2, 4), type = "rstudent")

av_plots(model_full1_2_22) # ІЦЖП - під питанням.

crPlots(model_full1_2_22) # Євро_Долар - U-shape.

resettest(model_full1_2_22, power = 2) # p-value = 0.2821

bptest(model_full1_2_22) # p-value = 0.4419

bc <- boxcox(model_full1_2_22, lambda = seq(-2, 2, by = 0.1))
lambda_opt <- bc$x[which.max(bc$y)]
cat("Оптимальне λ:", round(lambda_opt, 2), "\n") # 0.75 

observe_transforms_y(model_full1_2_22)
observe_transforms_x(model_full1_2_22, regressors = names(coef(model_full1_2_22))[-1])



# ==== 5) НОРМАЛЬНІСТЬ ====
qq_plot(model_full1_2_22)
shapiro.test(residuals(model_full1_2_22)) # p-value = 0.5828
ad.test(residuals(model_full1_2_22)) # p-value = 0.3265



# ==== 6) АВТОКОРЕЛЯЦІЯ ====
time_series_residuals(model_full1_2_22)
dwtest(model_full1_2_22) # DW = 1.6763, p-value = 0.03616
summary(model_full1_2_22)
coeftest(model_full1_2_22, vcov = vcovHAC(model_full1_2_22))



# ==== 7) ВІДБІР ЗМІННИХ ====
all_models <- regsubsets(Ціна ~ ., data = model1_df_22, nbest = 3, nvmax = 11)
best_models <- best_models_summary(all_models, 12)


# Найкраща модель із ТемпЗмінКошт є гіршою за ту модель по повній вибірці.
# Отже, ТемпЗмінКошт не включаємо.