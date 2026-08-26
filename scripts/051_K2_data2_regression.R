# ----- 46 СПОСТЕРЕЖЕНЬ -----
# ==== 1) МУЛЬТИКОЛІНЕАРНІСТЬ-1. КОРЕЛЯЦІЙНА МАТРИЦЯ ====
model2_df <- upd_model_df(model_df_2, remove = "Ч", room_num = 2)
colnames(model2_df)[colnames(model2_df) == "К2"] <- "Ціна"
model_cor_matrix(model2_df, "")

# Перевіримо значущість ЧистГрнКред і євро.
model_LOANS <- lm(Ціна ~ ЧистГрнКред, data = model2_df)
model_EUR <- lm(Ціна ~ Євро, data = model2_df)
models_summary(list(model_LOANS, model_EUR))

model2_df$`Євро_Долар` <- model2_df$Євро / model2_df$Долар
model2_df <- model2_df[!names(model2_df) %in% c("ІЦЖВ")]
model_cor_matrix(model2_df, "")
head(model2_df)



# ==== 2) ПОВНА МОДЕЛЬ ====
model_full2_2 <- lm(Ціна ~ . - Євро - Долар, data = model2_df)
summary(model_full2_2)



# ==== 3) АНАЛІЗ ЗАЛИШКІВ І ВИЯВЛЕННЯ ВИКИДІВ ====
residuals_plot(model_full2_2)

residuals_regressors_plot(model_full2_2, c(2, 4), type = "rstudent")

cooks_dist(model_full2_2)

df_fits(model_full2_2)

df_betas(model_full2_2)

# Спостереження 3 не пройшли жодного з тестів.



# ==== 4) ФУНКЦІОНАЛЬНІ ПЕРЕТВОРЕННЯ ====
# ЛІНІЙНІСТЬ І ГОМОСКЕДАСТИЧНІСТЬ
residuals_plot(model_full2_2)
plot(model_full2_2, which = 1)
plot(model_full2_2, which = 3) # Дисперсія повільно зростає.

residuals_regressors_plot(model_full2_2, c(2, 4), type = "rstudent")
# ІЦБ - воронка (слабка).

av_plots(model_full2_2) # ІЦЖП - під питанням.

crPlots(model_full2_2) # Євро_Долар - U-shape.

resettest(model_full2_2, power = 2) # p-value = 0.3153

bptest(model_full2_2) # p-value = 0.146

bc <- boxcox(model_full2_2, lambda = seq(-2, 2, by = 0.1))
lambda_opt <- bc$x[which.max(bc$y)]
cat("Оптимальне λ:", round(lambda_opt, 2), "\n") # -0.46 

model_log2_2 <- lm(log(Ціна) ~ . - Євро - Долар, data = model2_df)
summary(model_log2_2) # ІндМатСтан тепер значуща.



# ======== НОВА МОДЕЛЬ 1 ========
# ЛІНІЙНІСТЬ І ГОМОСКЕДАСТИЧНІСТЬ
residuals_plot(model_log2_2) # +
plot(model_log2_2, which = 1)
plot(model_log2_2, which = 3) # Дисперсія повільно зростає (уже повільніше).

residuals_regressors_plot(model_log2_2, c(2, 4), type = "rstudent")
# Слабкі воронки розтягнулись.

av_plots(model_log2_2) 

crPlots(model_log2_2) # +

resettest(model_log2_2, power = 2) # p-value = 0.01641

bptest(model_log2_2) # p-value = 0.3052

observe_transforms_x(model_log2_2, regressors = names(coef(model_log2_2))[-1])



# ==== 5) НОРМАЛЬНІСТЬ ====
qq_plot(model_log2_2)
shapiro.test(residuals(model_log2_2)) # p-value = 0.8645
ad.test(residuals(model_log2_2)) # p-value = 0.8704



# ==== 6) АВТОКОРЕЛЯЦІЯ ====
time_series_residuals(model_log2_2)
dwtest(model_log2_2) # DW = 0.68426, p-value = 5.501e-10
summary(model_log2_2)
coeftest(model_log2_2, vcov = vcovHAC(model_log2_2))


# ==== 7) ВІДБІР ЗМІННИХ ====
all_models <- regsubsets(log(Ціна) ~ . - Євро - Долар, data = model2_df, nbest = 3, nvmax = 11)
best_models <- best_models_summary(all_models, 12)


# 1.3.11 ІФС + ІндМатСтан + РівДолар + ЧистГрнКред  | Adj R² = 0.8715 (p = 5)
lm1 <- lm(log(Ціна) ~ ІФС + ІндМатСтан + РівДолар + ЧистГрнКред, data = model2_df)
summary(lm1)
coeftest(lm1, vcov = vcovHAC(lm1)) # pval(РівДолар) = 0.2507967    
# Поки що залишимо. Можливо, незначущість виправиться при додаванні взаємодій.


# 2. ІФС + ІЦЖП + ІндМатСтан + РівДолар + ЧистГрнКред + Євро_Долар  | Adj R² = 0.8709 (p = 7)
lm2 <- lm(log(Ціна) ~ ІФС + ІЦЖП + ІндМатСтан + РівДолар + ЧистГрнКред + Євро_Долар, data = model2_df)
summary(lm2)
coeftest(lm2, vcov = vcovHAC(lm2))

lm2 <- update(lm2, . ~ . - Євро_Долар)
summary(lm2)
coeftest(lm2, vcov = vcovHAC(lm2))

lm2 <- update(lm2, . ~ . - ІЦЖП)
summary(lm2)
coeftest(lm2, vcov = vcovHAC(lm2))

lm2 <- update(lm2, . ~ . - РівДолар)
summary(lm2)
coeftest(lm2, vcov = vcovHAC(lm2))
# Це модель 6.2.?

# -8. ІФС + ІндМатСтан + ЧистГрнКред + Євро_Долар  | Adj R² = 0.8686 (p = 5)
lm3 <- lm(log(Ціна) ~ ІФС + ІндМатСтан + ЧистГрнКред + Євро_Долар, data = model2_df)
summary(lm3)
coeftest(lm3, vcov = vcovHAC(lm3)) # p-value(Євро_Долар) = 0.490556
# Можливо, виправиться при додаванні взаємодій.

final_models2_2 <- list(lm1, lm2, lm3)



# ======== 8) ЕФЕКТИ ВЗАЄМОДІЇ ========
# Працюємо із двома моделями.
lm1 <- final_models2_2[[1]]
lm2 <- final_models2_2[[2]]
lm3 <- final_models2_2[[3]]
models_summary(list(lm1, lm2, lm3), model_full = model_log2_2)


#### lm1 ####
summary(lm1)
aic_base <- AIC(lm1)
bic_base <- BIC(lm1)

interactions_result <- observe_best_interactions(lm1, model2_df, top_n = 10)

# 1. ІндМатСтан × ЧистГрнКред | ΔAIC = 9.71 | p = 0.0015 
inter_lm <- update(lm1, . ~ . + ІндМатСтан:ЧистГрнКред)
summary(inter_lm)
coeftest(inter_lm, vcov = vcovHAC(inter_lm)) 

aic_base - AIC(inter_lm) # 9.708297
bic_base - BIC(inter_lm) # 7.879655
# РівДолар і досі незначуща.


# 2. РівДолар × ЧистГрнКред | ΔAIC = 9.41 | p = 0.0017
inter_lm <- update(lm1, . ~ . + РівДолар:ЧистГрнКред)
summary(inter_lm)
coeftest(inter_lm, vcov = vcovHAC(inter_lm)) 

aic_base - AIC(inter_lm) # 9.408173
bic_base - BIC(inter_lm) # 7.579531
inter_lm1 <- inter_lm

assumptions_check(inter_lm1)


#### lm2 ####
summary(lm2)
aic_base <- AIC(lm2)
bic_base <- BIC(lm2)

interactions_result <- observe_best_interactions(lm2, model2_df, top_n = 10)
# 1. ІндМатСтан × ЧистГрнКред | ΔAIC = 8.66 | p = 0.0022 
inter_lm <- update(lm2, . ~ . + ІндМатСтан:ЧистГрнКред)
summary(inter_lm)
coeftest(inter_lm, vcov = vcovHAC(inter_lm)) 
# ІФС - не значуща.

inter_lm <- update(inter_lm, . ~ . - ІФС)
summary(inter_lm)
coeftest(inter_lm, vcov = vcovHAC(inter_lm)) 
inter_lm2 <- inter_lm

assumptions_check(inter_lm2)


#### lm3 ####
summary(lm3)
aic_base <- AIC(lm3)
bic_base <- BIC(lm3)

interactions_result <- observe_best_interactions(lm3, model2_df, top_n = 10)
# 1. ІндМатСтан × ЧистГрнКред | ΔAIC = 7.91 | p = 0.0035 
inter_lm <- update(lm3, . ~ . + ІндМатСтан:ЧистГрнКред)
summary(inter_lm)
coeftest(inter_lm, vcov = vcovHAC(inter_lm)) 
# Незначущість Євро_Долар не виправилась.


final_models2_2 <- append(final_models2_2, list(inter_lm1, inter_lm2))



# ======== 9) ВАЛІДАЦІЯ МОДЕЛІ ========
suppressWarnings(
  models_validation(final_models2_2, data = model2_df, kfold_number = 5,
                    horizon = 1, init_window = 30)
)


# ======== 10) ВИБІР НАЙКРАЩОЇ МОДЕЛІ ========
# best_models1_2 <- list(inter_lm1)
len <- length(final_models2_2)
best_models2_2 <- list(final_models2_2[[2]], final_models2_2[[len - 1]], final_models2_2[[len]])
best_model2_2 <- list(final_models2_2[[len]])
models_summary(best_model2_2)
suppressWarnings(
  models_validation(best_model2_2, data = model2_df, kfold_number = 5,
                    horizon = 1, init_window = 30)
)
# Модель log(Ціна) ~ ІндМатСтан + ЧистГрнКред + ІндМатСтан:ЧистГрнКред
# найкраща.

compute_t_critical(best_model2_2[[1]])

# # Якби ми взяли модель для однокімнатних квартир...
# model_SGLROOM <- best_models1_2[[2]]
# model_DBLROOM <- lm(formula(model_SGLROOM), data = model2_df)
# models_summary(list(model_DBLROOM)) 
# 
# model_DBLROOM <- update(model_DBLROOM, . ~ . - Євро_Долар)
# models_summary(list(model_DBLROOM)) 
# # Гірше!