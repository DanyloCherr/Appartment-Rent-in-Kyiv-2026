# ----- 46 СПОСТЕРЕЖЕНЬ -----
# ==== 1) МУЛЬТИКОЛІНЕАРНІСТЬ-1. КОРЕЛЯЦІЙНА МАТРИЦЯ ====
model3_df <- upd_model_df(model_df_2, remove = "Ч", room_num = 3)
colnames(model3_df)[colnames(model3_df) == "К3"] <- "Ціна"
model_cor_matrix(model3_df, "")

model3_df$`Євро_Долар` <- model3_df$Євро / model3_df$Долар
model3_df <- model3_df[!names(model3_df) %in% c("ІЦЖВ")]
model_cor_matrix(model3_df, "")
head(model3_df)


# ==== 2) ПОВНА МОДЕЛЬ ====
model_full3_2 <- lm(Ціна ~ . - Євро - Долар, data = model3_df)
summary(model_full3_2)



# ==== 3) АНАЛІЗ ЗАЛИШКІВ І ВИЯВЛЕННЯ ВИКИДІВ ====
residuals_plot(model_full3_2)

residuals_regressors_plot(model_full3_2, c(2, 4), type = "rstudent")

cooks_dist(model_full3_2)

df_fits(model_full3_2)

df_betas(model_full3_2)

# Спостереження 1 3 46 не пройшли жодного з тестів.



# ==== 4) ФУНКЦІОНАЛЬНІ ПЕРЕТВОРЕННЯ ====
# ЛІНІЙНІСТЬ І ГОМОСКЕДАСТИЧНІСТЬ
residuals_plot(model_full3_2)
plot(model_full3_2, which = 1)
plot(model_full3_2, which = 3) # Дисперсія повільно зростає.

residuals_regressors_plot(model_full3_2, c(2, 4), type = "rstudent")

av_plots(model_full3_2) # ІЦЖП - під питанням.

crPlots(model_full3_2)

resettest(model_full3_2, power = 2) # p-value = 0.7295

bptest(model_full3_2) # p-value = 0.2429

bc <- boxcox(model_full3_2, lambda = seq(-2, 2, by = 0.1))
lambda_opt <- bc$x[which.max(bc$y)]
cat("Оптимальне λ:", round(lambda_opt, 2), "\n") # 0.02

model_log3_2 <- lm(log(Ціна) ~ . - Євро - Долар, data = model3_df)
summary(model_log3_2) # + значущість.



# ======== НОВА МОДЕЛЬ 1 ========
# ЛІНІЙНІСТЬ І ГОМОСКЕДАСТИЧНІСТЬ
residuals_plot(model_log3_2) # +
plot(model_log3_2, which = 1) # +
plot(model_log3_2, which = 3) # Майже стала диспорсія.

residuals_regressors_plot(model_log3_2, c(2, 4), type = "rstudent") # +

av_plots(model_log3_2) 

crPlots(model_log3_2) # +

resettest(model_log3_2, power = 2) # p-value = 0.06786

bptest(model_log3_2) # p-value = 0.7774

observe_transforms_x(model_log3_2, regressors = names(coef(model_log3_2))[-1])



# ==== 5) НОРМАЛЬНІСТЬ ====
qq_plot(model_log3_2)
shapiro.test(residuals(model_log3_2)) # p-value = 0.8379
ad.test(residuals(model_log3_2)) # p-value = 0.6566



# ==== 6) АВТОКОРЕЛЯЦІЯ ====
time_series_residuals(model_log3_2)
dwtest(model_log3_2) # DW = 1.0705, p-value = 9.134e-06
summary(model_log3_2)
coeftest(model_log3_2, vcov = vcovHAC(model_log3_2))



# ==== 7) ВІДБІР ЗМІННИХ ====
all_models <- regsubsets(log(Ціна) ~ . - Євро - Долар, data = model3_df, nbest = 3, nvmax = 11)
best_models <- best_models_summary(all_models, 15)


# 1.2.9. ІФС + ІГР + ІндМатСтан + РівДолар + ЧистГрнКред + Євро_Долар  | Adj R² = 0.8976 (p = 7)
lm1 <- lm(log(Ціна) ~ ІФС + ІГР + ІндМатСтан + РівДолар + ЧистГрнКред + Євро_Долар, data = model3_df)
summary(lm1)
coeftest(lm1, vcov = vcovHAC(lm1)) # pval(РівДолар) = 0.0716755     
# Поки що залишимо.

# 4.6.5. ІФС + ІЦБ + ІндМатСтан + РівДолар + ЧистГрнКред + Євро_Долар  | Adj R² = 0.8943 (p = 7)
lm2 <- lm(log(Ціна) ~ ІФС + ІЦБ + ІндМатСтан + РівДолар + ЧистГрнКред + Євро_Долар, data = model3_df)
summary(lm2)
coeftest(lm2, vcov = vcovHAC(lm2))
# Поки що залишимо.

# 7.7.3. ІФС + ІЦЖП + ІндМатСтан + РівДолар + ЧистГрнКред + Євро_Долар  | Adj R² = 0.8933 (p = 7)
lm3 <- lm(log(Ціна) ~ ІФС + ІЦЖП + ІндМатСтан + РівДолар + ЧистГрнКред + Євро_Долар, data = model3_df)
summary(lm3)
coeftest(lm3, vcov = vcovHAC(lm3)) # pval(ІЦЖП) = 0.0702749    
# Поки що залишимо.


# 6.1.2. ІФС + ІГР + ІндМатСтан + ЧистГрнКред + Євро_Долар  | Adj R² = 0.8934 (p = 6)
lm4 <- lm(log(Ціна) ~ ІФС + ІГР + ІндМатСтан + ЧистГрнКред + Євро_Долар, data = model3_df)
summary(lm4)
coeftest(lm4, vcov = vcovHAC(lm4)) # pval(ІндМатСтан) = 0.0538199   


# 10. ІФС + ІндМатСтан + РівДолар + ЧистГрнКред + Євро_Долар  | Adj R² = 0.8889 (p = 6)
lm5 <- lm(log(Ціна) ~ ІФС + ІндМатСтан + РівДолар + ЧистГрнКред + Євро_Долар, data = model3_df)
summary(lm5)
coeftest(lm5, vcov = vcovHAC(lm5)) # pval(РівДолар) = 0.0988044    

lm5 <- update(lm5, . ~ . - РівДолар)
summary(lm5)
coeftest(lm5, vcov = vcovHAC(lm5))
# Модель 11.3.11


final_models3_2 <- list(lm1, lm2, lm3, lm4, lm5)



# ======== 8) ЕФЕКТИ ВЗАЄМОДІЇ ========
# Працюємо із п'ятмьа моделями.
lm1 <- final_models3_2[[1]]
lm2 <- final_models3_2[[2]]
lm3 <- final_models3_2[[3]]
lm4 <- final_models3_2[[4]]
lm5 <- final_models3_2[[5]]

models_summary(list(lm1, lm2, lm3, lm4, lm5), model_full = model_log3_2)


#### lm1 ####
summary(lm1)
aic_base <- AIC(lm1)
bic_base <- BIC(lm1)

interactions_result <- observe_best_interactions(lm1, model3_df, top_n = 10)

# 1. ІндМатСтан × ЧистГрнКред | ΔAIC = 9.78 | p = 0.0019 
inter_lm <- update(lm1, . ~ . + ІндМатСтан:ЧистГрнКред)
models_summary(list(inter_lm))

inter_lm <- update(inter_lm, . ~ . - ІГР)
models_summary(list(inter_lm))

aic_base - AIC(inter_lm) # 7.399864
bic_base - BIC(inter_lm) # 7.399864

inter_lm1 <- inter_lm
assumptions_check(inter_lm1)


# 3. РівДолар × ЧистГрнКред | ΔAIC = 4.67 | p = 0.0197 
inter_lm <- update(lm1, . ~ . + РівДолар:ЧистГрнКред)
models_summary(list(inter_lm))

inter_lm <- update(inter_lm, . ~ . - РівДолар)
models_summary(list(inter_lm))

inter_lm <- update(inter_lm, . ~ . - ЧистГрнКред)
models_summary(list(inter_lm))

aic_base - AIC(inter_lm) # 4.51936
bic_base - BIC(inter_lm) # 6.348002

inter_lm2 <- inter_lm

assumptions_check(inter_lm2)



#### lm2 ####
summary(lm2)
aic_base <- AIC(lm2)
bic_base <- BIC(lm2)

interactions_result <- observe_best_interactions(lm2, model3_df, top_n = 10)

# 2. ІЦБ × ІндМатСтан | ΔAIC = 12.04 | p = 0.0007 
inter_lm <- update(lm2, . ~ . + ІЦБ:ІндМатСтан)
models_summary(list(inter_lm))

inter_lm <- update(inter_lm, . ~ . - РівДолар)
models_summary(list(inter_lm))

aic_base - AIC(inter_lm) # 14.11481
bic_base - BIC(inter_lm) # 14.11481

inter_lm3 <- inter_lm
assumptions_check(inter_lm3)


# 3. ІндМатСтан × ЧистГрнКред | ΔAIC = 11.23 | p = 0.0010 
inter_lm <- update(lm2, . ~ . + ЧистГрнКред:ІндМатСтан)
models_summary(list(inter_lm))

aic_base - AIC(inter_lm) # 11.6804
bic_base - BIC(inter_lm) # 9.851758

inter_lm4 <- inter_lm
assumptions_check(inter_lm4)


# 6. РівДолар × ЧистГрнКред | ΔAIC = 4.79 | p = 0.0186 
inter_lm <- update(lm2, . ~ . + ЧистГрнКред:РівДолар)
models_summary(list(inter_lm))

inter_lm <- update(inter_lm, . ~ . - РівДолар)
models_summary(list(inter_lm))

inter_lm <- update(inter_lm, . ~ . - ЧистГрнКред)
models_summary(list(inter_lm))

aic_base - AIC(inter_lm) # 5.254141
bic_base - BIC(inter_lm) # 7.082782

inter_lm5 <- inter_lm
assumptions_check(inter_lm5)



#### lm3 ####
summary(lm3)
aic_base <- AIC(lm3)
bic_base <- BIC(lm3)

interactions_result <- observe_best_interactions(lm3, model3_df, top_n = 10)

# 1. ІЦЖП × ІндМатСтан | ΔAIC = 12.58 | p = 0.0006 
inter_lm <- update(lm3, . ~ . + ІЦЖП:ІндМатСтан)
models_summary(list(inter_lm))

inter_lm <- update(inter_lm, . ~ . - РівДолар)
models_summary(list(inter_lm))

aic_base - AIC(inter_lm) # 14.44747
bic_base - BIC(inter_lm) # 14.44747

inter_lm6 <- inter_lm
assumptions_check(inter_lm6)


# -4. ІндМатСтан × ЧистГрнКред | ΔAIC = 8.63 | p = 0.0032 
inter_lm <- update(lm3, . ~ . + ЧистГрнКред:ІндМатСтан)
models_summary(list(inter_lm))

inter_lm <- update(inter_lm, . ~ . - ІндМатСтан)
models_summary(list(inter_lm))

inter_lm <- update(inter_lm, . ~ . - ЧистГрнКред)
models_summary(list(inter_lm))

inter_lm <- update(inter_lm, . ~ . - РівДолар)
models_summary(list(inter_lm)) # Ще незначущі?..


#### lm4 ####
models_summary(list(lm4))
aic_base <- AIC(lm4)
bic_base <- BIC(lm4)

interactions_result <- observe_best_interactions(lm4, model3_df, top_n = 10)

# 1. ІндМатСтан × ЧистГрнКред | ΔAIC = 8.06 | p = 0.0037 
inter_lm <- update(lm4, . ~ . + ЧистГрнКред:ІндМатСтан)
models_summary(list(inter_lm))

inter_lm <- update(inter_lm, . ~ . - ІГР)
models_summary(list(inter_lm))

aic_base - AIC(inter_lm) # 5.92721
bic_base - BIC(inter_lm) # 5.92721

inter_lm7 <- inter_lm
assumptions_check(inter_lm7)



#### lm5 ####
models_summary(list(lm5))
aic_base <- AIC(lm5)
bic_base <- BIC(lm5)

interactions_result <- observe_best_interactions(lm5, model3_df, top_n = 10)

# 2. ІндМатСтан × ЧистГрнКред | ΔAIC = 8.64 | p = 0.0025 
inter_lm <- update(lm5, . ~ . + ЧистГрнКред:ІндМатСтан)
models_summary(list(inter_lm))
# Така ж модель, як і inter_lm7.



final_models3_2 <- list(inter_lm1, inter_lm2, inter_lm3, inter_lm4, 
                        inter_lm5, inter_lm6, inter_lm7)



# ======== 9) ВАЛІДАЦІЯ МОДЕЛІ ========
suppressWarnings(
  models_validation(final_models3_2, data = model3_df, kfold_number = 5,
                    horizon = 1, init_window = 30)
)


# ======== 10) ВИБІР НАЙКРАЩОЇ МОДЕЛІ ========
models_summary(final_models3_2)
# Четверта модель гірша за інші: не найвищі показники, ІЦБ формально не значуще, 7 змінних.

best_models3_2 <- final_models3_2[-4]
models_summary(best_models3_2)
# Модель 2 має трохи гірші показники за 1, її складніше інтерпретувати.

best_models3_2 <- best_models3_2[-2]
models_summary(best_models3_2)
# Моделі 3 і 5 програють за показниками.

best_models3_2 <- best_models3_2[-c(3, 5)]
models_summary(best_models3_2)
# Перша модель має трохи нижчі показники за другу й третю.
# За найкращу візьмемо останню модель.
suppressWarnings(
  models_validation(list(best_models3_2[[length(best_models3_2)]]), data = model3_df, kfold_number = 5,
                    horizon = 1, init_window = 30)
)

