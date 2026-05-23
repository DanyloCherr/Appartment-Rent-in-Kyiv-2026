library(car)
library(lmtest)
library(MASS)
library(ggplot2)
library(sandwich)
library(leaps)
library(nlme)

# Схема побудови моделі.
# 1) МУЛЬТИКОЛІНЕАРНІСТЬ-1. КОРЕЛЯЦІЙНА МАТРИЦЯ.
# 2) ПОВНА МОДЕЛЬ.
# 3) МУЛЬТИКОЛІНЕАРНІСТЬ-2. VIF, ЧИСЛО ОБУМОВЛЕНОСТІ.
# 4) АНАЛІЗ ЗАЛИШКІВ І РОБОТА З ВИКИДАМИ.
# 5) ФУНКЦІОНАЛЬНІ ПЕРЕТВОРЕННЯ. ЛІНІЙНІСТЬ, ГОМОСКЕДАСТИЧНІСТЬ.
# 6) АВТОКОРЕЛЯЦІЯ.
# 6) Застосувати всі можливі регресії, якщо це можливо.
# 7) Відібрати найкращі моделі за декількома критеріями.
# 8) Додати interactions.

# Побудуємо модель на основі цін для однокімнатних квартир.

upd_model_df <- function(model_df, remove = c(), room_num = 1){
  K_remove <- switch(room_num,
                     "1" = c("К2", "К3"),
                     "2" = c("К1", "К3"),
                     "3" = c("К1", "К2"),
                     c()
  )
  result <- model_df[, !names(model_df) %in% c(K_remove, "Дата", remove)]
  return(result)
}


unit_length_scale <- function(x){ # Передаємо один стовпець
  centered <- x - mean(x)
  result <- centered / sqrt(sum(centered^2))
  return(result)
}


compute_vifs <- function(model){
  model_data <- model$model
  colnames(model_data)[1] <- "Y"
  
  model_df_norm <- as.data.frame(lapply(model_data, unit_length_scale))
  model_norm <- lm(Y ~ ., data = model_data)
  
  return(vif(model_norm))
}


condition_number <- function(df, singular_number = TRUE){ # Передаємо повну таблицю
  # Матриця регресорів
  X <- as.matrix(df[, -c(1, which(names(df) == "Ціна"))])
  
  if(singular_number){
    S <- svd(X)
    singular_values <- S$d
    cond_number <- max(singular_values) / min(singular_values)
  } else{
    eigen_values <- eigen(crossprod(X))$values
    cond_number <- max(eigen_values) / min(eigen_values)
  }
  
  return(cond_number)
}


residuals_plot <- function(model, type = "response"){
  par(mar = c(5, 5, 3, 2))
  
  residualPlot(model, 
               type = type,
               col = "steelblue", 
               pch = 19,
               cex = 1, 
               col.quad = "white", 
               id = list(n = 3, col = "red", cex = 1),
               grid = TRUE, 
               main = "",
               xlab = "Прогнозовані значення",
               ylab = "Залишки")
  
  par(mar = c(5, 4, 4, 2))
}


residuals_regressors_plot <- function(model, nr_nc, type = "response"){
  predictors <- attr(terms(model), "term.labels")
  pred_num <- length(predictors)
  par(mfrow = nr_nc)
  
  for(pred in predictors){
    residualPlot(model, variable = pred, 
                 type = type,
                 col = "steelblue", 
                 pch = 19,
                 cex = 1, 
                 id = list(n = 3, col = "red", cex = 1),
                 col.quad = "white",
                 grid = TRUE,
                 main = "",
                 xlab = pred,
                 ylab = "")
  }
  par(mfrow = c(1, 1))
}


cooks_dist <- function(model, head_param = 10){
  dists <- cooks.distance(model)
  sorted <- sort(dists, decreasing = TRUE)
  cat("Відстань Кука", "\n")
  print(head(sorted, head_param))
  
  threshold <- 4 / df.residual(model)
  cat("Порогове значення:", threshold, "\n")
  
  influential <- which(dists > threshold)
  # influential <- influential[order(dists[influential], decreasing = TRUE)]
  cat("Впливові точки:", influential, "\n\n")
}


df_betas <- function(model, head_param = 10){
  dfbetas_matrix <- dfbetas(model)
  cat("|DFBETAS|", "\n")
  threshold <- 2 / sqrt(nobs(model))
  cat("Порогове значення:", threshold, "\n")
  
  abs_df_betas <- abs(dfbetas_matrix)
  predictors <- colnames(dfbetas_matrix)
  
  all_influential <- c()
  
  for(pred in predictors){
    cat("----", pred, "----\n")
    dfvals <- dfbetas_matrix[, pred]
    
    sorted <- sort(dfvals, decreasing = TRUE)
    print(head(sorted, head_param))
    
    influential <- which(dfvals > threshold)
    cat("Впливові точки:", influential, "\n")
    
    all_influential <- c(all_influential, influential)
    all_influential <- sort(unique(all_influential), decreasing = FALSE)
  }
  cat("\n", "Всі впливові точки:", all_influential)
  
}


df_fits <- function(model, head_param = 10){
  dffits_vals <- dffits(model)
  abs_df_fits <- abs(dffits_vals)
  sorted <- sort(abs_df_fits, decreasing = TRUE)
  cat("|DFFITS|", "\n")
  print(head(sorted, head_param))
  
  threshold <- 2 * sqrt(length(coef(model)) / nobs(model))
  cat("Порогове значення:", threshold, "\n")
  
  influential <- which(abs_df_fits > threshold)
  # influential <- influential[order(abs_df_fits[influential], decreasing = TRUE)]
  cat("Впливові точки:", influential, "\n", "\n")
}


qq_plot <- function(model){
  qqPlot(rstudent(model),
         distribution = "norm",      
         col = "steelblue",         
         pch = 19,                   
         cex = 1,
         id = list(n = 3, col = "red", cex = 1),  
         grid = TRUE,                
         main = "",
         xlab = "Квантилі нормального розподілу",
         ylab = "Залишки")
}


plot_model_df <- function(full_period, model_df, points = NULL){
  column_to_table <- c(
    "РівДолар"        = "DOLLARIZATION_LEVEL",
    "Євро"            = "EUR",
    "Долар"           = "USD",
    "К1"              = "SGLROOM",
    "К2"              = "DBLROOM",
    "К3"              = "TRPROOM",
    "ІФС"             = "FS_INDEX",
    "ІГР"             = "GPR_INDEX",
    "ІЦБ"             = "CP_INDEX",
    "ЧисНасел"        = "POPULATION",
    "ЧистГрнКред"     = "NET_UAH_LOANS",
    "ІндМатСтан"      = "CONSUMER_CONFIDENCE",
    "ІЦЖВ"            = "SECOND_HAND_HPRICE_INDEX",
    "ІЦЖП"            = "NEW_HPRICE_INDEX",
    "ТемпЗмінКошт"    = "HH_DEPOSITS_ANNUAL_GROWTH",
    "БоргНавантаж"    = "HH_DEBT_BURDEN",
    "ВведЖитла"       = "HOUSING_COMPLETIONS",
    "ОцінкаДоходів"   = "HH_INCOME_ESTIMATE",
    "Активність"      = "ACTIVITY",
    "КредПортфель"    = "STOCK",
    "НовіКредити"     = "FLOW"
  )
  
  tables_to_plot <- column_to_table[names(model_df)]
  tables_to_plot <- tables_to_plot[!is.na(tables_to_plot)]
  
  combined_plots(full_period[tables_to_plot], highlight_df = points)
}


av_plots <- function(model){
  avPlots(model, 
          col = "steelblue", 
          pch = 19,
          cex = 0.7,
          lwd = 1.5,
          id = list(n = 5, col = "red"))
}


time_series_residuals <- function(model, normalized = FALSE){
  res_type <- if(inherits(model, "gls") && normalized) "normalized" else "response"
  
  res_df <- data.frame(
    order = 1:length(residuals(model, type = res_type)),
    residuals = residuals(model, type = res_type)
  )
  ggplot(res_df, aes(x = order, y = residuals)) +
    geom_line(color = "steelblue", alpha = 0.5) +
    geom_point(color = "steelblue", size = 2) +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
    labs(title = "Залишки в часовому ряді",
         x = "Порядок спостереження",
         y = "Залишки") +
    theme_minimal(base_size = 14)
}


plot_subsets <- function(subsets) {
  scales <- c("r2", "adjr2", "bic", "Cp")
  par(mfrow = c(2, 2))
  
  for (sc in scales) {
    plot(subsets, scale = sc)
    grid(nx = ncol(summary(subsets)$which), 
         ny = nrow(summary(subsets)$which), 
         col = "gray50", lty = "dotted")
  }
  
  par(mfrow = c(1, 1))
}


best_models_summary <- function(all_subsets, top_n = 3) {
  sm <- summary(all_subsets)
  which_mat <- sm$which
  adjr2 <- sm$adjr2
  bic <- sm$bic
  cp <- sm$cp
  p <- apply(which_mat, 1, sum)
  
  top_adjr2 <- order(adjr2, decreasing = TRUE)[1:top_n]
  
  top_bic <- order(bic)[1:top_n]
  
  cp_dist <- abs(cp - p)
  top_cp <- order(cp_dist)[1:top_n]
  
  format_model <- function(idx) {
    vars <- names(which(which_mat[idx, -1]))
    paste(vars, collapse = " + ")
  }
  
  cat("========== Adj R² ==========\n")
  for (i in 1:top_n) {
    cat(sprintf("%d. %s  | Adj R² = %.4f (p = %d)\n", i, format_model(top_adjr2[i]), adjr2[top_adjr2[i]], p[top_adjr2[i]]))
  }
  cat("\n========== BIC ==========\n")
  for (i in 1:top_n) {
    cat(sprintf("%d. %s  | BIC = %.2f (p = %d)\n", i, format_model(top_bic[i]), bic[top_bic[i]], p[top_bic[i]]))
  }
  cat("\n========== Cp ==========\n")
  for (i in 1:top_n) {
    cat(sprintf("%d. %s  | Cp = %.2f (p = %d)\n", i, format_model(top_cp[i]), cp[top_cp[i]], p[top_cp[i]]))
  }
  
  invisible(list(
    top_adjr2 = top_adjr2,
    top_bic = top_bic,
    top_cp = top_cp
  ))
}


# Перевіримо, чи покращують модель функціональні перетворення
observe_transforms_x <- function(model, regressors = c()){
  model_data <- model$model
  response <- "Ціна"
  names(model_data)[1] <- "Ціна"
  
  transforms <- c("linear", "sqrt", "square", "inverse", "log")
  
  col_names <- c("linear", "sqrt", "+sqrt", "square", "+square", 
                 "inverse","+inverse", "log","+log")
  
  aic_table <- matrix(NA, nrow = length(regressors),
                      ncol = length(col_names),
                      dimnames = list(regressors, col_names))
  
  aic_diffs <- aic_table
  AIC_init <- AIC(model)
  
  for(reg in regressors){
    for(trans in transforms){
      formula <- switch(trans,
                        "linear"  = paste0(response," ~ ."),
                        "log"     = paste0(response, " ~ . - ", reg, " + log(", reg, ")"),
                        "sqrt"    = paste0(response, " ~ . - ", reg, " + I(sqrt(", reg, "))"),
                        "square"  = paste0(response, " ~ . - ", reg, " + I(", reg, "^2)"),
                        "inverse" = paste0(response, " ~ . - ", reg, " + I(1/", reg, ")")
      )
      model_temp <- lm(as.formula(formula), data = model_data)
      AIC_temp <- AIC(model_temp)
      aic_table[reg, trans] <- AIC_temp
      aic_diffs[reg, trans] <- AIC_init - AIC_temp
      
      if(trans != "linear"){
        model_temp2 <- lm(as.formula(paste0(formula, " + ", reg)), data = model_data)
        AIC_temp2 <- AIC(model_temp2)
        pos = paste0("+", trans)
        aic_table[reg, pos] <- AIC_temp2
        aic_diffs[reg, pos] <- AIC_init - AIC_temp2
      }
    }
  }
  result = list(
    AIC_values = aic_table,
    AIC_diffs = aic_diffs
  )
  return(result)
}


observe_transforms_y <- function(model){
  model_data <- model$model
  response <- "Ціна"
  names(model_data)[1] <- "Ціна"
  transforms <- c("linear", "sqrt", "square", "inverse", "log")
  
  R2_vals <- matrix(NA, nrow = 1, ncol = length(transforms),
                    dimnames = list(paste0("R2(", response, ")"), transforms)) 
  # AIC чутливий до масштабу. Застосовуючи функціональні перетворення над відгуком, 
  # ми змінюємо масштаб, тому і залишки можуть штучно зменшитися.
  R2_diffs <- R2_vals
  
  R2_init <- summary(model)$r.squared
  y_true <- model_data[, response]
  
  for(trans in transforms){
    formula <- switch(trans,
                        "linear"  = paste0(response," ~ ."),
                        "log"     = paste0("log(", response,") ~ ."),
                        "sqrt"    = paste0("I(sqrt(", response,")) ~ ."),
                        "square"  = paste0("I(", response,"^2) ~ ."),
                        "inverse" = paste0("I(1/", response,") ~ .")
    )
    model_temp <- lm(as.formula(formula), data = model_data)
    
    # Для строгості будемо обчислювати R2 в оригінальній шкалі.
    
    y_pred <- switch(trans,
                     "linear"  = fitted(model_temp),
                     "log"     = exp(fitted(model_temp)),
                     "sqrt"    = fitted(model_temp)^2,
                     "square"  = sqrt(fitted(model_temp)),
                     "inverse" = 1 / fitted(model_temp)
    )
    
    R2_temp <- 1 - sum((y_true - y_pred)^2) / sum((y_true - mean(y_true))^2)
    R2_vals[1, trans] <- R2_temp
    R2_diffs[1, trans] <- R2_init - R2_temp
  }
  result = list(
    R2_values = R2_vals,
    R2_diffs = R2_diffs
  )
  return(result)
}


observe_best_interactions <- function(model, data, top_n = 5){
  predictors <- attr(terms(model), "term.labels")
  
  # Складні ефекти взаємодії не розглядаємо
  predictors <- predictors[!grepl("\\^|log\\(|sqrt\\(|I\\(", predictors)]
  
  results <- data.frame(
    interaction = character(),
    AIC = numeric(),
    delta_AIC = numeric(),
    p_value = numeric(),
    stringsAsFactors = FALSE
  )
  base_AIC <- AIC(model)
  
  for (i in 1:(length(predictors) - 1)) {
    for (j in (i + 1):length(predictors)) {
      p1 <- predictors[i]
      p2 <- predictors[j]
      
      formula_prod <- as.formula(paste(". ~ . +", p1, "*", p2))
      model_prod <- try(update(model, formula_prod), silent = TRUE)
      
      if (!inherits(model_prod, "try-error")) {
        aic_prod <- AIC(model_prod)
        p_prod <- anova(model, model_prod)$`Pr(>F)`[2]
        results <- rbind(results, data.frame(
          interaction = paste(p1, "×", p2),
          AIC = aic_prod,
          delta_AIC = base_AIC - aic_prod,
          p_value = p_prod
        ))
      }
      
      formula_ratio1 <- as.formula(paste(". ~ . + I(", p1, "/", p2, ")"))
      model_ratio1 <- try(update(model, formula_ratio1), silent = TRUE)
      
      if (!inherits(model_ratio1, "try-error")) {
        aic_ratio1 <- AIC(model_ratio1)
        p_ratio1 <- anova(model, model_ratio1)$`Pr(>F)`[2]
        results <- rbind(results, data.frame(
          interaction = paste(p1, "/", p2),
          AIC = aic_ratio1,
          delta_AIC = base_AIC - aic_ratio1,
          p_value = p_ratio1
        ))
      }
      
      formula_ratio2 <- as.formula(paste(". ~ . + I(", p2, "/", p1, ")"))
      model_ratio2 <- try(update(model, formula_ratio2), silent = TRUE)
      
      if (!inherits(model_ratio2, "try-error")) {
        aic_ratio2 <- AIC(model_ratio2)
        p_ratio2 <- anova(model, model_ratio2)$`Pr(>F)`[2]
        results <- rbind(results, data.frame(
          interaction = paste(p2, "/", p1),
          AIC = aic_ratio2,
          delta_AIC = base_AIC - aic_ratio2,
          p_value = p_ratio2
        ))
      }
    }
  }
  
  results <- results[order(-results$delta_AIC), ]
  
  cat("\n========== Найкращі", top_n, "взаємодій ==========\n")
  for (i in 1:min(top_n, nrow(results))) {
    r <- results[i, ]
    cat(sprintf("%d. %s | ΔAIC = %.2f | p = %.4f \n",
                i, r$interaction, r$delta_AIC, r$p_value))
  }
  invisible(results)
}


gls_dwtest <- function(gls_model){
  e <- residuals(gls_model, type = "normalized")
  # type = "normalized" - витягти залишки після домноження рівняння на К^-1
  dwtest(lm(e ~ 1))
}


analyze_selected_models <- function(all_subsets, data, indices, rho){
  sm <- summary(all_subsets)
  which_mat <- sm$which
  bic <- sm$bic
  
  for(i in seq_along(indices)) {
    idx <- indices[i]
    vars <- names(which(which_mat[idx, -1]))
    
    cat("\n========================================\n")
    cat(sprintf("Модель %d (BIC = %.2f):\n", i, bic[idx]))
    cat(paste(vars, collapse = " + "), "\n")
    cat("========================================\n")
    
    formula <- as.formula(paste("log(Ціна) ~", paste(vars, collapse = " + ")))
    model_ols <- lm(formula, data = data)
    
    cat("\n--- HAC (Newey-West) ---\n")
    hac_test <- coeftest(model_ols, vcov = vcovHAC(model_ols))
    print(hac_test)
    
    p_values <- hac_test[, 4]
    significant <- p_values < 0.05
    if (all(significant)) {
      cat("\n Усі змінні значущі (HAC)\n")
    } else {
      cat("\n Незначущі:", paste(names(p_values)[!significant], collapse = ", "), "\n")
    }
    
    cat("\n--- GLS (ρ = ", rho, ") ---\n", sep = "")
    model_gls <- try(gls(formula, 
                         correlation = corAR1(form = ~1, value = rho, fixed = TRUE),
                         data = data), silent = TRUE)
    
    if (inherits(model_gls, "try-error")) {
      cat("GLS не побудовано\n")
    } else {
      gls_table <- summary(model_gls)$tTable
      print(gls_table)
      
      p_gls <- gls_table[, "p-value"]
      sig_gls <- p_gls < 0.1
      if (all(sig_gls)) {
        cat("\nУсі змінні значущі (GLS)\n")
      } else {
        cat("\nНезначущі (GLS):", paste(rownames(gls_table)[!sig_gls], collapse = ", "), "\n")
      }
    }
  }
}


assumptions_check <- function(model){
  cat("── 1. Мультиколінеарність (VIF) ──\n")
  print(compute_vifs(model))

  cat("\n── 2. Залишки та викиди ──\n")
  cooks_dist(model)
  df_fits(model)
  #df_betas(model)
  
  cat("\n── 3. Лінійність ──\n")
  print(resettest(model, power = 2))
  
  cat("\n── 4. Гомоскедастичність ──\n")

  print(bptest(model)) 

  cat("\n── 5. Нормальність ──\n")
  print(shapiro.test(residuals(model)))
  
  cat("\n── 6. Автокореляція ──\n")
  print(dwtest(model)) 
}


# ----- 75 СПОСТЕРЕЖЕНЬ -----
# ==== 1) МУЛЬТИКОЛІНЕАРНІСТЬ-1. КОРЕЛЯЦІЙНА МАТРИЦЯ ====
cor_matrix_75 <- model_cor_matrix(model_df_75[, -1], "")
# Змінна Час сильно корелює із двома змінними - РівДолар, ЧисНасел.
# Приберемо її.


model1_75_df <- model_df_75[, !names(model_df_75) %in% c("К2", "К3", "Ч", "Дата")]
colnames(model1_75_df)[colnames(model1_75_df) == "К1"] <- "Ціна"
head(model1_75_df, 3)
# 7 предикторів



# ==== 2) ПОВНА МОДЕЛЬ ====

model_full1_75 <- lm(Ціна ~ ., data = model1_75_df)
summary(model_full1_75)

# ІГР має велике p-значення. Проте поки що його не видаляємо. Можливо, виправиться
# на подальших кроках.



# ==== 3) МУЛЬТИКОЛІНЕАРНІСТЬ-2. VIF, ЧИСЛО ОБУМОВЛЕНОСТІ ====
# Для діагнозтики мультиколінеарності стандартизуємо регресори за одиничною довжиною.
# Проте надалі будемо працювати з оригінальними одиницями.

model1_75_df_norm <- as.data.frame(lapply(model1_75_df, unit_length_scale))
# colMeans(model1_75_df_norm)
# head(model1_75_df_norm, 3)

model_full1_75_norm <- lm(Ціна ~ ., data = model1_75_df_norm)
vif(model_full1_75_norm)
# Євро, Долар, ЧисНасел - підозріло.

condition_number(model1_75_df_norm, 0)


# ==== 4) АНАЛІЗ ЗАЛИШКІВ І ВИЯВЛЕННЯ ВИКИДІВ ====
residuals_plot(model_full1_75)
# Автокореляція?

residuals_regressors_plot(model_full1_75, c(2, 4), type = "rstudent")
# Теоретично, R-стьюдентизовані залишки мають нам показати впливові точки,
# оскільки такі залишки є більш чутливими до викидів.
#
# Екстремальні значення: ІФС, ІГР, ЧисНасел.
# Можливо, ця проблема зникне, коли повернемося на 02, і попрацюємо з поділом
# даних і підозрілими точками.


# Відстань Кука.
cooks_dist(model_full1_75)

df_fits(model_full1_75)

df_betas(model_full1_75)

# Спостереження 2 4 30 35 36 75 не пройшли жодного з тестів.
model1_75_df_date <- model_df_75[, !names(model_df_75) %in% c("К2", "К3", "Ч")]
influential_points <- model1_75_df_date[c(2, 4, 30, 35, 36, 75), ]
influential_points

plot_model_df(data1, model1_75_df_date, influential_points)
# 2: Обріжемо вибірку по 2016 рік.
# 4: Навряд чи щось зробимо.
# 30: Із таблиці не скажеш, що це екстремальне значення. 
# Для Cook's D, DFFITS воно зовсім незначно перевищує поріг, 
# але DFBETAS для змінної Долар досить високе. Поки не чіпаємо.
# 35: Показники гірші, ніж для 30. У цій точці спостерігаються різкі перепади 
# для певних змінних, проте викидом її не назвеш.
# 36: Все трохи краще, ніж у точки 35. Значне перевищення порогу характерно 
# тільки для DFBETAS змінної ІЦБ.
# 75: За межу поділу на періоди візьмемо 2022-01-01.

# ЩОБ НЕ ЗАХАРАЩУВАТИ КОД, ПРОСТО ПОВЕРНЕМОСЯ НАЗАД, І ПІДПРАВИМО ЙОГО ТАМ!
# НАЗВИ ЗМІННИХ ЗАЛИШАЄМО ТАКИМИ Ж.
# Що нового після видалення крайніх спостережень: VIF, значимість предикторів у моделі - трохи краще.
# Проте тепер впливовим спостереженням вважається 72 (останнє). З ним уже навряд чи ми щось зробимо.


# Важливо пам'ятати, що e = (I-H)y, тобто для спостережень, що знаходяться
# далі від центроїда, залишки можуть бути заниженими, оскільки для них h_ii -> 1.


# ==== 5) ФУНКЦІОНАЛЬНІ ПЕРЕТВОРЕННЯ ====
# Дослідимо лінійність зв'язку та постійність дисперсії.


# ЛІНІЙНІСТЬ
plot(model_full1_75, which = 1)
# Якщо зв'язок між y та x лінійний LOESS пряма має бути горизонтальною.
# Лінія не зовсім пряма. Не очевидно.

residuals_regressors_plot(model_full1_75, c(2, 4), type = "rstudent")
# Тенденцій не видно (good)

av_plots(model_full1_75)

# ІФС, ІГР - під питанням: наявні основні хмари даних, в яких не спостерігається конкретного
# зв'язку. Також наявні декілька точок з великим левереджем, які визначають нахил прямої.
# Можливо, ІФС та ІГР не є хорошими лінійними предикторами, оскільки для більшості даних
# вони не пояснюють ціну.


# ГОМОСКЕДАСТИЧНІСТЬ
plot(model_full1_75, which = 3)
# Дисперсія залишків постійна, якщо їх середня величина приблизно однакова для всіх
# передбачених значень.
# Відхилення не значні, не очевдино.

residuals_regressors_plot(model_full1_75, c(2, 4), type = "rstudent")

# ІЦБ. Патерн воронки, що розкривається. Можливо, потрібне функціональне перетворення.
# ДОЛАР - теж?

bptest(model_full1_75)
# Тест Бройша-Паґана: p-value = 0.1951 - гіпотезу про гомоскедастичність не відхиляємо.


# ЛІНІЙНІСТЬ.

resettest(model_full1_75)
# p-value = 0.003052 < 0.05.
# Отже, члени із y^2, y^3 є значущими.
# Але із цього не видно, які предиктори мають не лінійний зв'язок.

resettest(model_full1_75, power = 2) # p-value = 0.1277
resettest(model_full1_75, power = 3) # p-value = 0.001122
# Бракує кубічних членів?
# Чи варто додавати кубічний член?..
# Кубічний член можуть тягти кілька екстремальних точок, а не систематичний патерн у даних,
# тому він є значущим.

crPlots(model_full1_75)
# Якщо відкинути особливість даних, то всі LOESS прямі виглядають +- нормально.


# Спробуємо застосувати "Стандартні" функціональні перетворерення до регресорів 
# і відгука.
observe_transforms_x(model_full1_75, regressors = names(coef(model_full1_75))[-1])
# Суттєвої різниці немає.

observe_transforms_y(model_full1_75)
# Суттєвої різниці немає.


# Метод Бокса-Кокса
bc <- boxcox(model_full1_75, lambda = seq(-2, 2, by = 0.1))

lambda_opt <- bc$x[which.max(bc$y)]
cat("Оптимальне λ:", round(lambda_opt, 2), "\n")
# Ага! А метод Бокса-Кокса пропонує нам взяти логарифм від відгука.
# Метод виправляє ненормальність і гетероскедастичність?
# Застосуємо логарифм, і проведемо діагнозтику спочатку.



# ======== НОВА МОДЕЛЬ 1 ========

# ==== 5.1.1) ПОВНА МОДЕЛЬ ====
model_log1_75 <- lm(log(Ціна) ~ ., data = model1_75_df)
summary(model_log1_75)
# pval(ІФС) = 0.49213 !!!



# ==== 5.1.2) АНАЛІЗ ЗАЛИШКІВ І ВИЯВЛЕННЯ ВИКИДІВ ====
residuals_plot(model_log1_75) # Нічого?

residuals_regressors_plot(model_log1_75, c(2, 4), type = "rstudent")

# Відстань Кука.
cooks_dist(model_log1_75)

df_fits(model_log1_75)

df_betas(model_log1_75)
# Із викидами ми все одно вже зробили все, що могли :)


# ==== 5.1.3) ЛІНІЙНІСТЬ, ГОМОСКЕДАСТИЧНІСТЬ ====

# ЛІНІЙНІСТЬ
plot(model_log1_75, which = 1)
# Те саме?

residuals_regressors_plot(model_log1_75, c(2, 4), type = "rstudent")

av_plots(model_log1_75)
# Трошечки краще, але основні проблеми з ІФС, ІГР залишаються.



# ГОМОСКЕДАСТИЧНІСТЬ
plot(model_log1_75, which = 3)
# Те саме?


bptest(model_log1_75)
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

summary(model_log1_75_Sq)$coefficients
coeftest(model_log1_75_Sq, vcov = vcovHAC(model_log1_75_Sq))
# Стандартні робастні похибки Ньюї-Веста є трохи вищими за звичайні.
# Проте якщо за рівень значимості брати 5%, то значимість регресорів не порушується.


# UPD. Виправляємо автокореляцію повної моделі.
# УЗАГАЛЬНЕНІ НАЙМЕНШІ КВАДРАТИ.
e <- residuals(model_log1_75_Sq)
rho <- cor(e[-1], e[-length(e)]) # 0.5924019
acf(residuals(model_log1_75_Sq), main = "ACF залишків")
# Із графіка функції автокореляції видно, що статистично значимимою є автокореляція першого порядку
# при rho = 0.59, що свідчить про наявність AR(1) структури в залишках.
# Отже, зафіксуємо параметр rho = 0.59. Якщо його не фіксувати, то gls його оцінює як 1, і видає
# зовсім неадекватну модель.

model_full_gls <- gls(log(Ціна) ~ . + I(Долар^2), correlation = corAR1(form = ~1, value = rho, fixed = TRUE),
                      data = model1_75_df)
summary(model_full_gls)

time_series_residuals(model_full_gls, normalized = TRUE)
gls_dwtest(model_full_gls) # DW = 1.4337, p-value = 0.0069
acf(residuals(model_full_gls, type = "normalized"), main = "ACF залишків")
# Спрацювало! Тепер модель не потерпає від автокореляції.
# Але використовувати повну модель ми не будемо, просто використаємо gls для
# найкращих моделей з відбору.




# ==== 7) ВІДБІР ЗМІННИХ ====
# GLS не працює із R2 і Cp, тому основну увагу приділяємо BIC.

# Усі можливі регресії
all_models <- regsubsets(log(Ціна) ~ . + I(Долар^2), data = model1_75_df, nbest = 3)

plot_subsets(all_models)
best_models <- best_models_summary(all_models, 10)


# 3. ЧисНасел + РівДолар  | BIC = -129.91 (p = 3)
lm_BIC3 <- lm(log(Ціна) ~ ЧисНасел + РівДолар, data = model1_75_df)
summary(lm_BIC3) # Adjusted R-squared:  0.8583 - не значно гірше за найкращі моделі по R2.
coeftest(lm_BIC3, vcov = vcovHAC(lm_BIC3))
final_models <- list("2vars_BIC3" = lm_BIC3)


# 1. Євро + Долар + ІГР + ІЦБ + ЧисНасел + РівДолар + I(Долар^2)  | Adj R² = 0.8901 (p = 8)
lm_Rsq1 <- lm(log(Ціна) ~ Євро + Долар + ІГР + ІЦБ + ЧисНасел + РівДолар + I(Долар^2), data = model1_75_df)
summary(lm_Rsq1)
anova(lm_BIC3, lm_Rsq1) # додаткові змінні дійсно покращують модель.  Модель із p = 8 краща.
coeftest(lm_Rsq1, vcov = vcovHAC(lm_Rsq1)) # Долар на межі.
final_models[["7vars_Rsq1"]] <- lm_Rsq1

# 3. Євро + Долар + ІЦБ + ЧисНасел + РівДолар + I(Долар^2)  | Adj R² = 0.8852 (p = 7)
lm_Rsq3 <- lm(log(Ціна) ~ Євро + Долар + ІЦБ + ЧисНасел + РівДолар + I(Долар^2), data = model1_75_df)
summary(lm_Rsq3) 
anova(lm_Rsq3, lm_Rsq1) # На межі. Додавання ІГР не значно покращує модель. Залишимо дві.
coeftest(lm_Rsq3, vcov = vcovHAC(lm_Rsq3))
lm_Rsq3_gls <- gls(log(Ціна) ~ Євро + Долар + ІЦБ + ЧисНасел + РівДолар + I(Долар^2), 
                   correlation = corAR1(form = ~1, value = rho, fixed = TRUE),
                   data = model1_75_df)
summary(lm_Rsq3_gls) # ІЦБ, Євро - не значущі.

lm_Rsq3_gls2 <- gls(log(Ціна) ~ Євро + Долар + ЧисНасел + РівДолар + I(Долар^2), 
                   correlation = corAR1(form = ~1, value = rho, fixed = TRUE),
                   data = model1_75_df)
summary(lm_Rsq3_gls2)


# 5. Євро + ІГР + ІЦБ + ЧисНасел + РівДолар + I(Долар^2)  | Adj R² = 0.8787 (p = 7)
lm_Rsq5 <- lm(log(Ціна) ~ Євро + ІГР + ІЦБ + ЧисНасел + РівДолар + I(Долар^2), data = model1_75_df)
summary(lm_Rsq5)
coeftest(lm_Rsq5, vcov = vcovHAC(lm_Rsq5))

lm_Rsq5_gls <- gls(log(Ціна) ~ Євро + ІГР + ІЦБ + ЧисНасел + РівДолар + I(Долар^2), 
                   correlation = corAR1(form = ~1, value = rho, fixed = TRUE),
                   data = model1_75_df)
summary(lm_Rsq5_gls) # Погано

best_bic <- best_models$top_bic
analyze_selected_models(all_models, model1_75_df, indices = best_bic, rho = rho)


# ======== ФІНАЛЬНА МОДЕЛЬ ========
names(final_models)

summary(lm_Rsq1)
summary(lm_BIC3)


# ==== 7.1) МУЛЬТИКОЛІНЕАРНІСТЬ-2. VIF, ЧИСЛО ОБУМОВЛЕНОСТІ ====
compute_vifs(lm_Rsq1)
compute_vifs(lm_Rsq3) # Трохи краще (логічно, бо змінних стало менше)
# Євро: 6.1 -> 6.4. Решта, крім Долара - майже те саме.
compute_vifs(lm_BIC3) 


# ==== 7.2) АНАЛІЗ ЗАЛИШКІВ І ВИЯВЛЕННЯ ВИКИДІВ ====
residuals_plot(lm_Rsq1)
residuals_plot(lm_Rsq3) # Рівномірніше
residuals_plot(lm_BIC3)

residuals_regressors_plot(lm_Rsq1, c(2, 4), type = "rstudent")
residuals_regressors_plot(lm_Rsq3, c(2, 4), type = "rstudent")
residuals_regressors_plot(lm_BIC3, c(2, 1), type = "rstudent")


# Відстань Кука.
cooks_dist(lm_Rsq1)
cooks_dist(lm_Rsq3)
cooks_dist(lm_BIC3)

df_fits(lm_Rsq1)
df_fits(lm_Rsq3)
df_fits(lm_BIC3)

df_betas(lm_Rsq1)
df_betas(lm_Rsq3)
df_betas(lm_BIC3)
# для lm_BIC3 впливових точок дуже мало



# ==== 7.3) ЛІНІЙНІСТЬ, ГОМОСКЕДАСТИЧНІСТЬ, НОРМАЛЬНІСТЬ ====

# ЛІНІЙНІСТЬ
plot(model_log1_75_Sq, which = 1)
plot(lm_Rsq1, which = 1)
plot(lm_Rsq3, which = 1)
# Різниці між двома моделями немає.
# LOESS лінія майже не змінилась
plot(lm_BIC3, which = 1)

av_plots(lm_Rsq1)
av_plots(lm_Rsq3) # Прибрали ІГР, і тепер всі графіки нормальні.
# Решта - те саме.
av_plots(lm_BIC3)


# ГОМОСКЕДАСТИЧНІСТЬ
plot(lm_Rsq1, which = 3) # Трохи краще.
plot(lm_Rsq3, which = 3)
# Покращення немає.
plot(lm_BIC3, which = 3) # Гірше.

bptest(lm_Rsq1) # p-value = 0.3187
bptest(lm_Rsq3) # p-value = 0.3431
# Було p-value = 0.2087.
bptest(lm_BIC3) # p-value = 0.7527


# ЛІНІЙНІСТЬ.
resettest(lm_Rsq1, power = 2) # p-value = 0.3529
resettest(lm_Rsq3, power = 2) # p-value = 0.741
resettest(lm_BIC3, power = 2) # p-value = 0.7762

crPlots(lm_Rsq1)
crPlots(lm_Rsq3) # Прибрали ІГР, і тепер всі графіки нормальні.
# Решта - те саме.
crPlots(lm_BIC3)

observe_transforms_x(lm_Rsq1, regressors = names(coef(lm_Rsq1))[-1])
# +square(ІГР) = 2.300244e - найкращий показник. Проте для збереження простоти моделі,
# залишимо як є.
observe_transforms_x(lm_Rsq3, regressors = names(coef(lm_Rsq3))[-1])
observe_transforms_x(lm_BIC3, regressors = names(coef(lm_BIC3))[-1])


# НОРМАЛЬНІСТЬ
qq_plot(lm_Rsq1) # Трохи гірше, ніж було.
shapiro.test(residuals(lm_Rsq1)) # p-value = 0.6991
qq_plot(lm_Rsq3)
shapiro.test(residuals(lm_Rsq3)) # p-value = 0.7598
qq_plot(lm_BIC3)
shapiro.test(residuals(lm_BIC3)) # p-value = 0.2112


# ==== 7.4) АВТОКОРЕЛЯЦІЯ ====
time_series_residuals(lm_Rsq1)
dwtest(lm_Rsq1) # DW = 0.80818, p-value = 8.422e-11.

time_series_residuals(lm_Rsq3)
dwtest(lm_Rsq3) # DW = 0.72441, p-value = 3.459e-12.

time_series_residuals(lm_BIC3)
dwtest(lm_BIC3) # DW = 0.50646, p-value = 9.334e-16

summary(lm_Rsq1)$coefficients
coeftest(lm_Rsq1, vcov = vcovHAC(lm_Rsq1))

summary(lm_Rsq3)$coefficients
coeftest(lm_Rsq3, vcov = vcovHAC(lm_Rsq3))
# Погано! Значущими залишаються тільки дві змінні:
# ЧисНасел     6.8728e-06  9.6461e-07  7.1250 1.043e-09 ***
# РівДолар    -3.7835e-02  3.7890e-03 -9.9856 9.437e-15 ***
# Це ті самі дві змінні, які були в моделі, що мала один із найвищих BIC.
# Отже, краще розглядати ще й ту модель.
# Повернімося назад, і проведімо діагнозтику і для неї.

# Зауважимо, що для моделі lm_Rsq1 стандартні похибки не є такими завищеними, тому
# вона, мабуть, є кращою за lm_Rsq3.

summary(lm_BIC3)$coefficients
coeftest(lm_BIC3, vcov = vcovHAC(lm_BIC3))
# Отже, для моделі з двома предикторами регресійні припущення виконуються.



# ======== 8) ЕФЕКТИ ВЗАЄМОДІЇ ========
# Працюємо із наступною моделлю:
summary(lm_Rsq1)
aic_base <- AIC(lm_Rsq1)
bic_base <- BIC(lm_Rsq1)

interactions_result <- observe_best_interactions(lm_Rsq1, model1_75_df, top_n = 10)

# 1. Долар × ІЦБ | ΔAIC = 5.62 | p = 0.0101 
inter_lm1 <- update(lm_Rsq1, . ~ . + Долар:ІЦБ)
summary(inter_lm1)
coeftest(inter_lm1, vcov = vcovHAC(inter_lm1)) # Долар^2 став не значущим.
inter_lm1 <- update(inter_lm1, . ~ . - I(Долар^2))
summary(inter_lm1)
coeftest(inter_lm1, vcov = vcovHAC(inter_lm1))
aic_base - AIC(inter_lm1)
bic_base - BIC(inter_lm1) # 3.894771
# inter_lm1 трохи краща


# 2. Долар / Євро | ΔAIC = 5.44 | p = 0.0110 
inter_lm2 <- update(lm_Rsq1, . ~ . + I(Долар / Євро))
summary(inter_lm2)
coeftest(inter_lm2, vcov = vcovHAC(inter_lm2))
bic_base - BIC(inter_lm2) # 3.167044
# inter_lm2 - краща, але складніша.


# 3. ІЦБ / Долар | ΔAIC = 5.15 | p = 0.0127 
inter_lm3 <- update(lm_Rsq1, . ~ . + I(ІЦБ / Долар))
summary(inter_lm3)
coeftest(inter_lm3, vcov = vcovHAC(inter_lm3))
bic_base - BIC(inter_lm3) # 2.876924
# Те саме, але ефект слабший.


# 4. РівДолар / Євро | ΔAIC = 4.83 | p = 0.0149 
inter_lm4 <- update(lm_Rsq1, . ~ . + I(РівДолар / Євро))
summary(inter_lm4)
coeftest(inter_lm4, vcov = vcovHAC(inter_lm4)) # ІЦБ - не значуща
inter_lm4 <- update(inter_lm4, . ~ . - ІЦБ)
summary(inter_lm4)
coeftest(inter_lm4, vcov = vcovHAC(inter_lm4))
aic_base - AIC(inter_lm4)
bic_base - BIC(inter_lm4) # 6.034939

assumptions_check(lm_BIC3) # DW = 0.50646
assumptions_check(lm_Rsq1) # DW = 0.80818
assumptions_check(inter_lm1) # DW = 0.94913
assumptions_check(inter_lm2) # DW = 1.0045
assumptions_check(inter_lm3) # DW = 0.97155
assumptions_check(inter_lm4) # DW = 1.0272
# Для всіх моделей виконуються всі припущення, крім некорельованості залишків.

final_models[["Долар × ІЦБ"]] <- inter_lm1
final_models[["Долар / Євро"]] <- inter_lm2
final_models[["ІЦБ / Долар"]] <- inter_lm3
final_models[["РівДолар / Євро"]] <- inter_lm4