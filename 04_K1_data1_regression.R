library(car)

# Схема побудови моделі.
# 1) МУЛЬТИКОЛІНЕАРНІСТЬ-1. КОРЕЛЯЦІЙНА МАТРИЦЯ.
# 2) ПОВНА МОДЕЛЬ.
# 3) МУЛЬТИКОЛІНЕАРНІСТЬ-2. VIF, ЧИСЛО ОБУМОВЛЕНОСТІ.
# 4) АНАЛІЗ ЗАЛИШКІВ І РОБОТА З ВИКИДАМИ.
# 5) ФУНКЦІОНАЛЬНІ ПЕРЕТВОРЕННЯ.
# 6) Застосувати всі можливі регресії, якщо це можливо.
# 7) Відібрати найкращі моделі за декількома критеріями.
# 8) Додати interactions.

# Побудуємо модель на основі цін для однокімнатних квартир.


unit_length_scale <- function(x){ # Передаємо один стовпець
  centered <- x - mean(x)
  result <- centered / sqrt(sum(centered^2))
  return(result)
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
  dists <- cooks.distance(model_full1_75)
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



avPlots(model_full1_75, 
        col = "steelblue", 
        pch = 19,
        cex = 0.7,
        lwd = 1.5,
        id = list(n = 5, col = "red"))

# ІЦБ. Патерн воронки, що розкривається. Можливо, потрібне функціональне перетворення.

qq_plot(model_full1_75)


# Важливо пам'ятати, що e = (I-H)y, тобто для спостережень, що знаходяться
# далі від центроїда, залишки можуть бути заниженими, оскільки для них h_ii -> 1.




# ----- 38 спостережень -----
cor_matrix_38 <- model_cor_matrix(model_df_38[, -1], "")


# ----- 26 спостережень -----
cor_matrix_26 <- model_cor_matrix(model_df_26[, -1], "")
