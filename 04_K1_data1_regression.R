library(car)
library(lmtest)
library(MASS)
library(ggplot2)
library(sandwich)

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


time_series_residuals <- function(model){
  res_df <- data.frame(
    order = 1:length(residuals(model)),
    residuals = residuals(model)
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
model_temp <- lm(Ціна ~ ІГР, data = model1_75_df)
summary(model_temp)

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


# ==== 5.2.4) ЛІНІЙНІСТЬ, ГОМОСКЕДАСТИЧНІСТЬ ====

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




# Усі можливі регресії
all_models <- regsubsets(log(Ціна) ~ Євро + Долар + I(Долар^2) + ІФС + ІГР + ІЦБ + ЧисНасел + РівДолар,
                         data = model1_75_df,
                         nbest = 3)

plot(all_models, scale = "adjr2")

# Найкраща за adj R²
best_adjr2 <- which.max(summary(all_models)$adjr2)

# Найкраща за BIC
best_bic <- which.min(summary(all_models)$bic)

# Найкраща за Cp
best_cp <- which.min(abs(summary(all_models)$cp - apply(summary(all_models)$which, 1, sum)))

cat("Найкращі моделі (індекси):\n")
cat("Adj R²:", best_adjr2, "\n")
cat("BIC:   ", best_bic, "\n")
cat("Cp:    ", best_cp, "\n")


# ----- 38 спостережень -----
cor_matrix_38 <- model_cor_matrix(model_df_38[, -1], "")


# ----- 26 спостережень -----
cor_matrix_26 <- model_cor_matrix(model_df_26[, -1], "")

