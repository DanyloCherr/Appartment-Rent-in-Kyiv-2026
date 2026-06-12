library(optimx)

# k = 2 - дві групи (чисельність населення Києва 1 та чисельність поза Києвом 2)
#   n(t + Δ) = n(t) - RΔn(t) + P'RΔn(t) + aΔ
#   n - вектор чисельності у групах[n1, n2]
#   R - діагональна матриця з r1, r2
#   P - матриця переходів
#   a - вектор зовнішніх змін
#   Δ - часовий крок

build_P_matrix_3states <- function(p21, p31) {
  P <- matrix(0, nrow = 3, ncol = 3)
  P[1, 1] <- 0
  P[1, 2] <- 1
  P[1, 3] <- 0
  
  P[2, 1] <- p21
  P[2, 2] <- 0
  P[2, 3] <- 1 - p21
  
  # P[3, 1] <- p31
  # P[3, 2] <- p32
  # P[3, 3] <- 1 - p32 - p31
  
  P[3, 1] <- p31
  P[3, 2] <- 1 - p31
  P[3, 3] <- 0
  
  return(P)
}


model_step_3 <- function(n, R, P, a, delta = 1) {
  R_matrix <- diag(R)
  outflow <- R_matrix %*% n * delta
  inflow <- t(P) %*% outflow
  n_new <- n - outflow + inflow + a * delta
  n_new[n_new < 0] <- 0
  return(list(n_new = n_new, inflow_to_kyiv = inflow[1], outflow_from_kyiv = outflow[1]))
}


simulate_trajectory_3 <- function(params, T_steps, n1_0, a_matrix, 
                                  p21_vector, p31_vector, r1_vector, r3_vector) {
  # r1 <- params["r1"]
  r2 <- params["r2"]
  # r3 <- params["r3"]
  # p21 <- params["p21"]
  p31 <- params["p31"]
  # p32 <- params["p32"]
  n2_0 <- params["n2_0"]
  n3_0 <- params["n3_0"]
  
  n <- matrix(0, nrow = T_steps, ncol = 3)
  n[1, ] <- c(n1_0, n2_0, n3_0)
  inflow_history <- numeric(T_steps)
  inflow_history[1] <- 0
  outflow_history <- numeric(T_steps)
  outflow_history[1] <- 0
  
  for(t in 1:(T_steps - 1)) {
    p21_t <- p21_vector[t]
    #p31_t <- p31_vector[t]
    #P <- build_P_matrix_3states(p21_t, p31_t)
    P <- build_P_matrix_3states(p21_t, p31)
    
    r1_t <- r1_vector[t]
    r3_t <- r3_vector[t]
    R <- c(r1_t, r2, r3_t)
    
    a_t <- a_matrix[t, ]
    step_result <- model_step_3(n[t, ], R, P, a_t, delta = 1)
    n[t + 1, ] <- step_result$n_new
    inflow_history[t + 1] <- step_result$inflow_to_kyiv
    outflow_history[t + 1] <- step_result$outflow_from_kyiv
  }
  
  return(list(n1 = n[, 1], n2 = n[, 2], n3 = n[, 3], inflow = inflow_history, outflow = outflow_history))
}


loss_function_3 <- function(params, n_real, a_matrix, p21_vector, p31_vector, r1_vector, r3_vector) {
  names(params) <- c("r2", "p31", "n2_0", "n3_0")
  
  n1_0 <- n_real[1]
  
  result <- tryCatch({
    sim <- simulate_trajectory_3(params, length(n_real), n1_0, a_matrix, 
                                 p21_vector, p31_vector, r1_vector, r3_vector)
    sim$n1
  }, error = function(e) {
    return(rep(NA, length(n_real)))
  })
  
  if (any(is.na(result)) || any(is.nan(result))) {
    return(1e10)
  }
  
  mse <- mean((n_real - result)^2)
  return(mse)
}


# Функція втрат, яка враховує офіційні дані
loss_function_with_real_inflow <- function(params, n_real, real_inflow, a_matrix,
                                           p21_vector, p31_vector, r1_vector, r3_vector){
  names(params) <- c("r2", "p31", "n2_0", "n3_0")
  n1_0 <- n_real[1]
  
  if (any(params < 0) || params["r2"] > 0.1) {
    return(1e10)
  }
  
  sim <- tryCatch({
    simulate_trajectory_3(params, length(n_real), n1_0, a_matrix, 
                          p21_vector, p31_vector, r1_vector, r3_vector)
  }, error = function(e) {
    return(NULL)
  })
  
  if (is.null(sim) || any(is.na(sim$n1))) {
    return(1e10)
  }
  
  # MSE для чисельності населення
  mse_pop <- mean((n_real - sim$n1)^2, na.rm = TRUE)
  
  mse_inflow <- mean((real_inflow - sim$inflow)^2, na.rm = TRUE)
  
  return(mse_pop + mse_inflow)
}


POPULATION_VECTOR <- POPULATION[-c(1, 75, 76), 2]  # грудень 2015 - грудень 2021

POPULATION_VECTOR <- POPULATION_VECTOR / 1000000


KYIV_INFLOW_REAL <- import(get_csv_name("Кількість прибулих. Київ")) # Для порівняння?
KYIV_INFLOW_REAL <- KYIV_INFLOW_REAL[, c(4, 5)]
KYIV_INFLOW_REAL[, 1] <- gsub("M", "", KYIV_INFLOW_REAL[, 1])
KYIV_INFLOW_REAL[, 1] <- as.Date(paste0(KYIV_INFLOW_REAL[, 1], "-01"), "%Y-%m-%d")
colnames(KYIV_INFLOW_REAL) <- c("Дата", "Кількість прибулих")


K_INFLOW_COEF_REAL <- import(get_csv_name("Коефіцієнт прибуття. Київ"))
K_INFLOW_COEF_REAL <- K_INFLOW_COEF_REAL[1:20, c(4, 5)]
K_INFLOW_COEF_REAL[, 1] <- gsub('"', '', K_INFLOW_COEF_REAL[, 1])
K_INFLOW_COEF_REAL[, 2] <- as.numeric(gsub('"', '', K_INFLOW_COEF_REAL[, 2]))
K_INFLOW_COEF_REAL[, 1] <- as.Date(paste0(K_INFLOW_COEF_REAL[, 1], "-12-01"), "%Y-%m-%d")
colnames(K_INFLOW_COEF_REAL) <- c("Дата", "Коефіцієнт прибуття")

K_OUTFLOW_COEF_REAL <- import(get_csv_name("Коефіцієнт вибуття. Київ"))
K_OUTFLOW_COEF_REAL <- K_OUTFLOW_COEF_REAL[, c(4, 5)]
K_OUTFLOW_COEF_REAL[, 1] <- as.Date(paste0(K_OUTFLOW_COEF_REAL[, 1], "-12-01"), "%Y-%m-%d")
colnames(K_OUTFLOW_COEF_REAL) <- c("Дата", "Коефіцієнт вибуття")


U_INFLOW_COEF_REAL  <- import(get_csv_name("Коефіцієнт прибуття. Україна"))
U_INFLOW_COEF_REAL <- U_INFLOW_COEF_REAL[73:92, c(3, 4)]
rownames(U_INFLOW_COEF_REAL) <- NULL
U_INFLOW_COEF_REAL[, 1] <- as.Date(paste0(U_INFLOW_COEF_REAL[, 1], "-12-01"), "%Y-%m-%d")
colnames(U_INFLOW_COEF_REAL) <- c("Дата", "Коефіцієнт прибуття")

U_OUTFLOW_COEF_REAL <- import(get_csv_name("Коефіцієнт вибуття. Україна"))
U_OUTFLOW_COEF_REAL <- U_OUTFLOW_COEF_REAL[, c(4, 5)]
U_OUTFLOW_COEF_REAL[, 1] <- as.Date(paste0(U_OUTFLOW_COEF_REAL[, 1], "-12-01"), "%Y-%m-%d")
colnames(U_OUTFLOW_COEF_REAL) <- c("Дата", "Коефіцієнт вибуття")


K_BIRTHRATE <- import(get_csv_name("Народжуваність. Київ"))
K_BIRTHRATE <- K_BIRTHRATE[1:205, c(4, 5)]
K_BIRTHRATE[, 1] <- gsub("M", "", K_BIRTHRATE[, 1])
K_BIRTHRATE[, 1] <- as.Date(paste0(K_BIRTHRATE[, 1], "-01"), "%Y-%m-%d")
colnames(K_BIRTHRATE) <- c("Дата", "Кількість живонароджених")

K_MORTRATE <- import(get_csv_name("Смертність. Київ"))
K_MORTRATE <- K_MORTRATE[-(1:33), c(3, 4)]
K_MORTRATE <- K_MORTRATE[1:205, ]
rownames(K_MORTRATE) <- NULL
K_MORTRATE[, 1] <- gsub("M", "", K_MORTRATE[, 1])
K_MORTRATE[, 1] <- as.Date(paste0(K_MORTRATE[, 1], "-01-01"), "%Y-%m-%d")
colnames(K_MORTRATE) <- c("Дата", "Кількість померлих")

K_NAT_INCREASE <- merge(K_BIRTHRATE, K_MORTRATE, by = "Дата", all = FALSE)
K_NAT_INCREASE$`Природний приріст` <- K_NAT_INCREASE$`Кількість живонароджених` - K_NAT_INCREASE$`Кількість померлих`
K_NAT_INCREASE <- K_NAT_INCREASE[, c(1, 4)]


U_BIRTHRATE <- import(get_csv_name("Народжуваність. Україна"))
U_BIRTHRATE <- U_BIRTHRATE[433:637, c(4, 5)]
rownames(U_BIRTHRATE) <- NULL
U_BIRTHRATE[, 1] <- gsub("M", "", U_BIRTHRATE[, 1])
U_BIRTHRATE[, 1] <- as.Date(paste0(U_BIRTHRATE[, 1], "-01"), "%Y-%m-%d")
colnames(U_BIRTHRATE) <- c("Дата", "Кількість живонароджених")

U_MORTRATE <- import(get_csv_name("Смертність. Україна"))
U_MORTRATE <- U_MORTRATE[1:205, c(4, 5)]
U_MORTRATE[, 1] <- gsub("M", "", U_MORTRATE[, 1])
U_MORTRATE[, 1] <- as.Date(paste0(U_MORTRATE[, 1], "-01"), "%Y-%m-%d")
colnames(U_MORTRATE) <- c("Дата", "Кількість померлих")

U_NAT_INCREASE <- merge(U_BIRTHRATE, U_MORTRATE, by = "Дата", all = FALSE)
U_NAT_INCREASE$`Природний приріст` <- U_NAT_INCREASE$`Кількість живонароджених` - U_NAT_INCREASE$`Кількість померлих`
U_NAT_INCREASE <- U_NAT_INCREASE[, c(1, 4)]


start_date <- as.Date("2015-12-01")
end_date <- as.Date("2021-12-01")

K_NAT_INCREASE_filt <- K_NAT_INCREASE %>%
  filter(Дата >= start_date & Дата <= end_date) %>%
  arrange(Дата)

U_NAT_INCREASE_filt <- U_NAT_INCREASE %>%
  filter(Дата >= start_date & Дата <= end_date) %>%
  arrange(Дата)

all(K_NAT_INCREASE_filt$Дата == U_NAT_INCREASE_filt$Дата)

# Розрахунок природного приросту для зовнішньої групи (Україна без Києва)
NAT_INCREASE <- data.frame(
  Дата = K_NAT_INCREASE_filt$Дата,
  kyiv = K_NAT_INCREASE_filt$`Природний приріст` / 1e6,
  ukraine_total = U_NAT_INCREASE_filt$`Природний приріст` / 1e6,
  external = (U_NAT_INCREASE_filt$`Природний приріст` - K_NAT_INCREASE_filt$`Природний приріст`) / 1e6
)


KYIV_INFLOW_REAL_filt <- KYIV_INFLOW_REAL %>%
  filter(Дата >= start_date & Дата <= end_date) %>%
  arrange(Дата)

nrow(KYIV_INFLOW_REAL_filt)

real_inflow <- KYIV_INFLOW_REAL_filt$`Кількість прибулих`

real_inflow <- real_inflow / 1e6


a_matrix <- matrix(0, nrow = nrow(NAT_INCREASE), ncol = 3)
a_matrix[, 1] <- NAT_INCREASE$kyiv
a_matrix[, 2] <- NAT_INCREASE$external
a_matrix[, 3] <- 0


# ---- ДИНАМІЧНИЙ ПРОЦЕС ----
# Будуємо динамічну систему: p21 = p21(t) 
# Для цього використаймо наявні (щорічні) дані про коеф. прибуття до Києва.
# При чому p31 = const, p32 = const, оскільки ми не маємо даних про прибуття
# з-за кордону, а також ці величини мають бути досить малими (основний
# рушій притоку до Києва - це p21)
# Використаймо лінійну інтерполяцію для отримання щомісячних даних.
all_months <- seq(start_date, end_date, by = "month")

interpolated <- approx(
  x = K_INFLOW_COEF_REAL$Дата,
  y = K_INFLOW_COEF_REAL$`Коефіцієнт прибуття`,
  xout = all_months,
  method = "linear"
)

k_inflow_coef_monthly <- interpolated$y

# p21_dynamic <- k_inflow_coef_monthly / max(k_inflow_coef_monthly, na.rm = TRUE) * 0.15
# Нормалізуємо коефіцієнт до [0, x], x < 1
p21_dynamic <- k_inflow_coef_monthly / 1e4 # Сильна кореляція.
summary(p21_dynamic)


# p21 = p21(t) - аналогічно. UPD. У нас все вибуле з Києва населення переходить у 
# стан 2, тому краще тут динамічним зробити r1.
interpolated <- approx(
  x = K_OUTFLOW_COEF_REAL$Дата,
  y = K_OUTFLOW_COEF_REAL$`Коефіцієнт вибуття`,
  xout = all_months,
  method = "linear"
)

k_outflow_coef_monthly <- interpolated$y

# r1_dynamic <- k_outflow_coef_monthly / max(k_outflow_coef_monthly, na.rm = TRUE) * 0.01
# Із x = 0.01 поки що працює найкраще.
r1_dynamic <- k_outflow_coef_monthly / 1e4 # Сильна кореляція.
summary(r1_dynamic)


interpolated <- approx(
  x = U_INFLOW_COEF_REAL$Дата,
  y = U_INFLOW_COEF_REAL$`Коефіцієнт прибуття`,
  xout = all_months,
  method = "linear"
)

u_outflow_coef_monthly <- interpolated$y

# r3_dynamic <- u_outflow_coef_monthly / max(u_outflow_coef_monthly, na.rm = TRUE) * 0.02
r3_dynamic <- u_outflow_coef_monthly / 1e4
summary(r3_dynamic)


# UPD: не використовується.
k_inflow_coef_monthly <- interpolated$y
p31_dynamic <- k_inflow_coef_monthly / max(k_inflow_coef_monthly, na.rm = TRUE) * 0.15
# Нормалізуємо коефіцієнт до [0, x], x < 1
# p31_dynamic <- k_inflow_coef_monthly / 1e4 # Сильна кореляція.
# range(p31_dynamic)
summary(p31_dynamic)



# ---- Початкові умови для 3 станів ----
n1_0 <- POPULATION_VECTOR[1]  # 2.863074 млн
n2_0 <- 39.74   # млн, Україна без Києва
n3_0 <- 6.0     # млн, українці за кордоном (оцінка) (5 млн дає такий же результат)


# init_params_3 <- c(
#   r1 = 0.005091697,
#   r2 = 0.005688598 ,
#   r3 = 0.0001,
#   p21 = 0.08488607 ,
#   p31 = 0.01,
#   p32 = 0.05,
#   n2_0 = n2_0,
#   n3_0 = n3_0
# )
# 
# lower_bounds_3 <- c(r1 = 0, r2 = 0, r3 = 0, p21 = 0, p31 = 0,
#                     p32 = 0, n2_0 = 30, n3_0 = 3)
# upper_bounds_3 <- c(r1 = 0.01, r2 = 0.01, r3 = 0.01, p21 = 0.2, p31 = 0.07,
#                     p32 = 0.1, n2_0 = 45, n3_0 = 10)
# 
# result <- optimx(
#   par = init_params_3,
#   fn = loss_function_with_real_inflow, # враховуємо ще й офіційні дані
#   lower = lower_bounds_3,
#   upper = upper_bounds_3,
#   method = "L-BFGS-B",
#   n_real = POPULATION_VECTOR,
#   a_matrix = a_matrix,
#   real_inflow = real_inflow
# )

init_params_3 <- c(
  r2 = 0.005688598,
  p31 = 0.07,
  n2_0 = n2_0,
  n3_0 = n3_0
)

lower_bounds_3 <- c(r2 = 0, p31 = 0, n2_0 = 30, n3_0 = 3)

upper_bounds_3 <- c(r2 = 0.01, r3 = 0.01, p31 = 0.07,
                    p32 = 0.1, n2_0 = 45, n3_0 = 10)
#                  r2   r3  p31        p32     n2_0     n3_0       value fevals gevals niter convcode  kkt1  kkt2 xtime
# L-BFGS-B 0.00466441 0.01 0.07 0.05373379 39.74439 6.000078 0.004924279     51     51    NA        0 FALSE FALSE  0.89

upper_bounds_3 <- c(r2 = 0.01, r3 = 0.01, p31 = 0.1,
                    p32 = 0.1, n2_0 = 45, n3_0 = 10)
#                   r2   r3 p31 p32     n2_0     n3_0       value fevals gevals niter convcode  kkt1  kkt2 xtime
# L-BFGS-B 0.003912488 0.01 0.1   0 39.74369 5.979817 0.003900309     99     99    NA       52 FALSE FALSE  1.72
# Але підозріло, що p32 = 0
# cor є [0.47, 0.71]

upper_bounds_3 <- c(r2 = 0.01, r3 = 0.1, p31 = 0.2, n2_0 = 45, n3_0 = 10)
#                   r2         r3        p31        p32     n2_0     n3_0       value fevals gevals niter convcode  kkt1
# L-BFGS-B 0.004133054 0.04769571 0.01945759 0.04839612 39.74223 5.999867 0.004319697     36     36    NA        0 FALSE
# kkt2 xtime
# L-BFGS-B FALSE  0.57
# cor є [0.4, 0.62]
# Більш реалістичні параметри.

upper_bounds_3 <- c(r2 = 0.03, p31 = 0.2, n2_0 = 45, n3_0 = 10) # 0.002890668     


# ---- Калібрування параметрів ----
result <- optimx(
  par = init_params_3,
  fn = loss_function_with_real_inflow, 
  lower = lower_bounds_3,
  upper = upper_bounds_3,
  method = "L-BFGS-B",
  n_real = POPULATION_VECTOR,
  a_matrix = a_matrix,
  p21_vector = p21_dynamic,
  p31_vector = p31_dynamic,
  r1_vector = r1_dynamic,
  r3_vector = r3_dynamic,
  real_inflow = real_inflow
)


print(result)


best_params_3 <- as.numeric(result[1, 1:7])
names(best_params_3) <- names(init_params_3)


zeros <- matrix(0, nrow = nrow(NAT_INCREASE), ncol = 3)
sim_result <- simulate_trajectory_3(best_params_3, length(POPULATION_VECTOR), n1_0, 
                                    a_matrix = a_matrix, p21_vector = p21_dynamic,
                                    p31_vector = p31_dynamic, r1_vector = r1_dynamic, 
                                    r3_vector = r3_dynamic)

# ---- Візуалізація ----
comparison_data <- data.frame(
  time = 1:length(POPULATION_VECTOR),
  real = POPULATION_VECTOR,
  simulated = sim_result$n1
)

ggplot(comparison_data, aes(x = time)) +
  geom_line(aes(y = real, color = "Real"), linewidth = 1) +
  geom_line(aes(y = simulated, color = "Simulated"), linewidth = 1, linetype = "dashed") +
  labs(
    title = "Довоєнна динаміка чисельності населення Києва",
    x = "Місяці (з грудня 2015)",
    y = "Чисельність населення, млн",
    color = ""
  ) +
  theme_minimal()

sim_population <- sim_result[[1]] * 1e6
sim_population
sim_result[[2]]
sim_result[[3]]
inflow <- sim_result[[4]] * 1e6
inflow
summary(inflow)
outflow <- sim_result[[5]] * 1e6
outflow

KYIV_INFLOW <- data.frame(
  Дата = POPULATION[-c(1, 75, 76), 1],
  Притік = inflow
)

KYIV_INFLOW <- KYIV_INFLOW[-1, ]
ggplot(KYIV_INFLOW, aes(x = Дата, y = Притік)) +
  geom_point(color = "steelblue", size = 1.5) +
  labs(
    title = "Змодельований приток населення до Києва",
    subtitle = "Щомісячна динаміка (грудень 2015 - грудень 2021)",
    x = "Дата",
    y = "Приток населення (осіб/міс)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


POPULATION_SIM <- data.frame(
  Дата = KYIV_INFLOW$Дата,
  МодЧисел = sim_population[-1]
)

KYIV_OUTFLOW <- data.frame(
  Дата = KYIV_INFLOW$Дата,
  Відтік = outflow[-1]
)


temp_df <- merge(model_df_75, KYIV_INFLOW, by = "Дата")
temp_df <- merge(temp_df, KYIV_OUTFLOW, by = "Дата")
temp_df <- merge(temp_df, POPULATION_SIM, by = "Дата")

model_cor_matrix(temp_df[, -1], "")


temp_df <- merge(temp_df, KYIV_INFLOW_REAL_filt, by = "Дата")
model_cor_matrix(temp_df[, -1], "")
#### ІДЕЯ #### 
#### Можна в якості еталону взяти реальну кількість прибулих, і по ній змоделювати те, що
#### ми хотіли.


# ==== Перевірка впливу в регресійній моделі ====
temp_df <- temp_df[, !names(temp_df) %in% c("Ч", "ЧисНасел", "РівДолар", "Відтік", "МодЧисел")]
temp_df <- upd_model_df(temp_df, room_num = 1)
model_cor_matrix(temp_df, "")

temp_model <- lm(К1 ~ Притік, data = temp_df)

model_full <- lm(К1 ~ ., data = temp_df)
summary(model_full)

# Кроковий відбір (в обидва боки)
model_step <- step(model_full, direction = "both", trace = 1)
summary(model_step)


# НАЙКРАЩІ РЕЗУЛЬТАТИ

#            r2       p31     n2_0     n3_0       value fevals gevals niter convcode  kkt1  kkt2 xtime
# L-BFGS-B 0.01 0.1630197 39.47748 9.075359 0.002536677     27     27    NA        0 FALSE FALSE  0.28


