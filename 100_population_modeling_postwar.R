# ---- Функція для моделювання після 2022 року ----
# Ця функція використовує ті самі рівняння, що й довоєнна модель,
# але дозволяє змінювати параметри p21, p31, r1, r3 у часі
# для врахування впливу війни.

simulate_postwar_trajectory <- function(params, T_steps, n0, a_matrix,
                                        p21_vector, p31_vector, 
                                        r1_vector, r3_vector,
                                        inflow_real = NULL) {
  # params: вектор з параметрами (r2, n2_0, n3_0)
  # T_steps: кількість місяців для моделювання
  # n0: початкові умови (n1_0, n2_0, n3_0) на кінець 2021 року
  # a_matrix: матриця природного приросту
  # p21_vector, p31_vector, r1_vector, r3_vector: динамічні параметри
  # inflow_real: (опціонально) реальні дані про прибуття для калібрування
  
  r2 <- params["r2"]
  n2_0 <- params["n2_0"]
  n3_0 <- params["n3_0"]
  
  n <- matrix(0, nrow = T_steps, ncol = 3)
  n[1, ] <- c(n0[1], n2_0, n3_0)
  inflow_history <- numeric(T_steps)
  inflow_history[1] <- 0
  outflow_history <- numeric(T_steps)
  outflow_history[1] <- 0
  
  for(t in 1:(T_steps - 1)) {
    # Отримуємо динамічні параметри для кроку t
    p21_t <- p21_vector[min(t, length(p21_vector))]
    p31_t <- p31_vector[min(t, length(p31_vector))]
    r1_t <- r1_vector[min(t, length(r1_vector))]
    r3_t <- r3_vector[min(t, length(r3_vector))]
    
    # Будуємо матрицю переходів
    P <- build_P_matrix_3states(p21_t, p31_t)
    R <- c(r1_t, r2, r3_t)
    
    a_t <- a_matrix[min(t, nrow(a_matrix)), ]
    step_result <- model_step_3(n[t, ], R, P, a_t, delta = 1)
    
    n[t + 1, ] <- step_result$n_new
    inflow_history[t + 1] <- step_result$inflow_to_kyiv
    outflow_history[t + 1] <- step_result$outflow_from_kyiv
  }
  
  return(list(n1 = n[, 1], n2 = n[, 2], n3 = n[, 3], 
              inflow = inflow_history, outflow = outflow_history))
}

# ---- Приклад використання ----
# Визначаємо початкові умови на кінець 2021 року (з попередньої моделі)
n1_0_post <- tail(sim_result$n1, 1)  # останнє змодельоване значення Києва
n2_0_post <- tail(sim_result$n2, 1)  # останнє значення зовнішньої групи
n3_0_post <- tail(sim_result$n3, 1)  # останнє значення групи за кордоном

# Задаємо параметри для післявоєнного періоду (потребують калібрування)
# Тут наведено ПРИКЛАДОВІ значення, які потрібно замінити на реальні
postwar_params <- c(
  r2 = 0.01,      # інтенсивність внутрішньої міграції
  n2_0 = n2_0_post,
  n3_0 = n3_0_post
)

# Динамічні параметри для післявоєнного періоду (потребують даних)
# Наприклад, можна використати тренди з довоєнного періоду
postwar_months <- 36  # 2022-2024 роки
postwar_p21 <- rep(p21_dynamic[length(p21_dynamic)], postwar_months)  # стабілізація
postwar_p31 <- rep(p31_dynamic[length(p31_dynamic)], postwar_months)
postwar_r1 <- rep(r1_dynamic[length(r1_dynamic)], postwar_months)
postwar_r3 <- rep(r3_dynamic[length(r3_dynamic)], postwar_months)

a_matrix_post <- zeros


# Виклик функції
postwar_result <- simulate_postwar_trajectory(
  params = postwar_params,
  T_steps = postwar_months,
  n0 = c(n1_0_post, n2_0_post, n3_0_post),
  a_matrix = a_matrix_post,  # потребує даних за 2022-2024
  p21_vector = postwar_p21,
  p31_vector = postwar_p31,
  r1_vector = postwar_r1,
  r3_vector = postwar_r3
)

print(postwar_result)

inflow_postwar <- postwar_result$inflow

start_date_post <- as.Date("2022-01-01")
end_date_post <- as.Date("2024-12-01")
postwar_months <- seq(start_date_post, end_date_post, by = "month")


# Створюємо таблицю
KYIV_INFLOW_POSTWAR <- data.frame(
  Дата = postwar_months,
  Притік = inflow_postwar
)


KYIV_INFLOW_POSTWAR <- KYIV_INFLOW_POSTWAR[-1, ]

temp_df_postwar <- merge(model_df_2, KYIV_INFLOW_POSTWAR, by = "Дата")

model_cor_matrix(temp_df_postwar[, -1], "")
