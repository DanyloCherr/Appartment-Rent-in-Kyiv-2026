cor_matrix_75
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


