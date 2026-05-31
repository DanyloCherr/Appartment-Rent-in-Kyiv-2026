library(corrplot)


print_heads <- function(df, n){
  for(df_name in names(df)){
    cat("\n", df_name, "\n")
    current_df <- df[[df_name]]
    print(head(current_df, n))
  }
}


sort_by_rows <- function(dfs_list){
  sorted_list <- dfs_list[order(sapply(dfs_list, nrow), decreasing = TRUE)]
  return(sorted_list)
}


aggregate_to_monthly <- function(df){
  df <- df %>% 
    mutate(month = floor_date(df$Дата, "month")) %>%
    group_by(month) %>%
    summarise(across(-1, ~mean(.x, na.rm = TRUE))) %>%
    rename(Дата = month)
  
  return(as.data.frame(df))
}


aggregate_to_monthly_all <- function(dfs_list){
  for(df_name in names(dfs_list)){
    dfs_list[[df_name]] <- aggregate_to_monthly(dfs_list[[df_name]])
  }
  return(dfs_list)
}


aggregate_to_quarterly <- function(df){
  df <- df %>% 
    mutate(quarter = floor_date(Дата, "quarter")) %>%
    group_by(quarter) %>%
    summarise(across(-1, ~ mean(.x, na.rm = TRUE))) %>%
    rename(Дата = quarter)
  
  return(as.data.frame(df))
}


aggregate_to_quarterly_all <- function(dfs_list) {
  for (df_name in names(dfs_list)) {
    dfs_list[[df_name]] <- aggregate_to_quarterly(dfs_list[[df_name]])
  }
  return(dfs_list)
}


model_cor_matrix <- function(model_df, title_param){
  # title_param - яку змінну було додано до даних
  cor_matrix <- cor(model_df, use = "complete.obs")
  corrplot(cor_matrix, method = "circle", type = "upper", 
           addCoef.col = "black", tl.col = "black",
           mar = c(2, 0, 0, 0))
  
  mtext(title_param, side = 1, line = 2, cex = 1.2)
}


# Таблиць із щоденними даними дуже мало, тому агрегуємо їх до щомісячних:
# Ціни, USD, EUR, ІФС, агрегуємо перші 7 таблиць

data1[1:7] <- aggregate_to_monthly_all(data1[1:7])
data1 <- sort_by_rows(data1)
print_heads(data1, 3)
sapply(data1, nrow)


# Таблиці із щомісячними спостереженнями:
# DOLLARIZATION_LEVEL, EUR, USD, MULTIROOM, SGLROOM, DBLROOM, TRPROOM, FS_INDEX,
# GPR_INDEX, CP_INDEX, POPULATION, NET_UAH_LOANS, CONSUMER_CONFIDENCE              
# 
# Таблиці із щоквартальними спостереженнями:
# SECOND_HAND_HPRICE_INDEX, NEW_HPRICE_INDEX, HH_DEPOSITS_ANNUAL_GROWTH  

# Отже, спочатку працюємо із першою групою предикторів.


### NET_UAH_LOANS - 38 спостережень, а CONSUMER_CONFIDENCE - 26, решта - 75. 
# Тому спочатку працюємо із усіма іншими таблицями, крім цих.

### SECOND_HAND_HPRICE_INDEX, NEW_HPRICE_INDEX - 20 спостережень,
# HH_DEPOSITS_ANNUAL_GROWTH - 13.


# ===== Таблиці предикторів =====
predictors <- c("EUR", "USD", "FS_INDEX", "GPR_INDEX", 
                "CP_INDEX", "POPULATION", "DOLLARIZATION_LEVEL") # Спільні предиктори для всіх К

# Щомісячні дані
df_predictors75 <- data1$EUR

for(name in predictors[-1]){
  df_predictors75 <- merge(df_predictors75, data1[[name]], by = "Дата", all = TRUE)
}
df_predictors75 <- na.omit(df_predictors75)

df_predictors38 <- merge(df_predictors75, data1$NET_UAH_LOANS, by = "Дата", all = TRUE)
df_predictors26 <- merge(df_predictors38, data1$CONSUMER_CONFIDENCE, by = "Дата", all = TRUE)

df_predictors38 <- na.omit(df_predictors38)
df_predictors26 <- na.omit(df_predictors26)


rownames(df_predictors75) <- NULL
rownames(df_predictors38) <- NULL
rownames(df_predictors26) <- NULL


# Оскільки спостереження є щомісячними, то в якості часової змінної візьмемо номер
# місяця від початку вибірки.
df_predictors75$Ч <- 1:nrow(df_predictors75)
head(df_predictors75)

df_predictors38$Ч <- 1:nrow(df_predictors38)
head(df_predictors38)

df_predictors26$Ч <- 1:nrow(df_predictors26)
head(df_predictors26)


# ===== 75 спостережень =====
model_df_75 <- merge(df_predictors75, data1$MULTIROOM, by = "Дата", all = TRUE)

# ===== 38 спостережень =====
model_df_38 <- merge(df_predictors38, data1$MULTIROOM, by = "Дата", all = FALSE)

# ===== 26 спостережень =====
model_df_26 <- merge(df_predictors26, data1$MULTIROOM, by = "Дата", all = FALSE)



# Щоквартальні дані
data1q <- aggregate_to_quarterly_all(data1)
data1q <- sort_by_rows(data1q)
print_heads(data1q, 3)
sapply(data1q, nrow)

predictorsq <- c(predictors, "SECOND_HAND_HPRICE_INDEX", "NEW_HPRICE_INDEX")

df_predictors20q <- data1q$EUR

for(name in predictorsq[-1]){
  df_predictors20q <- merge(df_predictors20q, data1q[[name]], by = "Дата")
}

df_predictors13q <- merge(df_predictors20q, data1q$NET_UAH_LOANS, by = "Дата")
df_predictors13q <- merge(df_predictors13q, data1q$HH_DEPOSITS_ANNUAL_GROWTH, by = "Дата")

df_predictors20q$Ч <- 1:nrow(df_predictors20q)
head(df_predictors20q)

df_predictors13q$Ч <- 1:nrow(df_predictors13q)
head(df_predictors13q)

# ===== 20 спостережень =====
model_df_20q <- merge(df_predictors20q, data1q$MULTIROOM, by = "Дата")

# ===== 13 спостережень ===== 
# Дуже мало. Утримаємося.
model_df_13q <- merge(df_predictors13q, data1q$MULTIROOM, by = "Дата")

