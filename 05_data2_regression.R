sapply(data2, nrow)
data2 <- sort_by_rows(data2)
sapply(data2, nrow)
data2[1:7] <- aggregate_to_monthly_all(data2[1:7])
sapply(data2, nrow)
print_heads(data2, 3)

# Щомісячні спостереження:
# EUR                       USD                   SGLROOM                   DBLROOM 
# 54                        53                        53                        53 
# MULTIROOM                   TRPROOM                  FS_INDEX                 GPR_INDEX 
# 53                        53                        47                        53 
# DOL_LEVEL_AVG       CONSUMER_CONFIDENCE            PRICE_INDICIES             NET_UAH_LOANS 
# 47                        47                        46                        46 

# HH_DEPOSITS_ANNUAL_GROWTH                   
#              22                              
# Якщо ця змінна буде значущою, то можемо ще сильніше звузити вибірку!

# Решта змінних випадає через малу кількість спостережень.

predictors2 <- c("EUR", "USD", "FS_INDEX", "GPR_INDEX", 
                "PRICE_INDICIES", "CONSUMER_CONFIDENCE", "DOL_LEVEL_AVG",
                "NET_UAH_LOANS") # Спільні предиктори для всіх К


# Щомісячні дані (46 спостережень)
df_predictors2 <- data2$EUR

for(name in predictors2[-1]){
  df_predictors2 <- merge(df_predictors2, data2[[name]], by = "Дата", all = FALSE)
}

rownames(df_predictors2) <- NULL


# Оскільки спостереження є щомісячними, то в якості часової змінної візьмемо номер
# місяця від початку вибірки.
df_predictors2$Ч <- 1:nrow(df_predictors2)
head(df_predictors2)

model_df_2 <- merge(df_predictors2, data2$MULTIROOM, by = "Дата", all = FALSE)
head(model_df_2)



