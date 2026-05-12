library(ggplot2)
library(patchwork)


combined_plots <- function(tables) {
  for (table_name in names(tables)) {
    current_df <- tables[[table_name]]
    
    p1 <- ggplot(current_df, aes(x = current_df[[1]], y = current_df[[2]])) +
      geom_path(color = "steelblue") + 
      labs(title = table_name,
           x = "Дата",
           y = colnames(current_df)[2]) +
      theme_minimal()
    
    obs_num <- nrow(current_df)
    if(obs_num < 70){
      p1 <- p1 + geom_point(color = "red", size = 1.25)
    }
    
    
    
    p2 <- ggplot(current_df, aes(y = current_df[[2]])) +
      geom_boxplot(fill = "steelblue", alpha = 0.7, staplewidth = 0.5) +
      scale_y_continuous(position = "right") +
      theme_minimal() +
      theme(
        axis.title.y = element_blank(),
        axis.text.x = element_blank()
      )
    
    combined <- p1 + p2
    
    print(combined)
  }
}


split_df_byDate <- function(df, date = "2022-02-01"){
  date <- as.Date(date)
  
  result <- list(
  before = df[df$Дата < date, ],
  after = df[df$Дата >= date, ]
  )
  
  return(result)
}

cat("Всього факторів:", length(all_tables))
combined_plots(all_tables[names(all_tables) != "MULTIROOM"])


# =======ПОТЕНЦІЙНІ ПРОБЛЕМИ=======
# Із графіків видно, що всі дані варто розділити: до лютого 2022 року та після.

data1 <- list()
data2 <- list()

for(table_name in names(all_tables)){
  split_data <- split_df_byDate(all_tables[[table_name]])
  if(nrow(split_data$before) >= 10){ # Чи нормально взяти за мінімум 10?
    data1[[table_name]] <- split_data$before
  }
  if(nrow(split_data$after) >= 10){
    data2[[table_name]] <- split_data$after
  }
}


# =====data1=====
cat("Змінні, що залишились в data1:", "\n", paste(names(data1), collapse = "\n"), 
    "\n", "Всього:", length(data1))


combined_plots(data1[names(data1) != "MULTIROOM"])


# -------USD-------
# Із 2022-02-24 уряд зафіксував ціну долара на рівні 29.25;
# із 2022-07-21 - 36.56 (до 2023-10-03 включно). 
# Отже, в період із 2022-02-24 до 2023-09-03 змінна Долар не має варіації,
# тобто предиктор перестає бути випадковим.
# 
# Рішення.
# 1) Видалити цей часовий період
# АБО
# 2) Додати індикаторну змінну в модель: тоді модель знатиме, що в певний 
# період долар був зафіксований.


# -------POPULATION-------
# Перші кілька спостережень виглядають як викиди. Можливо, в той час було
# змінено методику підрахунку чисельності населення.
