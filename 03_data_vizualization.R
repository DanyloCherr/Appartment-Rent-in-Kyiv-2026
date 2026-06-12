library(ggplot2)
library(patchwork)


combined_plots <- function(tables, highlight_df = NULL) {
  for (table_name in names(tables)) {
    current_df <- tables[[table_name]]
    
    x_col <- colnames(current_df)[1]
    y_col <- colnames(current_df)[2]
    p1 <- ggplot(current_df, aes(x = .data[[x_col]], y = .data[[y_col]])) +
      geom_path(color = "steelblue") + 
      labs(title = table_name,
           x = "",
           y = colnames(current_df)[2]) +
      theme_minimal()
    
    obs_num <- nrow(current_df)
    if(obs_num < 70){
      p1 <- p1 + geom_point(color = "red", size = 1.25)
    }
    
    if (!is.null(highlight_df)) {
      highlight_dates <- highlight_df[[1]]  # перший стовпець — дати
      highlight_data <- current_df[current_df[[x_col]] %in% highlight_dates, ]
      
      if (nrow(highlight_data) > 0) {
        p1 <- p1 + geom_point(data = highlight_data, 
                              aes(x = .data[[x_col]], y = .data[[y_col]]),
                              color = "purple", size = 2, shape = 18)
      }
    }
    
    p2 <- ggplot(current_df, aes(y = .data[[y_col]])) +
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


get_split_periods <- function(all_tables, min_obs = 10, date = "2022-02-01"){ # Чи нормально взяти за мінімум 10?
  data1 <- list()
  data2 <- list()
  
  for(table_name in names(all_tables)){
    split_data <- split_df_byDate(all_tables[[table_name]], date)
    if(nrow(split_data$before) >= min_obs){ 
      data1[[table_name]] <- split_data$before
    }
    if(nrow(split_data$after) >= min_obs){
      data2[[table_name]] <- split_data$after
    }
  }
  result <- list(
    period1 = data1,
    period2 = data2
  )
  return(result)
}
  

### Дані за весь період із 2015 до 2026.
cat("Всього таблиць:", length(all_tables)) # Пам'ятаймо, що дві таблиці - для другого періода.

combined_plots(all_tables[!names(all_tables) %in% c("MULTIROOM", "PRICE_INDICIES")])


# =======ПОТЕНЦІЙНІ ПРОБЛЕМИ=======
# Із графіків видно, що всі дані варто розділити: до лютого 2022 року та після.

splt_data <- get_split_periods(all_tables, date = "2022-01-01") # UPD
data1 <- splt_data$period1
data1 <- data1[!names(data1) %in% c("DOL_LEVEL_AVG", "NET_UAH_LOANS_P")] # Середнє рівня валютизації - для 2 періода.
for(name in names(data1)){ # UPD
  data1[[name]] <- data1[[name]][data1[[name]]$Дата >= "2016-01-01", ]
}

data2 <- splt_data$period2

### data1 (перший період)
cat("Змінні, що залишились в data1:", "\n", paste(names(data1), collapse = "\n"), 
    "\n", "Всього:", length(data1))
# Випало 6 змінних.

if(is_markdown){
  combined_plots(data1[names(data1) != "MULTIROOM"])
}


# =====TRPROOM=====
# Викиди на межі 2022 року?

# =====GPR_INDEX=====
# Викиди на межі 2022 року?

# =====FS_INDEX=====
# Початок виглядає підозріло.

# =====POPULATION=====
# Перші кілька спостережень виглядають як викиди. Можливо, в той час було
# змінено методику підрахунку чисельності населення.



### data2 (другий період)
cat("Змінні, що залишились в data2:", "\n", paste(names(data2), collapse = "\n"), 
    "\n", "Всього:", length(data2))
# Випала 1 змінна (чисельність населення).


if(!is_markdown){
  combined_plots(data2[names(data2) != "MULTIROOM"])
}


# =====USD=====
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

