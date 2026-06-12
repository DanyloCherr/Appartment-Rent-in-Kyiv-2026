na_number <- function(table){
  is_na <- is.na(table)
  return(list(sum(is_na), colSums(is_na), colMeans(is_na)))
}


import_validation <- function(tables){
  tables_with_nas <- 0
  for (table_name in names(tables)){
    cat("\n=============================\n")
    cat(table_name, "\n")
    current_df <- tables[[table_name]]
    print(head(current_df))
    print(tail(current_df))
    cat("rows:", nrow(current_df), "columns:", ncol(current_df), "\n")
    
    for (col_name in names(current_df)){
      col_type <- class(current_df[[col_name]])
      cat(col_name, "-", col_type, "")
    }
    
    cat("\n")
    na_info <- na_number(current_df)
    
    cat("NAs:", na_info[[1]], "\n") 
    print(na_info[[2]])
    print(na_info[[3]])
    
    if (na_info[1] > 0){
      tables_with_nas <- tables_with_nas + 1
    }
  }
  cat("\nDFs with NAs:", tables_with_nas)
}


all_summaries <- function(tables){
  for (table_name in names(tables)){
    cat("\n=============================\n")
    cat(table_name, "\n")
    print(summary(tables[[table_name]]))
  }
}


all_tables <- list(
  EUR = EUR,
  MULTIROOM = MULTIROOM,
  USD = USD,
  SGLROOM = SGLROOM,
  DBLROOM = DBLROOM,
  TRPROOM = TRPROOM,
  ACTIVITY = ACTIVITY,
  HH_DEBT_BURDEN = HH_DEBT_BURDEN,
  HOUSING_COMPLETIONS = HOUSING_COMPLETIONS,
  GPR_INDEX = GPR_INDEX[GPR_INDEX$Дата > "2008-01-01",],
  FS_INDEX = FS_INDEX,
  SECOND_HAND_HPRICE_INDEX = SECOND_HAND_HPRICE_INDEX,
  NEW_HPRICE_INDEX = NEW_HPRICE_INDEX,
  CP_INDEX = CP_INDEX,
  PRICE_INDICIES = PRICE_INDICIES,
  NET_UAH_LOANS = NET_UAH_LOANS,
  HH_INCOME_ESTIMATE = HH_INCOME_ESTIMATE,
  DOLLARIZATION_LEVEL = DOLLARIZATION_LEVEL,
  DOL_LEVEL_AVG = DOL_LEVEL_AVG,
  HH_DEPOSITS_ANNUAL_GROWTH = HH_DEPOSITS_ANNUAL_GROWTH,
  STOCK = STOCK,
  FLOW = FLOW,
  CONSUMER_CONFIDENCE = CONSUMER_CONFIDENCE,
  POPULATION = POPULATION,
  NET_UAH_LOANS_P = NET_UAH_LOANS_P
)


import_validation(all_tables)
# Пропущені значення мають лише ціни

MULTIROOM[!complete.cases(MULTIROOM), ]
# Бачимо, що всі NA-значення зосереджені в одних і тих самих рядках.
# Оскільки їх частка становить близько 0.64%, видалимо відповідні рядки.

MULTIROOM <- na.omit(MULTIROOM)
SGLROOM <- na.omit(SGLROOM)
DBLROOM <- na.omit(DBLROOM)
TRPROOM <- na.omit(TRPROOM)
all_tables$MULTIROOM <- na.omit(all_tables$MULTIROOM)
all_tables$SGLROOM <- na.omit(all_tables$SGLROOM)
all_tables$DBLROOM <- na.omit(all_tables$DBLROOM)
all_tables$TRPROOM <- na.omit(all_tables$TRPROOM)

PRICE_INDICIES[!complete.cases(PRICE_INDICIES), ]
PRICE_INDICIES <- na.omit(PRICE_INDICIES)
all_tables$PRICE_INDICIES <- na.omit(all_tables$PRICE_INDICIES)


import_validation(all_tables)

all_summaries(all_tables)

