library(rio)
library(lubridate)
library(dplyr)


get_csv_name <- function(filename){
  dir <- paste0(here(),"/Data", "/", filename, ".csv")
  return(dir)
}


importf <- function(filename, date_format){
  table <- import(get_csv_name(filename))
  
  # При виконанні source() функція перший елемент може повернутися як фактор (категорія), 
  # тому для надійності перетворимо його в текстовий символ.
  first_val <- as.character(table[1, 1])
  
  no_day_format <- grepl("^\\d{4}-\\d{1,2}$", first_val)
  if(no_day_format){ # якщо дата не містить дня тижня, то дописуємо перше число
    table[, 1] <- paste0(table[, 1], "-01")
  }
  
  table[, 1] <- as.Date(as.character(table[, 1]), format = date_format)
  table[, -1] <- sapply(table[, -1], function(x){
    x <- gsub(",", ".", x)
    x <- gsub("[^0-9.]", "", x)
    as.double(x)
  })
  
  return(table)
}


convert_quarters_to_dates <- function(filename) {
  data <- import(get_csv_name(filename))
  quarter_to_date <- function(q_string) {
    parts <- strsplit(q_string, "\\.")[[1]]
    quarter <- as.numeric(gsub("Q", "", parts[1]))
    year <- as.numeric(parts[2]) + 2000  # припускаємо 2000+
    
    # Перший місяць квартала
    month <- (quarter - 1) * 3 + 1
    
    as.Date(sprintf("%d-%02d-01", year, month))
  }
  
  data[[1]] <- as.Date(sapply(data[[1]], quarter_to_date))
  
  return(data)
}


convert_decimal_year_to_date <- function(filename) {
  data <- import(get_csv_name(filename))
  data[[1]] <- sapply(data[[1]], function(x) {
    year <- floor(x)
    month <- round((x - year) * 12) + 1
    if (month == 13) month <- 12
    as.Date(sprintf("%d-%02d-01", year, month))
  })
  
  data[[1]] <- as.Date(data[[1]], origin = "1970-01-01")
  
  return(data)
}


# ---Курс євро---
EUR <- importf("EUR", "%d.%m.%Y")
EUR[EUR$`Кількість одиниць` == 100, 7] <- round(EUR[EUR$`Кількість одиниць` == 100, 7] / 100, 2)
EUR <- EUR[, c(1, 7)]
colnames(EUR) <- c("Дата", "Євро")


# ---Курс долара---
USD <- importf("USD", "%d.%m.%Y")
USD[USD$`Кількість одиниць` == 100, 7] <- round(USD[USD$`Кількість одиниць` == 100, 7] / 100, 2)
USD <- USD[, c(1, 7)]
colnames(USD) <- c("Дата", "Долар")


# ---Ціни на квартири---
MULTIROOM <- importf("Price, dollar", "%Y-%m-%d")
MULTIROOM <- MULTIROOM[, -5]
colnames(MULTIROOM) <- c("Дата", "К1", "К2", "К3")


# ---1-кімнатні квартири---
SGLROOM <- MULTIROOM[, c(1, 2)]
colnames(SGLROOM)[2] <- "Ціна"


# ---2-кімнатні квартири---
DBLROOM <- MULTIROOM[, c(1, 3)]
colnames(DBLROOM)[2] <- "Ціна"


# ---3-кімнатні квартири---
TRPROOM <- MULTIROOM[, c(1, 4)]
colnames(TRPROOM)[2] <- "Ціна"


# ---Активність на ринку житла---
ACTIVITY <- convert_quarters_to_dates("Активність на ринку житла")
ACTIVITY <- ACTIVITY[, -2]
ACTIVITY[, 2] <- as.numeric(gsub(",", ".", ACTIVITY[, 2]))
colnames(ACTIVITY)[2] <- "Активність"


# ---Боргове навантаження домогосподарств---
HH_DEBT_BURDEN <- importf("Боргове навантаження домогосподарств", "%d.%m.%Y")
colnames(HH_DEBT_BURDEN)[2] <- "БоргНавантаж"


# ---Введення в експлуатацію житла---
HOUSING_COMPLETIONS <- convert_quarters_to_dates("Введення в експлуатацію житла")
HOUSING_COMPLETIONS[, 2] <- as.numeric(gsub(",", ".", HOUSING_COMPLETIONS[, 2]))
HOUSING_COMPLETIONS[, 3] <- as.numeric(gsub(",", ".", HOUSING_COMPLETIONS[, 3]))
HOUSING_COMPLETIONS$ВведЖитла <- HOUSING_COMPLETIONS$`Багатоквартирне житло` + HOUSING_COMPLETIONS$`Одноквартирні будинки та гуртожитки`
HOUSING_COMPLETIONS <- HOUSING_COMPLETIONS[, c(1, 4)]
colnames(HOUSING_COMPLETIONS)[1] <- "Дата"


# ---Індекс геополітичних ризиків---
GPR_INDEX <- convert_decimal_year_to_date("Індекс геополітичних ризиків")
colnames(GPR_INDEX) <- c("Дата", "ІГР")


# ---Індекс фінансового стресу---
FS_INDEX <- importf("Індекс фінансового стресу", "%d.%m.%Y")
FS_INDEX[, 2] <- as.numeric(gsub(",", ".", FS_INDEX[, 2]))
colnames(FS_INDEX) <- c("Дата", "ІФС")


# ---Індекс цін на житло на вторинному ринку---
SECOND_HAND_HPRICE_INDEX <- importf("Індекс цін на житло на вторинному ринку", "%Y-%m-%d")
colnames(SECOND_HAND_HPRICE_INDEX) <- c("Дата", "ІЦЖВ")


# ---Індекс цін на житло на первинному ринку---
NEW_HPRICE_INDEX <- importf("Індекс цін на житло на первинному ринку", "%Y-%m-%d")
colnames(NEW_HPRICE_INDEX) <- c("Дата", "ІЦЖП")


# ---Індекс цін у будівництві---
CP_INDEX <- importf("Індекс цін у будівництві", "%d.%m.%Y")
colnames(CP_INDEX) <- c("Дата", "ІЦБ")


# ---Індекси цін (щомісячні, для другого періоду)---
PRICE_INDICIES <- importf("Індекси цін у будівництві та на житло", "%d.%m.%Y")
PRICE_INDICIES[, 1] <- floor_date(PRICE_INDICIES[, 1], "month")
PRICE_INDICIES[, 2] <- as.numeric(gsub(",", ".", PRICE_INDICIES[, 2]))
PRICE_INDICIES[, 3] <- as.numeric(gsub(",", ".", PRICE_INDICIES[, 3]))
PRICE_INDICIES[, 4] <- as.numeric(gsub(",", ".", PRICE_INDICIES[, 4]))
colnames(PRICE_INDICIES) <- c("Дата", "ІЦБ", "ІЦЖП", "ІЦЖВ")


# ---Обсяг чистих гривневих кредитів---
NET_UAH_LOANS <- importf("Обсяг чистих гривневих кредитів", "%d.%m.%Y")
NET_UAH_LOANS[, 2] <- as.numeric(gsub(",", ".", NET_UAH_LOANS[, 2]))
colnames(NET_UAH_LOANS) <- c("Дата", "ЧистГрнКред")
NET_UAH_LOANS[[1]] <- floor_date(NET_UAH_LOANS[[1]], "month")


# ---Оцінка доходів населення---
HH_INCOME_ESTIMATE <- importf("Оцінка доходів населення", "%d.%m.%Y")
colnames(HH_INCOME_ESTIMATE) <- c("Дата", "ОцінкаДоходів")


# ---Рівень доларизації коштів фізосіб і бізнесу---
# Вимірюється у відсотках!
DOLLARIZATION_LEVEL <- importf("Рівень доларизації коштів фізосіб і бізнесу", "%d.%m.%Y")
colnames(DOLLARIZATION_LEVEL) <- c("Дата", "РівДолар")
DOLLARIZATION_LEVEL <- DOLLARIZATION_LEVEL[-1, -3]
rownames(DOLLARIZATION_LEVEL) <- NULL


# ---Рівень валютизації коштів фізосіб і бізнесу (для другого періода)---
DOL_LEVEL_AVG <- importf("Частка валютних коштів", "%d.%m.%Y")
DOL_LEVEL_AVG[, 2] <- as.numeric(gsub(",", ".", DOL_LEVEL_AVG[, 2]))
DOL_LEVEL_AVG[, 3] <- as.numeric(gsub(",", ".", DOL_LEVEL_AVG[, 3]))
DOL_LEVEL_AVG$РівДолар <- (DOL_LEVEL_AVG[, 2] + DOL_LEVEL_AVG[, 3]) / 2
DOL_LEVEL_AVG <- DOL_LEVEL_AVG[, c(1, 4)]
colnames(DOL_LEVEL_AVG) <- c("Дата", "РівДолар")

cor(merge(DOL_LEVEL_AVG, DOLLARIZATION_LEVEL, by = "Дата")[, -1])
# РівДолар.x РівДолар.y
# РівДолар.x  1.0000000  0.9971017
# РівДолар.y  0.9971017  1.0000000
# Отже, змінні статистично однакові. Можна не переживати.


# ---Річні темпи зміни коштів фізосіб---
# Вимірюється у відсотках!
HH_DEPOSITS_ANNUAL_GROWTH <- importf("Річні темпи зміни коштів фізосіб", "%d.%m.%Y")
colnames(HH_DEPOSITS_ANNUAL_GROWTH) <- c("Дата", "ТемпЗмінКошт")
HH_DEPOSITS_ANNUAL_GROWTH[[1]] <- floor_date(HH_DEPOSITS_ANNUAL_GROWTH[[1]], "month")
HH_DEPOSITS_ANNUAL_GROWTH[[2]] <- as.numeric(gsub(",", ".", HH_DEPOSITS_ANNUAL_GROWTH[, 2]))


# ---Розмір компенсації від держави---
# Вимірюється у відсотках!
# Таблиця містить дві величини
STATE_COMPENS_AMOUNT <- importf("Розмір компенсації від держави", "%d.%m.%Y")
STATE_COMPENS_AMOUNT <- STATE_COMPENS_AMOUNT[-1, -2]
colnames(STATE_COMPENS_AMOUNT) <- c("Дата", "КредПортфель", "НовіКредити")

STOCK <- STATE_COMPENS_AMOUNT[, c(1, 2)]
FLOW <- STATE_COMPENS_AMOUNT[, c(1, 3)]


# ---Споживчі настрої домогосподарств---
CONSUMER_CONFIDENCE <- importf("Споживчі настрої домогосподарств", "%d.%m.%Y")
colnames(CONSUMER_CONFIDENCE) <- c("Дата", "ІндМатСтан") # Індекс поточного особистого матеріального становища
CONSUMER_CONFIDENCE[[1]] <- floor_date(CONSUMER_CONFIDENCE[[1]], "month")
CONSUMER_CONFIDENCE[[2]] <- as.numeric(gsub(",", ".", CONSUMER_CONFIDENCE[, 2]))


# ---Чисельність населення---
POPULATION <- importf("Чисельність населення", "%d.%m.%Y")
colnames(POPULATION) <- c("Дата", "ЧисНасел") 
# Хоч спостереження чисельності населення і записані як щоденні, проте їх значення 
# є однаковими для всіх днів місяця. Тобто дані оцінюються щомісячно.
POPULATION <- as.data.frame(
  POPULATION %>%
  mutate(month = format(Дата, "%Y-%m")) %>%
  group_by(month) %>%
  slice(1) %>%
  ungroup() %>%
  dplyr::select(-month)
  )
# Також екстраполюємо дані за останній тиждень листопада 2015 р. на весь місяць.
POPULATION[1, 1] <- as.Date("2015-11-01", format = "%Y-%m-%d")
