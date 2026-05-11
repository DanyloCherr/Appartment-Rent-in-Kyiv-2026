get_csv_name <- function(filename){
  dir <- paste0("C:/Users/danil/R projects/Appartment Rent in Kyiv 2026/Data/", filename, ".csv")
  return(dir)
}


importf <- function(filename, date_format){
  table <- import(get_csv_name(filename))
  
  no_day_format <- grepl("^\\d{4}-\\d{1,2}$", table[1, 1])
  if(no_day_format){ # якщо дата не містить дня тижня, то дописуємо перше число
    table[, 1] <- paste0(table[, 1], "-01")
  }
  
  
  table[, 1] <- as.Date(table[, 1], format = date_format)
  table[, -1] <- sapply(table[, -1], function(x){
    x <- gsub(",", ".", x)
    x <- gsub("[^0-9.]", "", x)
    as.double(x)
  })
  
  return(table)
}


# ---Курс євро---
EUR <- importf("EUR", "%d.%m.%Y")

# Видалимо зайві (порожні) стовпці та встановимо коректний заголовок
EUR <- EUR[-1, -c(3,4)]
colnames(EUR) <- c("Дата", "Євро")

# Після видалення рядка змістимо нумерацію рядків, щоб номери йшли підряд від 1
rownames(EUR) <- NULL


# ---Курс долара, кімнатність---
MULTIROOM <- importf("Price, dollar", "%d.%m.%y")
colnames(MULTIROOM) <- c("Дата", "К1", "К2", "К3", "Долар")


# ---Курс долара---
USD <- MULTIROOM[, c(1, 5)]


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
ACTIVITY <- importf("Активність на ринку житла", "%d.%m.%Y")
colnames(ACTIVITY)[2] <- "Активність"


# ---Боргове навантаження домогосподарств---
HH_DEBT_BURDEN <- importf("Боргове навантаження домогосподарств", "%d.%m.%Y")
colnames(HH_DEBT_BURDEN)[2] <- "БоргНавантаж"


# ---Введення в експлуатацію житла---
HOUSING_COMPLITIONS <- importf("Введення в експлуатацію житла", "%d.%m.%Y")
colnames(HOUSING_COMPLITIONS)[2] <- "ВведЖитла"


# ---Індекс геополітичних ризиків---
GPR_INDEX <- importf("Індекс геополітичних ризиків", "%d.%m.%Y")
colnames(GPR_INDEX)[2] <- "ІГР"


# ---Індекс фінансового стресу---
FS_INDEX <- importf("Індекс фінансового стресу", "%d.%m.%Y")
colnames(FS_INDEX)[2] <- "ІФС"


# ---Індекс цін на житло на вторинному ринку---
SECOND_HAND_HPRICE_INDEX <- importf("Індекс цін на житло на вторинному ринку", "%d.%m.%Y")
colnames(SECOND_HAND_HPRICE_INDEX) <- c("Дата", "ІЦЖВ")


# ---Індекс цін на житло на вторинному ринку---
SECOND_HAND_HPRICE_INDEX <- importf("Індекс цін на житло на вторинному ринку", "%Y-%m-%d")
colnames(SECOND_HAND_HPRICE_INDEX) <- c("Дата", "ІЦЖВ")


# ---Індекс цін на житло на первинному ринку---
NEW_HPRICE_INDEX <- importf("Індекс цін на житло на первинному ринку", "%Y-%m-%d")
colnames(NEW_HPRICE_INDEX) <- c("Дата", "ІЦЖП")


# ---Індекс цін у будівництві---
CP_INDEX <- importf("Індекс цін у будівництві", "%d.%m.%Y")
colnames(CP_INDEX) <- c("Дата", "ІЦБ")


# ---Обсяг чистих гривневих кредитів---
NET_UAH_LOANS <- importf("Обсяг чистих гривневих кредитів", "%d.%m.%Y")
colnames(NET_UAH_LOANS) <- c("Дата", "ЧистГрнКред")


# ---Оцінка доходів населення---
HH_INCOME_ESTIMATE <- importf("Оцінка доходів населення", "%d.%m.%Y")
colnames(HH_INCOME_ESTIMATE) <- c("Дата", "ОцінкаДоходів")


# ---Рівень доларизації коштів фізосіб і бізнесу---
# Вимірюється у відсотках!
DOLLARIZATION_LEVEL <- importf("Рівень доларизації коштів фізосіб і бізнесу", "%d.%m.%Y")
colnames(DOLLARIZATION_LEVEL) <- c("Дата", "РівДолар")
DOLLARIZATION_LEVEL <- DOLLARIZATION_LEVEL[-1, -3]
rownames(DOLLARIZATION_LEVEL) <- NULL


# ---Річні темпи зміни коштів фізосіб---
# Вимірюється у відсотках!
HH_DEPOSITS_ANNUAL_GROWTH <- importf("Річні темпи зміни коштів фізосіб", "%d.%m.%Y")
colnames(HH_DEPOSITS_ANNUAL_GROWTH) <- c("Дата", "ТемпЗмінКошт")
HH_DEPOSITS_ANNUAL_GROWTH <- HH_DEPOSITS_ANNUAL_GROWTH[-1, c(1,2)]
rownames(HH_DEPOSITS_ANNUAL_GROWTH) <- NULL


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


# ---Чисельність населення---
POPULATION <- importf("Чисельність населення", "%d.%m.%Y")
colnames(POPULATION) <- c("Дата", "ЧисНасел") 
